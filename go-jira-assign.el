;;; go-jira-assign.el --- Assignee switching for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (consult "1.0"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Assignee switching and assignee initials badges for go-jira.
;;
;; The users assignable to an issue are permission- and project-specific, so
;; they are fetched per-issue from Jira.  The picker offers "Assign to me"
;; first, flipping to "None (unassign)" when the issue is already assigned to
;; the current user.

;;; Code:

(require 'color)
(require 'consult)
(require 'json)
(require 'seq)
(require 'svg)

(declare-function go-jira--find-exe "go-jira")
(declare-function go-jira-view-ticket "go-jira")
(declare-function go-jira-board-refresh "go-jira-board")
(declare-function go-jira--current-ticket "go-jira-status")
(declare-function go-jira--read-issue-key "go-jira-status")

(defvar go-jira--ticket-number)

;;; REST helpers

(defun go-jira--current-user ()
  "Return the current Jira user as a plist of :account-id and :display-name."
  (let* ((j (go-jira--find-exe))
         (cmd (format "%s request '/rest/api/2/myself' --method GET" j))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output)))
          (list :account-id (gethash 'accountId parsed)
                :display-name (gethash 'displayName parsed)))
      (error
       (error "Failed to fetch current Jira user: %s\nOutput: %s"
              (error-message-string err) output)))))

(defun go-jira--issue-assignee (key)
  "Return issue KEY's current assignee as a plist, or nil when unassigned.
The plist has :account-id and :display-name."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/api/2/issue/%s?fields=assignee" key))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (fields (gethash 'fields parsed))
               (assignee (when fields (gethash 'assignee fields))))
          (when assignee
            (list :account-id (gethash 'accountId assignee)
                  :display-name (gethash 'displayName assignee))))
      (error
       (error "Failed to fetch assignee for %s: %s\nOutput: %s"
              key (error-message-string err) output)))))

(defun go-jira--parse-assignable-users (users)
  "Parse USERS, the assignable-search response (a list of hash-tables).
Returns a list of plists with :account-id, :display-name and :email."
  (mapcar
   (lambda (u)
     (list :account-id (gethash 'accountId u)
           :display-name (gethash 'displayName u)
           :email (gethash 'emailAddress u)))
   users))

(defun go-jira--fetch-assignable-users (key)
  "Fetch users assignable to issue KEY.
Returns a list of user plists (see `go-jira--parse-assignable-users')."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/api/2/user/assignable/search?issueKey=%s&maxResults=1000" key))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output)))
          (go-jira--parse-assignable-users parsed))
      (error
       (error "Failed to fetch assignable users for %s: %s\nOutput: %s"
              key (error-message-string err) output)))))

(defun go-jira--apply-assignee (key account-id)
  "Assign issue KEY to the user with ACCOUNT-ID.
A nil ACCOUNT-ID unassigns the issue.  Signals an error on failure, returns
non-nil on success."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "rest/api/2/issue/%s/assignee" key))
         ;; Bind `json-null' so `:null' always encodes as JSON null regardless
         ;; of the caller's global json settings.
         (json-null :null)
         (payload (json-encode `(:accountId ,(or account-id json-null)))))
    (with-temp-buffer
      (let ((exit-code (call-process j nil (current-buffer) nil
                                     "request" endpoint payload
                                     "--method" "PUT"))
            (result (buffer-string)))
        (if (or (not (zerop exit-code))
                (string-match-p "\"errorMessages\"\\|\"errors\"" result))
            (error "Failed to assign %s: %s" key (string-trim result))
          t)))))

;;; Candidates

(defun go-jira--assignee-candidates (users me current-assignee table)
  "Build assignee completion candidates in TABLE, returning display strings.
USERS is the list of assignable user plists, ME the current user plist and
CURRENT-ASSIGNEE the issue's assignee plist (or nil).  The first candidate is
\"Assign to me\", flipping to \"None (unassign)\" when the issue is already
assigned to ME.  The rest are the assignable users, excluding ME, sorted by
display name.  Each display string maps in TABLE to an action plist with
:account-id and :label."
  (let* ((my-id (plist-get me :account-id))
         (assigned-to-me (and current-assignee
                              (equal (plist-get current-assignee :account-id) my-id)))
         (first-label (if assigned-to-me "None (unassign)" "Assign to me"))
         (first-action (if assigned-to-me
                           (list :account-id nil :label "Unassigned")
                         (list :account-id my-id :label (plist-get me :display-name))))
         (others (thread-last
                   users
                   (seq-remove (lambda (u) (equal (plist-get u :account-id) my-id)))
                   (seq-sort-by (lambda (u) (or (plist-get u :display-name) ""))
                                #'string-lessp)))
         candidates)
    (puthash first-label first-action table)
    (push first-label candidates)
    (dolist (u others)
      (let* ((name (or (plist-get u :display-name) ""))
             (email (plist-get u :email))
             (display (if (and email (not (string-empty-p email)))
                          (format "%s <%s>" name email)
                        name)))
        (puthash display (list :account-id (plist-get u :account-id) :label name) table)
        (push display candidates)))
    (nreverse candidates)))

;;; Badges

(defun go-jira--initials (name)
  "Return the upcased first letters of the first and last words of NAME.
Return nil when NAME has no letters."
  (when-let* ((words (split-string (or name "") "[^[:alpha:]]+" t)))
    (upcase (concat (substring (car words) 0 1)
                    (when (cdr words) (substring (car (last words)) 0 1))))))

(defun go-jira--assignee-color (name)
  "Return a background color for assignee NAME, the same in every session.
The name picks the hue.  The CIELAB lightness is fixed, so white initials
read equally well on every hue."
  ;; md5, because `sxhash-equal' values may change between Emacs versions.
  (let* ((hue (degrees-to-radians
               (mod (string-to-number (substring (md5 name nil nil 'utf-8) 0 6) 16)
                    360)))
         (rgb (color-lab-to-srgb 50 (* 45 (cos hue)) (* 45 (sin hue)))))
    (apply #'color-rgb-to-hex (append (mapcar #'color-clamp rgb) '(2)))))

(defun go-jira--assignee-badge (name)
  "Return the initials of assignee NAME as a rounded SVG label, or nil.
Text terminals and Emacs builds without SVG support get nil.  The sizes
follow the default font, so the label fits inside the heading line."
  (when-let* (((display-graphic-p))
              ((image-type-available-p 'svg))
              (initials (go-jira--initials name)))
    (let* ((ch (frame-char-height))
           (h (round (* 0.8 ch)))
           (font-size (round (* 0.55 ch)))
           (pad (round (* 0.3 ch)))
           ;; 0.62 em approximates the advance width of a monospace glyph.
           (w (+ (* 2 pad) (* (length initials) (ceiling (* 0.62 font-size)))))
           (svg (svg-create w h)))
      (svg-rectangle svg 0 0 w h :rx 4 :fill (go-jira--assignee-color name))
      ;; y is the text baseline; 0.36 em below the middle centers capitals.
      (svg-text svg initials
                :x (/ w 2.0) :y (+ (/ h 2.0) (* 0.36 font-size))
                :text-anchor "middle" :font-size font-size :font-weight "bold"
                :font-family (face-attribute 'default :family) :fill "white")
      (propertize initials
                  'display (svg-image svg :ascent 'center)
                  'help-echo name))))

(defun go-jira--put-assignee-badge (beg end name)
  "Display the badge of assignee NAME right after the text from BEG to END.
An overlay holds the badge, so the heading text, which `go-jira-edit' sends
back as the summary, stays clean.  Erasing that text deletes the overlay."
  (when-let* ((badge (go-jira--assignee-badge name)))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'after-string (concat badge " ")))))

;;; Context

(defun go-jira--refresh-after-assign (key)
  "Refresh the current view after reassigning issue KEY.
Re-fetches the board or re-renders the ticket view so the assignee shown
reflects the change; does nothing in other buffers."
  (cond
   ((derived-mode-p 'go-jira-board-view-mode)
    (go-jira-board-refresh))
   ((derived-mode-p 'go-jira-view-mode)
    (go-jira-view-ticket key))))

;;;###autoload
(defun go-jira-assign (&optional key)
  "Assign a Jira issue to a user.
Interactively KEY is read from the minibuffer, defaulting to the issue in the
current context (a view buffer, board entry, or ticket at point); as an Embark
action the target ticket is injected into that prompt.  Presents the users
assignable to the issue, offering \"Assign to me\" first, or \"None\" to
unassign when the issue is already assigned to you."
  (interactive (list (go-jira--read-issue-key)))
  (let ((key (or key (go-jira--current-ticket) (read-string "Jira issue: "))))
    (when (string-empty-p (string-trim (or key "")))
      (user-error "No Jira issue specified"))
    (let* ((me (go-jira--current-user))
           (current-assignee (go-jira--issue-assignee key))
           (users (go-jira--fetch-assignable-users key)))
      (unless users
        (user-error "No assignable users for %s" key))
      (let* ((table (make-hash-table :test 'equal))
             (candidates (go-jira--assignee-candidates users me current-assignee table))
             (choice (consult--read
                      candidates
                      :prompt (format "Assign %s to: " key)
                      :sort nil
                      :require-match t
                      :category 'jira-user))
             (action (and choice (gethash choice table))))
        (when action
          (go-jira--apply-assignee key (plist-get action :account-id))
          (message "%s \u2192 %s" key (plist-get action :label))
          (go-jira--refresh-after-assign key))))))

(provide 'go-jira-assign)
;;; go-jira-assign.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
