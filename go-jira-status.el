;;; go-jira-status.el --- Workflow status switching for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (consult "1.0"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Status (workflow transition) support for go-jira.
;;
;; The valid next states of an issue are workflow-specific, so they are
;; fetched per-issue from Jira rather than modelled locally.  Colors are
;; derived from Jira's own status category (new/indeterminate/done), which
;; every status in every workflow rolls up to, with an optional per-status
;; override for finer distinctions.

;;; Code:

(require 'consult)
(require 'json)

(declare-function go-jira--find-exe "go-jira")
(declare-function go-jira--ticket-arg-or-ticket-at-point "go-jira")
(declare-function go-jira-view-ticket "go-jira")
(declare-function go-jira-board-refresh "go-jira-board")
(declare-function go-jira-eldoc--cache-invalidate "go-jira-eldoc")
(declare-function org-entry-properties "org")

(defvar go-jira--ticket-number)

;;; Faces

(defface go-jira-status-todo-face
  '((t :inherit font-lock-keyword-face))
  "Face for Jira statuses in the To Do category (statusCategory key \"new\")."
  :group 'go-jira)

(defface go-jira-status-in-progress-face
  '((t :inherit warning))
  "Face for Jira statuses in the In Progress category (key \"indeterminate\")."
  :group 'go-jira)

(defface go-jira-status-done-face
  '((t :inherit success))
  "Face for Jira statuses in the Done category (statusCategory key \"done\")."
  :group 'go-jira)

(defface go-jira-status-default-face
  '((t :inherit default))
  "Face for Jira statuses with an unknown or missing category."
  :group 'go-jira)

(defcustom go-jira-status-face-alist nil
  "Alist mapping exact Jira status names to faces, overriding category colors.
Keys are matched case-insensitively against the status name.  Values are
faces (a face symbol or a plist of face attributes).  When a status name is
not listed here, its color is derived from the Jira status category via
`go-jira--status-category->face'."
  :type '(alist :key-type string :value-type sexp)
  :group 'go-jira)

;;; Color resolution

(defun go-jira--status-category->face (category-key)
  "Return the face for a Jira status CATEGORY-KEY.
CATEGORY-KEY is the `statusCategory.key' string, one of \"new\",
\"indeterminate\" or \"done\"."
  (pcase category-key
    ("new" 'go-jira-status-todo-face)
    ("indeterminate" 'go-jira-status-in-progress-face)
    ("done" 'go-jira-status-done-face)
    (_ 'go-jira-status-default-face)))

(defun go-jira--status-face (status-name category-key)
  "Return the face for a Jira status.
STATUS-NAME is looked up in `go-jira-status-face-alist' case-insensitively;
when absent, the face falls back to the one for CATEGORY-KEY."
  (or (and status-name
           (cdr (assoc-string status-name go-jira-status-face-alist t)))
      (go-jira--status-category->face category-key)))

(defun go-jira--fontify-status-keys (limit)
  "Fontify issue keys carrying the `jira-status-key' property up to LIMIT.
Each such region is given the face stored in that property, so status color
survives Org's own heading fontification."
  (let ((pos (point)))
    (while (< pos limit)
      (if-let* ((face (get-text-property pos 'jira-status-key)))
          (let ((end (or (next-single-property-change pos 'jira-status-key nil limit)
                         limit)))
            (put-text-property pos end 'face face)
            (setq pos end))
        (setq pos (or (next-single-property-change pos 'jira-status-key nil limit)
                      limit))))
    nil))

;;; Transitions

(defun go-jira--parse-transitions (parsed)
  "Parse transitions from PARSED, a hash-table from the transitions endpoint.
Returns a list of plists with :id, :name, :to-name and :category-key."
  (mapcar
   (lambda (tr)
     (let* ((to (gethash 'to tr))
            (category (when to (gethash 'statusCategory to))))
       (list :id (gethash 'id tr)
             :name (gethash 'name tr)
             :to-name (when to (gethash 'name to))
             :category-key (when category (gethash 'key category)))))
   (gethash 'transitions parsed)))

(defun go-jira--fetch-transitions (key)
  "Fetch the workflow transitions available for issue KEY.
Returns a list of transition plists (see `go-jira--parse-transitions')."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/api/2/issue/%s/transitions" key))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output)))
          (go-jira--parse-transitions parsed))
      (error
       (error "Failed to fetch transitions for %s: %s\nOutput: %s"
              key (error-message-string err) output)))))

(defun go-jira--apply-transition (key transition-id)
  "Transition issue KEY using TRANSITION-ID.
Signals an error on failure, returns non-nil on success."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "rest/api/2/issue/%s/transitions" key))
         (payload (json-encode `(:transition (:id ,transition-id)))))
    (with-temp-buffer
      (let ((exit-code (call-process j nil (current-buffer) nil
                                     "request" endpoint payload
                                     "--method" "POST"))
            (result (buffer-string)))
        (if (or (not (zerop exit-code))
                (string-match-p "\"errorMessages\"\\|\"errors\"" result))
            (error "Failed to transition %s: %s" key (string-trim result))
          t)))))

;;; Context resolution

(defun go-jira--current-ticket ()
  "Resolve the Jira issue key for the current context, or nil.
Prefers the buffer-local ticket of a view buffer, then the ISSUE_KEY
property of the Org entry at point, then a ticket symbol at point."
  (or (bound-and-true-p go-jira--ticket-number)
      (and (derived-mode-p 'org-mode)
           (cdr (assoc "ISSUE_KEY" (org-entry-properties))))
      (go-jira--ticket-arg-or-ticket-at-point)))

(defun go-jira--refresh-after-transition (key)
  "Refresh the current view after transitioning issue KEY.
Re-fetches the board (so a card moves to its new column) or re-renders the
ticket view; does nothing in other buffers."
  (cond
   ((derived-mode-p 'go-jira-board-view-mode)
    (go-jira-board-refresh))
   ((derived-mode-p 'go-jira-view-mode)
    (go-jira-view-ticket key))))

(defun go-jira--invalidate-issue-cache (key)
  "Drop cached data for issue KEY after its status changed.
Clears the eldoc/popup cache entry (when that feature is loaded) so a hover
reflects the new status immediately instead of the stale color."
  (when (fboundp 'go-jira-eldoc--cache-invalidate)
    (go-jira-eldoc--cache-invalidate key)))

(defun go-jira--transition-candidates (transitions table)
  "Build colored completion candidates for TRANSITIONS.
Each candidate is stored in hash-table TABLE mapping its display string to
the transition plist.  Returns the list of display strings."
  (mapcar
   (lambda (tr)
     (let* ((to-name (or (plist-get tr :to-name) (plist-get tr :name)))
            (tr-name (plist-get tr :name))
            (face (go-jira--status-face to-name (plist-get tr :category-key)))
            (label (if (and tr-name to-name
                            (not (string-equal-ignore-case tr-name to-name)))
                       (format "%s (%s)" to-name tr-name)
                     to-name))
            (display (propertize label 'face face)))
       (puthash display tr table)
       display))
   transitions))

(defun go-jira--read-issue-key ()
  "Read a Jira issue key, defaulting to the current context.
Reading the key here (rather than resolving it silently) is what lets an
Embark action inject its target into this prompt.  Embark injects only into
an action's first minibuffer read, so keeping that read here keeps it out of
the later transitions picker."
  (let ((default (go-jira--current-ticket)))
    (read-string (format-prompt "Jira issue" default) nil nil default)))

;;;###autoload
(defun go-jira-change-status (&optional key)
  "Change the workflow status of a Jira issue.
Interactively KEY is read from the minibuffer, defaulting to the issue in the
current context (a view buffer, board entry, or ticket at point); as an
Embark action the target ticket is injected into that prompt.  Presents the
transitions valid from the issue's current state (colored by status category)
and applies the choice."
  (interactive (list (go-jira--read-issue-key)))
  (let ((key (or key (go-jira--current-ticket) (read-string "Jira issue: "))))
    (when (string-empty-p (string-trim (or key "")))
      (user-error "No Jira issue specified"))
    (let ((transitions (go-jira--fetch-transitions key)))
      (unless transitions
        (user-error "No available transitions for %s" key))
      (let* ((table (make-hash-table :test 'equal))
             (candidates (go-jira--transition-candidates transitions table))
             (choice (consult--read
                      candidates
                      :prompt (format "Transition %s to: " key)
                      :sort nil
                      :require-match t
                      :category 'jira-transition))
             (tr (and choice (gethash choice table))))
        (when tr
          (go-jira--apply-transition key (plist-get tr :id))
          (go-jira--invalidate-issue-cache key)
          (message "%s \u2192 %s" key (or (plist-get tr :to-name) (plist-get tr :name)))
          (go-jira--refresh-after-transition key))))))

(provide 'go-jira-status)
;;; go-jira-status.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
