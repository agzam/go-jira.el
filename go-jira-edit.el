;;; go-jira-edit.el --- Edit Jira issues (title, description, comments) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides editing UI for Jira issues using `header-line-format'
;; for always-visible edit controls, plus JSON templates and custom
;; editor scripts to submit changes.
;;
;; Features:
;; - Edit title, description, and comments
;; - Persistent header-line indicator visible at any scroll position
;; - Org-mode markup support (converted to Jira markup on submit)
;; - Universal JSON-based submission flow
;; - Works in go-jira-view-mode and go-jira-board-view-mode

;;; Code:

(require 'go-jira-comment)
(require 'go-jira-markup)
(require 'cl-lib)
(require 'org)
(require 'json)

(declare-function go-jira--find-exe "go-jira")
(declare-function go-jira-view-mode-refresh "go-jira")
(declare-function go-jira-board-refresh "go-jira-board")

(defgroup go-jira-edit nil
  "Universal editing functionality for go-jira."
  :group 'go-jira
  :prefix "go-jira-edit-")

(defcustom go-jira-debug nil
  "Enable debug output for go-jira operations."
  :type 'boolean
  :group 'go-jira-edit)

;;; Edit session management

(defvar-local go-jira-edit--active-overlay nil
  "Non-nil when an edit session is active in this buffer.
Stores the overlay object for backward compatibility, but the visible
indicator is now shown via `header-line-format'.")

(defvar-local go-jira-edit--saved-header-line nil
  "Saved `header-line-format' to restore when edit mode is deactivated.")

(defvar go-jira-edit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map to get all org keybindings
    (set-keymap-parent map org-mode-map)
    ;; Add our specific bindings
    (define-key map (kbd "C-c C-c") #'go-jira-edit-submit)
    (define-key map (kbd "C-c C-k") #'go-jira-edit-abort)
    (define-key map (kbd "C-c M-c") #'go-jira-comment-add)
    (define-key map (kbd "C-c C-r") #'go-jira-comment-reply)
    map)
  "Keymap active in description edit overlay region.
Inherits from org-mode-map, overrides submit and abort.")

(define-minor-mode go-jira-edit-mode
  "Minor mode active in the description edit overlay."
  :lighter " JiraDesc"
  :keymap go-jira-edit-mode-map)

(defun go-jira-edit--ticket-heading-start (&optional start)
  "Find the actual ticket heading position.
START is the beginning of the region to search (default `point-min').
Moves to the first Org heading at or after START."
  (save-excursion
    (goto-char (or start (point-min)))
    (unless (org-at-heading-p)
      (re-search-forward "^\\*+ " nil t)
      (goto-char (line-beginning-position)))
    (point)))

(defun go-jira-edit--extract-title (&optional context)
  "Extract the ticket title.
CONTEXT is a plist with ticket bounds.  If nil, uses current context."
  (let ((ctx (or context (go-jira-edit--get-ticket-context))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (go-jira-edit--ticket-heading-start (plist-get ctx :start)))
        (let* ((element (org-element-at-point))
               (raw-value (org-element-property :raw-value element)))
          ;; Strip the "KEY: " prefix to get just the title
          (when (and raw-value (string-match "^[A-Z]+-[0-9]+:?\\s-*\\(.+\\)" raw-value))
            (match-string 1 raw-value)))))))

(defun go-jira-edit--extract-description (&optional context)
  "Extract the description content as Org-mode text.
CONTEXT is a plist with ticket bounds.  If nil, uses current context."
  (let* ((ctx (or context (go-jira-edit--get-ticket-context)))
         (start (plist-get ctx :start))
         (end (plist-get ctx :end))
         (level (plist-get ctx :level))
         (desc-level (+ level 1)))  ; Description is one level below ticket
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (go-jira-edit--ticket-heading-start start))
        ;; Find Description heading within ticket bounds
        (if (re-search-forward
             (format "^\\*\\{%d\\} Description[ \t]*$" desc-level)
             end t)
            (progn
              (goto-char (line-beginning-position))
              (let* ((element (org-element-at-point))
                     (contents-begin (org-element-property :contents-begin element))
                     (contents-end (org-element-property :contents-end element)))
                (if (and contents-begin contents-end)
                    (go-jira-markup-shift-headings
                     (string-trim (buffer-substring contents-begin contents-end))
                     (- desc-level))
                  "")))
          ;; No description found - return empty string
          "")))))

(defun go-jira-edit--create-overlay (context)
  "Start an edit session for the ticket specified by CONTEXT.
CONTEXT is a plist with :key, :start, :end from
`go-jira-edit--get-ticket-context'.  Shows a persistent header line
with submit/abort controls that remains visible regardless of scroll
position."
  (when go-jira-edit--active-overlay
    (user-error "Already editing"))

  (let ((ticket-key (plist-get context :key))
        (ticket-start (plist-get context :start)))
    (unless ticket-key
      (user-error "Could not determine ticket key"))

    ;; Handle narrowing for board mode FIRST
    (when (derived-mode-p 'go-jira-board-view-mode)
      (goto-char ticket-start)
      (org-narrow-to-subtree)
      ;; CRITICAL: Disable org-element caching entirely in narrowed buffer
      ;; The cache contains absolute positions that break after narrowing
      (setq-local org-element-use-cache nil)
      (when (fboundp 'org-element-cache-reset)
        (org-element-cache-reset)))

    ;; Create a zero-width overlay to serve as the edit session token.
    ;; It stores ticket metadata and original values but doesn't modify
    ;; buffer content.  The visible indicator is the header line.
    (let ((ov (make-overlay (point-min) (point-min) nil nil t)))
      (overlay-put ov 'go-jira-edit-edit t)
      (overlay-put ov 'go-jira-ticket ticket-key)
      (setq go-jira-edit--active-overlay ov))

    ;; Show persistent header line with edit instructions
    (setq go-jira-edit--saved-header-line header-line-format)
    (setq header-line-format
          (list
           (propertize " Edit Issue " 'face '(:weight bold :inherit font-lock-function-name-face))
           (propertize " │ " 'face 'shadow)
           (propertize "C-c C-c" 'face '(:weight bold :inherit success))
           (propertize " submit " 'face 'shadow)
           (propertize "│ " 'face 'shadow)
           (propertize "C-c C-k" 'face '(:weight bold :inherit error))
           (propertize " abort" 'face 'shadow)))

    ;; Store original values for change detection AFTER narrowing
    ;; Clear org-element cache again to ensure no stale positions
    (when (fboundp 'org-element-cache-reset)
      (org-element-cache-reset))

    (when go-jira-debug
      (message "DEBUG [store originals]: narrowed=%s, point-min=%s, point-max=%s, point=%s"
               (buffer-narrowed-p) (point-min) (point-max) (point)))

    ;; Don't pass old context - let extraction functions get fresh context from narrowed buffer
    (overlay-put go-jira-edit--active-overlay 'ticket-context context)
    (overlay-put go-jira-edit--active-overlay 'original-title (go-jira-edit--extract-title))
    (overlay-put go-jira-edit--active-overlay 'original-description (go-jira-edit--extract-description))
    (overlay-put go-jira-edit--active-overlay 'original-comments (go-jira-edit--extract-comments))

    ;; Disable read-only mode - let user edit freely
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)
      ;; Also remove read-only text properties from the visible region
      (remove-text-properties (point-min) (point-max) '(read-only nil)))

    (when go-jira-debug
      (message "DEBUG: After disabling read-only: buffer-read-only=%s" buffer-read-only))

    ;; Enable edit mode
    (go-jira-edit-mode 1)

    ;; Mark buffer as unmodified so we can detect user changes
    (set-buffer-modified-p nil)

    (message "Edit mode enabled. Submit or abort when done")))

(defun go-jira-edit--has-changes-p ()
  "Check if anything has been modified.
Returns non-nil if the buffer has been modified since edit started."
  (buffer-modified-p))

(defun go-jira-edit--remove-overlay ()
  "Remove the active edit session and restore read-only mode."
  (when go-jira-edit--active-overlay
    ;; Remove the zero-width overlay
    (delete-overlay go-jira-edit--active-overlay)
    (setq go-jira-edit--active-overlay nil)

    ;; Restore header line
    (setq header-line-format go-jira-edit--saved-header-line)
    (setq go-jira-edit--saved-header-line nil)

    ;; Disable edit mode
    (go-jira-edit-mode -1)

    ;; Restore buffer read-only state
    (setq buffer-read-only t)

    ;; Widen if narrowed (board mode narrows to ticket subtree)
    (when (buffer-narrowed-p)
      (widen))))

;;; Ticket context detection (works in both view and board modes)

(defun go-jira-edit--get-ticket-context ()
  "Get the ticket context (key and subtree bounds) based on current mode.
Returns a plist with :key, :start, :end, :level properties.
If buffer is narrowed, uses narrowed bounds instead of absolute positions."
  (cond
   ;; View mode: entire buffer is one ticket
   ((derived-mode-p 'go-jira-view-mode)
    (list :key (bound-and-true-p go-jira--ticket-number)
          :start (point-min)
          :end (point-max)
          :level 1))

   ;; Board mode: find ticket subtree at level 2
   ((derived-mode-p 'go-jira-board-view-mode)
    (save-excursion
      ;; If narrowed, treat it like view mode - use point-min/max
      (if (buffer-narrowed-p)
          (progn
            (goto-char (point-min))
            ;; Find first org heading (skip overlay instructions if present)
            (unless (org-at-heading-p)
              (re-search-forward "^\\*+ " nil t)
              (goto-char (line-beginning-position)))
            (let* ((element (org-element-at-point))
                   (heading (org-element-property :raw-value element))
                   (key (when (and heading (string-match "\\([A-Z]+-[0-9]+\\)" heading))
                          (match-string 1 heading)))
                   (level (org-outline-level)))
              (list :key key
                    :start (point-min)
                    :end (point-max)
                    :level level)))
        ;; Not narrowed - find ticket in full buffer
        (org-back-to-heading t)
        ;; Walk up to level 2 (ticket level)
        (while (and (> (org-outline-level) 2)
                    (org-up-heading-safe)))

        (when (= (org-outline-level) 2)
          (let* ((element (org-element-at-point))
                 (heading (org-element-property :raw-value element))
                 (key (when (string-match "\\([A-Z]+-[0-9]+\\)" heading)
                        (match-string 1 heading)))
                 (start (org-element-property :begin element))
                 (end (org-element-property :end element)))
            (list :key key :start start :end end :level 2))))))

   (t (user-error "Not in a Jira buffer"))))

;;; Comment extraction

(defun go-jira-edit--extract-comment-body (element heading-level)
  "Extract body text from comment ELEMENT at HEADING-LEVEL.
Excludes the property drawer and any reply sub-headings, so that only the
comment's own prose is returned.  Heading levels are normalised back to
root-relative."
  (let* ((body-begin (go-jira-comment-body-start element))
         (contents-end (org-element-property :contents-end element))
         (reply-level (1+ heading-level)))
    (if (not (and body-begin contents-end (< body-begin contents-end)))
        ""
      ;; Walk through contents, collecting text ranges that aren't replies
      (let (body-parts
            (pos body-begin))
        (save-excursion
          (goto-char body-begin)
          (while (and (< (point) contents-end)
                      (re-search-forward
                       (format "^\\*\\{%d\\} " reply-level) contents-end t))
            (goto-char (line-beginning-position))
            (let* ((sub-el (org-element-at-point))
                   (sub-begin (org-element-property :begin sub-el))
                   (sub-end (org-element-property :end sub-el)))
              (when (go-jira-comment-parent-id-at sub-begin)
                ;; Collect text before this reply
                (when (< pos sub-begin)
                  (push (buffer-substring pos sub-begin) body-parts))
                (setq pos sub-end))
              (goto-char sub-end))))
        ;; Collect remaining text after last reply
        (when (< pos contents-end)
          (push (buffer-substring pos contents-end) body-parts))
        (go-jira-markup-shift-headings
         (string-trim (apply #'concat (nreverse body-parts)))
         (- heading-level))))))

(defun go-jira-edit--extract-comments (&optional context)
  "Extract comments (including replies) from the ticket.
CONTEXT is a plist with ticket bounds.  If nil, uses current context.
Returns a list of plists with :body, :marker at the comment's heading,
optionally :id for existing comments, and :parent-id for replies.

A sub-heading counts as a reply only when it carries the parent property.
Jira comment bodies may themselves contain headings, and position alone
cannot tell the two apart."
  (let* ((ctx (or context (go-jira-edit--get-ticket-context)))
         (start (plist-get ctx :start))
         (end (plist-get ctx :end))
         (level (plist-get ctx :level))
         (comments-level (+ level 1))  ; Comments heading is one level below ticket
         (comment-level (+ level 2))   ; Individual comments are two levels below ticket
         (reply-level (+ level 3))     ; Reply comments are three levels below ticket
         comments)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (go-jira-edit--ticket-heading-start start))
        ;; Find Comments section within ticket bounds
        (when (re-search-forward
               (format "^\\*\\{%d\\} Comments[ \t]*$" comments-level)
               end t)
          (goto-char (line-beginning-position))
          (let* ((comments-element (org-element-at-point))
                 (comments-end (org-element-property :end comments-element)))
            ;; Parse all comment headings (both parent and reply levels)
            (goto-char (org-element-property :contents-begin comments-element))
            (while (and (< (point) comments-end)
                        (re-search-forward
                         (format "^\\*\\{%d,%d\\} " comment-level reply-level)
                         comments-end t))
              (goto-char (line-beginning-position))
              (let* ((comment-element (org-element-at-point))
                     (el-level (org-element-property :level comment-element))
                     (comment-id (go-jira-comment-id-at))
                     (parent-id (go-jira-comment-parent-id-at)))

                (cond
                 ;; Parent comment (at comment-level)
                 ((= el-level comment-level)
                  (let ((body (go-jira-edit--extract-comment-body comment-element comment-level)))
                    (unless (string-empty-p body)
                      (push (list :body body :id comment-id :marker (point-marker))
                            comments)))
                  ;; Descend to look for replies; an empty comment has none.
                  (goto-char (or (org-element-property :contents-begin comment-element)
                                 (org-element-property :end comment-element))))

                 ;; Reply (at reply-level, and only with a parent recorded)
                 ((and (= el-level reply-level) parent-id)
                  (let* ((body-begin (go-jira-comment-body-start comment-element))
                         (contents-end (org-element-property :contents-end comment-element))
                         (body (if (and body-begin contents-end (< body-begin contents-end))
                                   (go-jira-markup-shift-headings
                                    (string-trim (buffer-substring body-begin contents-end))
                                    (- reply-level))
                                 "")))
                    (unless (string-empty-p body)
                      (push (list :body body :id comment-id :parent-id parent-id
                                  :marker (point-marker))
                            comments)))
                  (goto-char (org-element-property :end comment-element)))

                 ;; A heading inside a comment body - already part of that body
                 (t (goto-char (org-element-property :end comment-element))))))))))
    (nreverse comments)))

;;; Drafting new comments

(defun go-jira-edit--ensure-jira-buffer ()
  "Signal unless the current buffer shows a Jira ticket."
  (unless (or (derived-mode-p 'go-jira-view-mode)
              (derived-mode-p 'go-jira-board-view-mode))
    (user-error "Not in a Jira buffer")))

(defun go-jira-edit--ensure-session ()
  "Start an edit session unless one is already running."
  (unless go-jira-edit--active-overlay
    (let ((context (go-jira-edit--get-ticket-context)))
      (unless (plist-get context :key)
        (user-error "Could not determine ticket key"))
      (go-jira-edit--create-overlay context))))

(defun go-jira-edit--at-comments-heading-p (&optional context)
  "Return non-nil when point sits on the ticket's Comments heading.
CONTEXT defaults to the current ticket context."
  (let ((comments-level (1+ (plist-get (or context (go-jira-edit--get-ticket-context))
                                       :level))))
    (save-excursion
      (when (ignore-errors (org-back-to-heading t) t)
        (string-match-p
         (format "\\`\\*\\{%d\\} Comments[ \t]*\\'" comments-level)
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)))))))

(defun go-jira-edit--comments-section-start (context)
  "Return the position just after the Comments heading of CONTEXT's ticket.
Creates the section at the end of the ticket when it has none, which is
the case for a ticket nobody has commented on yet."
  (let* ((start (plist-get context :start))
         (end (plist-get context :end))
         (comments-level (1+ (plist-get context :level))))
    (goto-char (go-jira-edit--ticket-heading-start start))
    (if (re-search-forward
         (format "^\\*\\{%d\\} Comments[ \t]*$" comments-level) end t)
        (progn (forward-line 1) (point))
      (goto-char end)
      (skip-chars-backward " \t\n" start)
      (unless (bolp) (insert "\n"))
      (insert (format "%s Comments\n" (make-string comments-level ?*)))
      (point))))

(defun go-jira-edit--draft-comment (context)
  "Insert a comment draft at the top of CONTEXT's Comments section.
Newest first, matching the order the renderer uses."
  (goto-char (go-jira-edit--comments-section-start context))
  (go-jira-comment-insert-draft (+ (plist-get context :level) 2)))

(defun go-jira-edit--draft-reply (context parent-id)
  "Insert a reply draft to PARENT-ID at the end of its thread in CONTEXT."
  (unless (go-jira-comment-goto-id parent-id (plist-get context :end))
    (user-error "Comment %s is no longer in this buffer" parent-id))
  (org-end-of-subtree t t)
  (go-jira-comment-insert-draft (+ (plist-get context :level) 3) parent-id))

;;;###autoload
(defun go-jira-comment-add ()
  "Draft a new comment on the current Jira issue.
Creates the Comments section when the ticket has none, starts an edit
session if one is not already running, and leaves point in the draft.
Submit it with \\[go-jira-edit-submit]."
  (interactive)
  (go-jira-edit--ensure-jira-buffer)
  (go-jira-edit--ensure-session)
  (go-jira-edit--draft-comment (go-jira-edit--get-ticket-context))
  (message "Write the comment, then C-c C-c to submit"))

;;;###autoload
(defun go-jira-comment-reply ()
  "Draft a reply to the Jira comment at point.
The parent is recorded in a property drawer, which is the only thing that
separates a reply from a heading written inside a comment body."
  (interactive)
  (go-jira-edit--ensure-jira-buffer)
  (let ((parent-id (go-jira-comment-thread-root-id)))
    (unless parent-id
      (user-error "No comment at point to reply to"))
    (go-jira-edit--ensure-session)
    (go-jira-edit--draft-reply (go-jira-edit--get-ticket-context) parent-id)
    (message "Write the reply, then C-c C-c to submit")))

;;; Public API

;;;###autoload
(defun go-jira-edit ()
  "Edit the current Jira issue (title, description, comments).
Works in both go-jira-view-mode and go-jira-board-view-mode.
On the Comments heading it also drafts a new comment, since that is the
only place in a ticket where there is nothing else to edit."
  (interactive)
  (go-jira-edit--ensure-jira-buffer)

  (when go-jira-edit--active-overlay
    (user-error "Already editing"))

  (if (go-jira-edit--at-comments-heading-p)
      (go-jira-comment-add)
    (go-jira-edit--ensure-session)))

(defun go-jira-edit--comment-changed-p (comment originals)
  "Return non-nil when COMMENT has to be sent to Jira.
ORIGINALS are the comments captured when the edit session started.  A
comment with no ID is new.  One with an ID goes only when its body
differs, matched by ID rather than by position."
  (let ((id (plist-get comment :id)))
    (or (null id)
        (let ((orig (cl-find-if (lambda (c) (equal (plist-get c :id) id))
                                originals)))
          (or (null orig)
              (not (string-equal (plist-get orig :body)
                                 (plist-get comment :body))))))))

(defun go-jira-edit--submit-comment (ticket-key comment)
  "Send COMMENT to Jira for TICKET-KEY.
Records the ID Jira assigns to a new comment on its heading, so that a
later edit amends it rather than posting a copy."
  (let ((body (go-jira-markup-from-org (plist-get comment :body)))
        (id (plist-get comment :id)))
    (if id
        (go-jira-comment-put ticket-key id body)
      (go-jira-comment-record-id
       (plist-get comment :marker)
       (go-jira-comment-post ticket-key body (plist-get comment :parent-id))))))

(defun go-jira-edit-submit ()
  "Submit the edited title, description and comments to Jira."
  (interactive)
  (unless go-jira-edit--active-overlay
    (user-error "No active edit to submit"))

  ;; Check if there are any changes
  (when go-jira-debug
    (message "DEBUG: buffer-modified-p=%s, narrowed=%s"
             (buffer-modified-p) (buffer-narrowed-p)))

  (if (not (go-jira-edit--has-changes-p))
      (progn
        (go-jira-edit--remove-overlay)
        (message "No changes to submit"))
    (go-jira-edit--submit-changes)))

(defun go-jira-edit--submit-changes ()
  "Send the edited title, description and comments of the active session."
  (let* ((ticket-key (overlay-get go-jira-edit--active-overlay 'go-jira-ticket))
         ;; Get original values
         (orig-title (overlay-get go-jira-edit--active-overlay 'original-title))
         (orig-description (overlay-get go-jira-edit--active-overlay 'original-description))
         (orig-comments (overlay-get go-jira-edit--active-overlay 'original-comments))
         ;; Get current values - context will handle narrowing correctly
         (_ (when go-jira-debug
              (message "DEBUG: About to extract. Narrowed=%s, point-min=%s, point-max=%s"
                       (buffer-narrowed-p) (point-min) (point-max))
              (message "DEBUG: First line at point-min: %S"
                       (save-excursion
                         (goto-char (point-min))
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
         (title-org (go-jira-edit--extract-title))
         (desc-org (go-jira-edit--extract-description))
         (comments-current (go-jira-edit--extract-comments))
         ;; Detect what changed
         (title-changed (not (string-equal (or orig-title "") (or title-org ""))))
         (desc-changed (not (string-equal (or orig-description "") (or desc-org ""))))
         ;; Debug output
         (_ (when go-jira-debug
              (message "DEBUG: orig-title=%S, current-title=%S, changed=%s"
                       orig-title title-org title-changed)
              (message "DEBUG: orig-desc=%S, current-desc=%S, changed=%s"
                       orig-description desc-org desc-changed)
              (message "DEBUG: orig-comments=%S, current-comments=%S"
                       orig-comments comments-current)))
         ;; Filter to only changed comments
         (comments (cl-remove-if-not
                    (lambda (comment)
                      (go-jira-edit--comment-changed-p comment orig-comments))
                    comments-current))
         (comments-changed (> (length comments) 0))
         ;; Convert to Jira format
         (title-text title-org)
         (desc-jira (if (and desc-org (not (string-empty-p desc-org)))
                        (go-jira-markup-from-org desc-org)
                      ""))
         (json-file (make-temp-file "jira-edit-" nil ".json"))
         (editor-script (make-temp-file "jira-editor-" nil ".sh")))

    (unless ticket-key
      (user-error "No ticket key found"))

    (unwind-protect
        (progn
          (when go-jira-debug
            (message "Submitting changes for %s... (title:%s desc:%s comments:%s)"
                     ticket-key
                     (if title-changed "changed" "unchanged")
                     (if desc-changed "changed" "unchanged")
                     (if comments-changed (format "%d changed" (length comments)) "unchanged")))

          (let ((j (go-jira--find-exe))
                (success t)
                (request-count 0))

            ;; Step 1: Update title and/or description (only if changed)
            (when (or title-changed desc-changed)
              (setq request-count (1+ request-count))
              (when go-jira-debug
                (message "[Request %d/%d] Updating title and description..."
                         request-count
                         (+ (if (or title-changed desc-changed) 1 0)
                            (if comments-changed (length comments) 0))))
              (let ((fields-json (json-encode (list :fields (list :summary title-text
                                                                  :description desc-jira)))))
                (with-temp-file json-file
                  (insert fields-json))

                (with-temp-file editor-script
                  (insert "#!/bin/bash\n")
                  (insert (format "cat %s > \"$1\"\n" (shell-quote-argument json-file))))
                (set-file-modes editor-script #o755)

                (with-temp-buffer
                  (let ((exit-code (call-process j nil (current-buffer) nil
                                                 "edit" ticket-key
                                                 "--editor" editor-script
                                                 "--template" "json")))
                    (let ((result (buffer-string)))
                      (when (or (not (zerop exit-code))
                                (string-match-p "error\\|failed" (downcase result)))
                        (setq success nil)
                        (message "Failed to update title/description: %s" result)))))))

            ;; Step 2: Update comments (only if changed)
            (when (and success comments-changed comments)
              (when go-jira-debug
                (message "[Submitting %d comment operations...]" (length comments)))
              (dolist (comment comments)
                (setq request-count (1+ request-count))
                (when go-jira-debug
                  (message "[Request %d] %s comment (id:%s parent:%s)..."
                           request-count
                           (if (plist-get comment :id) "Editing" "Adding")
                           (or (plist-get comment :id) "new")
                           (or (plist-get comment :parent-id) "none")))
                (condition-case err
                    (go-jira-edit--submit-comment ticket-key comment)
                  (error
                   (setq success nil)
                   (message "%s" (error-message-string err))))))



            ;; Remove overlay after successful submission
            (go-jira-edit--remove-overlay)

            (if success
                (if go-jira-debug
                    (message "Updated successfully (%d API calls)" request-count)
                  (message "Updated successfully"))
              (user-error "Failed to update.  Check *Messages* for details"))))

      ;; Cleanup temp files
      (when (file-exists-p json-file)
        (delete-file json-file))
      (when (file-exists-p editor-script)
        (delete-file editor-script)))))

(defun go-jira-edit-abort ()
  "Abort the current edit without submitting."
  (interactive)
  (unless go-jira-edit--active-overlay
    (user-error "No active edit to abort"))

  ;; Check if there are any changes
  (if (go-jira-edit--has-changes-p)
      ;; There are changes - ask for confirmation
      (when (yes-or-no-p "Abort edit without submitting? ")
        (go-jira-edit--remove-overlay)
        (message "Edit aborted")
        ;; Refresh to restore original content (discard user's edits)
        (cond
         ((derived-mode-p 'go-jira-view-mode)
          (go-jira-view-mode-refresh))
         ((derived-mode-p 'go-jira-board-view-mode)
          (go-jira-board-refresh))))
    ;; No changes - quietly abort
    (go-jira-edit--remove-overlay)))

(provide 'go-jira-edit)
;;; go-jira-edit.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
