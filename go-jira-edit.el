;;; go-jira-edit.el --- Edit Jira issues (title, description, comments) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a universal overlay-based UI for editing Jira issues.
;; Uses JSON templates and custom editor scripts to submit changes.
;;
;; Features:
;; - Edit title and description with overlay UI
;; - Org-mode markup support (converted to Jira markup on submit)
;; - Universal JSON-based submission flow
;; - Works in go-jira-view-mode

;;; Code:

(require 'go-jira-markup)
(require 'org)
(require 'json)

(declare-function go-jira--find-exe "go-jira")
(declare-function go-jira-view-mode-refresh "go-jira")

(defgroup go-jira-edit nil
  "Universal editing functionality for go-jira."
  :group 'go-jira
  :prefix "go-jira-edit-")

(defcustom go-jira-debug nil
  "Enable debug output for go-jira operations."
  :type 'boolean
  :group 'go-jira-edit)

;;; Description overlay management

(defvar-local go-jira-edit--active-overlay nil
  "The currently active description edit overlay in this buffer.")

(defvar go-jira-edit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map to get all org keybindings
    (set-keymap-parent map org-mode-map)
    ;; Add our specific bindings
    (define-key map (kbd "C-c C-c") #'go-jira-edit-submit)
    (define-key map (kbd "C-c C-k") #'go-jira-edit-abort)
    map)
  "Keymap active in description edit overlay region.
Inherits from org-mode-map, overrides submit and abort.")

(define-minor-mode go-jira-edit-mode
  "Minor mode active in the description edit overlay."
  :lighter " JiraDesc"
  :keymap go-jira-edit-mode-map)

(defun go-jira-edit--extract-title (&optional context)
  "Extract the ticket title.
CONTEXT is a plist with ticket bounds.  If nil, uses current context."
  (let ((ctx (or context (go-jira-edit--get-ticket-context))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (plist-get ctx :start))
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
        (goto-char start)
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
                    (string-trim (buffer-substring-no-properties contents-begin contents-end))
                  "")))
          ;; No description found - return empty string
          "")))))

(defun go-jira-edit--create-overlay (context)
  "Create an edit overlay for the ticket specified by CONTEXT.
CONTEXT is a plist with :key, :start, :end from
go-jira-edit--get-ticket-context.  The overlay appears at the top of the
ticket with submit/abort controls."
  (when go-jira-edit--active-overlay
    (user-error "Already editing"))

  (let ((ticket-key (plist-get context :key))
        (ticket-start (plist-get context :start)))
    (unless ticket-key
      (user-error "Could not determine ticket key"))

    ;; Handle narrowing for board mode FIRST, before inserting overlay
    (when (derived-mode-p 'go-jira-board-view-mode)
      ;; (widen)  ; First widen in case already narrowed
      (goto-char ticket-start)
      (org-narrow-to-subtree)
      ;; CRITICAL: Disable org-element caching entirely in narrowed buffer
      ;; The cache contains absolute positions that break after narrowing
      (setq-local org-element-use-cache nil)
      (when (fboundp 'org-element-cache-reset)
        (org-element-cache-reset)))

    ;; Create overlay at the top of the ticket, before the ticket heading
    ;; In board mode (narrowed), this is at point-min
    ;; In view mode (not narrowed), this is at ticket-start
    (save-excursion
      (goto-char (point-min))

      (let* ((overlay-start (point))
             (inhibit-read-only t)
             (instructions
              (concat
               (propertize "Edit Issue" 'face '(:weight bold :inherit font-lock-function-name-face))
               (propertize " │ " 'face 'shadow)
               (propertize "C-c C-c" 'face 'success)
               (propertize " submit" 'face 'shadow)
               (propertize " │ " 'face 'shadow)
               (propertize "C-c C-k" 'face 'error)
               (propertize " abort" 'face 'shadow)
               "\n")))

        (insert instructions)
        (let ((ov (make-overlay overlay-start (point) nil nil t)))
          (overlay-put ov 'go-jira-edit-edit t)
          (overlay-put ov 'go-jira-ticket ticket-key)
          (overlay-put ov 'priority 1000)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'face '(:box (:line-width 1 :style released-button)))

          (setq go-jira-edit--active-overlay ov))))

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
  "Remove the active edit overlay and restore read-only mode."
  (when go-jira-edit--active-overlay
    ;; Widen if we're in board mode (was narrowed)
    ;; (when (derived-mode-p 'go-jira-board-view-mode)
    ;;   (widen))

    ;; Remove the instruction overlay
    (let ((inhibit-read-only t))
      (when (overlay-buffer go-jira-edit--active-overlay)
        (delete-region (overlay-start go-jira-edit--active-overlay)
                       (overlay-end go-jira-edit--active-overlay)))
      (delete-overlay go-jira-edit--active-overlay)
      (setq go-jira-edit--active-overlay nil)

      ;; Disable edit mode
      (go-jira-edit-mode -1)

      ;; Restore buffer read-only state
      (setq buffer-read-only t))))

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

;;; Comment extraction and user identification

(defvar go-jira-edit--current-user-id nil
  "Cache for current user's Jira account ID.")

(defun go-jira-edit--get-current-user-id ()
  "Get the current user's Jira account ID.
Fetches from Jira API and caches the result."
  (or go-jira-edit--current-user-id
      (let* ((j (go-jira--find-exe))
             (json-output (shell-command-to-string (format "%s session --template json" j))))
        (condition-case nil
            (let* ((json-object-type 'hash-table)
                   (json-key-type 'symbol)
                   (parsed (json-read-from-string json-output))
                   (account-id (gethash 'accountId parsed)))
              (setq go-jira-edit--current-user-id account-id)
              account-id)
          (error nil)))))

(defun go-jira-edit--extract-comments (&optional context)
  "Extract user's comments from the ticket.
CONTEXT is a plist with ticket bounds.  If nil, uses current context.
Returns a list of plists with :body and optionally :id for existing comments."
  (let* ((ctx (or context (go-jira-edit--get-ticket-context)))
         (start (plist-get ctx :start))
         (end (plist-get ctx :end))
         (level (plist-get ctx :level))
         (comments-level (+ level 1))  ; Comments heading is one level below ticket
         (comment-level (+ level 2))   ; Individual comments are two levels below ticket
         ;; (_current-user (go-jira-edit--get-current-user-id))
         comments)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        ;; Find Comments section within ticket bounds
        (when (re-search-forward
               (format "^\\*\\{%d\\} Comments[ \t]*$" comments-level)
               end t)
          (goto-char (line-beginning-position))
          (let* ((comments-element (org-element-at-point))
                 (comments-end (org-element-property :end comments-element)))
            ;; Parse all comment headings under Comments
            (goto-char (org-element-property :contents-begin comments-element))
            (while (and (< (point) comments-end)
                        (re-search-forward (format "^\\*\\{%d\\} " comment-level) comments-end t))
              (goto-char (line-beginning-position))
              (let* ((comment-element (org-element-at-point))
                     (heading-text (org-element-property :raw-value comment-element))
                     ;; Extract comment ID from URL if present
                     (comment-id (when (string-match "focusedCommentId=\\([0-9]+\\)" heading-text)
                                   (match-string 1 heading-text)))
                     (is-new (not comment-id))
                     ;; Extract body content
                     (contents-begin (org-element-property :contents-begin comment-element))
                     (contents-end (org-element-property :contents-end comment-element))
                     (body (if (and contents-begin contents-end)
                               (string-trim (buffer-substring-no-properties contents-begin contents-end))
                             "")))

                ;; Include new comments OR existing comments (with ID in URL)
                ;; Jira will reject edits to comments not owned by user anyway
                (when (and (not (string-empty-p body))
                           (or is-new comment-id))
                  (push (list :body body :id comment-id) comments))

                ;; Move to next heading
                (goto-char (org-element-property :end comment-element))))))))
    (nreverse comments)))

;;; Public API

;;;###autoload
(defun go-jira-edit ()
  "Edit the current Jira issue (title, description, comments).
Works in both go-jira-view-mode and go-jira-board-view-mode.
Creates an overlay with submit/abort controls."
  (interactive)
  (unless (or (derived-mode-p 'go-jira-view-mode)
              (derived-mode-p 'go-jira-board-view-mode))
    (user-error "Not in a Jira buffer"))

  (when go-jira-edit--active-overlay
    (user-error "Already editing"))

  (let ((context (go-jira-edit--get-ticket-context)))
    (unless (plist-get context :key)
      (user-error "Could not determine ticket key"))

    ;; Create the edit overlay
    (go-jira-edit--create-overlay context)))

(defun go-jira-edit-submit ()
  "Submit the edited title and description to Jira."
  (interactive)
  (unless go-jira-edit--active-overlay
    (user-error "No active edit to submit"))

  ;; Check if there are any changes
  (when go-jira-debug
    (message "DEBUG: buffer-modified-p=%s, narrowed=%s"
             (buffer-modified-p) (buffer-narrowed-p)))

  (unless (go-jira-edit--has-changes-p)
    (go-jira-edit--remove-overlay)
    (message "No changes to submit")
    (cl-return-from go-jira-edit-submit))

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
         (comments (cl-remove-if
                    (lambda (comment)
                      (let* ((id (plist-get comment :id))
                             (body (plist-get comment :body))
                             (orig (cl-find-if (lambda (c)
                                                 (equal (plist-get c :id) id))
                                               orig-comments)))
                        ;; Keep if: new (no id) OR body changed
                        (and orig  ; Has original (not new)
                             (string-equal (plist-get orig :body) body))))  ; Body unchanged
                    comments-current))
         (comments-changed (> (length comments) 0))
         ;; Convert to Jira format
         (title-text title-org)
         (desc-jira (if (and desc-org (not (string-empty-p desc-org)))
                        (go-jira-markup-from-org desc-org)
                      ""))
         (json-file (make-temp-file "jira-edit-" nil ".json"))
         (editor-script (make-temp-file "jira-editor-" nil ".sh"))
         (buffer (current-buffer)))

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
                (let* ((body (plist-get comment :body))
                       (id (plist-get comment :id))
                       (jira-body (go-jira-markup-from-org body))
                       (comment-op (if id
                                       (list :edit (list :id id :body jira-body))
                                     (list :add (list :body jira-body))))
                       (comment-json (json-encode (list :update (list :comment (vector comment-op))))))

                  (with-temp-file json-file
                    (insert comment-json))

                  (when go-jira-debug
                    (message "[Request %d] %s comment (id:%s)..."
                             request-count
                             (if id "Editing" "Adding")
                             (or id "new")))

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
                          (message "Failed to update comment: %s" result))))))))

            ;; Remove overlay after successful submission
            (go-jira-edit--remove-overlay)

            (if success
                (progn
                  (if go-jira-debug
                      (message "Updated successfully (%d API calls)" request-count)
                    (message "Updated successfully"))
                  ;; Refresh the view
                  (with-current-buffer buffer
                    (when (derived-mode-p 'go-jira-view-mode)
                      (go-jira-view-mode-refresh))))
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
        ;; Refresh to restore original content
        (when (derived-mode-p 'go-jira-view-mode)
          (go-jira-view-mode-refresh)))
    ;; No changes - quietly abort
    (go-jira-edit--remove-overlay)))

(provide 'go-jira-edit)
;;; go-jira-edit.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
