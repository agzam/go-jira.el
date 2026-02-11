;;; go-jira-description.el --- Edit Jira issue descriptions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an overlay-based UI for editing Jira issue descriptions.
;;
;; Features:
;; - Overlay-based description editing area
;; - Org-mode markup support (converted to Jira markup on submit)
;; - Works in go-jira-view-mode

;;; Code:

(require 'go-jira-markup)
(require 'org)

(defgroup go-jira-description nil
  "Description editing functionality for go-jira."
  :group 'go-jira
  :prefix "go-jira-description-")

;;; Description overlay management

(defvar-local go-jira-description--active-overlay nil
  "The currently active description edit overlay in this buffer.")

(defvar go-jira-description-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map to get all org keybindings
    (set-keymap-parent map org-mode-map)
    ;; Add our specific bindings
    (define-key map (kbd "C-c C-c") #'go-jira-description-submit)
    (define-key map (kbd "C-c C-k") #'go-jira-description-abort)
    map)
  "Keymap active in description edit overlay region.
Inherits from org-mode-map, overrides submit and abort.")

(define-minor-mode go-jira-description-mode
  "Minor mode active in the description edit overlay."
  :lighter " JiraDesc"
  :keymap go-jira-description-mode-map)

(defun go-jira-description--get-description-bounds ()
  "Find the Description section boundaries.
Returns a cons cell (START . END) where START is after the Description
heading and END is before the next heading or end of buffer.
Creates the Description section if it doesn't exist."
  (save-excursion
    (goto-char (point-min))
    ;; Look for the Description heading
    (if (re-search-forward "^\\*\\* Description[ \t]*$" nil t)
        ;; Found it - get bounds
        (let ((start (progn (forward-line 1) (point)))
              (end (save-excursion
                     (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                         (line-beginning-position)
                       (point-max)))))
          (cons start end))
      ;; Description section doesn't exist - create it
      (goto-char (point-min))
      ;; Find the end of the first heading (ticket title)
      (when (re-search-forward "^\\* " nil t)
        (end-of-line)
        (let ((inhibit-read-only t))
          (insert "\n** Description\n")
          (let ((start (point)))
            (insert "\n")
            (cons start (point))))))))

(defun go-jira-description--extract-content ()
  "Extract the current description content as Org-mode text.
Returns the description text between the Description heading
and the next heading, with leading/trailing whitespace trimmed."
  (when-let ((bounds (go-jira-description--get-description-bounds)))
    (let ((content (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (string-trim content))))

(defun go-jira-description--create-overlay ()
  "Create a description edit overlay in the current buffer.
The overlay appears at the top of the ticket with submit/abort controls."
  (when go-jira-description--active-overlay
    (user-error "Description is already being edited"))

  (when-let* ((ticket-key (bound-and-true-p go-jira--ticket-number))
              (bounds (go-jira-description--get-description-bounds)))

    ;; Create overlay at the top of the buffer, before the ticket title
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\* " nil t)
        (goto-char (line-beginning-position))

        (let* ((overlay-start (point))
               (inhibit-read-only t)
               (instructions
                (concat
                 (propertize "Edit Description" 'face '(:weight bold :inherit font-lock-function-name-face))
                 (propertize " │ " 'face 'shadow)
                 (propertize "C-c C-c" 'face 'success)
                 (propertize " submit" 'face 'shadow)
                 (propertize " │ " 'face 'shadow)
                 (propertize "C-c C-k" 'face 'error)
                 (propertize " abort" 'face 'shadow)
                 "\n"))
               (desc-bounds (go-jira-description--get-description-bounds)))

          (insert instructions)
          (let ((overlay-end (point))
                (ov (make-overlay overlay-start (point) nil nil t)))
            (overlay-put ov 'go-jira-description-edit t)
            (overlay-put ov 'go-jira-ticket ticket-key)
            (overlay-put ov 'priority 1000)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'face '(:box (:line-width 1 :style released-button)))

            (setq go-jira-description--active-overlay ov))

          ;; Create a read-only overlay for everything before description
          (let ((before-ov (make-overlay (point-min) (car desc-bounds))))
            (overlay-put before-ov 'go-jira-description-ro t)
            (overlay-put before-ov 'priority 999)
            (overlay-put before-ov 'evaporate t)
            (overlay-put before-ov 'modification-hooks '(go-jira-description--prevent-modification)))

          ;; Create a read-only overlay for everything after description
          (when (< (cdr desc-bounds) (point-max))
            (let ((after-ov (make-overlay (cdr desc-bounds) (point-max))))
              (overlay-put after-ov 'go-jira-description-ro t)
              (overlay-put after-ov 'priority 999)
              (overlay-put after-ov 'evaporate t)
              (overlay-put after-ov 'modification-hooks '(go-jira-description--prevent-modification))))

          ;; Disable read-only mode
          (setq buffer-read-only nil)

          ;; Enable description edit mode
          (go-jira-description-mode 1)

          ;; Position cursor at the start of description content
          (goto-char (car desc-bounds))

          ;; Mark buffer as unmodified so we can detect user changes
          (set-buffer-modified-p nil)

          (message "Edit description. Submit or abort when done"))))))

(defun go-jira-description--has-changes-p ()
  "Check if the description has been modified.
Returns non-nil if the buffer has been modified since edit started."
  (buffer-modified-p))

(defun go-jira-description--prevent-modification (overlay after beg end &optional len)
  "Prevent modification of read-only overlay regions.
This is attached to read-only overlays to prevent editing outside
the Description section."
  (unless after
    (user-error "This section is read-only during description editing")))

(defun go-jira-description--remove-overlay ()
  "Remove the active description edit overlay and restore read-only mode."
  (when go-jira-description--active-overlay
    ;; Remove all read-only overlays FIRST
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'go-jira-description-ro)
        (delete-overlay ov)))

    ;; Now we can safely remove the instruction overlay
    (let ((inhibit-read-only t))
      (when (overlay-buffer go-jira-description--active-overlay)
        (delete-region (overlay-start go-jira-description--active-overlay)
                       (overlay-end go-jira-description--active-overlay)))
      (delete-overlay go-jira-description--active-overlay)
      (setq go-jira-description--active-overlay nil)

      ;; Disable description edit mode
      (go-jira-description-mode -1)

      ;; Restore buffer read-only state
      (setq buffer-read-only t))))

;;; Public API

;;;###autoload
(defun go-jira-edit-description ()
  "Edit the description of the current Jira ticket.
Moves cursor to the Description section, creates an overlay with
submit/abort controls, and makes the description editable."
  (interactive)
  (unless (derived-mode-p 'go-jira-view-mode)
    (user-error "Not in a Jira ticket view buffer"))

  (when go-jira-description--active-overlay
    (user-error "Description is already being edited"))

  (let ((ticket-key (bound-and-true-p go-jira--ticket-number)))
    (unless ticket-key
      (user-error "Could not determine ticket key"))

    ;; Ensure Description section exists
    (go-jira-description--get-description-bounds)

    ;; Move to Description section
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Description[ \t]*$" nil t)
      (forward-line 1))

    ;; Create the edit overlay
    (go-jira-description--create-overlay)))

(defun go-jira-description-submit ()
  "Submit the edited description to Jira."
  (interactive)
  (unless go-jira-description--active-overlay
    (user-error "No active description edit to submit"))

  ;; Check if there are any changes
  (unless (go-jira-description--has-changes-p)
    (go-jira-description--remove-overlay)
    (message "No changes to submit")
    (cl-return-from go-jira-description-submit))

  (let* ((ticket-key (overlay-get go-jira-description--active-overlay 'go-jira-ticket))
         (org-content (go-jira-description--extract-content))
         (jira-content (if (and org-content (not (string-empty-p org-content)))
                           (go-jira-markup-from-org org-content)
                         "")))

    (unless ticket-key
      (user-error "No ticket key found"))

    ;; Remove the overlay before submitting
    (let ((buffer (current-buffer)))
      (go-jira-description--remove-overlay)

      (message "Submitting description for %s..." ticket-key)

      ;; Submit the description via jira CLI
      (let* ((j (go-jira--find-exe))
             (exit-code nil))
        (with-temp-buffer
          (setq exit-code
                (call-process j nil (current-buffer) nil
                              "edit" ticket-key
                              "--noedit" "-o"
                              (format "description=%s" jira-content)))
          (let ((result (buffer-string)))
            (if (or (not (zerop exit-code))
                    (string-match-p "error\\|failed" (downcase result)))
                (progn
                  (message "Failed to update description: %s" result)
                  (user-error "Failed to update description. Check *Messages* for details"))
              (message "Description updated successfully")
              ;; Refresh the view to show the updated description
              (with-current-buffer buffer
                (when (derived-mode-p 'go-jira-view-mode)
                  (go-jira-view-mode-refresh))))))))))

(defun go-jira-description-abort ()
  "Abort the current description edit without submitting."
  (interactive)
  (unless go-jira-description--active-overlay
    (user-error "No active description edit to abort"))

  ;; Check if there are any changes
  (if (go-jira-description--has-changes-p)
      ;; There are changes - ask for confirmation
      (when (yes-or-no-p "Abort description edit without submitting? ")
        (go-jira-description--remove-overlay)
        (message "Description edit aborted")
        ;; Refresh to restore original content
        (when (derived-mode-p 'go-jira-view-mode)
          (go-jira-view-mode-refresh)))
    ;; No changes - quietly abort
    (go-jira-description--remove-overlay)))

(provide 'go-jira-description)
;;; go-jira-description.el ends here
