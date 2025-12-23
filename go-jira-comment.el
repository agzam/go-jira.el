;;; go-jira-comment.el --- Add comments to Jira tickets -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an overlay-based UI for composing and submitting comments
;; to Jira tickets. The UI is inspired by gptel's tool-use interface,
;; creating an editable region in the buffer with submit/abort actions.
;;
;; Features:
;; - Overlay-based comment composition area
;; - Org-mode markup support (converted to Jira markup on submit)
;; - Submit with C-c C-c, abort with C-c C-k
;; - Works in both go-jira-view-mode and go-jira-board-view-mode

;;; Code:

(require 'go-jira-markup)
(require 'org)

(defgroup go-jira-comment nil
  "Comment functionality for go-jira."
  :group 'go-jira
  :prefix "go-jira-comment-")

(defcustom go-jira-comment-initial-text ""
  "Initial text to insert in the comment overlay."
  :type 'string
  :group 'go-jira-comment)

;;; Comment overlay management

(defvar-local go-jira-comment--active-overlay nil
  "The currently active comment overlay in this buffer.")

(defvar go-jira-comment-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from org-mode-map to get all org keybindings
    (set-keymap-parent map org-mode-map)
    ;; Add our specific bindings
    (define-key map (kbd "C-c C-c") #'go-jira-comment-submit)
    (define-key map (kbd "C-c C-k") #'go-jira-comment-abort)
    map)
  "Keymap active in comment overlay region.
Inherits from org-mode-map, overrides C-c C-c and C-c C-k.")

(define-minor-mode go-jira-comment-mode
  "Minor mode active in the comment composition overlay."
  :lighter " JiraComment"
  :keymap go-jira-comment-mode-map
  ;; Make buffer locally editable when this mode is active
  (if go-jira-comment-mode
      (setq-local buffer-read-only nil)
    (setq-local buffer-read-only t)))

(defun go-jira-comment--get-ticket-key ()
  "Get the ticket key from the current buffer context.
Works in both go-jira-view-mode and go-jira-board-view-mode."
  (cond
   ;; In go-jira-view-mode
   ((bound-and-true-p go-jira--ticket-number)
    go-jira--ticket-number)
   
   ;; In go-jira-board-view-mode
   ((derived-mode-p 'go-jira-board-view-mode)
    (save-excursion
      ;; First, go back to the current heading (in case we're in text)
      (ignore-errors (org-back-to-heading t))
      ;; Now walk up to find a level 2 heading with ticket key
      (while (and (> (org-outline-level) 2)
                  (org-up-heading-safe)))
      ;; Now we should be at level 2 (or gave up at level 1)
      (when (= (org-outline-level) 2)
        (let ((heading (org-get-heading t t t t)))
          ;; Extract ticket key from heading like "SAC-29811: title"
          (when (string-match "\\`\\([A-Z]\\{2,10\\}-[0-9]+\\)" heading)
            (match-string 1 heading))))))
   
   (t
    (user-error "Could not determine ticket key from current context"))))

(defun go-jira-comment--find-comments-section ()
  "Find the Comments section in the current ticket's subtree.
Returns the position after the Comments heading, or nil if not found."
  (save-excursion
    ;; First go to the current ticket heading (level 2)
    (ignore-errors (org-back-to-heading t))
    (while (and (> (org-outline-level) 2)
                (org-up-heading-safe)))
    ;; Now search forward but stay within this ticket's subtree
    (let ((ticket-end (save-excursion
                        (or (and (org-get-next-sibling) (point))
                            (point-max)))))
      (when (re-search-forward "^\\*\\{2,\\} Comments[ \t]*$" ticket-end t)
        (forward-line 1)
        (point)))))

(defun go-jira-comment--create-overlay (ticket-key)
  "Create a comment overlay for TICKET-KEY in the current buffer."
  (when go-jira-comment--active-overlay
    (user-error "A comment is already being composed in this buffer"))
  
  (let* ((comments-pos (go-jira-comment--find-comments-section))
         (insert-pos (if comments-pos
                         comments-pos
                       ;; No Comments section exists, create one
                       (save-excursion
                         (org-back-to-heading t)
                         (while (and (> (org-outline-level) 2)
                                     (org-up-heading-safe)))
                         ;; Go to end of this ticket's subtree
                         (org-end-of-subtree t t)
                         (unless (bolp) (insert "\n"))
                         (insert "\n*** Comments\n")
                         (point)))))
    
    (goto-char insert-pos)
    
    ;; Make sure the comment area is visible
    (org-reveal)
    (org-show-entry)
    (org-show-children)
    
    ;; Insert comment composition area
    (let* ((start (point))
           (inhibit-read-only t)
           (instructions 
            (concat
             "\n"
             (propertize "Add Comment " 'face '(:weight bold :inherit font-lock-function-name-face))
             (propertize "│ " 'face 'shadow)
             (propertize "C-c C-c" 'face 'success)
             (propertize " submit" 'face 'shadow)
             (propertize " │ " 'face 'shadow)
             (propertize "C-c C-k" 'face 'error)
             (propertize " abort" 'face 'shadow)
             "\n"
             (propertize (make-string 60 ?─) 'face 'shadow)
             "\n\n"))
           (separator 
            (concat
             "\n"
             (propertize (make-string 60 ?─) 'face 'shadow)
             "\n\n")))
      
      (insert instructions)
      (let ((content-start (point)))
        (insert go-jira-comment-initial-text)
        (insert separator)
        
        (let ((ov (make-overlay start (point) nil nil t)))
          (overlay-put ov 'go-jira-comment t)
          (overlay-put ov 'go-jira-ticket ticket-key)
          (overlay-put ov 'priority 100)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'content-start content-start)
          (overlay-put ov 'content-end (- (point) (length separator)))
          (overlay-put ov 'local-map go-jira-comment-mode-map)
          (overlay-put ov 'priority 1000)
          
          ;; Enable comment mode in the overlay region
          (setq go-jira-comment--active-overlay ov)
          (go-jira-comment-mode 1)
          
          ;; Position cursor at the start of editable content
          (goto-char content-start)
          
          ;; Select the initial placeholder text so user can start typing
          (set-mark (+ content-start (length go-jira-comment-initial-text)))
          (activate-mark)
          
          (message "Type your comment. C-c C-c to submit, C-c C-k to abort"))))))

(defun go-jira-comment--remove-overlay ()
  "Remove the active comment overlay."
  (when go-jira-comment--active-overlay
    (let ((inhibit-read-only t)
          (start (overlay-start go-jira-comment--active-overlay))
          (end (overlay-end go-jira-comment--active-overlay)))
      (delete-overlay go-jira-comment--active-overlay)
      (setq go-jira-comment--active-overlay nil)
      (go-jira-comment-mode -1)
      ;; Delete the comment section text
      (when (and start end)
        (delete-region start end)))))

(defun go-jira-comment--get-overlay-content ()
  "Extract the comment content from the active overlay."
  (when go-jira-comment--active-overlay
    (let* ((ov-start (overlay-start go-jira-comment--active-overlay))
           (ov-end (overlay-end go-jira-comment--active-overlay))
           (content-start (overlay-get go-jira-comment--active-overlay 'content-start))
           ;; Find the separator line dynamically
           (separator-pos (save-excursion
                            (goto-char content-start)
                            (if (search-forward "────────────────────────────────────────────────────────" ov-end t)
                                (line-beginning-position)
                              ov-end)))
           (content (buffer-substring-no-properties content-start separator-pos)))
      (string-trim content))))

;;; Public API

;;;###autoload
(defun go-jira-add-comment ()
  "Add a comment to the current Jira ticket.
Creates an overlay-based composition area where you can write your
comment using Org-mode markup. The comment will be converted to Jira
markup when submitted.

Available commands:
  C-c C-c - Submit the comment
  C-c C-k - Abort without submitting"
  (interactive)
  (let ((ticket-key (go-jira-comment--get-ticket-key)))
    (unless ticket-key
      (user-error "Could not determine ticket key"))
    
    ;; In board view, ensure issue content is fetched first
    (when (derived-mode-p 'go-jira-board-view-mode)
      (save-excursion
        (org-back-to-heading t)
        (while (and (> (org-outline-level) 2)
                    (org-up-heading-safe)))
        ;; Check if this issue has been expanded/fetched
        (let* ((last-fetch-time (gethash ticket-key go-jira--expanded-issues))
               (current-time (float-time))
               (cache-expired (or (not last-fetch-time)
                                  (< (+ last-fetch-time go-jira-board-cache-duration)
                                     current-time))))
          (when cache-expired
            (message "Fetching content for %s..." ticket-key)
            (require 'go-jira-board)
            (go-jira--fetch-and-insert-issue-content ticket-key)
            (puthash ticket-key current-time go-jira--expanded-issues)))))
    
    (let ((inhibit-read-only t))
      (go-jira-comment--create-overlay ticket-key))))

(defun go-jira-comment-submit ()
  "Submit the comment in the current overlay."
  (interactive)
  (unless go-jira-comment--active-overlay
    (user-error "No active comment to submit"))
  
  (let* ((ticket-key (overlay-get go-jira-comment--active-overlay 'go-jira-ticket))
         (org-content (go-jira-comment--get-overlay-content))
         (jira-content (go-jira-markup-from-org org-content)))
    
    (unless ticket-key
      (user-error "No ticket key found"))
    
    (when (or (not jira-content) (string-empty-p (string-trim jira-content)))
      (user-error "Comment is empty"))
    
    ;; Remove the overlay before submitting
    (let ((buffer (current-buffer)))
      (go-jira-comment--remove-overlay)
      
      (message "Submitting comment to %s..." ticket-key)
      (message "Comment content: %s" jira-content)
      
      ;; Submit the comment via jira CLI
      (let* ((j (go-jira--find-exe))
             (exit-code nil))
        (with-temp-buffer
          (setq exit-code
                (call-process j nil (current-buffer) nil
                              "comment" ticket-key
                              "--noedit" "-m" jira-content))
          (let ((result (buffer-string)))
            (if (or (not (zerop exit-code))
                    (string-match-p "error\\|failed" (downcase result)))
                (progn
                  (message "Failed to add comment: %s" result)
                  (user-error "Failed to add comment. Check *Messages* for details"))
              (message "Comment added successfully")
              ;; Refresh the view to show the new comment
              (with-current-buffer buffer
                (cond
                 ((derived-mode-p 'go-jira-view-mode)
                  (go-jira-view-mode-refresh))
                 ((derived-mode-p 'go-jira-board-view-mode)
                  (go-jira-board-refresh))
                 (t
                  (message "Comment added")))))))))))

(defun go-jira-comment-abort ()
  "Abort the current comment without submitting."
  (interactive)
  (unless go-jira-comment--active-overlay
    (user-error "No active comment to abort"))
  
  (when (yes-or-no-p "Abort comment without submitting? ")
    (go-jira-comment--remove-overlay)
    (message "Comment aborted")))

(provide 'go-jira-comment)
;;; go-jira-comment.el ends here
