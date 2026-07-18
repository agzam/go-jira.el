;;; go-jira-eldoc.el --- Eldoc integration for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Eldoc integration to show Jira ticket descriptions on hover in normal mode.
;; Also provides posframe popup support to display descriptions in GUI Emacs.

;;; Code:

(require 'go-jira)

;; Forward declaration for Evil's buffer-local state variable so a clean
;; byte-compilation (Evil is optional) does not warn about a free variable.
(defvar evil-state)

(defvar go-jira-eldoc-cache (make-hash-table :test 'equal)
  "Cache for Jira ticket descriptions to avoid repeated API calls.")

(defvar go-jira-eldoc-cache-ttl 3600
  "Time-to-live for cache entries in seconds (default: 1 hour).")

(defvar go-jira-popup-max-width 80
  "Maximum width for popup description text.")

(defvar go-jira-popup-buffer " *jira-ticket-popup*"
  "Buffer name for Jira ticket popup.")

(defcustom go-jira-popup-idle-delay 0.5
  "Number of seconds to wait before fetching ticket info.
Only fetch after cursor has been stable on a ticket for this duration."
  :type 'number
  :group 'go-jira)

(defvar-local go-jira--pending-timer nil
  "Timer for pending popup fetch request.")

(defvar-local go-jira--active-process nil
  "Currently active async process fetching ticket data.")

(defface go-jira-popup-border-face
  '((t :inherit font-lock-comment-face))
  "Face for Jira popup border."
  :group 'go-jira)

(defvar go-jira--last-ticket nil
  "Ticket currently shown in the popup, or nil.
Global on purpose: the popup is a single frame-level posframe shared by
every buffer, so hiding it from any context must clear this.")

(defvar-local go-jira--popup-source-buffer nil
  "Buffer the visible popup was created from.")

(defvar go-jira--posframe-available-p nil
  "Whether posframe is available.")

(setq go-jira--posframe-available-p
      (and (display-graphic-p)
           (require 'posframe nil t)))

;;; Internal cache functions

(defun go-jira-eldoc--cache-get (ticket)
  "Get cached info for TICKET if still valid."
  (when-let* ((entry (gethash ticket go-jira-eldoc-cache))
              (timestamp (car entry))
              (info (cdr entry)))
    (if (< (- (float-time) timestamp) go-jira-eldoc-cache-ttl)
        info
      (remhash ticket go-jira-eldoc-cache)
      nil)))

(defun go-jira-eldoc--cache-put (ticket info)
  "Store TICKET INFO in cache with current timestamp."
  (puthash ticket (cons (float-time) info) go-jira-eldoc-cache))

(defun go-jira-eldoc--cache-invalidate (ticket)
  "Drop the cached info for TICKET so it is refetched on next request."
  (remhash ticket go-jira-eldoc-cache))

(defun go-jira-eldoc--clear-cache ()
  "Clear the entire Jira eldoc cache."
  (interactive)
  (clrhash go-jira-eldoc-cache)
  (message "Jira eldoc cache cleared"))

(defun go-jira-eldoc--parse-info (json-string)
  "Parse JSON-STRING from `jira view --template json' into an info plist.
Returns a plist with :summary, :status and :category, or nil when the
summary is empty or the JSON cannot be parsed."
  (condition-case nil
      (let* ((json-object-type 'hash-table)
             (json-key-type 'symbol)
             (json-array-type 'list)
             (parsed (json-read-from-string json-string))
             (fields (gethash 'fields parsed))
             (summary (when fields (gethash 'summary fields)))
             (status (when fields (gethash 'status fields)))
             (status-name (when status (gethash 'name status)))
             (category (when status (gethash 'statusCategory status)))
             (category-key (when category (gethash 'key category))))
        (when (and summary (not (string-empty-p summary)))
          (list :summary summary :status status-name :category category-key)))
    (error nil)))

(defun go-jira-eldoc--fetch-description-async (ticket callback)
  "Fetch info for TICKET asynchronously, call CALLBACK with the result.
CALLBACK receives two arguments: TICKET and an info plist with :summary,
:status and :category keys.  Returns the process object, or nil when the
result is served from cache."
  (if-let ((cached (go-jira-eldoc--cache-get ticket)))
      (progn
        (funcall callback ticket cached)
        nil)
    (let* ((j (go-jira--find-exe))
           (cmd (format "%s view %s --template json" j ticket))
           (output-buffer (generate-new-buffer " *jira-fetch*"))
           (proc (make-process
                  :name (format "jira-fetch-%s" ticket)
                  :buffer output-buffer
                  :command (list shell-file-name "-c" cmd)
                  :sentinel
                  (lambda (process event)
                    (when (string-match-p "finished" event)
                      (with-current-buffer (process-buffer process)
                        (when-let ((info (go-jira-eldoc--parse-info (buffer-string))))
                          (go-jira-eldoc--cache-put ticket info)
                          (funcall callback ticket info))))
                    (kill-buffer (process-buffer process))))))
      proc)))

(defun go-jira-eldoc--ticket-at-point ()
  "Return Jira ticket at point if present, nil otherwise."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (thing (thing-at-point 'symbol t)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") thing)
      thing)))

;;; Posframe popup functions

(defun go-jira-popup--format-description (ticket info)
  "Format TICKET and INFO for popup display.
INFO is an info plist with :summary/:status/:category, or a plain summary
string.  The ticket key is bolded and tinted by status, and a colored
[STATUS] tag is shown when a status is available."
  (let* ((summary (if (stringp info) info (plist-get info :summary)))
         (status (unless (stringp info) (plist-get info :status)))
         (category (unless (stringp info) (plist-get info :category)))
         (face (go-jira--status-face status category))
         (ticket-part (propertize ticket 'face `(bold ,face)))
         (status-part (if status
                          (concat " " (propertize (format "[%s]" status) 'face face))
                        ""))
         (full-text (format "%s%s: %s" ticket-part status-part (or summary "")))
         (max-width go-jira-popup-max-width))
    (with-temp-buffer
      (insert full-text)
      (fill-region (point-min) (point-max) max-width)
      (buffer-string))))

(defun go-jira-popup--hide ()
  "Hide the Jira ticket popup."
  (when go-jira--posframe-available-p
    (posframe-hide go-jira-popup-buffer))
  (setq go-jira--last-ticket nil))

(defun go-jira-popup--cancel-pending ()
  "Cancel any pending fetch timer or process."
  (when go-jira--pending-timer
    (cancel-timer go-jira--pending-timer)
    (setq go-jira--pending-timer nil))
  (when (and go-jira--active-process
             (process-live-p go-jira--active-process))
    (delete-process go-jira--active-process)
    (setq go-jira--active-process nil)))

(defun go-jira-popup--hide-if-foreign (&rest _)
  "Hide the popup unless the selected window shows its source buffer.
Wired to `window-selection-change-functions' and
`window-buffer-change-functions', which fire only on genuine window,
buffer or tab switches.  A `buffer-list-update-hook' guard, by contrast,
also fires for the throwaway buffers other code creates while the popup
is up, tearing it down almost immediately."
  (when-let* ((popup-buf (get-buffer go-jira-popup-buffer))
              (source-buf (buffer-local-value 'go-jira--popup-source-buffer popup-buf)))
    (unless (eq (window-buffer (selected-window)) source-buf)
      (go-jira-popup--hide))))

(defun go-jira-popup--show (ticket description)
  "Show DESCRIPTION for TICKET in a posframe popup."
  (if go-jira--posframe-available-p
      (let ((text (go-jira-popup--format-description ticket description))
            (current-buf (current-buffer)))
        (posframe-show
         go-jira-popup-buffer
         :string text
         :position (point)
         :poshandler #'posframe-poshandler-point-bottom-left-corner
         :border-width 1
         :border-color (face-foreground 'go-jira-popup-border-face nil t)
         :background-color (face-background 'default nil t)
         :foreground-color (face-foreground 'default nil t)
         :internal-border-width 12
         :internal-border-color (face-background 'default nil t)
         :left-fringe 8
         :right-fringe 8
         :override-parameters '((no-accept-focus . t)))
        ;; Store which buffer this popup belongs to
        (with-current-buffer go-jira-popup-buffer
          (setq-local go-jira--popup-source-buffer current-buf)))
    ;; Fallback: do nothing, eldoc will handle it
    nil))

(defun go-jira-popup--display (ticket info &optional echo-fallback)
  "Display INFO for TICKET.
Show the posframe popup when available; otherwise, when ECHO-FALLBACK is
non-nil, show the description in the echo area."
  (cond (go-jira--posframe-available-p
         (go-jira-popup--show ticket info))
        (echo-fallback
         (message "%s" (go-jira-popup--format-description ticket info)))))

(defun go-jira-popup--request (ticket &optional guard echo-fallback)
  "Fetch info for TICKET (async when uncached) and display it.
GUARD, when non-nil, is a predicate called in the origin buffer right
before displaying; display is skipped when it returns nil.  ECHO-FALLBACK
is forwarded to `go-jira-popup--display'.  Returns the fetch process, or
nil when the info was served from cache."
  (let ((buf (current-buffer)))
    (go-jira-eldoc--fetch-description-async
     ticket
     (lambda (tkt info)
       (when (and (buffer-live-p buf)
                  (eq buf (current-buffer))
                  (or (null guard) (funcall guard)))
         (go-jira-popup--display tkt info echo-fallback))))))

(defun go-jira-popup--auto-active-p ()
  "Return non-nil when the automatic popup may react to point movement.
Automatic popups are limited to Evil's normal state so they do not flash
while typing."
  (and (bound-and-true-p evil-mode)
       (eq evil-state 'normal)))

(defun go-jira-popup--update ()
  "Show or hide the Jira popup for the ticket at point.
Runs from `post-command-hook' in `go-jira-popup-mode' buffers."
  (if-let* ((_ (go-jira-popup--auto-active-p))
            (ticket (go-jira-eldoc--ticket-at-point)))
      ;; Already showing this ticket: keep the popup as-is.
      (unless (equal ticket go-jira--last-ticket)
        (go-jira-popup--cancel-pending)
        (setq go-jira--last-ticket ticket)
        (if (go-jira-eldoc--cache-get ticket)
            (go-jira-popup--request ticket)
          ;; Debounce, then show only if still on this ticket.  Re-checking
          ;; the ticket (rather than a captured position) keeps the popup
          ;; working when point shifts within the same ticket during the delay.
          (let ((buf (current-buffer)))
            (setq go-jira--pending-timer
                  (run-with-idle-timer
                   go-jira-popup-idle-delay nil
                   (lambda ()
                     (setq go-jira--pending-timer nil)
                     (when (and (buffer-live-p buf)
                                (eq buf (current-buffer))
                                (equal ticket (go-jira-eldoc--ticket-at-point)))
                       (setq go-jira--active-process
                             (go-jira-popup--request
                              ticket
                              (lambda ()
                                (equal ticket (go-jira-eldoc--ticket-at-point))))))))))))
    (go-jira-popup--cancel-pending)
    (go-jira-popup--hide)))

;;; Public API

;;;###autoload
(defun go-jira-eldoc-function (callback &rest _ignored)
  "Eldoc documentation function for Jira tickets.
Calls CALLBACK with the ticket description when point is on a Jira ticket.
Designed to work with `eldoc-documentation-functions'."
  (when-let* ((ticket (go-jira-eldoc--ticket-at-point))
              ;; Only fetch if in normal state (not while typing)
              (_ (and (bound-and-true-p evil-mode)
                      (eq evil-state 'normal))))
    ;; Fetch asynchronously to avoid blocking
    (go-jira-eldoc--fetch-description-async
     ticket
     (lambda (tkt info)
       (let ((summary (if (stringp info) info (plist-get info :summary)))
             (status (unless (stringp info) (plist-get info :status))))
         (funcall callback
                  (if status
                      (format "%s [%s]: %s" tkt status summary)
                    (format "%s: %s" tkt summary))
                  :thing tkt
                  :face 'font-lock-doc-face))))))

;;;###autoload
(define-minor-mode go-jira-eldoc-mode
  "Minor mode to show Jira ticket descriptions in eldoc."
  :global nil
  :lighter nil
  (if go-jira-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'go-jira-eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions #'go-jira-eldoc-function t)))

;;;###autoload
(defun go-jira-eldoc-enable ()
  "Enable `go-jira-eldoc-mode' in current buffer."
  (interactive)
  (go-jira-eldoc-mode 1)
  (eldoc-mode 1))

;;;###autoload
(define-minor-mode go-jira-popup-mode
  "Minor mode to show Jira ticket descriptions in posframe popups."
  :global nil
  :lighter nil
  (if go-jira-popup-mode
      (progn
        (add-hook 'post-command-hook #'go-jira-popup--update nil t)
        ;; Clean up when entering insert mode
        (when (bound-and-true-p evil-mode)
          (add-hook 'evil-insert-state-entry-hook #'go-jira-popup--hide nil t))
        ;; Hide when the user switches window, buffer or tab.
        (add-hook 'window-selection-change-functions #'go-jira-popup--hide-if-foreign)
        (add-hook 'window-buffer-change-functions #'go-jira-popup--hide-if-foreign))
    (remove-hook 'post-command-hook #'go-jira-popup--update t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'go-jira-popup--hide t))
    (remove-hook 'window-selection-change-functions #'go-jira-popup--hide-if-foreign)
    (remove-hook 'window-buffer-change-functions #'go-jira-popup--hide-if-foreign)
    (go-jira-popup--cancel-pending)
    (go-jira-popup--hide)))

;;;###autoload
(defun go-jira-popup-show ()
  "Display a Jira popup for the ticket at point.
Fetch the ticket when needed, then show it immediately, ignoring the idle
delay and the Evil-state gating that `go-jira-popup-mode' applies.  Falls
back to the echo area when posframe is unavailable.

With `go-jira-popup-mode' active the popup is dismissed as usual on point
movement or window switch; otherwise dismiss it with `go-jira-popup-hide'."
  (interactive)
  (if-let* ((ticket (go-jira-eldoc--ticket-at-point)))
      (progn
        (go-jira-popup--cancel-pending)
        (setq go-jira--last-ticket ticket)
        (setq go-jira--active-process
              (go-jira-popup--request ticket nil :echo)))
    (user-error "No Jira ticket at point")))

;;;###autoload
(defun go-jira-popup-hide ()
  "Hide the Jira ticket popup and cancel any pending fetch."
  (interactive)
  (go-jira-popup--cancel-pending)
  (go-jira-popup--hide))

;;;###autoload
(defun go-jira-popup-enable ()
  "Enable `go-jira-popup-mode' in current buffer."
  (interactive)
  (go-jira-popup-mode 1))

;;;###autoload
(defun go-jira-enable-popup+eldoc ()
  "Enable both `go-jira-eldoc-mode' and `go-jira-popup-mode' in current buffer.
In GUI Emacs with posframe, the popup will be shown.
In terminal or without posframe, eldoc provides fallback."
  (interactive)
  (go-jira-eldoc-enable)
  (go-jira-popup-enable))

(provide 'go-jira-eldoc)
;;; go-jira-eldoc.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
