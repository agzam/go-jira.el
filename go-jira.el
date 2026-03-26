;;; go-jira.el --- Emacs interface to go-jira CLI tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (consult "1.0") (s "1.13.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; interface to the go-jira CLI tool (https://github.com/go-jira/jira)
;;
;; Features:
;; - Search and browse Jira issues
;; - View issue details
;; - Edit title, description, comments
;; - Convert issue keys to org-mode/markdown links
;; - Generate git branch names from issues
;; - Integration with consult for fuzzy searching

;;; Code:

(require 'json)
(require 'consult)
(require 'thingatpt)
(require 'ansi-color)
(require 'markdown-mode)
(require 's)

(require 'go-jira-edit)

(defgroup go-jira nil
  "Emacs interface to go-jira CLI tool."
  :group 'tools
  :prefix "go-jira-")

(defcustom go-jira-default-search-format-string "text ~ \"%s\""
  "Default, initial format string for search."
  :type 'string
  :group 'go-jira)

(defcustom go-jira-attachment-cache-dir
  (expand-file-name "go-jira/attachments" (or (getenv "XDG_CACHE_HOME")
                                               "~/.cache"))
  "Directory for caching downloaded Jira attachments.
Images are stored as CACHE-DIR/ATTACHMENT-ID/FILENAME to avoid
collisions and enable cache-busting by attachment ID."
  :type 'directory
  :group 'go-jira)

(defcustom go-jira-display-images t
  "When non-nil, download and display inline images in ticket views.
Images are downloaded asynchronously via `jira attach get' and
displayed using Org-mode inline image display."
  :type 'boolean
  :group 'go-jira)

;;; Internal utilities

(defun go-jira--find-exe (&optional exe)
  "Find and return executable EXE or throw an error.
Defaults to \"jira\" if EXE is not provided."
  (if-let ((ex (executable-find (or exe "jira"))))
      ex
    (error "ERROR: Could not locate %s" (or exe "jira"))))

(defun go-jira--ticket-arg-or-ticket-at-point (&optional ticket)
  "Resolve TICKET based on argument or `symbol-at-point'.
If TICKET is provided, return it.
Otherwise, check for a ticket at point.
Falls back to last kill in `kill-ring' if it's a valid ticket."
  (let* ((ticket-pattern "\\`[A-Z]\\{2,10\\}-[0-9]+\\'")
         (satp (thing-at-point 'symbol t))
         (ticket-at-point (when (and satp
                                     (string-match-p ticket-pattern satp))
                            satp))
         (kill-ring-ticket (when (and (not ticket)
                                      (not ticket-at-point)
                                      kill-ring)
                             (let ((last-kill (car kill-ring)))
                               (when (string-match-p ticket-pattern last-kill)
                                 last-kill)))))
    (or ticket ticket-at-point kill-ring-ticket)))

(defun go-jira--base-url ()
  "Get the Jira instance base URL.
Uses the serverInfo REST endpoint to determine the base URL."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
         (cmd (format "%s request '/rest/api/2/serverInfo' --method GET | %s -r '.baseUrl'" j jq))
         (result (string-trim (shell-command-to-string cmd))))
    (if (or (string-empty-p result) (string-match-p "error" result))
        (error "Failed to get Jira base URL")
      result)))

(defun go-jira--summary+url (ticket)
  "Fetch summary and url for a given TICKET.
Returns a plist with :ticket, :url, and :summary."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
         (cmd (format (concat
                       "%s view %s --template json | %s -r '{"
                       "summary: .fields.summary,"
                       "url: \"\\( .self | split(\"/rest\")[0] )/browse/\\( .key )\"}'")
                      j ticket jq))
         (res (shell-command-to-string cmd)))
    (if (string-match-p "jq: .* error:" res)
        (user-error res)
      (let* ((json-object-type 'hash-table)
             (json-key-type 'symbol)
             (json-array-type 'list)
             (parsed (json-read-from-string res))
             (summary (gethash 'summary parsed))
             (url (gethash 'url parsed)))
        (list :ticket ticket :url url :summary summary)))))

;;; Attachment / image support

(defun go-jira--parse-attachments (parsed-json)
  "Extract attachment map from PARSED-JSON (a hash-table from `json-read').
Returns an alist of (FILENAME . (:id ID :attrs nil :cache-path PATH))
suitable for binding to `go-jira-markup--attachment-map'."
  (when-let* ((fields (gethash 'fields parsed-json))
              (attachments (gethash 'attachment fields)))
    (let (result)
      (dolist (att attachments)
        (let* ((id (gethash 'id att))
               (filename (gethash 'filename att))
               (mime (or (gethash 'mimeType att) ""))
               ;; Only map image attachments
               (image-p (string-prefix-p "image/" mime)))
          (when (and image-p filename id)
            (let ((cache-path (expand-file-name
                               filename
                               (expand-file-name id go-jira-attachment-cache-dir))))
              (push (cons filename (list :id id :attrs nil :cache-path cache-path))
                    result)))))
      (nreverse result))))

(defun go-jira--download-attachments-async (buffer)
  "Download missing image attachments referenced in BUFFER.
Scans for [[file:...]] links pointing to the attachment cache,
checks which files are missing locally, and downloads them
asynchronously via `jira attach get'.  Always displays already-cached
images immediately, then refreshes display after new downloads complete."
  (when (and go-jira-display-images (buffer-live-p buffer))
    ;; Always display any already-cached images right away
    (with-current-buffer buffer
      (org-display-inline-images))
    ;; Then check for missing images that need downloading
    (let (to-download)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
            (let ((path (match-string 1)))
              (when (and (string-prefix-p (expand-file-name go-jira-attachment-cache-dir)
                                          (expand-file-name path))
                         (not (file-exists-p path)))
                ;; Extract attachment ID from the path: .../ATTACHMENT-ID/filename
                (let ((dir (file-name-directory path)))
                  (when (string-match "/\\([0-9]+\\)/$" dir)
                    (push (list :id (match-string 1 dir)
                                :path path)
                          to-download))))))))
      (when to-download
        (let* ((j (go-jira--find-exe))
               (pending (length to-download))
               (try-finish
                (lambda ()
                  (setq pending (1- pending))
                  (when (zerop pending)
                    ;; All downloads done — refresh inline images
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (org-display-inline-images)))))))
          (dolist (item to-download)
            (let* ((att-id (plist-get item :id))
                   (path (plist-get item :path))
                   (dir (file-name-directory path)))
              ;; Ensure cache directory exists
              (make-directory dir t)
              ;; Download asynchronously
              (make-process
               :name (format "jira-attach-%s" att-id)
               :command (list j "attach" "get" att-id "--output" path)
               :sentinel (lambda (proc _event)
                           (when (memq (process-status proc) '(exit signal))
                             (funcall try-finish)))))))))))

;;; Comment threading

(defun go-jira--fetch-comment-parent-ids (issue-key)
  "Fetch parent IDs for comments on ISSUE-KEY.
The embedded comments in `jira view --template json' omit the parentId
field, so we hit the dedicated comment endpoint which includes it.
Returns a hash-table mapping comment-id (string) to parent-id (string)."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "rest/api/2/issue/%s/comment" issue-key))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd))
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json-array-type 'list)
         (parent-map (make-hash-table :test 'equal)))
    (condition-case nil
        (let* ((parsed (json-read-from-string output))
               (comments (gethash 'comments parsed)))
          (dolist (c comments)
            (when-let ((parent-id (gethash 'parentId c))
                       (id (gethash 'id c)))
              (puthash (format "%s" id) (format "%s" parent-id) parent-map))))
      (error nil))
    parent-map))

(defun go-jira--thread-comments (comments parent-map)
  "Organise COMMENTS into threaded display order using PARENT-MAP.
COMMENTS is the flat list from the API (chronological, oldest first).
PARENT-MAP is a hash-table of child-id -> parent-id strings.

Returns a list of (COMMENT . IS-REPLY) cons cells where top-level
comments appear newest-first and replies sit chronologically under
their parent."
  (if (zerop (hash-table-count parent-map))
      ;; No threading info - fall back to simple reverse (current behaviour)
      (mapcar (lambda (c) (cons c nil)) (reverse comments))
    (let ((children-map (make-hash-table :test 'equal))
          top-level
          result)
      ;; Partition into top-level vs replies
      (dolist (c comments)
        (let* ((id (format "%s" (gethash 'id c)))
               (parent-id (gethash id parent-map)))
          (if parent-id
              (push c (gethash parent-id children-map))
            (push c top-level))))
      ;; top-level is newest-first (push reversed the chronological input).
      ;; children-map values are likewise reversed; we nreverse them below so
      ;; replies read chronologically under each parent.
      (dolist (parent top-level)
        (push (cons parent nil) result)
        (let* ((pid (format "%s" (gethash 'id parent)))
               (replies (nreverse (gethash pid children-map))))
          (dolist (reply replies)
            (push (cons reply t) result))))
      (nreverse result))))

;;; Public API - Ticket information

(defun go-jira-summary (ticket)
  "Retrieve the summary of TICKET number."
  (interactive)
  (let ((j (go-jira--find-exe)))
    (string-trim
     (shell-command-to-string
      (format "%s view %s --gjq 'fields.summary'" j ticket)))))

;;;###autoload
(defun go-jira-ticket->url (ticket)
  "Extract browsable url for the TICKET number."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
         (cmd (format "%s view %s --template json | %s -r '\"\\(.self | split(\"/rest\")[0])/browse/\\(.key)\"'"
                      j ticket jq))
         (res (shell-command-to-string cmd)))
    (if (string-match-p "jq: .* error:" res)
        (user-error res)
      (string-trim res))))

;;;###autoload
(defun go-jira-ticket->link (&optional ticket-arg)
  "Convert the TICKET-ARG number at point to `org-mode' link."
  (interactive)
  (let* ((ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (sum+url (go-jira--summary+url ticket))
         (ticket (plist-get sum+url :ticket))
         (url (plist-get sum+url :url))
         (summary (plist-get sum+url :summary))
         (result (if (eq major-mode 'org-mode)
                     (format "[[%s][%s: %s]]" url ticket summary)
                   (format "[%s: %s](%s)" ticket summary url))))
    (if ticket-arg
        result
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (delete-region (car bounds) (cdr bounds))
        (insert result)))))

;;;###autoload
(defun go-jira-ticket->num+description (&optional ticket-arg)
  "Convert the TICKET-ARG to number and description.
e.g., XYZ-1234 becomes XYZ-1234 - This ticket does nothing"
  (interactive)
  (let* ((ticket-regex "\\b[A-Z]+-[0-9]+\\b")
         (ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (already-desc-p (unless ticket-arg
                           (save-excursion
                             (beginning-of-thing 'symbol)
                             (looking-at-p (concat ticket-regex " - '.*'")))))
         (sum+url (go-jira--summary+url ticket))
         (summary (plist-get sum+url :summary))
         (result (if (derived-mode-p 'org-mode)
                     (format "%s - ~%s~" ticket summary)
                  (format "%s - '%s'" ticket summary))))
    (if ticket-arg
        result
      (unless already-desc-p
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (delete-region (car bounds) (cdr bounds))
          (insert result))))))

;;;###autoload
(defun go-jira-ticket->git-branch-name (&optional ticket-arg)
  "Convert TICKET-ARG to a git branch name.
e.g., SAC-28812 with Add New Metadata to tap-asana
becomes SAC-28812__add_new_metadata_tap-asana"
  (interactive)
  (let* ((ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (sum+url (go-jira--summary+url ticket))
         (summary (plist-get sum+url :summary))
         ;; Clean and format the summary
         (clean-summary
          (replace-regexp-in-string
           "_+" "_"                    ; Collapse multiple underscores
           (replace-regexp-in-string
            "^_\\|_$" ""               ; Remove leading/trailing underscores
            (replace-regexp-in-string
             "[^a-z0-9-]+" "_"         ; Replace non-alphanumeric (except hyphen) with underscore
             (replace-regexp-in-string
              "\\b\\(the\\|and\\|or\\|of\\|to\\|in\\|for\\|a\\|an\\|is\\|are\\|was\\|were\\|be\\|been\\|with\\|from\\|at\\|by\\|on\\)\\b" ""
              (downcase summary))))))
         (branch-name (format "%s__%s" ticket clean-summary)))
    ;; Truncate if too long (keep ticket number intact)
    (when (< 80 (length branch-name))
      (setq branch-name (concat (substring branch-name 0 77) "...")))
    (kill-new branch-name)
    (message "Branch name copied: '%s'" branch-name)
    branch-name))

;;; Ticket viewing and browsing

(defun go-jira--fontify-jira-headings (limit)
  "Fontify text with jira-heading property up to LIMIT."
  (let ((pos (point)))
    (while (and (< pos limit)
                (setq pos (next-single-property-change pos 'jira-heading nil limit)))
      (when-let ((level (get-text-property pos 'jira-heading)))
        (let* ((end (next-single-property-change pos 'jira-heading nil limit))
               (face (intern (format "markdown-header-face-%d" level))))
          (put-text-property pos end 'face face)
          (setq pos end))))
    nil))

(defvar go-jira-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-e") #'go-jira-edit)
    (define-key map (kbd "C-c C-y") #'go-jira-view-mode-copy-url)
    (define-key map (kbd "C-c C-o") #'go-jira-view-mode-open-browser)
    (define-key map (kbd "C-c M-r") #'go-jira-view-mode-refresh)
    (define-key map (kbd "C-c M-q") #'kill-buffer-and-window)
    map)
  "Keymap for `go-jira-view-mode'.")

(define-derived-mode go-jira-view-mode org-mode "Jira-View"
  "Major mode for viewing Jira tickets in `org-mode' format.
\\{go-jira-view-mode-map}"
  :group 'go-jira
  (require 'go-jira-edit)
  (setq-local buffer-read-only t)
  
  ;; Add font-lock for Jira headings
  (font-lock-add-keywords nil
   '((go-jira--fontify-jira-headings))))

(defun go-jira-view-mode-open-browser ()
  "Open ticket in browser from jira view mode."
  (interactive)
  (when-let ((ticket (buffer-local-value 'go-jira--ticket-number (current-buffer))))
    (browse-url (go-jira-ticket->url ticket))))

(defun go-jira-view-mode-copy-url ()
  "Copy URL for ticket in current buffer."
  (interactive)
  (when-let ((ticket (buffer-local-value 'go-jira--ticket-number (current-buffer))))
    (kill-new (go-jira-ticket->url ticket))
    (message "Copied URL for %s" ticket)))

(defun go-jira-view-mode-refresh ()
  "Refresh the current ticket view."
  (interactive)
  (when-let ((ticket (buffer-local-value 'go-jira--ticket-number (current-buffer))))
    (go-jira-view-ticket ticket)))

;;;###autoload
(defun go-jira-view-ticket (ticket)
  "View the TICKET in a buffer with Jira markup converted to Org-mode."
  (interactive "sJira ticket number: ")
  (require 'go-jira-markup)
  (let* ((j (go-jira--find-exe))
         (buf (get-buffer-create (format "*Jira: %s*" ticket)))
         (json-cmd (format "%s view %s --template json" j ticket))
         (json-output (shell-command-to-string json-cmd))
         (subtasks-out (thread-last
                         ticket
                         (format "%s list --query 'parent = %s'" j)
                         shell-command-to-string ansi-color-apply))
         (linked-items (thread-last
                         ticket
                         (format "%s list --query 'issue in linkedIssues(%s)'" j)
                         shell-command-to-string ansi-color-apply)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string json-output))
               (key (gethash 'key parsed))
               (fields (gethash 'fields parsed))
               (summary (when fields (gethash 'summary fields)))
               (description (when fields (gethash 'description fields)))
               (comment-data (when fields (gethash 'comment fields)))
               (comments (when comment-data (gethash 'comments comment-data)))
               ;; Build browse URL from `self' — avoids extra API calls.
               ;; self looks like "https://host/rest/api/2/issue/12345"
               (self-url (gethash 'self parsed))
               (base-url (when self-url
                           (if (string-match "\\(.*\\)/rest/" self-url)
                               (format "%s/browse/%s" (match-string 1 self-url) key)
                             ;; Fallback: fetch via API (single call, not per-comment)
                             (go-jira-ticket->url key))))
               ;; Parse attachment map for image support
               (attachment-map (go-jira--parse-attachments parsed))
               ;; Fetch comment parent IDs for threading
               (parent-map (when comments
                             (go-jira--fetch-comment-parent-ids key))))
          (with-current-buffer buf
            (setq-local buffer-read-only nil)
            (erase-buffer)
            (insert (format "* %s: %s\n" key summary))
            ;; Bind attachment map for markup conversion (image path rewriting)
            (let ((go-jira-markup--attachment-map attachment-map))
              (when description
                (insert "** Description\n")
                (insert (go-jira-markup-shift-headings
                         (go-jira-markup-to-org description) 2))
                (insert "\n\n"))
              (unless (s-blank-p subtasks-out)
                (insert "** Subtasks\n")
                (insert (string-trim subtasks-out))
                (insert "\n\n"))
              (unless (s-blank-p linked-items)
                (insert "** Linked work items\n")
                (insert (string-trim linked-items))
                (insert "\n\n"))
              (when comments
                (insert "** Comments\n")
                (let ((threaded (go-jira--thread-comments comments parent-map)))
                  (dolist (entry threaded)
                    (let* ((comment (car entry))
                           (is-reply (cdr entry))
                           (level (if is-reply 4 3))
                           (stars (make-string level ?*))
                           (author (gethash 'author comment))
                           (author-name (when author (gethash 'displayName author)))
                           (author-id (when author (gethash 'accountId author)))
                           (created (gethash 'created comment))
                           (body (gethash 'body comment))
                           (comment-id (gethash 'id comment)))
                      (when body
                        (let* ((timestamp (when created
                                            (condition-case nil
                                                (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time created))
                                              (error created))))
                               (timestamp-link (if (and comment-id base-url)
                                                   (format "[[%s?focusedCommentId=%s&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-%s][%s]]"
                                                           base-url comment-id comment-id timestamp)
                                                 timestamp))
                               (heading-start (point)))
                          (insert (format "%s %s - %s\n"
                                          stars
                                          (or author-name "Unknown")
                                          (or timestamp-link timestamp "")))
                          ;; Store author ID as text property on the heading
                          (when author-id
                            (put-text-property heading-start (point) 'jira-comment-author author-id))
                          ;; Mark replies with an org property so edit-mode can
                          ;; distinguish them from body headings at the same level.
                          (when is-reply
                            (let ((parent-id (gethash (format "%s" comment-id) parent-map)))
                              (insert (format ":PROPERTIES:\n:JIRA_PARENT_ID: %s\n:END:\n"
                                              (or parent-id "")))))
                          ;; Only convert if body has markup
                          (if (string-match-p "[{*_#+h-]\\|\\[\\[" body)
                              (insert (go-jira-markup-shift-headings
                                       (go-jira-markup-to-org body) level))
                            (insert body))
                          (insert "\n\n"))))))))
            (go-jira-view-mode)
            (put 'go-jira--ticket-number 'permanent-local t)
            (setq-local go-jira--ticket-number ticket)
            (goto-char (point-min))
            ;; Download and display images asynchronously
            (go-jira--download-attachments-async buf))
          (display-buffer buf)
          (select-window (get-buffer-window buf)))
      (error
       (message "Failed to parse issue JSON: %s" (error-message-string err))))))


;;;###autoload
(defun go-jira-search (&optional query)
  "Search Jira issues using QUERY.
If QUERY is not provided, uses `go-jira-default-search-format-string'."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        ;; place cursor between the quotes
        (search-backward "\""))
    (consult--read
     (consult--async-pipeline
      (consult--async-throttle)
      (consult--async-process
       (lambda (input)
         (when (not (string-match-p "\"\"" input)) ; query has no empty quote blocks
           (list "jira" "list" "--query" input)))))
     :initial (or query (format go-jira-default-search-format-string ""))
     :sort nil ; records must be of the exact order as the go-jira app output
     :state (lambda (action cand)
              (when (and cand (member action '(preview return)))
                (when-let ((ticket (progn (string-match "^[^:]+" cand)
                                          (match-string 0 cand))))
                  (go-jira-view-ticket ticket)))))))

(with-eval-after-load 'embark
  (require 'go-jira-embark))

(provide 'go-jira)
;;; go-jira.el ends here
