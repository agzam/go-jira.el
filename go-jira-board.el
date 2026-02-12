;;; go-jira-board.el --- Jira board browsing for go-jira -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Ag Ibragimov
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.
;;; Commentary:
;; jira board viewer
;;; Code:

(require 'json)
(require 'consult)
(require 'go-jira)
(require 'go-jira-markup)
(require 'go-jira-edit)


(defcustom go-jira-default-project "SAC"
  "Default Jira project key for board browsing."
  :type 'string
  :group 'go-jira)

(defcustom go-jira-board-show-active-sprint-only t
  "When non-nil, show only issues in the active sprint(s).
When nil, show all issues on the board (matching the board's filter).
Note: When enabled, this fetches issues from all active sprints on the board."
  :type 'boolean
  :group 'go-jira)

(defcustom go-jira-board-cache-duration 120
  "Number of seconds to cache issue content before refetching.
When expanding an issue heading, content will be fetched only if the cache
has expired.  Set to 0 to always fetch fresh data."
  :type 'integer
  :group 'go-jira)

(defcustom go-jira-default-board-id nil
  "Default board ID for quick access via `go-jira-browse-default-board'.
Set this to your most frequently used board ID to jump directly to it
without board selection.  You can find the board ID in the board URL or by
inspecting the board-data plist from `go-jira-browse-boards'."
  :type '(choice (const :tag "None" nil)
                 (integer :tag "Board ID"))
  :group 'go-jira)

(defcustom go-jira-default-board-name nil
  "Default board name corresponding to `go-jira-default-board-id'.
This is used for display purposes and to construct the buffer name."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Board Name"))
  :group 'go-jira)

;;; Internal board API functions

(defun go-jira--fetch-boards (project)
  "Fetch all boards for PROJECT.
Returns a list of plists with :id, :name, :type, :project keys."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board?projectKeyOrId=%s" project))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (boards (gethash 'values parsed)))
          (unless boards
            (error "No boards found for project: %s" project))
          (mapcar
           (lambda (board)
             (list :id (gethash 'id board)
                   :name (gethash 'name board)
                   :type (gethash 'type board)
                   :project (gethash 'projectKey (gethash 'location board))))
           boards))
      (error
       (error "Failed to fetch boards: %s\nOutput: %s" (error-message-string err) output)))))

(defun go-jira--fetch-board-config (board-id)
  "Fetch configuration for BOARD-ID.
Returns a plist with :filter-id and :columns keys."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board/%d/configuration" board-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (filter (gethash 'filter parsed))
               (filter-id (when filter (gethash 'id filter)))
               (column-config (gethash 'columnConfig parsed))
               (columns (when column-config (gethash 'columns column-config))))
          (list :filter-id filter-id
                :columns (go-jira--parse-board-columns columns)))
      (error
       (error "Failed to fetch board config for board %d: %s" board-id (error-message-string err))))))

(defun go-jira--parse-board-columns (columns)
  "Parse COLUMNS from board configuration API response.
Returns an alist of (column-name . (status-id-list))."
  (mapcar
   (lambda (col)
     (let ((name (gethash 'name col))
           (statuses (gethash 'statuses col)))
       (cons name
             (mapcar (lambda (status) (gethash 'id status))
                     statuses))))
   columns))

(defun go-jira--fetch-filter-jql (filter-id)
  "Fetch JQL query string for FILTER-ID."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/api/2/filter/%s" filter-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (jql (gethash 'jql parsed)))
          (unless jql
            (error "No JQL found for filter: %s" filter-id))
          jql)
      (error
       (error "Failed to fetch filter JQL for filter %s: %s" filter-id (error-message-string err))))))

(defun go-jira--fetch-active-sprints (board-id)
  "Fetch active sprint IDs for BOARD-ID.
Returns a list of sprint IDs, or nil if no active sprints."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board/%d/sprint?state=active" board-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (sprints (gethash 'values parsed)))
          (when sprints
            (mapcar (lambda (sprint) (gethash 'id sprint)) sprints)))
      (error
       (message "Warning: Could not fetch active sprints: %s" (error-message-string err))
       nil))))

(defun go-jira--get-board-data (board-id name type project)
  "Fetch complete board data for BOARD-ID, NAME, TYPE, PROJECT.
Returns a plist with all board information including JQL and columns."
  (message "Fetching board configuration...")
  (let* ((config (go-jira--fetch-board-config board-id))
         (filter-id (plist-get config :filter-id))
         (columns (plist-get config :columns))
         (jql (when filter-id
                (message "Fetching filter query...")
                (go-jira--fetch-filter-jql filter-id)))
         (active-sprint-ids (progn
                              (message "Fetching active sprints...")
                              (go-jira--fetch-active-sprints board-id))))
    (message "Board data retrieved")
    (list :id board-id
          :name name
          :type type
          :project project
          :filter-id filter-id
          :jql jql
          :columns columns
          :active-sprint-ids active-sprint-ids)))

;;; Issue fetching and board display

(defun go-jira--fetch-board-issues (board-id &optional sprint-ids)
  "Fetch all issues for BOARD-ID using the Jira board API.
If SPRINT-IDS (a list) is provided, fetch issues from those sprints.
Handles pagination automatically.  Returns a list of issue plists."
  (let* ((j (go-jira--find-exe))
         (all-issues '()))
    (if sprint-ids
        ;; Fetch from multiple sprints and combine
        (dolist (sprint-id sprint-ids)
          (let ((start-at 0)
                (max-results 100)
                (total nil))
            (while (or (null total) (< start-at total))
              (let* ((endpoint (format "/rest/agile/1.0/board/%d/sprint/%d/issue?startAt=%d&maxResults=%d"
                                       board-id sprint-id start-at max-results))
                     (cmd (format "%s request '%s' --method GET" j endpoint))
                     (output (shell-command-to-string cmd)))
                (condition-case err
                    (let* ((json-object-type 'hash-table)
                           (json-key-type 'symbol)
                           (json-array-type 'list)
                           (parsed (json-read-from-string output))
                           (issues (gethash 'issues parsed))
                           (returned (length issues)))
                      (setq total (gethash 'total parsed))
                      (when issues
                        (setq all-issues (append all-issues (mapcar #'go-jira--parse-issue issues))))
                      (setq start-at (+ start-at returned))
                      (when (zerop returned)
                        (setq start-at total)))
                  (error
                   (error "Failed to fetch sprint issues: %s\nOutput: %s"
                          (error-message-string err) output)))))))
      ;; Fetch all board issues (no sprint filter)
      (let ((start-at 0)
            (max-results 100)
            (total nil))
        (while (or (null total) (< start-at total))
          (let* ((endpoint (format "/rest/agile/1.0/board/%d/issue?startAt=%d&maxResults=%d"
                                   board-id start-at max-results))
                 (cmd (format "%s request '%s' --method GET" j endpoint))
                 (output (shell-command-to-string cmd)))
            (condition-case err
                (let* ((json-object-type 'hash-table)
                       (json-key-type 'symbol)
                       (json-array-type 'list)
                       (parsed (json-read-from-string output))
                       (issues (gethash 'issues parsed))
                       (returned (length issues)))
                  (setq total (gethash 'total parsed))
                  (when issues
                    (setq all-issues (append all-issues (mapcar #'go-jira--parse-issue issues))))
                  (setq start-at (+ start-at returned))
                  (when (zerop returned)
                    (setq start-at total)))
              (error
               (error "Failed to fetch board issues: %s\nOutput: %s"
                      (error-message-string err) output)))))))
    (message "Fetched %d issues from board" (length all-issues))
    all-issues))

(defun go-jira--parse-issue (issue-json)
  "Parse ISSUE-JSON hash-table into a plist."
  (let* ((key (gethash 'key issue-json))
         (fields (gethash 'fields issue-json))
         (summary (gethash 'summary fields))
         (status (gethash 'status fields))
         (status-id (when status (gethash 'id status)))
         (status-name (when status (gethash 'name status)))
         (assignee (gethash 'assignee fields))
         (assignee-name (when assignee (gethash 'displayName assignee)))
         (priority (gethash 'priority fields))
         (priority-name (when priority (gethash 'name priority)))
         (issuetype (gethash 'issuetype fields))
         (issuetype-name (when issuetype (gethash 'name issuetype)))
         (labels (gethash 'labels fields)))
    (list :key key
          :summary summary
          :status-id status-id
          :status-name status-name
          :assignee assignee-name
          :priority priority-name
          :issuetype issuetype-name
          :labels (when labels (mapcar #'identity labels)))))

(defun go-jira--group-issues-by-column (issues columns)
  "Group ISSUES by board COLUMNS using status IDs.
Returns an alist of (column-name . (issue-list))."
  (let ((grouped '()))
    (dolist (col columns)
      (let* ((col-name (car col))
             (status-ids (cdr col))
             (col-issues (seq-filter
                          (lambda (issue)
                            (member (plist-get issue :status-id) status-ids))
                          issues)))
        (push (cons col-name col-issues) grouped)))
    (nreverse grouped)))

(defun go-jira--build-board-buffer (board-data issues)
  "Build and return `org-mode' buffer for BOARD-DATA with ISSUES."
  (let* ((board-name (plist-get board-data :name))
         (columns (plist-get board-data :columns))
         (grouped (go-jira--group-issues-by-column issues columns))
         (buf-name (format "*Jira Board: %s*" board-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq-local buffer-read-only nil)
      (erase-buffer)

      ;; Insert org-columns setup
      (insert "#+COLUMNS: %50ITEM %12TODO %15ASSIGNEE %12PRIORITY %10ISSUETYPE %25LABELS\n")
      (insert "#+TITLE: " board-name "\n\n")

      ;; Insert each column with its issues
      (dolist (col-group grouped)
        (let ((col-name (car col-group))
              (col-issues (cdr col-group)))
          (insert "* " col-name "\n")
          (when col-issues
            (dolist (issue col-issues)
              (go-jira--insert-issue issue)))))

      (go-jira-board-view-mode)
      (setq-local go-jira--board-data board-data)
      (goto-char (point-min))
      (org-global-cycle 2))
    buf))

(defun go-jira--insert-issue (issue)
  "Insert ISSUE as org heading with properties."
  (let ((key (plist-get issue :key))
        (summary (plist-get issue :summary))
        (status (or (plist-get issue :status-name) ""))
        (assignee (or (plist-get issue :assignee) "Unassigned"))
        (priority (or (plist-get issue :priority) ""))
        (issuetype (or (plist-get issue :issuetype) ""))
        (labels (plist-get issue :labels)))
    (insert "** " key ": " summary "\n")
    (insert ":PROPERTIES:\n")
    (insert ":ISSUE_KEY: " key "\n")
    (insert ":TODO: " status "\n")
    (insert ":ASSIGNEE: " assignee "\n")
    (insert ":PRIORITY: " priority "\n")
    (insert ":ISSUETYPE: " issuetype "\n")
    (when labels
      (insert ":LABELS: " (mapconcat #'identity labels ", ") "\n"))
    (insert ":END:\n")))

;;; Board view mode

(defvar go-jira--board-data nil
  "Buffer-local variable storing board data plist.")

(defvar go-jira--expanded-issues nil
  "Buffer-local hash table tracking which issues have been expanded and fetched.")

(defvar go-jira-board-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'go-jira-board-view-issue)
    (define-key map (kbd "C-c M-e") #'go-jira-edit)
    (define-key map (kbd "C-c C-o") #'go-jira-board-browse-issue-url)
    (define-key map (kbd "C-c C-y") #'go-jira-board-view-copy-issue-url)
    (define-key map (kbd "C-c M-o") #'go-jira-board-view-open-browser)
    (define-key map (kbd "C-c M-y") #'go-jira-board-view-copy-url)
    (define-key map (kbd "C-c M-r") #'go-jira-board-refresh)
    (define-key map (kbd "C-c M-q") #'quit-window)
    map)
  "Keymap for go-jira-board-view-mode.")

(defun go-jira--on-cycle-expand-issue (state)
  "Hook function to fetch issue content on heading expansion.
STATE is the visibility state after cycling."
  (when (and (eq major-mode 'go-jira-board-view-mode)
             (eq state 'subtree)
             (= (org-outline-level) 2))
    (when-let* ((props (org-entry-properties))
                (key (cdr (assoc "ISSUE_KEY" props))))
      (let* ((last-fetch-time (gethash key go-jira--expanded-issues))
             (current-time (float-time))
             (cache-expired (or (not last-fetch-time)
                                (< (+ last-fetch-time go-jira-board-cache-duration)
                                   current-time))))
        (when cache-expired
          (message "Fetching content for %s..." key)
          (puthash key current-time go-jira--expanded-issues)
          (go-jira--fetch-and-insert-issue-content-async key (current-buffer)))))))

(defun go-jira--fetch-issue-details (issue-key)
  "Fetch detailed issue information for ISSUE-KEY as JSON.
Returns a plist with :description, :comments, etc."
  (let* ((j (go-jira--find-exe))
         (cmd (format "%s view %s --template json" j issue-key))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (fields (gethash 'fields parsed))
               (description (when fields (gethash 'description fields)))
               (comment-data (when fields (gethash 'comment fields)))
               (comments (when comment-data (gethash 'comments comment-data))))
          (list :description description
                :comments comments))
      (error
       (message "Warning: Failed to fetch issue details: %s" (error-message-string err))
       nil))))

(defun go-jira--fetch-subtasks (issue-key)
  "Fetch subtasks for ISSUE-KEY.
Returns the output as a string, or nil if empty."
  (require 'ansi-color)
  (let* ((j (go-jira--find-exe))
         (cmd (format "%s list --query 'parent = %s'" j issue-key))
         (output (ansi-color-apply (shell-command-to-string cmd))))
    (unless (string-blank-p output)
      (string-trim output))))

(defun go-jira--fetch-linked-items (issue-key)
  "Fetch linked work items for ISSUE-KEY.
Returns the output as a string, or nil if empty."
  (require 'ansi-color)
  (let* ((j (go-jira--find-exe))
         (cmd (format "%s list --query 'issue in linkedIssues(%s)'" j issue-key))
         (output (ansi-color-apply (shell-command-to-string cmd))))
    (unless (string-blank-p output)
      (string-trim output))))

(defun go-jira--adjust-heading-levels (text base-level)
  "Adjust `org-mode' heading levels in TEXT to be relative to BASE-LEVEL.
Only actual Org headings are adjusted; lines inside source/example blocks
are left untouched.  Uses `org-element' to identify real headings."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (let* ((tree (org-element-parse-buffer))
           ;; Collect heading positions from deepest to shallowest so
           ;; earlier edits don't shift later positions.
           (headings '()))
      (org-element-map tree 'headline
        (lambda (hl)
          (push (list (org-element-property :begin hl)
                      (org-element-property :level hl))
                headings)))
      ;; headings is already deepest-last due to push; process in order
      (dolist (h headings)
        (let ((pos (nth 0 h))
              (level (nth 1 h)))
          (goto-char pos)
          (when (looking-at "\\(\\*+\\) ")
            (replace-match (concat (make-string (+ level base-level) ?*) " "))))))
    (buffer-string)))

(defun go-jira--insert-issue-content (issue-key details subtasks linked-items buffer)
  "Insert fetched DETAILS for ISSUE-KEY into BUFFER at the issue heading.
SUBTASKS and LINKED-ITEMS are optional strings.  DETAILS is a plist with
:description, :comments, and :self-url."
  (when (and details (buffer-live-p buffer))
    (with-current-buffer buffer
      (save-excursion
        ;; Find the heading for this issue-key
        (goto-char (point-min))
        (unless (re-search-forward
                 (format "^\\(\\*+\\) %s:" (regexp-quote issue-key)) nil t)
          (error "Could not find heading for %s" issue-key))
        (org-back-to-heading t)
        (let* ((current-level (org-outline-level))
               (description-level (1+ current-level))
               (comment-author-level (+ current-level 2))
               ;; Build browse URL from self-url (no API call needed)
               (self-url (plist-get details :self-url))
               (base-url (when self-url
                           (when (string-match "\\(.*\\)/rest/" self-url)
                             (format "%s/browse/%s" (match-string 1 self-url) issue-key)))))
          (org-end-of-meta-data t)
          (when (looking-at org-property-drawer-re)
            (goto-char (match-end 0))
            (forward-line 1))
          ;; Remove "Loading..." placeholder if present
          (when (looking-at "^Loading\\.\\.\\.\n")
            (let ((inhibit-read-only t))
              (delete-region (match-beginning 0) (match-end 0))))
          (let ((inhibit-read-only t)
                (description (plist-get details :description))
                (comments (plist-get details :comments))
                (after-change-functions (remove 'org-fold-core--fix-folded-region after-change-functions)))

            ;; Insert description
            (when description
              (insert "\n")
              (let ((converted (go-jira-markup-to-org description)))
                (when converted
                  (insert (go-jira--adjust-heading-levels converted current-level)))
                (insert "\n")))

            ;; Insert subtasks
            (when subtasks
              (insert (format "%s Subtasks\n" (make-string description-level ?*)))
              (insert subtasks)
              (insert "\n\n"))

            ;; Insert linked items
            (when linked-items
              (insert (format "%s Linked work items\n" (make-string description-level ?*)))
              (insert linked-items)
              (insert "\n\n"))

            ;; Insert comments
            (when comments
              (insert (format "%s Comments\n" (make-string description-level ?*)))
              (dolist (comment (reverse comments))
                (let* ((author (gethash 'author comment))
                       (author-name (when author (gethash 'displayName author)))
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
                                             timestamp)))
                      (insert (format "%s %s - %s\n"
                                      (make-string comment-author-level ?*)
                                      (or author-name "Unknown")
                                      (or timestamp-link timestamp "")))
                      (if (string-match-p "[{*_#+h-]\\|\\[\\[" body)
                          (let ((converted (go-jira-markup-to-org body)))
                            (when converted
                              (insert (go-jira--adjust-heading-levels converted comment-author-level))))
                        (insert body))
                      (insert "\n"))))))

            (unless (looking-at-p "^\\s-*$")
              (insert "\n"))))
        (org-cycle-hide-drawers nil)))))

(defun go-jira--fetch-and-insert-issue-content (issue-key)
  "Fetch and insert content for ISSUE-KEY at current heading (synchronous)."
  (let ((details (go-jira--fetch-issue-details issue-key))
        (subtasks (go-jira--fetch-subtasks issue-key))
        (linked (go-jira--fetch-linked-items issue-key)))
    (go-jira--insert-issue-content issue-key details subtasks linked (current-buffer))))

(defun go-jira--fetch-and-insert-issue-content-async (issue-key buffer)
  "Fetch content for ISSUE-KEY asynchronously, then insert into BUFFER.
Shows a \"Loading...\" placeholder immediately, fires off the jira CLI
in the background, and fills in real content when the response arrives."
  ;; Insert placeholder at the issue heading
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "^\\(\\*+\\) %s:" (regexp-quote issue-key)) nil t)
          (org-back-to-heading t)
          (org-end-of-meta-data t)
          (when (looking-at org-property-drawer-re)
            (goto-char (match-end 0))
            (forward-line 1))
          (let ((inhibit-read-only t))
            (insert "Loading...\n"))))))

  ;; Fire off async fetch — all three calls in parallel
  (let* ((j (go-jira--find-exe))
         (detail-buf (generate-new-buffer (format " *jira-detail-%s*" issue-key)))
         (subtask-buf (generate-new-buffer (format " *jira-subtask-%s*" issue-key)))
         (linked-buf (generate-new-buffer (format " *jira-linked-%s*" issue-key)))
         ;; Track completion of all 3 processes
         (pending (list 'details 'subtasks 'linked))
         (try-finish
          (lambda ()
            (when (null pending)
              ;; All done — parse and insert
              (condition-case err
                  (let* ((json-output (with-current-buffer detail-buf (buffer-string)))
                         (details (condition-case nil
                                      (let* ((json-object-type 'hash-table)
                                             (json-key-type 'symbol)
                                             (json-array-type 'list)
                                             (parsed (json-read-from-string json-output))
                                             (self-url (gethash 'self parsed))
                                             (fields (gethash 'fields parsed))
                                             (description (when fields (gethash 'description fields)))
                                             (comment-data (when fields (gethash 'comment fields)))
                                             (comments (when comment-data (gethash 'comments comment-data))))
                                        (list :description description
                                              :comments comments
                                              :self-url self-url))
                                    (error nil)))
                         (subtasks (let ((s (string-trim
                                            (ansi-color-apply
                                             (with-current-buffer subtask-buf (buffer-string))))))
                                    (unless (string-blank-p s) s)))
                         (linked (let ((s (string-trim
                                           (ansi-color-apply
                                            (with-current-buffer linked-buf (buffer-string))))))
                                   (unless (string-blank-p s) s))))
                    (go-jira--insert-issue-content issue-key details subtasks linked buffer)
                    (message "Loaded %s" issue-key))
                (error
                 (message "Error loading %s: %s" issue-key (error-message-string err))))
              ;; Clean up temp buffers
              (kill-buffer detail-buf)
              (kill-buffer subtask-buf)
              (kill-buffer linked-buf)))))
    ;; Process 1: issue details (JSON)
    (make-process
     :name (format "jira-detail-%s" issue-key)
     :buffer detail-buf
     :command (list j "view" issue-key "--template" "json")
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (setq pending (delq 'details pending))
                   (funcall try-finish))))
    ;; Process 2: subtasks
    (make-process
     :name (format "jira-subtask-%s" issue-key)
     :buffer subtask-buf
     :command (list j "list" "--query" (format "parent = %s" issue-key))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (setq pending (delq 'subtasks pending))
                   (funcall try-finish))))
    ;; Process 3: linked items
    (make-process
     :name (format "jira-linked-%s" issue-key)
     :buffer linked-buf
     :command (list j "list" "--query" (format "issue in linkedIssues(%s)" issue-key))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (setq pending (delq 'linked pending))
                   (funcall try-finish))))))

(define-derived-mode go-jira-board-view-mode org-mode "Jira-Board"
  "Major mode for viewing Jira boards in `org-mode' format.
\\{go-jira-board-view-mode-map}"
  :group 'go-jira
  (setq-local buffer-read-only t)
  (setq-local go-jira--expanded-issues (make-hash-table :test 'equal))
  (add-hook 'org-cycle-hook #'go-jira--on-cycle-expand-issue nil t)

  ;; Add font-lock for Jira headings
  (font-lock-add-keywords nil
   '((go-jira--fontify-jira-headings))))

(defun go-jira-board-view-issue ()
  "View the Jira issue at point."
  (interactive)
  (when-let* ((props (org-entry-properties))
              (key (cdr (assoc "ISSUE_KEY" props))))
    (go-jira-view-ticket key)))

(defun go-jira-board-browse-issue-url ()
  "Open the Jira issue at point in browser."
  (interactive)
  (when-let* ((props (org-entry-properties))
              (key (cdr (assoc "ISSUE_KEY" props))))
    (browse-url (go-jira-ticket->url key))))

(defun go-jira-board-view--issue-key-at-point ()
  "Return the ISSUE_KEY property for the entry at point, or nil."
  (when-let* ((props (org-entry-properties))
              (key (cdr (assoc "ISSUE_KEY" props))))
    key))

(defun go-jira-board-view-copy-issue-url ()
  "Copy URL for the Jira issue at point."
  (interactive)
  (if-let ((key (go-jira-board-view--issue-key-at-point)))
      (progn
        (kill-new (go-jira-ticket->url key))
        (message "Copied URL for %s" key))
    (user-error "No issue at point")))

(defun go-jira-board-view--board-url ()
  "Return the browsable URL for the current board."
  (if-let ((board-data (buffer-local-value 'go-jira--board-data (current-buffer))))
      (let ((board-id (plist-get board-data :id)))
        (format "%s/secure/RapidBoard.jspa?rapidView=%d"
                (go-jira--base-url) board-id))
    (user-error "No board data found in current buffer")))

(defun go-jira-board-view-open-browser ()
  "Open the current board in browser."
  (interactive)
  (browse-url (go-jira-board-view--board-url)))

(defun go-jira-board-view-copy-url ()
  "Copy URL for the current board."
  (interactive)
  (let ((url (go-jira-board-view--board-url)))
    (kill-new url)
    (message "Copied board URL: %s" url)))

(defun go-jira-board-refresh ()
  "Refresh the current board view."
  (interactive)
  (if-let ((board-data (buffer-local-value 'go-jira--board-data (current-buffer))))
      (progn
        (message "Refreshing board...")
        (go-jira-display-board board-data))
    (user-error "No board data found in current buffer")))

;;; Public API

;;;###autoload
(defun go-jira-browse-boards (&optional project display)
  "Browse and select a Jira board.
With prefix arg, prompt for PROJECT.  Otherwise use
`go-jira-default-project'.

When called interactively, automatically displays the board.
When called from Lisp, returns a plist with complete board data
including JQL query and column mappings.  Pass DISPLAY non-nil
to display the board immediately."
  (interactive
   (list (when current-prefix-arg
           (read-string "Project: " go-jira-default-project))
         t))
  (let* ((project (or project go-jira-default-project))
         (_ (message "Fetching boards for project %s..." project))
         (boards (go-jira--fetch-boards project))
         ;; Create lookup table: board-name -> board-data
         (board-table (make-hash-table :test 'equal))
         (candidates (mapcar (lambda (board)
                               (let* ((name (plist-get board :name))
                                      (type (plist-get board :type))
                                      (project (plist-get board :project))
                                      (display-name (format "%s [%s] (%s)" name type project)))
                                 (puthash display-name board board-table)
                                 display-name))
                             boards))
         (selected-str (consult--read
                        candidates
                        :prompt (format "Board [%s]: " project)
                        :sort nil
                        :require-match t
                        :category 'jira-board
                        :history 'jira-board-history))
         (selected (when selected-str
                     (gethash selected-str board-table))))
    (when selected
      (let ((board-data (go-jira--get-board-data
                         (plist-get selected :id)
                         (plist-get selected :name)
                         (plist-get selected :type)
                         (plist-get selected :project))))
        (when display
          (go-jira-display-board board-data))
        board-data))))

;;;###autoload
(defun go-jira-display-board (&optional board-data force-refresh)
  "Display a Jira board in `org-mode' format.
If BOARD-DATA is not provided, prompts for board selection via
`go-jira-browse-boards'.  BOARD-DATA should be a plist with :id
and :columns keys.

When `go-jira-board-show-active-sprint-only' is non-nil,
only shows issues in the active sprints.  Otherwise shows all
issues on the board.

If a buffer for this board already exists and FORCE-REFRESH is nil,
simply switches to that buffer without re-fetching data.  Use
FORCE-REFRESH non-nil to force fetching fresh data.

When called interactively, uses prefix argument to force refresh."
  (interactive (list nil current-prefix-arg))
  (let ((board-data (or board-data (go-jira-browse-boards))))
    (unless board-data
      (user-error "No board selected"))
    (let* ((board-name (plist-get board-data :name))
           (buf-name (format "*Jira Board: %s*" board-name))
           (existing-buf (get-buffer buf-name)))
      ;; If buffer exists and we're not forcing a refresh, just switch to it
      (if (and existing-buf (not force-refresh))
          (progn
            (switch-to-buffer existing-buf)
            (message "Switched to existing board buffer. Press 'r' to refresh or use C-u to force refresh."))
        ;; Otherwise, fetch and rebuild
        (let* ((board-id (plist-get board-data :id))
               (active-sprint-ids (plist-get board-data :active-sprint-ids))
               (sprint-ids (when go-jira-board-show-active-sprint-only
                             active-sprint-ids)))
          (when (and go-jira-board-show-active-sprint-only (not active-sprint-ids))
            (message "Warning: No active sprints found, showing all board issues"))
          (when (and sprint-ids (> (length sprint-ids) 1))
            (message "Fetching issues from %d active sprints..." (length sprint-ids)))
          (message "Fetching issues for board: %s..." board-name)
          (let* ((issues (go-jira--fetch-board-issues board-id sprint-ids))
                 (buf (go-jira--build-board-buffer board-data issues)))
            (switch-to-buffer buf)
            (message "Loaded %d issues. " (length issues))))))))

;;;###autoload
(defun go-jira-set-default-board ()
  "Interactively select and set your default board.
Prompts for board selection and then sets `go-jira-default-board-id'
and `go-jira-default-board-name' for use with `go-jira-browse-default-board'."
  (interactive)
  (let ((board-data (go-jira-browse-boards nil nil)))
    (when board-data
      (customize-save-variable 'go-jira-default-board-id (plist-get board-data :id))
      (customize-save-variable 'go-jira-default-board-name (plist-get board-data :name))
      (message "Default board set to: %s (ID: %d)"
               go-jira-default-board-name
               go-jira-default-board-id))))

;;;###autoload
(defun go-jira-browse-default-board (&optional force-refresh)
  "Jump to or refresh your default Jira board.
Uses `go-jira-default-board-id' and `go-jira-default-board-name'.
If these are not set, prompts to browse and select a board.

If a buffer for the default board already exists, switches to it.
With prefix argument FORCE-REFRESH, fetches fresh data even if
the buffer exists.

This is a convenience command for quickly accessing your most
frequently used board without going through board selection."
  (interactive "P")
  (if (and go-jira-default-board-id go-jira-default-board-name)
      (let* ((buf-name (format "*Jira Board: %s*" go-jira-default-board-name))
             (existing-buf (get-buffer buf-name)))
        (if (and existing-buf (not force-refresh))
            (progn
              (switch-to-buffer existing-buf)
              (message "Switched to default board: %s. Press 'r' to refresh." go-jira-default-board-name))
          ;; Need to fetch board data
          (message "Fetching default board: %s..." go-jira-default-board-name)
          (let* ((config (go-jira--fetch-board-config go-jira-default-board-id))
                 (filter-id (plist-get config :filter-id))
                 (columns (plist-get config :columns))
                 (jql (when filter-id (go-jira--fetch-filter-jql filter-id)))
                 (active-sprint-ids (go-jira--fetch-active-sprints go-jira-default-board-id))
                 (board-data (list :id go-jira-default-board-id
                                   :name go-jira-default-board-name
                                   :type "scrum"  ; We don't store type, assume scrum
                                   :project go-jira-default-project
                                   :filter-id filter-id
                                   :jql jql
                                   :columns columns
                                   :active-sprint-ids active-sprint-ids)))
            (go-jira-display-board board-data t))))
    ;; No default board configured, fall back to browse
    (message "No default board configured. Use M-x go-jira-set-default-board to configure one.")
    (when (y-or-n-p "No default board configured.  Set one now? ")
      (call-interactively #'go-jira-set-default-board))))

(provide 'go-jira-board)
;;; go-jira-board.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:

