;;; go-jira-url.el --- Open or copy the Jira URL at point -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.6.0
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Opens or copies the URL of what point is on in a ticket view or a board:
;; a link, a ticket key, a comment, the ticket, or the board.  Org property
;; inheritance finds the comment and the ticket from any position, headings
;; nested in a description or a comment body included.

;;; Code:

(require 'org)
(require 'org-element)
(require 'thingatpt)
(require 'go-jira-comment)

(declare-function go-jira-ticket->url "go-jira")
(declare-function go-jira-board-view--board-url "go-jira-board")

(defun go-jira--link-at-point ()
  "Return the Org link object point is on, or nil.
A file link to a missing file does not count: Jira user mentions render
as such links, and Org could only report that the file does not exist."
  (let ((context (org-element-context)))
    (when (and (eq (org-element-type context) 'link)
               (or (not (equal (org-element-property :type context) "file"))
                   (file-exists-p (org-element-property :path context))))
      context)))

(defun go-jira--key-at-point ()
  "Return the Jira issue key point is on, or nil.
Unlike `go-jira--ticket-arg-or-ticket-at-point', never reads the kill
ring: a key copied from elsewhere says nothing about what point is on."
  (when-let* ((symbol (thing-at-point 'symbol t))
              ((string-match-p "\\`[A-Z]\\{2,10\\}-[0-9]+\\'" symbol)))
    symbol))

(defun go-jira--url-at-point ()
  "Return the URL of what point is on in a ticket view or a board.
The innermost wins: a link, then a ticket key, then the comment, the
ticket and the board that point is in."
  (unless (or (derived-mode-p 'go-jira-view-mode)
              (derived-mode-p 'go-jira-board-view-mode))
    (user-error "Not in a Jira buffer"))
  (if-let* ((link (go-jira--link-at-point)))
      (org-element-property :raw-link link)
    (let ((key (go-jira--key-at-point))
          (ticket (org-entry-get nil "ISSUE_KEY" t))
          (comment-id (org-entry-get nil go-jira-comment-id-property t)))
      (cond
       (key (go-jira-ticket->url key))
       ((and ticket comment-id)
        (go-jira-comment--anchor-url (go-jira-ticket->url ticket) comment-id))
       (ticket (go-jira-ticket->url ticket))
       ((derived-mode-p 'go-jira-board-view-mode)
        (go-jira-board-view--board-url))
       (t (user-error "No Jira comment, ticket or board at point"))))))

(defun go-jira-open-at-point ()
  "Open what point is on in a ticket view or a board.
A link opens the way Org opens links.  Anything else opens in the browser:
the ticket key under point, or else the comment, the ticket or the board
that point is in."
  (interactive)
  (let ((url (go-jira--url-at-point)))
    (if (go-jira--link-at-point)
        (org-open-at-point)
      (browse-url url))))

(defun go-jira-copy-url-at-point ()
  "Copy the URL of what point is on in a ticket view or a board.
It is the URL `go-jira-open-at-point' opens, or the target of a link."
  (interactive)
  (let ((url (go-jira--url-at-point)))
    (kill-new url)
    (message "%s" url)))

(provide 'go-jira-url)
;;; go-jira-url.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
