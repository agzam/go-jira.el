;;; go-jira-embark.el --- Embark integration for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.4.0
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Embark integration for go-jira, providing actions for Jira tickets at point.

;;; Code:

(require 'embark)
(require 'org-macs)

(defun go-jira-embark-target-ticket-at-point ()
  "Target jira ticket at point for embark."
  (when-let* ((jira-ticket-pattern "\\b[A-Z]+-[0-9]+\\b")
              (bounds (org-in-regexp jira-ticket-pattern 1))
              (beg (car bounds))
              (end (cdr bounds)))
    `(jira-ticket ,(buffer-substring-no-properties beg end)
      . ,(cons beg end))))

(defun go-jira-embark-comment-on-ticket (ticket)
  "Open TICKET and draft a comment on it.
Drafting happens in the ticket buffer rather than in a compose buffer of
its own, so adding a comment takes the same path from everywhere."
  (interactive "sJira ticket number: ")
  (go-jira-view-ticket ticket)
  (go-jira-comment-add))

(defvar-keymap go-jira-embark-jira-ticket-map
  :doc "Keymap for Jira ticket actions."
  :parent embark-general-map)

;; `keymap-set' merges into an existing `j' prefix rather than replacing it,
;; so a user-defined prefix of their own keeps its other bindings.
(keymap-set go-jira-embark-jira-ticket-map "j c" #'go-jira-embark-comment-on-ticket)

(add-to-list 'embark-target-finders #'go-jira-embark-target-ticket-at-point)
(add-to-list 'embark-keymap-alist '(jira-ticket . go-jira-embark-jira-ticket-map))

(provide 'go-jira-embark)
;;; go-jira-embark.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
