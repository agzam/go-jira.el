;;; go-jira-embark.el --- Embark integration for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.3.0
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

(defvar-keymap go-jira-embark-jira-ticket-map
  :doc "Keymap for Jira ticket actions."
  :parent embark-general-map)

(add-to-list 'embark-target-finders #'go-jira-embark-target-ticket-at-point)
(add-to-list 'embark-keymap-alist '(jira-ticket . go-jira-embark-jira-ticket-map))

(provide 'go-jira-embark)
;;; go-jira-embark.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
