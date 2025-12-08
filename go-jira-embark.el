;;; go-jira-embark.el --- Embark integration for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (embark "0.20") (org "9.8"))
;; Keywords: tools, jira
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Embark integration for go-jira, providing actions for Jira tickets at point.

;;; Code:

(require 'go-jira)
(require 'org-macs)

;;;###autoload
(defun go-jira-embark-target-ticket-at-point ()
  "Target jira ticket at point for embark."
  (when-let* ((jira-ticket-pattern "\\b[A-Z]+-[0-9]+\\b")
              (bounds (org-in-regexp jira-ticket-pattern 1))
              (beg (car bounds))
              (end (cdr bounds)))
    `(jira-ticket ,(buffer-substring-no-properties beg end)
      . ,(cons beg end))))

(provide 'go-jira-embark)
;;; go-jira-embark.el ends here
