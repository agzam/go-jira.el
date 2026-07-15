;;; go-jira-eldoc-tests.el --- Tests for go-jira-eldoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for eldoc/popup ticket info parsing and formatting.
;;
;;; Code:

(require 'buttercup)

(unless (featurep 'consult)
  (provide 'consult))

(unless (featurep 'go-jira)
  (provide 'go-jira)
  (defun go-jira--find-exe (&optional _exe) "jira")
  (defun go-jira--ticket-arg-or-ticket-at-point (&optional ticket) ticket)
  (defun go-jira-view-ticket (_key) nil))

(unless (featurep 'go-jira-board)
  (provide 'go-jira-board)
  (defun go-jira-board-refresh () nil))

(unless (featurep 'go-jira-status)
  (load-file "go-jira-status.el"))

(unless (featurep 'go-jira-eldoc)
  (load-file "go-jira-eldoc.el"))

(describe "go-jira-eldoc--parse-info"
  (it "extracts summary, status and category"
    (expect (go-jira-eldoc--parse-info
             "{\"fields\":{\"summary\":\"Fix the thing\",\"status\":{\"name\":\"In Progress\",\"statusCategory\":{\"key\":\"indeterminate\"}}}}")
            :to-equal '(:summary "Fix the thing" :status "In Progress" :category "indeterminate")))

  (it "returns nil for an empty summary"
    (expect (go-jira-eldoc--parse-info "{\"fields\":{\"summary\":\"\"}}") :to-be nil))

  (it "returns nil for unparseable JSON"
    (expect (go-jira-eldoc--parse-info "not json") :to-be nil)))

(describe "go-jira-popup--format-description"
  (it "shows a status tag for an info plist"
    (expect (substring-no-properties
             (go-jira-popup--format-description
              "SAC-1" '(:summary "Fix the thing" :status "In Progress" :category "indeterminate")))
            :to-equal "SAC-1 [In Progress]: Fix the thing"))

  (it "remains backward-compatible with a plain summary string"
    (expect (substring-no-properties
             (go-jira-popup--format-description "SAC-1" "Legacy summary"))
            :to-equal "SAC-1: Legacy summary")))

(describe "go-jira-eldoc--cache-invalidate"
  (it "removes a single ticket's cached entry, leaving others intact"
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s" :status "Done" :category "done"))
    (go-jira-eldoc--cache-put "SAC-2" '(:summary "t" :status "To Do" :category "new"))
    (go-jira-eldoc--cache-invalidate "SAC-1")
    (expect (go-jira-eldoc--cache-get "SAC-1") :to-be nil)
    (expect (go-jira-eldoc--cache-get "SAC-2")
            :to-equal '(:summary "t" :status "To Do" :category "new"))))

(provide 'go-jira-eldoc-tests)
;;; go-jira-eldoc-tests.el ends here
