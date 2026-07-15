;;; go-jira-status-tests.el --- Tests for go-jira-status -*- lexical-binding: t; -*-
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
;;  Tests for Jira workflow status switching and status colorization.
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

(defun go-jira-status-tests--transition (id name to-name category-key)
  "Build a transitions-endpoint style hash-table entry.
ID, NAME, TO-NAME and CATEGORY-KEY mirror the fields the API returns."
  (let ((tr (make-hash-table :test 'equal))
        (to (make-hash-table :test 'equal))
        (cat (make-hash-table :test 'equal)))
    (puthash 'key category-key cat)
    (puthash 'name to-name to)
    (puthash 'statusCategory cat to)
    (puthash 'id id tr)
    (puthash 'name name tr)
    (puthash 'to to tr)
    tr))

(describe "go-jira--parse-transitions"
  (it "extracts id, name, target name and category"
    (let ((parsed (make-hash-table :test 'equal)))
      (puthash 'transitions
               (list (go-jira-status-tests--transition "31" "To Review" "Code Review" "indeterminate")
                     (go-jira-status-tests--transition "41" "Done" "Done" "done"))
               parsed)
      (expect (go-jira--parse-transitions parsed)
              :to-equal
              '((:id "31" :name "To Review" :to-name "Code Review" :category-key "indeterminate")
                (:id "41" :name "Done" :to-name "Done" :category-key "done")))))

  (it "returns nil when there are no transitions"
    (let ((parsed (make-hash-table :test 'equal)))
      (puthash 'transitions nil parsed)
      (expect (go-jira--parse-transitions parsed) :to-equal nil))))

(describe "go-jira--status-category->face"
  (it "maps the three known Jira status categories"
    (expect (go-jira--status-category->face "new") :to-be 'go-jira-status-todo-face)
    (expect (go-jira--status-category->face "indeterminate") :to-be 'go-jira-status-in-progress-face)
    (expect (go-jira--status-category->face "done") :to-be 'go-jira-status-done-face))

  (it "falls back to the default face for unknown or missing categories"
    (expect (go-jira--status-category->face "wat") :to-be 'go-jira-status-default-face)
    (expect (go-jira--status-category->face nil) :to-be 'go-jira-status-default-face)))

(describe "go-jira--status-face"
  (it "derives the face from the category when no override matches"
    (let ((go-jira-status-face-alist nil))
      (expect (go-jira--status-face "In Progress" "indeterminate")
              :to-be 'go-jira-status-in-progress-face)))

  (it "prefers a per-status override, matched case-insensitively"
    (let ((go-jira-status-face-alist '(("code review" . my-review-face))))
      (expect (go-jira--status-face "Code Review" "indeterminate")
              :to-be 'my-review-face)))

  (it "falls back to the category face when the status name is nil"
    (let ((go-jira-status-face-alist '(("code review" . my-review-face))))
      (expect (go-jira--status-face nil "done")
              :to-be 'go-jira-status-done-face))))

(describe "go-jira--fontify-status-keys"
  (it "applies the stored face to the marked region only"
    (with-temp-buffer
      (insert "SAC-1 rest")
      (put-text-property 1 6 'jira-status-key 'go-jira-status-done-face)
      (goto-char (point-min))
      (go-jira--fontify-status-keys (point-max))
      (expect (get-text-property 1 'face) :to-be 'go-jira-status-done-face)
      (expect (get-text-property 7 'face) :to-be nil))))

(describe "go-jira--transition-candidates"
  (it "labels a transition by its target status, annotating a differing name"
    (let* ((table (make-hash-table :test 'equal))
           (cands (go-jira--transition-candidates
                   '((:id "31" :name "To Review" :to-name "Code Review" :category-key "indeterminate")
                     (:id "41" :name "Done" :to-name "Done" :category-key "done"))
                   table)))
      (expect (mapcar #'substring-no-properties cands)
              :to-equal '("Code Review (To Review)" "Done"))
      (expect (plist-get (gethash (car cands) table) :id) :to-equal "31"))))

(describe "go-jira--fetch-transitions"
  :var (orig-exe orig-shell)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe))
    (setq orig-shell (symbol-function 'shell-command-to-string)))
  (after-each
    (fset 'go-jira--find-exe orig-exe)
    (fset 'shell-command-to-string orig-shell))

  (it "parses the transitions endpoint response"
    (fset 'go-jira--find-exe (lambda (&optional _e) "jira"))
    (fset 'shell-command-to-string
          (lambda (_cmd)
            "{\"transitions\":[{\"id\":\"21\",\"name\":\"Start\",\"to\":{\"name\":\"In Progress\",\"statusCategory\":{\"key\":\"indeterminate\"}}}]}"))
    (expect (go-jira--fetch-transitions "SAC-1")
            :to-equal '((:id "21" :name "Start" :to-name "In Progress" :category-key "indeterminate")))))

(describe "go-jira--apply-transition"
  :var (orig-exe orig-call captured exit-code output)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe))
    (setq orig-call (symbol-function 'call-process))
    (setq captured nil exit-code 0 output "")
    (fset 'go-jira--find-exe (lambda (&optional _e) "jira"))
    (fset 'call-process
          (lambda (_program &optional _infile _destination _display &rest args)
            (setq captured args)
            (when (and output (not (string-empty-p output)))
              (insert output))
            exit-code)))
  (after-each
    (fset 'go-jira--find-exe orig-exe)
    (fset 'call-process orig-call))

  (it "POSTs a transition payload to the transitions endpoint"
    (setq exit-code 0 output "")
    (expect (go-jira--apply-transition "SAC-1" "31") :to-be t)
    (expect captured :to-equal
            '("request" "rest/api/2/issue/SAC-1/transitions"
              "{\"transition\":{\"id\":\"31\"}}" "--method" "POST")))

  (it "signals an error when Jira returns errorMessages"
    (setq exit-code 0 output "{\"errorMessages\":[\"boom\"]}")
    (expect (go-jira--apply-transition "SAC-1" "31") :to-throw 'error))

  (it "signals an error on a non-zero exit code"
    (setq exit-code 1 output "")
    (expect (go-jira--apply-transition "SAC-1" "31") :to-throw 'error)))

(describe "go-jira--read-issue-key"
  :var (orig-read orig-current captured-default)
  (before-each
    (setq orig-read (symbol-function 'read-string)
          orig-current (symbol-function 'go-jira--current-ticket)
          captured-default nil)
    (fset 'read-string
          (lambda (_prompt &optional _init _hist default &rest _)
            (setq captured-default default)
            (or default ""))))
  (after-each
    (fset 'read-string orig-read)
    (fset 'go-jira--current-ticket orig-current))

  (it "reads with the current context ticket as the default"
    (fset 'go-jira--current-ticket (lambda () "SAC-42"))
    (expect (go-jira--read-issue-key) :to-equal "SAC-42")
    (expect captured-default :to-equal "SAC-42")))

(describe "go-jira-change-status Embark target handling"
  :var (orig-read orig-current orig-fetch orig-cread orig-apply orig-refresh log)
  (before-each
    (setq orig-read (symbol-function 'read-string)
          orig-current (symbol-function 'go-jira--current-ticket)
          orig-fetch (symbol-function 'go-jira--fetch-transitions)
          orig-cread (symbol-function 'consult--read)
          orig-apply (symbol-function 'go-jira--apply-transition)
          orig-refresh (symbol-function 'go-jira--refresh-after-transition)
          log nil)
    (fset 'go-jira--current-ticket (lambda () "SAC-CONTEXT"))
    (fset 'go-jira--fetch-transitions
          (lambda (k) (push (cons :fetched k) log)
            (list (list :id "1" :name "Done" :to-name "Done" :category-key "done"))))
    (fset 'consult--read
          (lambda (cands &rest _)
            (push (cons :picker (mapcar #'substring-no-properties cands)) log)
            nil))
    (fset 'go-jira--apply-transition (lambda (&rest _) (push (cons :applied t) log) t))
    (fset 'go-jira--refresh-after-transition (lambda (&rest _) nil)))
  (after-each
    (fset 'read-string orig-read)
    (fset 'go-jira--current-ticket orig-current)
    (fset 'go-jira--fetch-transitions orig-fetch)
    (fset 'consult--read orig-cread)
    (fset 'go-jira--apply-transition orig-apply)
    (fset 'go-jira--refresh-after-transition orig-refresh))

  (it "acts on the ticket from its first read and keeps the transitions picker unpolluted"
    ;; Embark injects its target into an action's first minibuffer read and
    ;; submits it, overriding the context default.  The transitions picker,
    ;; being a later read, must receive its own candidates, not the target.
    (fset 'read-string (lambda (&rest _) "SAC-999"))
    (call-interactively 'go-jira-change-status)
    (expect (cdr (assq :fetched log)) :to-equal "SAC-999")
    (expect (cdr (assq :picker log)) :to-equal '("Done")))

  (it "falls back to the context ticket when the read returns its default"
    (fset 'read-string (lambda (_prompt &optional _init _hist default &rest _) default))
    (call-interactively 'go-jira-change-status)
    (expect (cdr (assq :fetched log)) :to-equal "SAC-CONTEXT")))

(describe "go-jira-change-status cache invalidation"
  :var (orig-read orig-current orig-fetch orig-cread orig-apply orig-refresh
        orig-invalidate invalidated)
  (before-each
    (setq orig-read (symbol-function 'read-string)
          orig-current (symbol-function 'go-jira--current-ticket)
          orig-fetch (symbol-function 'go-jira--fetch-transitions)
          orig-cread (symbol-function 'consult--read)
          orig-apply (symbol-function 'go-jira--apply-transition)
          orig-refresh (symbol-function 'go-jira--refresh-after-transition)
          orig-invalidate (and (fboundp 'go-jira-eldoc--cache-invalidate)
                               (symbol-function 'go-jira-eldoc--cache-invalidate))
          invalidated nil)
    (fset 'read-string (lambda (&rest _) "SAC-1"))
    (fset 'go-jira--current-ticket (lambda () "SAC-1"))
    (fset 'go-jira--fetch-transitions
          (lambda (_k) (list (list :id "1" :name "Done" :to-name "Done" :category-key "done"))))
    (fset 'consult--read (lambda (cands &rest _) (car cands)))
    (fset 'go-jira--apply-transition (lambda (&rest _) t))
    (fset 'go-jira--refresh-after-transition (lambda (&rest _) nil))
    (fset 'go-jira-eldoc--cache-invalidate (lambda (k) (setq invalidated k))))
  (after-each
    (fset 'read-string orig-read)
    (fset 'go-jira--current-ticket orig-current)
    (fset 'go-jira--fetch-transitions orig-fetch)
    (fset 'consult--read orig-cread)
    (fset 'go-jira--apply-transition orig-apply)
    (fset 'go-jira--refresh-after-transition orig-refresh)
    (if orig-invalidate
        (fset 'go-jira-eldoc--cache-invalidate orig-invalidate)
      (fmakunbound 'go-jira-eldoc--cache-invalidate)))

  (it "invalidates the ticket's cache after a successful transition"
    (call-interactively 'go-jira-change-status)
    (expect invalidated :to-equal "SAC-1")))

(provide 'go-jira-status-tests)
;;; go-jira-status-tests.el ends here
