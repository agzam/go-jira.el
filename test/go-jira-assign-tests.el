;;; go-jira-assign-tests.el --- Tests for go-jira-assign -*- lexical-binding: t; -*-
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
;;  Tests for Jira assignee switching.
;;
;;; Code:

(require 'buttercup)

(unless (featurep 'consult)
  (provide 'consult))

;; Minimal fallbacks so the module loads and specs run whatever the suite's
;; load order is.  Guarded by `fboundp' so the real definitions (pulled in
;; elsewhere in the suite) win; we deliberately do not `provide' go-jira or
;; go-jira-status here, so their own bootstraps still load the real files.
(unless (fboundp 'go-jira--find-exe)
  (defun go-jira--find-exe (&optional _exe) "jira"))
(unless (fboundp 'go-jira--current-ticket)
  (defun go-jira--current-ticket () nil))
(unless (fboundp 'go-jira--read-issue-key)
  (defun go-jira--read-issue-key ()
    (read-string "Jira issue: " nil nil (go-jira--current-ticket))))
(unless (fboundp 'go-jira-view-ticket)
  (defun go-jira-view-ticket (_key) nil))
(unless (fboundp 'go-jira-board-refresh)
  (defun go-jira-board-refresh () nil))

(unless (featurep 'go-jira-assign)
  (load-file "go-jira-assign.el"))

(defun go-jira-assign-tests--user (account-id display-name email)
  "Build an assignable-search style hash-table for a user.
ACCOUNT-ID, DISPLAY-NAME and EMAIL mirror the fields the API returns."
  (let ((u (make-hash-table :test 'equal)))
    (puthash 'accountId account-id u)
    (puthash 'displayName display-name u)
    (puthash 'emailAddress email u)
    u))

(describe "go-jira--parse-assignable-users"
  (it "extracts account id, display name and email"
    (expect (go-jira--parse-assignable-users
             (list (go-jira-assign-tests--user "a1" "Ann" "ann@x.io")
                   (go-jira-assign-tests--user "b2" "Bob" "bob@x.io")))
            :to-equal
            '((:account-id "a1" :display-name "Ann" :email "ann@x.io")
              (:account-id "b2" :display-name "Bob" :email "bob@x.io"))))

  (it "returns nil when there are no users"
    (expect (go-jira--parse-assignable-users nil) :to-equal nil)))

(describe "go-jira--assignee-candidates"
  (it "offers Assign to me first, excludes me, and sorts the rest by name"
    (let* ((table (make-hash-table :test 'equal))
           (me '(:account-id "me" :display-name "Me Myself"))
           (users '((:account-id "z" :display-name "Zed" :email "zed@x.io")
                    (:account-id "me" :display-name "Me Myself" :email "me@x.io")
                    (:account-id "a" :display-name "Ann" :email "ann@x.io")))
           (cands (go-jira--assignee-candidates users me nil table)))
      (expect cands :to-equal '("Assign to me" "Ann <ann@x.io>" "Zed <zed@x.io>"))
      (expect (plist-get (gethash "Assign to me" table) :account-id) :to-equal "me")
      (expect (plist-get (gethash "Assign to me" table) :label) :to-equal "Me Myself")
      (expect (plist-get (gethash "Ann <ann@x.io>" table) :account-id) :to-equal "a")))

  (it "offers None (unassign) first when the issue is already assigned to me"
    (let* ((table (make-hash-table :test 'equal))
           (me '(:account-id "me" :display-name "Me Myself"))
           (current '(:account-id "me" :display-name "Me Myself"))
           (users '((:account-id "a" :display-name "Ann" :email "ann@x.io")))
           (cands (go-jira--assignee-candidates users me current table)))
      (expect (car cands) :to-equal "None (unassign)")
      (expect (gethash "None (unassign)" table)
              :to-equal '(:account-id nil :label "Unassigned"))
      (expect (member "Assign to me" cands) :to-equal nil)))

  (it "keeps Assign to me first when the issue is assigned to someone else"
    (let* ((table (make-hash-table :test 'equal))
           (me '(:account-id "me" :display-name "Me"))
           (current '(:account-id "other" :display-name "Other"))
           (users '((:account-id "other" :display-name "Other" :email "o@x.io")))
           (cands (go-jira--assignee-candidates users me current table)))
      (expect cands :to-equal '("Assign to me" "Other <o@x.io>"))))

  (it "falls back to the display name when a user has no email"
    (let* ((table (make-hash-table :test 'equal))
           (me '(:account-id "me" :display-name "Me"))
           (users '((:account-id "a" :display-name "Ann" :email nil)))
           (cands (go-jira--assignee-candidates users me nil table)))
      (expect cands :to-equal '("Assign to me" "Ann")))))

(describe "go-jira--fetch-assignable-users"
  :var (orig-exe orig-shell)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe)
          orig-shell (symbol-function 'shell-command-to-string))
    (fset 'go-jira--find-exe (lambda (&optional _e) "jira")))
  (after-each
    (fset 'go-jira--find-exe orig-exe)
    (fset 'shell-command-to-string orig-shell))

  (it "parses the assignable-search response"
    (fset 'shell-command-to-string
          (lambda (_cmd)
            "[{\"accountId\":\"a1\",\"displayName\":\"Ann\",\"emailAddress\":\"ann@x.io\"}]"))
    (expect (go-jira--fetch-assignable-users "SAC-1")
            :to-equal '((:account-id "a1" :display-name "Ann" :email "ann@x.io")))))

(describe "go-jira--current-user"
  :var (orig-exe orig-shell)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe)
          orig-shell (symbol-function 'shell-command-to-string))
    (fset 'go-jira--find-exe (lambda (&optional _e) "jira")))
  (after-each
    (fset 'go-jira--find-exe orig-exe)
    (fset 'shell-command-to-string orig-shell))

  (it "parses accountId and displayName from /myself"
    (fset 'shell-command-to-string
          (lambda (_cmd) "{\"accountId\":\"me\",\"displayName\":\"Me\",\"name\":null}"))
    (expect (go-jira--current-user) :to-equal '(:account-id "me" :display-name "Me"))))

(describe "go-jira--issue-assignee"
  :var (orig-exe orig-shell)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe)
          orig-shell (symbol-function 'shell-command-to-string))
    (fset 'go-jira--find-exe (lambda (&optional _e) "jira")))
  (after-each
    (fset 'go-jira--find-exe orig-exe)
    (fset 'shell-command-to-string orig-shell))

  (it "returns the assignee plist when the issue is assigned"
    (fset 'shell-command-to-string
          (lambda (_cmd)
            "{\"fields\":{\"assignee\":{\"accountId\":\"a1\",\"displayName\":\"Ann\"}}}"))
    (expect (go-jira--issue-assignee "SAC-1")
            :to-equal '(:account-id "a1" :display-name "Ann")))

  (it "returns nil when the issue is unassigned"
    (fset 'shell-command-to-string
          (lambda (_cmd) "{\"fields\":{\"assignee\":null}}"))
    (expect (go-jira--issue-assignee "SAC-1") :to-equal nil)))

(describe "go-jira--apply-assignee"
  :var (orig-exe orig-call captured exit-code output)
  (before-each
    (setq orig-exe (symbol-function 'go-jira--find-exe)
          orig-call (symbol-function 'call-process)
          captured nil exit-code 0 output "")
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

  (it "PUTs an accountId payload to the assignee endpoint"
    (setq exit-code 0 output "")
    (expect (go-jira--apply-assignee "SAC-1" "a1") :to-be t)
    (expect captured :to-equal
            '("request" "rest/api/2/issue/SAC-1/assignee"
              "{\"accountId\":\"a1\"}" "--method" "PUT")))

  (it "sends a null accountId to unassign"
    (setq exit-code 0 output "")
    (expect (go-jira--apply-assignee "SAC-1" nil) :to-be t)
    (expect captured :to-equal
            '("request" "rest/api/2/issue/SAC-1/assignee"
              "{\"accountId\":null}" "--method" "PUT")))

  (it "signals an error when Jira returns errorMessages"
    (setq exit-code 0 output "{\"errorMessages\":[\"boom\"]}")
    (expect (go-jira--apply-assignee "SAC-1" "a1") :to-throw 'error))

  (it "signals an error on a non-zero exit code"
    (setq exit-code 1 output "")
    (expect (go-jira--apply-assignee "SAC-1" "a1") :to-throw 'error)))

(describe "go-jira-assign Embark target handling"
  :var (orig-read orig-current orig-user orig-assignee orig-fetch
        orig-cread orig-apply orig-refresh log)
  (before-each
    (setq orig-read (symbol-function 'read-string)
          orig-current (symbol-function 'go-jira--current-ticket)
          orig-user (symbol-function 'go-jira--current-user)
          orig-assignee (symbol-function 'go-jira--issue-assignee)
          orig-fetch (symbol-function 'go-jira--fetch-assignable-users)
          orig-cread (symbol-function 'consult--read)
          orig-apply (symbol-function 'go-jira--apply-assignee)
          orig-refresh (symbol-function 'go-jira--refresh-after-assign)
          log nil)
    (fset 'go-jira--current-ticket (lambda () "SAC-CONTEXT"))
    (fset 'go-jira--current-user (lambda () '(:account-id "me" :display-name "Me")))
    (fset 'go-jira--issue-assignee (lambda (_k) nil))
    (fset 'go-jira--fetch-assignable-users
          (lambda (k)
            (push (cons :fetched k) log)
            (list '(:account-id "a" :display-name "Ann" :email "ann@x.io"))))
    (fset 'consult--read
          (lambda (cands &rest _)
            (push (cons :picker (mapcar #'substring-no-properties cands)) log)
            nil))
    (fset 'go-jira--apply-assignee (lambda (&rest _) (push (cons :applied t) log) t))
    (fset 'go-jira--refresh-after-assign (lambda (&rest _) nil)))
  (after-each
    (fset 'read-string orig-read)
    (fset 'go-jira--current-ticket orig-current)
    (fset 'go-jira--current-user orig-user)
    (fset 'go-jira--issue-assignee orig-assignee)
    (fset 'go-jira--fetch-assignable-users orig-fetch)
    (fset 'consult--read orig-cread)
    (fset 'go-jira--apply-assignee orig-apply)
    (fset 'go-jira--refresh-after-assign orig-refresh))

  (it "acts on the ticket from its first read and keeps the picker unpolluted"
    ;; Embark injects its target into an action's first minibuffer read and
    ;; submits it, overriding the context default.  The assignee picker, being
    ;; a later read, must receive its own candidates, not the target.
    (fset 'read-string (lambda (&rest _) "SAC-999"))
    (call-interactively 'go-jira-assign)
    (expect (cdr (assq :fetched log)) :to-equal "SAC-999")
    (expect (cdr (assq :picker log)) :to-equal '("Assign to me" "Ann <ann@x.io>")))

  (it "falls back to the context ticket when the read returns its default"
    (fset 'read-string (lambda (_prompt &optional _init _hist default &rest _) default))
    (call-interactively 'go-jira-assign)
    (expect (cdr (assq :fetched log)) :to-equal "SAC-CONTEXT")))

(describe "go-jira-assign applying a choice"
  :var (orig-read orig-current orig-user orig-assignee orig-fetch
        orig-cread orig-apply orig-refresh applied)
  (before-each
    (setq orig-read (symbol-function 'read-string)
          orig-current (symbol-function 'go-jira--current-ticket)
          orig-user (symbol-function 'go-jira--current-user)
          orig-assignee (symbol-function 'go-jira--issue-assignee)
          orig-fetch (symbol-function 'go-jira--fetch-assignable-users)
          orig-cread (symbol-function 'consult--read)
          orig-apply (symbol-function 'go-jira--apply-assignee)
          orig-refresh (symbol-function 'go-jira--refresh-after-assign)
          applied 'unset)
    (fset 'read-string (lambda (&rest _) "SAC-1"))
    (fset 'go-jira--current-ticket (lambda () "SAC-1"))
    (fset 'go-jira--current-user (lambda () '(:account-id "me" :display-name "Me")))
    (fset 'go-jira--issue-assignee (lambda (_k) nil))
    (fset 'go-jira--fetch-assignable-users
          (lambda (_k) (list '(:account-id "a" :display-name "Ann" :email "ann@x.io"))))
    (fset 'go-jira--apply-assignee (lambda (_k id) (setq applied id) t))
    (fset 'go-jira--refresh-after-assign (lambda (&rest _) nil)))
  (after-each
    (fset 'read-string orig-read)
    (fset 'go-jira--current-ticket orig-current)
    (fset 'go-jira--current-user orig-user)
    (fset 'go-jira--issue-assignee orig-assignee)
    (fset 'go-jira--fetch-assignable-users orig-fetch)
    (fset 'consult--read orig-cread)
    (fset 'go-jira--apply-assignee orig-apply)
    (fset 'go-jira--refresh-after-assign orig-refresh))

  (it "applies the account id of the picked user"
    (fset 'consult--read (lambda (cands &rest _) (nth 1 cands)))
    (call-interactively 'go-jira-assign)
    (expect applied :to-equal "a"))

  (it "applies a nil account id (unassign) when None is picked while assigned to me"
    (fset 'go-jira--issue-assignee (lambda (_k) '(:account-id "me" :display-name "Me")))
    (fset 'consult--read (lambda (cands &rest _) (car cands)))
    (call-interactively 'go-jira-assign)
    (expect applied :to-be nil)))

(provide 'go-jira-assign-tests)
;;; go-jira-assign-tests.el ends here
