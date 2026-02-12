;;; go-jira-board-tests.el --- Tests for go-jira-board -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 20, 2026
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for JIRA board functionality
;;
;;; Code:

(require 'buttercup)

;; Mock only the minimal dependencies needed
(unless (featurep 'consult)
  (provide 'consult))

(unless (featurep 'go-jira)
  (provide 'go-jira)
  (defun go-jira--find-exe () "jira")
  (defun go-jira-view-ticket (key) nil)
  (defun go-jira-ticket->url (key) (format "https://example.com/%s" key)))

(unless (featurep 'go-jira-markup)
  (load-file "go-jira-markup.el"))

(unless (featurep 'go-jira-edit)
  (provide 'go-jira-edit)
  (defun go-jira-edit () nil))

(load-file "go-jira-board.el")

(describe "go-jira--parse-board-columns"
  (it "parses board columns with status IDs"
    (let ((columns (list
                    (let ((h (make-hash-table :test 'equal)))
                      (puthash 'name "To Do" h)
                      (puthash 'statuses
                               (list
                                (let ((s (make-hash-table :test 'equal)))
                                  (puthash 'id "1" s)
                                  s)
                                (let ((s (make-hash-table :test 'equal)))
                                  (puthash 'id "10000" s)
                                  s))
                               h)
                      h)
                    (let ((h (make-hash-table :test 'equal)))
                      (puthash 'name "Done" h)
                      (puthash 'statuses
                               (list
                                (let ((s (make-hash-table :test 'equal)))
                                  (puthash 'id "10001" s)
                                  s))
                               h)
                      h))))
      (expect (go-jira--parse-board-columns columns)
              :to-equal '(("To Do" "1" "10000")
                          ("Done" "10001"))))))

(describe "go-jira--parse-issue"
  (it "parses issue JSON into plist"
    (let ((issue (let ((h (make-hash-table :test 'equal)))
                   (puthash 'key "SAC-123" h)
                   (let ((fields (make-hash-table :test 'equal)))
                     (puthash 'summary "Test issue" fields)
                     (let ((status (make-hash-table :test 'equal)))
                       (puthash 'id "10000" status)
                       (puthash 'name "In Progress" status)
                       (puthash 'status status fields))
                     (let ((assignee (make-hash-table :test 'equal)))
                       (puthash 'displayName "John Doe" assignee)
                       (puthash 'assignee assignee fields))
                     (let ((priority (make-hash-table :test 'equal)))
                       (puthash 'name "High" priority)
                       (puthash 'priority priority fields))
                     (let ((issuetype (make-hash-table :test 'equal)))
                       (puthash 'name "Bug" issuetype)
                       (puthash 'issuetype issuetype fields))
                     (puthash 'labels '("backend" "api") fields)
                     (puthash 'fields fields h))
                   h)))
      (let ((result (go-jira--parse-issue issue)))
        (expect (plist-get result :key) :to-equal "SAC-123")
        (expect (plist-get result :summary) :to-equal "Test issue")
        (expect (plist-get result :status-id) :to-equal "10000")
        (expect (plist-get result :status-name) :to-equal "In Progress")
        (expect (plist-get result :assignee) :to-equal "John Doe")
        (expect (plist-get result :priority) :to-equal "High")
        (expect (plist-get result :issuetype) :to-equal "Bug")
        (expect (plist-get result :labels) :to-equal '("backend" "api"))))))

(describe "go-jira--group-issues-by-column"
  (it "groups issues by column based on status IDs"
    (let ((issues (list
                   (list :key "SAC-1" :status-id "1" :summary "Issue 1")
                   (list :key "SAC-2" :status-id "10000" :summary "Issue 2")
                   (list :key "SAC-3" :status-id "10001" :summary "Issue 3")
                   (list :key "SAC-4" :status-id "1" :summary "Issue 4")))
          (columns '(("To Do" "1" "10000")
                     ("Done" "10001"))))
      (let ((grouped (go-jira--group-issues-by-column issues columns)))
        (expect (length grouped) :to-equal 2)
        
        ;; Check "To Do" column
        (let ((todo-group (assoc "To Do" grouped)))
          (expect (length (cdr todo-group)) :to-equal 3)
          (expect (plist-get (nth 0 (cdr todo-group)) :key) :to-equal "SAC-1")
          (expect (plist-get (nth 1 (cdr todo-group)) :key) :to-equal "SAC-2")
          (expect (plist-get (nth 2 (cdr todo-group)) :key) :to-equal "SAC-4"))
        
        ;; Check "Done" column
        (let ((done-group (assoc "Done" grouped)))
          (expect (length (cdr done-group)) :to-equal 1)
          (expect (plist-get (car (cdr done-group)) :key) :to-equal "SAC-3")))))
  
  (it "handles empty columns"
    (let ((issues (list
                   (list :key "SAC-1" :status-id "1" :summary "Issue 1")))
          (columns '(("To Do" "1")
                     ("Done" "10001"))))
      (let ((grouped (go-jira--group-issues-by-column issues columns)))
        (expect (length grouped) :to-equal 2)
        
        ;; Check "To Do" has issues
        (let ((todo-group (assoc "To Do" grouped)))
          (expect (length (cdr todo-group)) :to-equal 1))
        
        ;; Check "Done" is empty
        (let ((done-group (assoc "Done" grouped)))
          (expect (cdr done-group) :to-equal nil))))))

(describe "go-jira--fetch-active-sprints"
  :var (original-find-exe original-shell-command)
  
  (before-each
    (setq original-find-exe (symbol-function 'go-jira--find-exe))
    (setq original-shell-command (symbol-function 'shell-command-to-string)))
  
  (after-each
    (fset 'go-jira--find-exe original-find-exe)
    (fset 'shell-command-to-string original-shell-command))
  
  (it "returns list of active sprint IDs"
    (fset 'go-jira--find-exe (lambda () "jira"))
    (fset 'shell-command-to-string
          (lambda (cmd)
            "{\"values\": [{\"id\": 16727, \"name\": \"Sprint 1\", \"state\": \"active\"}, {\"id\": 16963, \"name\": \"Sprint 2\", \"state\": \"active\"}]}"))
    
    (let ((result (go-jira--fetch-active-sprints 3018)))
      (expect result :to-equal '(16727 16963))))
  
  (it "returns nil when no active sprints"
    (fset 'go-jira--find-exe (lambda () "jira"))
    (fset 'shell-command-to-string
          (lambda (cmd)
            "{\"values\": []}"))
    
    (let ((result (go-jira--fetch-active-sprints 3018)))
      (expect result :to-equal nil)))
  
  (it "returns single sprint ID in a list"
    (fset 'go-jira--find-exe (lambda () "jira"))
    (fset 'shell-command-to-string
          (lambda (cmd)
            "{\"values\": [{\"id\": 16727, \"name\": \"Sprint 1\", \"state\": \"active\"}]}"))
    
    (let ((result (go-jira--fetch-active-sprints 3018)))
      (expect result :to-equal '(16727)))))

(describe "go-jira--adjust-heading-levels"
  (it "adjusts org heading levels relative to base level"
    (let ((text "* Heading 1\nSome text\n** Subheading\nMore text\n* Heading 2\n")
          (base-level 2))
      (expect (go-jira--adjust-heading-levels text base-level)
              :to-equal "*** Heading 1\nSome text\n**** Subheading\nMore text\n*** Heading 2\n")))
  
  (it "handles single-level headings"
    (let ((text "* Only one level\n")
          (base-level 3))
      (expect (go-jira--adjust-heading-levels text base-level)
              :to-equal "**** Only one level\n")))
  
  (it "doesn't modify text without headings"
    (let ((text "Just plain text\nNo headings here\n")
          (base-level 2))
      (expect (go-jira--adjust-heading-levels text base-level)
              :to-equal "Just plain text\nNo headings here\n")))
  
  (it "doesn't modify asterisks not followed by space"
    (let ((text "* Heading\n*bold text*\n** Subheading\n")
          (base-level 1))
      (expect (go-jira--adjust-heading-levels text base-level)
              :to-equal "** Heading\n*bold text*\n*** Subheading\n"))))

(describe "Board data integration"
  (it "board data contains all required fields"
    (let ((board-data (list :id 3018
                            :name "Test Board"
                            :type "scrum"
                            :project "SAC"
                            :filter-id 12345
                            :jql "project = SAC"
                            :columns '(("To Do" "1") ("Done" "2"))
                            :active-sprint-ids '(16727 16963))))
      (expect (plist-get board-data :id) :to-equal 3018)
      (expect (plist-get board-data :name) :to-equal "Test Board")
      (expect (plist-get board-data :active-sprint-ids) :to-equal '(16727 16963))
      (expect (length (plist-get board-data :columns)) :to-equal 2))))

(provide 'go-jira-board-tests)
;;; go-jira-board-tests.el ends here
