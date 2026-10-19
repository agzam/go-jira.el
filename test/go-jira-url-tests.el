;;; go-jira-url-tests.el --- Tests for go-jira-url -*- lexical-binding: t; -*-
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
;;  Tests for opening and copying the URL of what point is on.
;;
;;; Code:

(require 'buttercup)
(require 'org)

(unless (featurep 'go-jira)
  (provide 'go-jira)
  (defun go-jira--find-exe (&optional _exe) "jira")
  (defun go-jira-ticket->url (key) (format "https://example.com/%s" key)))

(unless (fboundp 'go-jira-board-view--board-url)
  (defun go-jira-board-view--board-url () "https://example.com/board"))

(load-file "go-jira-url.el")

(defconst go-jira-url-tests--board
  "#+COLUMNS: %50ITEM %12TODO
#+TITLE: Test Board

* To Do
** SAC-123: Test issue
:PROPERTIES:
:ISSUE_KEY: SAC-123
:END:
Description mentions SAC-999 and https://example.org/doc here.
*** A heading inside the description
Description body.
*** Subtasks
SAC-124:  A subtask
*** Comments
**** New comment
Top-level draft body.
**** John Doe - [[https://jira.test/browse/SAC-123?focusedCommentId=456][2026-10-19 Mon 10:00]]
:PROPERTIES:
:JIRA_COMMENT_ID: 456
:END:
Comment body.
***** A heading inside the comment body
Nested comment body.
***** Jane Roe - 2026-10-19 Mon 11:00
:PROPERTIES:
:JIRA_COMMENT_ID: 457
:JIRA_PARENT_ID: 456
:END:
[[file:~accountid:712020:39b527c2]] - Reply body.
***** New comment
:PROPERTIES:
:JIRA_PARENT_ID: 456
:END:
Reply draft body.
* Done
** SAC-789: Done issue
:PROPERTIES:
:ISSUE_KEY: SAC-789
:END:
"
  "A board with one expanded ticket, in the shape the board renderer writes.")

(defconst go-jira-url-tests--view
  "* SAC-123: Test issue
:PROPERTIES:
:ISSUE_KEY: SAC-123
:END:
** Description
The description.
** Comments
*** John Doe - 2026-10-19 Mon 10:00
:PROPERTIES:
:JIRA_COMMENT_ID: 456
:END:
Comment body.
"
  "A ticket view, in the shape `go-jira-view-ticket' writes.")

(defmacro go-jira-url-tests--with-buffer (content mode &rest body)
  "Run BODY in an Org buffer holding CONTENT, with `major-mode' set to MODE."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (org-mode)
     (setq major-mode ,mode)
     (goto-char (point-min))
     ,@body))

(defun go-jira-url-tests--url-at (regexp)
  "Return the URL at the start of the first match of REGEXP."
  (goto-char (point-min))
  (re-search-forward regexp)
  (goto-char (match-beginning 0))
  (go-jira--url-at-point))

(defun go-jira-url-tests--ticket (key)
  "Return the browse URL the specs expect for ticket KEY."
  (format "https://jira.test/browse/%s" key))

(defun go-jira-url-tests--comment (key id)
  "Return the URL the specs expect for comment ID on ticket KEY."
  (go-jira-comment--anchor-url (go-jira-url-tests--ticket key) id))

(defconst go-jira-url-tests--board-url "https://jira.test/board/3018"
  "The URL the specs expect for the board.")

(describe "go-jira--url-at-point"
  (before-each
    (spy-on 'go-jira-ticket->url :and-call-fake #'go-jira-url-tests--ticket)
    (spy-on 'go-jira-board-view--board-url
            :and-return-value go-jira-url-tests--board-url))

  (describe "in a board"
    (it "gives the board outside every ticket"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "^#\\+TITLE")
                :to-equal go-jira-url-tests--board-url)
        (expect (go-jira-url-tests--url-at "^\\* To Do")
                :to-equal go-jira-url-tests--board-url)
        (expect (go-jira-url-tests--url-at "^\\* Done")
                :to-equal go-jira-url-tests--board-url)))

    (it "gives the ticket from its heading, drawer and description"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (dolist (re '("^\\*\\* SAC-123" "^:ISSUE_KEY:" "^Description mentions"
                      "^\\*\\*\\* A heading inside the description"
                      "^Description body"))
          (expect (go-jira-url-tests--url-at re)
                  :to-equal (go-jira-url-tests--ticket "SAC-123")))))

    (it "gives the ticket from its Comments heading and a top-level draft"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "^\\*\\*\\* Comments")
                :to-equal (go-jira-url-tests--ticket "SAC-123"))
        (expect (go-jira-url-tests--url-at "^Top-level draft body")
                :to-equal (go-jira-url-tests--ticket "SAC-123"))))

    (it "gives each ticket its own URL"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "^\\*\\* SAC-789")
                :to-equal (go-jira-url-tests--ticket "SAC-789"))))

    (it "gives the ticket a key under point names"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "SAC-999")
                :to-equal (go-jira-url-tests--ticket "SAC-999"))
        (expect (go-jira-url-tests--url-at "^SAC-124")
                :to-equal (go-jira-url-tests--ticket "SAC-124"))))

    (it "gives the comment from its heading, body and a heading in the body"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (dolist (re '("^\\*\\*\\*\\* John Doe" "^Comment body"
                      "^\\*\\*\\*\\*\\* A heading inside the comment body"
                      "^Nested comment body"))
          (expect (go-jira-url-tests--url-at re)
                  :to-equal (go-jira-url-tests--comment "SAC-123" "456")))))

    (it "gives a reply its own URL"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "Reply body")
                :to-equal (go-jira-url-tests--comment "SAC-123" "457"))))

    (it "reads a user mention as plain text"
      ;; A Jira mention renders as a file link to a file that does not exist.
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "\\[\\[file:~accountid")
                :to-equal (go-jira-url-tests--comment "SAC-123" "457"))))

    (it "gives a reply draft the thread it answers"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "^Reply draft body")
                :to-equal (go-jira-url-tests--comment "SAC-123" "456"))))

    (it "gives a link its own target"
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (expect (go-jira-url-tests--url-at "https://example\\.org")
                :to-equal "https://example.org/doc")
        (expect (go-jira-url-tests--url-at "2026-10-19 Mon 10:00\\]")
                :to-equal "https://jira.test/browse/SAC-123?focusedCommentId=456")))

    (it "resolves the same in a buffer narrowed to the ticket"
      ;; An edit session narrows the board to the ticket it edits.
      (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
        (re-search-forward "^\\*\\* SAC-123")
        (org-narrow-to-subtree)
        (expect (go-jira-url-tests--url-at "^Nested comment body")
                :to-equal (go-jira-url-tests--comment "SAC-123" "456"))
        (expect (go-jira-url-tests--url-at "^Description body")
                :to-equal (go-jira-url-tests--ticket "SAC-123")))))

  (describe "in a ticket view"
    (it "gives the ticket from its heading, description and Comments heading"
      (go-jira-url-tests--with-buffer go-jira-url-tests--view 'go-jira-view-mode
        (dolist (re '("^\\* SAC-123" "^The description" "^\\*\\* Comments"))
          (expect (go-jira-url-tests--url-at re)
                  :to-equal (go-jira-url-tests--ticket "SAC-123")))
        (expect 'go-jira-board-view--board-url :not :to-have-been-called)))

    (it "gives the comment from its body"
      (go-jira-url-tests--with-buffer go-jira-url-tests--view 'go-jira-view-mode
        (expect (go-jira-url-tests--url-at "^Comment body")
                :to-equal (go-jira-url-tests--comment "SAC-123" "456")))))

  (it "refuses a buffer that is not a ticket view or a board"
    (with-temp-buffer
      (insert "SAC-123\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira--url-at-point) :to-throw 'user-error))))

(describe "go-jira-open-at-point"
  (before-each
    (spy-on 'go-jira-ticket->url :and-call-fake #'go-jira-url-tests--ticket)
    (spy-on 'browse-url)
    (spy-on 'org-open-at-point))

  (it "opens a comment in the browser"
    (go-jira-url-tests--with-buffer go-jira-url-tests--view 'go-jira-view-mode
      (re-search-forward "^Comment body")
      (go-jira-open-at-point)
      (expect 'browse-url :to-have-been-called-with
              (go-jira-url-tests--comment "SAC-123" "456"))
      (expect 'org-open-at-point :not :to-have-been-called)))

  (it "opens a link the way Org does"
    (go-jira-url-tests--with-buffer go-jira-url-tests--board 'go-jira-board-view-mode
      (re-search-forward "https://example\\.org")
      (go-jira-open-at-point)
      (expect 'org-open-at-point :to-have-been-called)
      (expect 'browse-url :not :to-have-been-called)))

  (it "opens a link to a file that exists the way Org does"
    (go-jira-url-tests--with-buffer
        (format "* SAC-123: T\n:PROPERTIES:\n:ISSUE_KEY: SAC-123\n:END:\n[[file:%s]]\n"
                (expand-file-name "go-jira-url.el"))
        'go-jira-view-mode
      (re-search-forward "^\\[\\[file:")
      (go-jira-open-at-point)
      (expect 'org-open-at-point :to-have-been-called)
      (expect 'browse-url :not :to-have-been-called))))

(describe "go-jira-copy-url-at-point"
  (before-each
    (spy-on 'go-jira-ticket->url :and-call-fake #'go-jira-url-tests--ticket))

  (it "puts the URL on the kill ring"
    (let ((kill-ring nil))
      (go-jira-url-tests--with-buffer go-jira-url-tests--view 'go-jira-view-mode
        (re-search-forward "^The description")
        (go-jira-copy-url-at-point)
        (expect (car kill-ring) :to-equal (go-jira-url-tests--ticket "SAC-123"))))))

(provide 'go-jira-url-tests)
;;; go-jira-url-tests.el ends here
