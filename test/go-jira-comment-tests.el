;;; go-jira-comment-tests.el --- Tests for go-jira-comment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for comment identity: the property drawer, the heading link, and
;;  the body boundary that keeps drawer text out of Jira.
;;
;;; Code:

(require 'buttercup)
(require 'org)

(load-file "go-jira-comment.el")

(defconst go-jira-comment-test--base-url
  "https://example.atlassian.net/browse/SAC-123")

(defun go-jira-comment-test--render (&rest args)
  "Render a comment heading with ARGS and return the buffer text."
  (with-temp-buffer
    (org-mode)
    (apply #'go-jira-comment-insert-heading args)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun go-jira-comment-test--links-on-first-line ()
  "Return (LINK-COUNT . TEXT-AFTER-LAST-LINK) for the first line of the buffer."
  (goto-char (point-min))
  (let ((count 0)
        (tail ""))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (setq count (1+ count))
          (setq tail (buffer-substring-no-properties
                      (org-element-property :end link)
                      (point-max))))))
    (cons count tail)))

;;; Tests: heading rendering

(describe "go-jira-comment-insert-heading"
  (it "records the comment ID in a property drawer"
    (expect (go-jira-comment-test--render
             3 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211639" nil
             go-jira-comment-test--base-url)
            :to-match ":JIRA_COMMENT_ID: 1211639"))

  (it "records the parent ID for a reply"
    (expect (go-jira-comment-test--render
             4 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211640" "1211639"
             go-jira-comment-test--base-url)
            :to-match ":JIRA_PARENT_ID: 1211639"))

  (it "omits the parent property for a top-level comment"
    (expect (go-jira-comment-test--render
             3 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211639" nil
             go-jira-comment-test--base-url)
            :not :to-match ":JIRA_PARENT_ID:"))

  (it "omits the parent property rather than writing it empty"
    ;; A failed parent lookup used to write `:JIRA_PARENT_ID: ', which now
    ;; reads as a reply to nothing.
    (expect (go-jira-comment-test--render
             4 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211640" ""
             go-jira-comment-test--base-url)
            :not :to-match ":JIRA_PARENT_ID:"))

  (it "keeps the comment ID when no base URL is available"
    ;; Board mode has no fallback for a missing base URL, so the heading
    ;; renders without a link and the drawer is the only identity record.
    (let ((text (go-jira-comment-test--render
                 3 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211639" nil nil)))
      (expect text :to-match ":JIRA_COMMENT_ID: 1211639")
      (expect text :not :to-match "\\[\\[")))

  (it "keeps the comment ID when Jira reports no creation time"
    (let ((text (go-jira-comment-test--render
                 3 "Ag Ibragimov" nil "1211639" nil
                 go-jira-comment-test--base-url)))
      (expect text :to-match ":JIRA_COMMENT_ID: 1211639")))

  (it "falls back to Unknown for a missing author"
    (expect (go-jira-comment-test--render
             3 nil "2026-09-02T14:14:00.000+0200" "1211639" nil
             go-jira-comment-test--base-url)
            :to-match "^\\*\\*\\* Unknown - ")))

;;; Tests: the heading link parses

(describe "the rendered comment heading"
  (it "parses as exactly one link with nothing trailing"
    ;; A bracketed timestamp inside the link description truncates it at the
    ;; first "]]" and leaves a stray bracket behind.
    (with-temp-buffer
      (org-mode)
      (go-jira-comment-insert-heading
       3 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211639" nil
       go-jira-comment-test--base-url)
      (let ((result (go-jira-comment-test--links-on-first-line)))
        (expect (car result) :to-equal 1)
        (expect (cdr result) :to-equal ""))))

  (it "links to the comment anchor"
    (expect (go-jira-comment-test--render
             3 "Ag Ibragimov" "2026-09-02T14:14:00.000+0200" "1211639" nil
             go-jira-comment-test--base-url)
            :to-match "focusedCommentId=1211639")))

;;; Tests: reading identity back

(describe "go-jira-comment-id-at"
  (it "reads the ID from the drawer"
    (with-temp-buffer
      (insert "* C\n:PROPERTIES:\n:JIRA_COMMENT_ID: 456\n:END:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-id-at) :to-equal "456")))

  (it "prefers the drawer over a stale anchor in the heading"
    (with-temp-buffer
      (insert "* C - [[https://x?focusedCommentId=999][ts]]\n"
              ":PROPERTIES:\n:JIRA_COMMENT_ID: 456\n:END:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-id-at) :to-equal "456")))

  (it "falls back to the heading anchor when there is no drawer"
    ;; Buffers rendered before comments carried a drawer must still edit.
    (with-temp-buffer
      (insert "* C - [[https://x?focusedCommentId=789][ts]]\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-id-at) :to-equal "789")))

  (it "returns nil for a comment with neither"
    (with-temp-buffer
      (insert "* New comment\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-id-at) :to-be nil))))

(describe "go-jira-comment-parent-id-at"
  (it "reads the parent ID from the drawer"
    (with-temp-buffer
      (insert "* R\n:PROPERTIES:\n:JIRA_PARENT_ID: 456\n:END:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-parent-id-at) :to-equal "456")))

  (it "returns nil for a heading without the property"
    (with-temp-buffer
      (insert "* A heading inside a comment body\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-parent-id-at) :to-be nil))))

;;; Tests: the body boundary

(describe "go-jira-comment-body-start"
  (it "starts the body after the property drawer"
    (with-temp-buffer
      (insert "* C\n:PROPERTIES:\n:JIRA_COMMENT_ID: 456\n:END:\nbody text\n")
      (org-mode)
      (goto-char (point-min))
      (let ((start (go-jira-comment-body-start (org-element-at-point))))
        (expect (buffer-substring-no-properties start (point-max))
                :to-equal "body text\n"))))

  (it "starts the body at the first line when there is no drawer"
    (with-temp-buffer
      (insert "* C\nbody text\n")
      (org-mode)
      (goto-char (point-min))
      (let ((start (go-jira-comment-body-start (org-element-at-point))))
        (expect (buffer-substring-no-properties start (point-max))
                :to-equal "body text\n"))))

  (it "returns nil for a heading with no contents"
    (with-temp-buffer
      (insert "* C\n")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-comment-body-start (org-element-at-point)) :to-be nil))))

;;; Tests: writing to Jira

(describe "go-jira-comment write requests"
  (let (captured output exit-code orig-call orig-exe)
    (before-each
      (setq captured nil output "{}" exit-code 0
            orig-call (symbol-function 'call-process)
            orig-exe (and (fboundp 'go-jira--find-exe)
                          (symbol-function 'go-jira--find-exe)))
      (fset 'go-jira--find-exe (lambda (&optional _e) "jira"))
      (fset 'call-process
            (lambda (_program &optional _infile _destination _display &rest args)
              (setq captured args)
              (unless (string-empty-p output) (insert output))
              exit-code)))
    (after-each
      (fset 'call-process orig-call)
      (if orig-exe
          (fset 'go-jira--find-exe orig-exe)
        (fmakunbound 'go-jira--find-exe)))

    (it "POSTs a new comment to the comment endpoint"
      (setq output "{\"id\":\"1211651\"}")
      (expect (go-jira-comment-post "SAC-123" "hello") :to-equal "1211651")
      (expect captured :to-equal
              '("request" "rest/api/2/issue/SAC-123/comment"
                "{\"body\":\"hello\"}" "--method" "POST")))

    (it "POSTs a reply with a numeric parentId"
      ;; Jira rejects a string here, so the ID read back from the drawer has
      ;; to be converted.
      (setq output "{\"id\":\"1211652\"}")
      (go-jira-comment-post "SAC-123" "hello" "1211639")
      (expect (nth 2 captured) :to-equal
              "{\"body\":\"hello\",\"parentId\":1211639}"))

    (it "omits parentId for a top-level comment"
      (setq output "{\"id\":\"1\"}")
      (go-jira-comment-post "SAC-123" "hello" "")
      (expect (nth 2 captured) :not :to-match "parentId"))

    (it "PUTs an edit to the comment's own endpoint"
      (setq output "{\"id\":\"456\"}")
      (expect (go-jira-comment-put "SAC-123" "456" "edited") :to-be t)
      (expect captured :to-equal
              '("request" "rest/api/2/issue/SAC-123/comment/456"
                "{\"body\":\"edited\"}" "--method" "PUT")))

    (it "accepts a comment whose text mentions an error"
      ;; The old check matched "error" anywhere in the output, and the
      ;; response echoes the comment body back.
      (setq output "{\"id\":\"1\",\"body\":\"the job failed with an error\"}")
      (expect (go-jira-comment-post "SAC-123" "the job failed with an error")
              :to-equal "1"))

    (it "signals when Jira reports errorMessages"
      (setq output "{\"errorMessages\":[\"Comment body can not be empty!\"],\"errors\":{}}")
      (expect (go-jira-comment-post "SAC-123" "") :to-throw 'error))

    (it "signals on a non-zero exit code"
      (setq exit-code 1 output "")
      (expect (go-jira-comment-post "SAC-123" "hello") :to-throw 'error))))

(describe "go-jira-comment-record-id"
  (it "writes the ID Jira assigned onto the comment heading"
    (with-temp-buffer
      (insert "* New comment\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (go-jira-comment-record-id (point-marker) "1211651")
      (goto-char (point-min))
      (expect (go-jira-comment-id-at) :to-equal "1211651")))

  (it "does nothing without an ID"
    (with-temp-buffer
      (insert "* New comment\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (go-jira-comment-record-id (point-marker) nil)
      (expect (buffer-string) :not :to-match ":PROPERTIES:"))))

(provide 'go-jira-comment-tests)
;;; go-jira-comment-tests.el ends here
