;;; go-jira-edit-tests.el --- Tests for go-jira-edit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: February 12, 2026
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for JIRA edit functionality (title, description, comments)
;;
;;; Code:

(require 'buttercup)
(require 'org)

;; Mock dependencies
(unless (featurep 'go-jira)
  (provide 'go-jira)
  (defun go-jira--find-exe () "jira")
  (defun go-jira-view-mode-refresh () nil))

(unless (featurep 'go-jira-markup)
  (provide 'go-jira-markup)
  (defun go-jira-markup-from-org (text) text))  ; Simple passthrough for tests

(load-file "go-jira-edit.el")

;;; Test fixtures

(defconst go-jira-edit-test--view-mode-buffer
  "* SAC-123: Test Issue Title

** Description
This is the description.
It has multiple lines.

** Comments
*** John Doe - [[https://example.com?focusedCommentId=456][2024-01-01]]
First comment body.

*** Jane Smith - [[https://example.com?focusedCommentId=789][2024-01-02]]
Second comment body.
"
  "Mock view mode buffer content.")

(defconst go-jira-edit-test--board-mode-buffer
  "#+TITLE: Test Board
#+COLUMNS: %50ITEM %12TODO %15ASSIGNEE %12PRIORITY %10ISSUETYPE %25LABELS

* To Do

** SAC-123: Test Issue Title
:PROPERTIES:
:ASSIGNEE: John Doe
:PRIORITY: High
:END:

*** Description
This is the description.

*** Comments
**** John Doe - [[https://example.com?focusedCommentId=456][2024-01-01]]
First comment.

** SAC-456: Another Issue
:PROPERTIES:
:ASSIGNEE: Jane Smith
:END:

*** Description
Another description.

* Done

** SAC-789: Done Issue
*** Description
Done issue description.
"
  "Mock board mode buffer content.")

;;; Helper functions

(defun go-jira-edit-test--setup-view-buffer ()
  "Create a buffer with view mode content and enable go-jira-view-mode."
  (let ((buf (generate-new-buffer "*test-jira-view*")))
    (with-current-buffer buf
      (insert go-jira-edit-test--view-mode-buffer)
      (org-mode)
      (goto-char (point-min))
      ;; Mock go-jira-view-mode
      (setq major-mode 'go-jira-view-mode)
      (setq go-jira--ticket-number "SAC-123")
      buf)))

(defun go-jira-edit-test--setup-board-buffer ()
  "Create a buffer with board mode content and enable go-jira-board-view-mode."
  (let ((buf (generate-new-buffer "*test-jira-board*")))
    (with-current-buffer buf
      (insert go-jira-edit-test--board-mode-buffer)
      (org-mode)
      (goto-char (point-min))
      ;; Mock go-jira-board-view-mode
      (setq major-mode 'go-jira-board-view-mode)
      buf)))

;;; Tests: Context Detection

(describe "go-jira-edit--get-ticket-context"
  (describe "in view mode"
    (it "returns context with ticket key and full buffer bounds"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (expect (plist-get ctx :key) :to-equal "SAC-123")
          (expect (plist-get ctx :start) :to-equal (point-min))
          (expect (plist-get ctx :end) :to-equal (point-max))
          (expect (plist-get ctx :level) :to-equal 1))
        (kill-buffer))))
  
  (describe "in board mode"
    (it "returns context for ticket at point (not narrowed)"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        ;; Position at first ticket (SAC-123)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (expect (plist-get ctx :key) :to-equal "SAC-123")
          (expect (plist-get ctx :level) :to-equal 2)
          (expect (plist-get ctx :start) :to-be-truthy)
          (expect (plist-get ctx :end) :to-be-truthy))
        (kill-buffer)))
    
    (it "returns context using narrowed bounds when narrowed"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        ;; Position at first ticket and narrow
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (org-narrow-to-subtree)
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (expect (plist-get ctx :key) :to-equal "SAC-123")
          (expect (plist-get ctx :start) :to-equal (point-min))
          (expect (plist-get ctx :end) :to-equal (point-max))
          (expect (buffer-narrowed-p) :to-be-truthy))
        (widen)
        (kill-buffer)))))

;;; Tests: Title Extraction

(describe "go-jira-edit--extract-title"
  (describe "in view mode"
    (it "extracts title without KEY prefix"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((title (go-jira-edit--extract-title)))
          (expect title :to-equal "Test Issue Title"))
        (kill-buffer))))
  
  (describe "in board mode"
    (it "extracts title from unnrarrowed buffer"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (let* ((ctx (go-jira-edit--get-ticket-context))
               (title (go-jira-edit--extract-title ctx)))
          (expect title :to-equal "Test Issue Title"))
        (kill-buffer)))
    
    (it "extracts title from narrowed buffer"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (org-narrow-to-subtree)
        (let ((title (go-jira-edit--extract-title)))
          (expect title :to-equal "Test Issue Title"))
        (widen)
        (kill-buffer)))))

;;; Tests: Description Extraction

(describe "go-jira-edit--extract-description"
  (describe "in view mode"
    (it "extracts description content"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((desc (go-jira-edit--extract-description)))
          (expect desc :to-match "This is the description")
          (expect desc :to-match "multiple lines"))
        (kill-buffer))))
  
  (describe "in board mode"
    (it "extracts description from unnarrowed buffer"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (let* ((ctx (go-jira-edit--get-ticket-context))
               (desc (go-jira-edit--extract-description ctx)))
          (expect desc :to-match "This is the description"))
        (kill-buffer)))
    
    (it "extracts description from narrowed buffer"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (org-narrow-to-subtree)
        (let ((desc (go-jira-edit--extract-description)))
          (expect desc :to-match "This is the description"))
        (widen)
        (kill-buffer)))
    
    (it "returns empty string when no description section"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-456")
        (org-narrow-to-subtree)
        ;; Delete description section
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\*\\* Description" nil t)
          (let ((inhibit-read-only t))
            (org-cut-subtree)))
        (let ((desc (go-jira-edit--extract-description)))
          (expect desc :to-equal ""))
        (widen)
        (kill-buffer)))))

;;; Tests: Comment Extraction

(describe "go-jira-edit--extract-comments"
  (describe "in view mode"
    (it "extracts comments with IDs"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((comments (go-jira-edit--extract-comments)))
          (expect (length comments) :to-equal 2)
          (expect (plist-get (car comments) :id) :to-equal "456")
          (expect (plist-get (car comments) :body) :to-match "First comment")
          (expect (plist-get (cadr comments) :id) :to-equal "789"))
        (kill-buffer))))
  
  (describe "in board mode"
    (it "extracts comments from narrowed buffer"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (org-narrow-to-subtree)
        (let ((comments (go-jira-edit--extract-comments)))
          (expect (length comments) :to-equal 1)
          (expect (plist-get (car comments) :id) :to-equal "456")
          (expect (plist-get (car comments) :body) :to-match "First comment"))
        (widen)
        (kill-buffer)))))

;;; Tests: Overlay Creation

(describe "go-jira-edit--create-overlay"
  (describe "in view mode"
    (it "creates edit session with header line"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((original-content (buffer-string))
              (ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)

          ;; Check overlay exists
          (expect go-jira-edit--active-overlay :to-be-truthy)
          (expect (overlay-get go-jira-edit--active-overlay 'go-jira-ticket)
                  :to-equal "SAC-123")

          ;; Header line should be set with instructions
          (expect header-line-format :to-be-truthy)
          ;; Verify the header-line-format list contains our propertized strings
          (let ((header-text (mapconcat
                              (lambda (s) (substring-no-properties s))
                              header-line-format "")))
            (expect header-text :to-match "Edit Issue")
            (expect header-text :to-match "C-c C-c"))

          ;; Buffer content should NOT be modified (no inserted text)
          (expect (buffer-string) :to-equal original-content)

          ;; Clean up
          (go-jira-edit--remove-overlay))
        (kill-buffer))))

  (describe "in board mode"
    (it "creates edit session and narrows to ticket"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)

          ;; Check narrowed
          (expect (buffer-narrowed-p) :to-be-truthy)

          ;; Check overlay exists
          (expect go-jira-edit--active-overlay :to-be-truthy)

          ;; Header line should be set
          (let ((header-text (mapconcat
                              (lambda (s) (substring-no-properties s))
                              header-line-format "")))
            (expect header-text :to-match "Edit Issue"))

          ;; First line should be the ticket heading, not an instruction line
          (goto-char (point-min))
          (expect (buffer-substring (point) (line-end-position))
                  :to-match "SAC-123")

          ;; Clean up
          (go-jira-edit--remove-overlay)
          (widen))
        (kill-buffer)))))

;;; Tests: Extraction with active edit session

(describe "go-jira-edit--extract-title with active edit session"
  (describe "in view mode"
    (it "extracts title correctly while edit session is active"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)
          (let ((title (go-jira-edit--extract-title)))
            (expect title :to-equal "Test Issue Title"))
          (go-jira-edit--remove-overlay))
        (kill-buffer))))

  (describe "in board mode"
    (it "extracts title correctly while edit session is active"
      (with-current-buffer (go-jira-edit-test--setup-board-buffer)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* SAC-123")
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)
          (let ((title (go-jira-edit--extract-title)))
            (expect title :to-equal "Test Issue Title"))
          (go-jira-edit--remove-overlay)
          (widen))
        (kill-buffer)))))

(describe "go-jira-edit--extract-description with active edit session"
  (describe "in view mode"
    (it "extracts description correctly while edit session is active"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)
          (let ((desc (go-jira-edit--extract-description)))
            (expect desc :to-match "This is the description")
            (expect desc :to-match "multiple lines"))
          (go-jira-edit--remove-overlay))
        (kill-buffer)))))

(describe "go-jira-edit--extract-comments with active edit session"
  (describe "in view mode"
    (it "extracts comments correctly while edit session is active"
      (with-current-buffer (go-jira-edit-test--setup-view-buffer)
        (let ((ctx (go-jira-edit--get-ticket-context)))
          (go-jira-edit--create-overlay ctx)
          (let ((comments (go-jira-edit--extract-comments)))
            (expect (length comments) :to-equal 2)
            (expect (plist-get (car comments) :id) :to-equal "456")
            (expect (plist-get (cadr comments) :id) :to-equal "789"))
          (go-jira-edit--remove-overlay))
        (kill-buffer)))))

;;; Tests: Title change detection (the actual edit flow)

(describe "go-jira-edit title change detection"
  (it "detects title change when user edits the heading"
    (with-current-buffer (go-jira-edit-test--setup-view-buffer)
      (let ((ctx (go-jira-edit--get-ticket-context)))
        (go-jira-edit--create-overlay ctx)
        (let ((orig-title (overlay-get go-jira-edit--active-overlay 'original-title)))
          ;; Verify original was stored correctly
          (expect orig-title :to-equal "Test Issue Title")
          ;; Simulate user editing the title
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "Test Issue Title")
            (replace-match "New Title" t t))
          ;; Now extract again and compare
          (let* ((new-title (go-jira-edit--extract-title))
                 (changed (not (string-equal (or orig-title "") (or new-title "")))))
            (expect new-title :to-equal "New Title")
            (expect changed :to-be-truthy)))
        (go-jira-edit--remove-overlay))
      (kill-buffer)))

  (it "detects no change when title is not modified"
    (with-current-buffer (go-jira-edit-test--setup-view-buffer)
      (let ((ctx (go-jira-edit--get-ticket-context)))
        (go-jira-edit--create-overlay ctx)
        (let* ((orig-title (overlay-get go-jira-edit--active-overlay 'original-title))
               (current-title (go-jira-edit--extract-title))
               (changed (not (string-equal (or orig-title "") (or current-title "")))))
          (expect orig-title :to-equal "Test Issue Title")
          (expect current-title :to-equal "Test Issue Title")
          (expect changed :to-be nil))
        (go-jira-edit--remove-overlay))
      (kill-buffer))))

;;; Tests: Heading level shifting

(describe "go-jira-markup-shift-headings"
  (it "shifts heading levels by positive offset"
    (expect (go-jira-markup-shift-headings "* Heading\nText" 2)
            :to-equal "*** Heading\nText"))

  (it "shifts heading levels by negative offset"
    (expect (go-jira-markup-shift-headings "**** Wrap that\nSome text" -3)
            :to-equal "* Wrap that\nSome text"))

  (it "handles multiple heading levels"
    (expect (go-jira-markup-shift-headings "**** Section\nText\n***** Sub-section\nMore text" -3)
            :to-equal "* Section\nText\n** Sub-section\nMore text"))

  (it "does not modify text without headings"
    (expect (go-jira-markup-shift-headings "Just plain text\nwith multiple lines" -3)
            :to-equal "Just plain text\nwith multiple lines"))

  (it "clamps to level 1 minimum"
    (expect (go-jira-markup-shift-headings "** Shallow heading\nText" -3)
            :to-equal "* Shallow heading\nText"))

  (it "returns text unchanged when offset is 0"
    (expect (go-jira-markup-shift-headings "** Heading\nText" 0)
            :to-equal "** Heading\nText"))

  (it "does not modify bold/asterisk text that is not a heading"
    (expect (go-jira-markup-shift-headings "This has *bold* and **double bold** inline" -3)
            :to-equal "This has *bold* and **double bold** inline")))

;;; Tests: Comment extraction with headings inside body

(describe "go-jira-edit--extract-comments with headings in body"
  (it "normalizes heading levels in extracted comment body"
    (with-current-buffer (go-jira-edit-test--setup-view-buffer)
      ;; Add a heading inside the first comment
      (goto-char (point-min))
      (re-search-forward "First comment body\\.")
      (replace-match "**** Wrap that\nFirst comment body." t t)
      (let ((comments (go-jira-edit--extract-comments)))
        ;; comment-level is 3 (view mode: ticket=1, comments section=2, comment=3)
        ;; So **** (level 4) should become * (level 1)
        (expect (plist-get (car comments) :body) :to-match "^\\* Wrap that"))
      (kill-buffer))))

;;; Tests: Change Detection

(describe "go-jira-edit--has-changes-p"
  (it "returns nil for unmodified buffer"
    (with-current-buffer (go-jira-edit-test--setup-view-buffer)
      (set-buffer-modified-p nil)
      (expect (go-jira-edit--has-changes-p) :to-be nil)
      (kill-buffer)))
  
  (it "returns t for modified buffer"
    (with-current-buffer (go-jira-edit-test--setup-view-buffer)
      (set-buffer-modified-p t)
      (expect (go-jira-edit--has-changes-p) :to-be-truthy)
      (kill-buffer))))

(provide 'go-jira-edit-tests)
;;; go-jira-edit-tests.el ends here
