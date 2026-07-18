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

(describe "go-jira-popup--display"
  (before-each (spy-on 'go-jira-popup--show))

  (it "uses the posframe popup when available"
    (let ((go-jira--posframe-available-p t)
          (info '(:summary "s" :status "Done" :category "done")))
      (go-jira-popup--display "SAC-1" info)
      (expect 'go-jira-popup--show :to-have-been-called-with "SAC-1" info)))

  (it "falls back to the echo area only when asked"
    (let ((go-jira--posframe-available-p nil)
          (inhibit-message t))
      (expect (substring-no-properties
               (go-jira-popup--display
                "SAC-1" '(:summary "s" :status "Done" :category "done") t))
              :to-equal "SAC-1 [Done]: s")
      (expect 'go-jira-popup--show :not :to-have-been-called)))

  (it "does nothing without posframe and without an echo fallback"
    (let ((go-jira--posframe-available-p nil))
      (expect (go-jira-popup--display "SAC-1" "s") :to-be nil)
      (expect 'go-jira-popup--show :not :to-have-been-called))))

(describe "go-jira-popup--request"
  (before-each
    (clrhash go-jira-eldoc-cache)
    (spy-on 'go-jira-popup--display))

  (it "displays cached info synchronously"
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s" :status "Done" :category "done"))
    (go-jira-popup--request "SAC-1")
    (expect 'go-jira-popup--display
            :to-have-been-called-with
            "SAC-1" '(:summary "s" :status "Done" :category "done") nil))

  (it "skips display when the guard returns nil"
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s"))
    (go-jira-popup--request "SAC-1" (lambda () nil))
    (expect 'go-jira-popup--display :not :to-have-been-called))

  (it "displays when the guard returns non-nil"
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s"))
    (go-jira-popup--request "SAC-1" (lambda () t))
    (expect 'go-jira-popup--display :to-have-been-called)))

(describe "go-jira-popup-show"
  (before-each
    (clrhash go-jira-eldoc-cache)
    (setq go-jira--last-ticket nil)
    (spy-on 'go-jira-popup--display))

  (it "errors when point is not on a ticket"
    (spy-on 'go-jira-eldoc--ticket-at-point :and-return-value nil)
    (expect (go-jira-popup-show) :to-throw 'user-error))

  (it "displays and records the ticket at point"
    (spy-on 'go-jira-eldoc--ticket-at-point :and-return-value "SAC-1")
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s"))
    (go-jira-popup-show)
    (expect 'go-jira-popup--display :to-have-been-called-with "SAC-1" '(:summary "s") :echo)
    (expect go-jira--last-ticket :to-equal "SAC-1")))

(describe "go-jira-popup--update"
  (before-each
    (clrhash go-jira-eldoc-cache)
    (setq go-jira--last-ticket nil)
    (spy-on 'go-jira-popup--auto-active-p :and-return-value t)
    (spy-on 'go-jira-popup--request)
    (spy-on 'go-jira-popup--hide))

  (it "shows a cached ticket and records it"
    (spy-on 'go-jira-eldoc--ticket-at-point :and-return-value "SAC-1")
    (go-jira-eldoc--cache-put "SAC-1" '(:summary "s"))
    (go-jira-popup--update)
    (expect 'go-jira-popup--request :to-have-been-called-with "SAC-1")
    (expect go-jira--last-ticket :to-equal "SAC-1"))

  (it "hides when point is not on a ticket"
    (spy-on 'go-jira-eldoc--ticket-at-point :and-return-value nil)
    (go-jira-popup--update)
    (expect 'go-jira-popup--hide :to-have-been-called))

  (it "keeps the popup when already on the same ticket"
    (spy-on 'go-jira-eldoc--ticket-at-point :and-return-value "SAC-1")
    (setq go-jira--last-ticket "SAC-1")
    (go-jira-popup--update)
    (expect 'go-jira-popup--request :not :to-have-been-called)
    (expect 'go-jira-popup--hide :not :to-have-been-called)))

(describe "go-jira-popup--hide-if-foreign"
  (before-each (spy-on 'go-jira-popup--hide))

  (it "keeps the popup when the selected window shows the source buffer"
    (let ((src (get-buffer-create "go-jira-src-test"))
          (popup (get-buffer-create go-jira-popup-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer popup
              (setq-local go-jira--popup-source-buffer src))
            (spy-on 'window-buffer :and-return-value src)
            (go-jira-popup--hide-if-foreign)
            (expect 'go-jira-popup--hide :not :to-have-been-called))
        (kill-buffer src))))

  (it "hides the popup when a different buffer is selected"
    (let ((src (get-buffer-create "go-jira-src-test"))
          (other (get-buffer-create "go-jira-other-test"))
          (popup (get-buffer-create go-jira-popup-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer popup
              (setq-local go-jira--popup-source-buffer src))
            (spy-on 'window-buffer :and-return-value other)
            (go-jira-popup--hide-if-foreign)
            (expect 'go-jira-popup--hide :to-have-been-called))
        (kill-buffer src)
        (kill-buffer other))))

  (it "does nothing when there is no popup buffer"
    (when (get-buffer go-jira-popup-buffer)
      (kill-buffer go-jira-popup-buffer))
    (go-jira-popup--hide-if-foreign)
    (expect 'go-jira-popup--hide :not :to-have-been-called)))

(provide 'go-jira-eldoc-tests)
;;; go-jira-eldoc-tests.el ends here
