;;; go-jira-embark-tests.el --- Tests for go-jira-embark -*- lexical-binding: t; -*-
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
;;  Tests for the Embark target finder and the ticket action keymap.
;;
;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'embark)
(require 'org)

(load-file "go-jira-embark.el")

(describe "go-jira-embark-target-ticket-at-point"
  (it "targets a ticket key under point"
    (with-temp-buffer
      (insert "see SAC-30264 for details")
      (org-mode)
      (goto-char (point-min))
      (search-forward "SAC-3")
      (expect (car (go-jira-embark-target-ticket-at-point)) :to-equal 'jira-ticket)
      (expect (cadr (go-jira-embark-target-ticket-at-point)) :to-equal "SAC-30264")))

  (it "returns nil away from a ticket key"
    (with-temp-buffer
      (insert "no ticket here")
      (org-mode)
      (goto-char (point-min))
      (expect (go-jira-embark-target-ticket-at-point) :to-be nil))))

(describe "go-jira-embark-copy-ticket-url"
  (it "kills the resolved URL and echoes nothing but the URL"
    (let ((kill-ring nil)
          echoed)
      (cl-letf (((symbol-function 'go-jira-ticket->url)
                 (lambda (ticket)
                   (format "https://example.atlassian.net/browse/%s" ticket)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        (go-jira-embark-copy-ticket-url "SAC-30264"))
      (expect (car kill-ring)
              :to-equal "https://example.atlassian.net/browse/SAC-30264")
      (expect echoed
              :to-equal "https://example.atlassian.net/browse/SAC-30264"))))

(describe "go-jira-embark-jira-ticket-map"
  (it "binds j c to the comment action"
    (expect (keymap-lookup go-jira-embark-jira-ticket-map "j c")
            :to-be #'go-jira-embark-comment-on-ticket))

  (it "binds j l to the copy-URL action"
    (expect (keymap-lookup go-jira-embark-jira-ticket-map "j l")
            :to-be #'go-jira-embark-copy-ticket-url))

  (it "keeps other bindings under an existing j prefix"
    ;; The package binds into `j' with `keymap-set', which merges; a user
    ;; config that owns the rest of the prefix must not lose its bindings.
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map embark-general-map)
      (keymap-set map "j s" #'ignore)
      (keymap-set map "j c" #'go-jira-embark-comment-on-ticket)
      (expect (keymap-lookup map "j s") :to-be #'ignore)
      (expect (keymap-lookup map "j c") :to-be #'go-jira-embark-comment-on-ticket))))

(provide 'go-jira-embark-tests)
;;; go-jira-embark-tests.el ends here
