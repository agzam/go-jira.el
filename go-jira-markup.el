;;; go-jira-markup.el --- Jira markup to Org-mode converter -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira, markup
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert Jira wiki markup to Org-mode format using Pandoc.
;;
;; This module uses Pandoc (https://pandoc.org/) for converting between
;; Jira wiki markup and Org-mode format.  Pandoc's jira reader/writer is
;; built on the `jira-wiki-markup' Haskell library which provides a proper
;; AST-based parser, avoiding the fragility of regexp-based conversion.
;;
;; Pandoc handles most conversions correctly, but some Jira markup elements
;; are lossy or broken through Pandoc's pipeline (superscript, subscript,
;; citation, color markup, language-less code blocks).  These are handled
;; via pre/post-processing around the Pandoc call.
;;
;; Jira content headings (h1. through h6.) are converted to plain text
;; with `jira-heading' text properties rather than Org headings, because
;; the converted content lives inside an Org subtree (e.g., under
;; "** Description").

;;; Code:

(require 'cl-lib)

;;; Pandoc integration

(defvar go-jira-markup--pandoc-exe nil
  "Cached path to the pandoc executable.")

(defun go-jira-markup--find-pandoc ()
  "Find and return the pandoc executable path, or signal an error."
  (or go-jira-markup--pandoc-exe
      (setq go-jira-markup--pandoc-exe
            (or (executable-find "pandoc")
                (error "Could not find `pandoc' executable.  Install from https://pandoc.org/")))))

(defun go-jira-markup--pandoc-convert (text from to &optional extra-args)
  "Convert TEXT from format FROM to format TO using pandoc.
EXTRA-ARGS is an optional list of additional command-line arguments."
  (with-temp-buffer
    (insert text)
    (let ((args (append (list (point-min) (point-max)
                              (go-jira-markup--find-pandoc)
                              t            ; delete input
                              t            ; output to current buffer
                              nil          ; display
                              "-f" from "-t" to
                              "--wrap=none")
                        extra-args)))
      (apply #'call-process-region args))
    (buffer-string)))

;;; Placeholder-based protection for Pandoc-lossy elements
;;
;; Some Jira markup elements are mangled or lost by Pandoc's jira→org→jira
;; pipeline.  We protect them by replacing with unique placeholders before
;; Pandoc processes the text, then restoring them afterward.

(defvar go-jira-markup--placeholders nil
  "Alist of (PLACEHOLDER . ORIGINAL) for the current conversion.")

(defun go-jira-markup--placeholder (tag original)
  "Generate a unique placeholder for ORIGINAL, tagged with TAG.
Stores the mapping in `go-jira-markup--placeholders'."
  (let ((ph (format "GOJIRA0%s0%d0ARIOJOG"
                     tag (length go-jira-markup--placeholders))))
    (push (cons ph original) go-jira-markup--placeholders)
    ph))

(defun go-jira-markup--restore-placeholders (text)
  "Restore all placeholders in TEXT with their original content."
  (dolist (pair go-jira-markup--placeholders)
    (setq text (replace-regexp-in-string
                (regexp-quote (car pair)) (cdr pair) text t t)))
  text)

;;; Jira → Org: pre-processing (protect Pandoc-lossy elements)

(defun go-jira-markup--jira-pre-process (text)
  "Pre-process Jira TEXT before Pandoc conversion.
Protects elements that Pandoc handles incorrectly:
- Superscript: ^text^ (Pandoc breaks the round-trip)
- Subscript: ~text~ (Pandoc breaks the round-trip)
- Citation: ??text?? (Pandoc converts to em-dash, lossy)
- Color: {color:X}text{color} (Pandoc strips entirely)
- Code blocks without language: {code}...{code} (Pandoc guesses java)"
  (setq go-jira-markup--placeholders nil)

  ;; Protect superscript: ^text^ → placeholder
  ;; Must not match ^{text} in code blocks (already handled by pandoc)
  (setq text (replace-regexp-in-string
              "\\^\\([^^]+?\\)\\^"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder
                   "SUP" (format "^{%s}" content))))
              text))

  ;; Protect subscript: ~text~ → placeholder
  (setq text (replace-regexp-in-string
              "~\\([^~]+?\\)~"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder
                   "SUB" (format "_{%s}" content))))
              text))

  ;; Protect citation: ??text?? → placeholder (will become /text/ in org)
  (setq text (replace-regexp-in-string
              "\\?\\?\\([^?]+?\\)\\?\\?"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder
                   "CITE" (format "/«%s»/" content))))
              text))

  ;; Protect color: {color:X}text{color} → placeholder
  (setq text (replace-regexp-in-string
              "{color:\\([^}]+\\)}\\(\\(?:.\\|\n\\)*?\\){color}"
              (lambda (match)
                (let ((color (match-string 1 match))
                      (content (match-string 2 match)))
                  (go-jira-markup--placeholder
                   "COLOR" (format "{color:%s}%s{color}" color content))))
              text))

  ;; Protect opening {code} without language (Pandoc guesses java)
  ;; Only match opening {code} blocks, not closing ones.
  ;; Opening {code} is followed by content; closing {code} is preceded by content.
  ;; Strategy: match {code}\n...{code} blocks and replace only the opener.
  (setq text (replace-regexp-in-string
              "{code}\\([\n]\\(?:.\\|\n\\)*?\\){code}"
              "{code:NOLANG}\\1{code}"
              text))

  text)

(defun go-jira-markup--jira-post-process-org (text)
  "Post-process Org TEXT after Pandoc jira→org conversion.
Restores placeholders to their Org equivalents."
  ;; Restore placeholders - they survived pandoc as literal text
  (setq text (go-jira-markup--restore-placeholders text))

  ;; NOTE: We keep #+begin_src NOLANG as-is in the org output.
  ;; NOLANG is a marker indicating the original Jira {code} block had no
  ;; language.  The org→jira path converts {code:NOLANG} back to {code}.

  text)

;;; Org → Jira: pre/post-processing

(defun go-jira-markup--org-pre-process (text)
  "Pre-process Org TEXT before Pandoc org→jira conversion.
Protects elements that Pandoc's Org→Jira writer handles incorrectly:
- Citation markers «text» → placeholder (to restore as ??text??)
- Color markers from jira-pre-process → placeholder"
  (setq go-jira-markup--placeholders nil)

  ;; Protect citation markers: /«text»/ → placeholder
  ;; These were inserted during jira→org as the org representation of ??text??
  (setq text (replace-regexp-in-string
              "/«\\([^»]+?\\)»/"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder "CITE" (format "??%s??" content))))
              text))

  ;; Protect {color} blocks that survived as literal text
  (setq text (replace-regexp-in-string
              "{color:\\([^}]+\\)}\\(\\(?:.\\|\n\\)*?\\){color}"
              (lambda (match)
                (go-jira-markup--placeholder "COLOR" match))
              text))

  ;; Protect superscript: ^{text} → placeholder that pandoc won't mangle
  ;; Must handle both at-start-of-word and after-character positions
  (setq text (replace-regexp-in-string
              "\\^{\\([^}]+\\)}"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder "SUP" (format "^%s^" content))))
              text))

  ;; Protect subscript: _{text} → placeholder
  ;; Be careful not to match org underline _text_ — only _{text} with braces
  (setq text (replace-regexp-in-string
              "_{\\([^}]+\\)}"
              (lambda (match)
                (let ((content (match-string 1 match)))
                  (go-jira-markup--placeholder "SUB" (format "~%s~" content))))
              text))

  text)

(defun go-jira-markup--jira-post-process (text)
  "Post-process Jira TEXT after Pandoc org→jira conversion.
- Restores placeholders
- Strips {anchor:} markers
- Fixes unnecessary paren escaping
- Fixes bare #+begin_src → {noformat} back to {code}"
  ;; Restore all placeholders
  (setq text (go-jira-markup--restore-placeholders text))

  ;; Strip {anchor:...} markers pandoc adds to headings
  (setq text (replace-regexp-in-string "{anchor:[^}]*}" "" text))

  ;; Fix unnecessary paren escaping: \( → (
  (setq text (replace-regexp-in-string "\\\\(" "(" text))

  ;; Fix NOLANG marker: {code:NOLANG} → {code}
  (setq text (replace-regexp-in-string "{code:NOLANG}" "{code}" text t t))

  ;; Fix trailing blank line before {code} and {noformat}
  ;; Pandoc adds an extra newline: "code\n\n{code}" → "code\n{code}"
  (setq text (replace-regexp-in-string "\n\n{code}" "\n{code}" text t t))
  (setq text (replace-regexp-in-string "\n\n{noformat}" "\n{noformat}" text t t))

  text)

;;; Heading conversion (text properties ↔ org headings)

(defun go-jira-markup--org-headings-to-text-properties (text)
  "Convert Org headings in TEXT to plain text with `jira-heading' properties.
Pandoc outputs real Org headings (* heading, ** heading, etc.) but
the converted content lives inside an Org subtree, so we convert
them to propertized plain text that `go-jira--fontify-jira-headings'
can render with appropriate faces."
  (let ((lines (split-string text "\n"))
        (result '()))
    (dolist (line lines)
      (if (string-match "^\\(\\*+\\) \\(.*\\)" line)
          (let* ((level (length (match-string 1 line)))
                 (content (match-string 2 line)))
            (push (propertize content
                              'jira-heading level
                              'font-lock-multiline t)
                  result))
        (push line result)))
    (mapconcat #'identity (nreverse result) "\n")))

(defun go-jira-markup--text-properties-to-org-headings (text)
  "Convert `jira-heading' text properties in TEXT back to Org headings.
This is the reverse of `go-jira-markup--org-headings-to-text-properties',
preparing text for Pandoc's Org reader."
  (let ((lines (split-string text "\n"))
        (result '()))
    (dolist (line lines)
      (if-let ((level (get-text-property 0 'jira-heading line)))
          (push (format "%s %s" (make-string level ?*) line) result)
        (push line result)))
    (mapconcat #'identity (nreverse result) "\n")))

;;; Public API

;;;###autoload
(defun go-jira-markup-to-org (jira-text)
  "Convert JIRA-TEXT (Jira wiki markup) to Org-mode format.
Uses Pandoc for the heavy lifting, with pre/post-processing for
elements Pandoc doesn't handle well.  Headings are converted to
`jira-heading' text properties instead of real Org headings.
Returns the converted text as a string."
  (when (and jira-text (not (string-empty-p jira-text)))
    (let* (;; Pre-process: protect pandoc-lossy elements
           (text (go-jira-markup--jira-pre-process jira-text))
           ;; Run pandoc jira → org
           (text (go-jira-markup--pandoc-convert text "jira" "org"))
           ;; Post-process: restore placeholders, fix _nolang_
           (text (go-jira-markup--jira-post-process-org text))
           ;; Convert org headings to text properties
           (text (go-jira-markup--org-headings-to-text-properties text))
           ;; Trim trailing whitespace
           (text (string-trim-right text)))
      text)))

;;;###autoload
(defun go-jira-markup-from-org (org-text)
  "Convert ORG-TEXT (Org-mode markup) to Jira wiki markup format.
Uses Pandoc for the heavy lifting, with pre/post-processing for
elements Pandoc doesn't handle well.  `jira-heading' text properties
are converted back to Org headings before Pandoc processes the text.
Returns the converted text as a string."
  (when (and org-text (not (string-empty-p org-text)))
    (let* (;; Convert jira-heading properties to real org headings
           (text (go-jira-markup--text-properties-to-org-headings org-text))
           ;; Pre-process: protect pandoc-lossy org elements
           (text (go-jira-markup--org-pre-process text))
           ;; Run pandoc org → jira
           (text (go-jira-markup--pandoc-convert text "org" "jira"))
           ;; Post-process: restore placeholders, strip anchors, fix escaping
           (text (go-jira-markup--jira-post-process text))
           ;; Trim
           (text (string-trim text)))
      text)))

(provide 'go-jira-markup)
;;; go-jira-markup.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
