;;; go-jira-markup.el --- Jira markup to Org-mode converter -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.4.0
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
;; Jira content headings (h1. through h6.) become real Org headings
;; (* through ******).  The caller is responsible for adjusting heading
;; levels to fit the surrounding Org tree (e.g., shifting by +2 when
;; inserting under "** Description").  See `go-jira-markup-shift-headings'.

;;; Code:

(require 'cl-lib)

;;; Image attachment support

(defvar go-jira-markup--attachment-map nil
  "Alist of (FILENAME . (:id ID :attrs ATTRS-STRING :cache-path PATH)).
Bound dynamically by the caller (e.g., `go-jira-view-ticket') to provide
attachment metadata for image resolution.  The markup layer uses this to
rewrite Jira `!filename|attrs!' image references to local file paths.
ATTRS-STRING is the raw Jira attributes (e.g., \"width=535,alt=...\").")

(defvar go-jira-markup--image-widths nil
  "Alist of (FILENAME . WIDTH-STRING) for the current conversion.
Populated during Jira→Org pre-processing from image attributes.
Used during Org post-processing to insert #+ATTR_ORG lines.")

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
- Code blocks without language: {code}...{code} (Pandoc guesses java)
- Code blocks with missing newlines: {code:X}content{code} (Pandoc needs newlines)"
  (setq go-jira-markup--placeholders nil)

  ;; FIRST: Normalize code blocks and mark NOLANG in one pass.
  ;; Jira allows {code:lang}content{code} on same line or without trailing newline.
  ;; Pandoc requires {code:lang}\ncontent\n{code} to parse correctly.
  ;; Also, {code} without language needs marking as {code:NOLANG} so Pandoc
  ;; doesn't guess "java".
  ;;
  ;; Strategy: Walk through ALL {code...} tags, pair openers with closers.
  ;; - At depth 0: {code:LANG} or {code} is an opener
  ;; - At depth 1: {code} is a closer
  ;; - Skip {{code}} (Jira inline code) by checking for preceding {
  ;; For each opener-closer pair, ensure \n after opener and \n before closer.
  ;; For openers without language, replace with {code:NOLANG}.
  (let ((result "")
        (consumed 0)         ; how far we've appended to result
        (search-from 0)      ; where to search for next {code} tag
        (in-block nil)       ; nil when outside code block, non-nil inside
        (opener-start nil)   ; start pos of current opener tag in text
        (opener-end nil)     ; end pos of current opener tag in text
        (opener-tag nil))    ; the opener tag text (possibly rewritten)
    (while (string-match "{code\\(\\(?::[^}]*\\)?\\)}" text search-from)
      (let* ((tag-start (match-beginning 0))
             (tag-end (match-end 0))
             (lang-part (match-string 1 text)) ; "" for {code}, ":python" for {code:python}
             ;; Check if preceded by { (inline {{code}})
             (is-inline (and (> tag-start 0)
                             (eq (aref text (1- tag-start)) ?{))))
        (if is-inline
            ;; Skip inline {{code}} markup — pass through as-is
            (progn
              (setq result (concat result (substring text consumed tag-end)))
              (setq consumed tag-end)
              (setq search-from tag-end))
          (if (not in-block)
              ;; At depth 0: this is an opener — record position,
              ;; advance search but not consumed (text before opener
              ;; gets appended when the closer is found)
              (progn
                (setq opener-start tag-start)
                (setq opener-end tag-end)
                (setq opener-tag (if (string-empty-p lang-part)
                                     "{code:NOLANG}"
                                   (substring text tag-start tag-end)))
                (setq in-block t)
                (setq search-from tag-end))
            ;; At depth 1: this is a closer
            (let ((content (substring text opener-end tag-start)))
              ;; Ensure newline after opener
              (unless (string-prefix-p "\n" content)
                (setq content (concat "\n" content)))
              ;; Ensure newline before closer
              (unless (string-suffix-p "\n" content)
                (setq content (concat content "\n")))
              ;; Append: text before opener + normalized block
              (setq result (concat result
                                   (substring text consumed opener-start)
                                   opener-tag content "{code}"))
              (setq consumed tag-end)
              (setq search-from tag-end)
              (setq in-block nil))))))
    (setq text (concat result (substring text consumed))))

  ;; Protect image references: !filename|attrs! → rewrite to cached path.
  ;; Jira image syntax: !filename.png! or !filename.png|width=535,alt="..."!
  ;; Pandoc converts these to [[file:filename.png]], but:
  ;; 1) Attributes (width, alt) are lost
  ;; 2) The file path is just the bare filename — we need the cached local path
  ;; We rewrite them before Pandoc so they point to the cache, and record
  ;; width attributes for insertion as #+ATTR_ORG after Pandoc runs.
  ;; External URLs (!http://...!) are left alone — Pandoc handles those fine.
  (setq go-jira-markup--image-widths nil)
  (setq text (replace-regexp-in-string
              "!\\([^!\n|]+\\(?:\\.[a-zA-Z0-9]+\\)\\)\\(?:|\\([^!\n]*\\)\\)?!"
              (lambda (match)
                (let* ((filename (match-string 1 match))
                       (attrs (match-string 2 match))
                       (attachment (assoc filename go-jira-markup--attachment-map))
                       (cache-path (when attachment (plist-get (cdr attachment) :cache-path)))
                       ;; Extract width from attrs.  Must use save-match-data
                       ;; because string-match clobbers the match data that
                       ;; replace-regexp-in-string needs for the replacement range.
                       (width (when attrs
                                (save-match-data
                                  (when (string-match "width=\\([0-9]+\\)" attrs)
                                    (match-string 1 attrs))))))
                  (if (or (string-prefix-p "http://" filename)
                          (string-prefix-p "https://" filename))
                      ;; External URL — leave for Pandoc
                      match
                    ;; Record width for #+ATTR_ORG insertion in post-process
                    (when width
                      (push (cons (or cache-path filename) width)
                            go-jira-markup--image-widths))
                    ;; Rewrite to cache path if we have attachment info
                    (if cache-path
                        (format "!%s!" cache-path)
                      ;; No attachment map or unknown filename — keep bare filename
                      (format "!%s!" filename)))))
              text))

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

  text)

(defun go-jira-markup--jira-post-process-org (text)
  "Post-process Org TEXT after Pandoc jira→org conversion.
Restores placeholders to their Org equivalents."
  ;; Restore placeholders - they survived pandoc as literal text
  (setq text (go-jira-markup--restore-placeholders text))

  ;; Normalize image links: Pandoc produces [[path]] for absolute paths but
  ;; Org needs [[file:path]] to display inline images.  Also insert
  ;; #+ATTR_ORG: :width when width info was extracted during pre-processing.
  ;; We handle ALL image links (with and without width), normalizing them
  ;; to [[file:path]] format.
  ;;
  ;; First handle images with recorded width info:
  (when go-jira-markup--image-widths
    (dolist (pair go-jira-markup--image-widths)
      (let* ((path (car pair))
             (width (cdr pair))
             ;; Match both [[file:path]] and [[path]]
             (link-pattern (format "\\[\\[\\(?:file:\\)?%s\\]\\]"
                                   (regexp-quote path)))
             (replacement (format "#+ATTR_ORG: :width %s\n[[file:%s]]" width path)))
        (setq text (replace-regexp-in-string link-pattern replacement text t)))))
  ;; Then normalize any remaining absolute-path image links without width:
  ;; [[/some/path/image.png]] → [[file:/some/path/image.png]]
  (setq text (replace-regexp-in-string
              "\\[\\[\\(/[^]]*\\.\\(?:png\\|jpe?g\\|gif\\|bmp\\|svg\\|webp\\|ico\\)\\)\\]\\]"
              "[[file:\\1]]"
              text))

  ;; NOTE: We keep #+begin_src NOLANG as-is in the org output.
  ;; NOLANG is a marker indicating the original Jira {code} block had no
  ;; language.  The org→jira path converts {code:NOLANG} back to {code}.

  text)

;;; Org → Jira: pre/post-processing

(defun go-jira-markup--org-pre-process (text)
  "Pre-process Org TEXT before Pandoc org→jira conversion.
Protects elements that Pandoc's Org→Jira writer handles incorrectly:
- Citation markers «text» → placeholder (to restore as ??text??)
- Color markers from jira-pre-process → placeholder
- Underscores in words (to prevent pandoc treating them as subscript/emphasis)
- Image links with cache paths → placeholder (to restore as !filename|attrs!)"
  (setq go-jira-markup--placeholders nil)

  ;; Handle image links: strip #+ATTR_ORG width lines, rewrite cached image
  ;; paths back to bare filenames with original Jira attributes.
  ;; #+ATTR_ORG: :width 535
  ;; [[file:/cache/path/id/image.png]]
  ;; →  placeholder (restored as !image.png|width=535!)
  ;;
  ;; Also handles images without #+ATTR_ORG:
  ;; [[file:/cache/path/id/image.png]]  →  !image.png!
  (let ((image-rx (concat "\\(?:^#\\+ATTR_ORG:[[:space:]]+:width[[:space:]]+\\([0-9]+\\)\n\\)?"
                          "\\[\\[file:\\([^]]+\\)\\]\\]")))
    (setq text (replace-regexp-in-string
                image-rx
                (lambda (match)
                  (let* ((width (match-string 1 match))
                         (path (match-string 2 match))
                         (filename (file-name-nondirectory path))
                         ;; Only treat as image if extension looks like an image
                         (image-ext-p (string-match-p
                                       "\\.\\(png\\|jpe?g\\|gif\\|bmp\\|svg\\|webp\\|ico\\)\\'"
                                       filename)))
                    (if image-ext-p
                        (let ((jira-img (if width
                                           (format "!%s|width=%s!" filename width)
                                         (format "!%s!" filename))))
                          (go-jira-markup--placeholder "IMG" jira-img))
                      ;; Not an image — leave as-is
                      match)))
                text)))

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

  ;; CRITICAL FIX: Protect underscores in words (variable names, file paths, etc.)
  ;; Pandoc treats _text_ as subscript/emphasis, which breaks my_variable → my{~}variable{~}
  ;; We need to protect underscores that are:
  ;; - Surrounded by word characters (alphanumeric, not whitespace/punctuation)
  ;; - NOT part of org emphasis markup (_text_ at word boundaries)
  ;; - NOT in org special syntax like #+begin_src, #+end_src, etc.
  ;;
  ;; Strategy: Process line by line, skip lines that start with #+
  ;; For other lines, replace words containing underscores
  ;; This preserves actual org emphasis _like this_ while protecting my_variable
  (setq text (mapconcat
              (lambda (line)
                (if (string-match-p "^[[:space:]]*#\\+" line)
                    ;; Skip org special syntax lines (#+begin_src, #+end_src, etc.)
                    line
                  ;; Process normal text lines
                  (replace-regexp-in-string
                   ;; Match: word containing at least one underscore surrounded by alphanumerics
                   "\\<\\([[:alnum:]_]*[[:alnum:]]\\)_\\([[:alnum:]][[:alnum:]_]*\\)\\>"
                   (lambda (match)
                     (let ((word (match-string 0 match)))
                       ;; Replace ALL underscores in this word with placeholders
                       (replace-regexp-in-string
                        "_"
                        (lambda (_) (go-jira-markup--placeholder "UNDERSCORE" "_"))
                        word)))
                   line)))
              (split-string text "\n")  ;; Don't omit nulls - preserve empty lines
              "\n"))

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

  ;; Fix older pandoc (< 3.6) converting #+begin_example → {code:example}
  ;; instead of {noformat}.  Normalize to {noformat}.
  (setq text (replace-regexp-in-string
              "{code:example}\\(\\(?:.\\|\n\\)*?\\){code}"
              "{noformat}\\1{noformat}"
              text))

  ;; Fix trailing blank line before {code} and {noformat}
  ;; Pandoc adds an extra newline: "code\n\n{code}" → "code\n{code}"
  (setq text (replace-regexp-in-string "\n\n{code}" "\n{code}" text t t))
  (setq text (replace-regexp-in-string "\n\n{noformat}" "\n{noformat}" text t t))

  text)

;;; Heading level adjustment

(defun go-jira-markup-shift-headings (text offset)
  "Shift Org heading levels in TEXT by OFFSET.
Positive OFFSET increases levels (e.g., `*' → `***' with offset 2).
Negative OFFSET decreases levels (clamped to minimum 1).
Only modifies lines that start with `*+ ' (Org heading syntax)."
  (if (zerop offset)
      text
    (replace-regexp-in-string
     (rx line-start (group (+ "*")) " ")
     (lambda (match)
       (let* ((stars (match-string 1 match))
              (new-level (max 1 (+ (length stars) offset))))
         (concat (make-string new-level ?*) " ")))
     text)))

;;; Public API

;;;###autoload
(defun go-jira-markup-to-org (jira-text)
  "Convert JIRA-TEXT (Jira wiki markup) to Org-mode format.
Uses Pandoc for the heavy lifting, with pre/post-processing for
elements Pandoc doesn't handle well.  Jira content headings (h1.
through h6.) become real Org headings; the caller is responsible for
adjusting levels to fit the surrounding Org tree structure.
Returns the converted text as a string."
  (when (and jira-text (not (string-empty-p jira-text)))
    (let* (;; Pre-process: protect pandoc-lossy elements
           (text (go-jira-markup--jira-pre-process jira-text))
           ;; Run pandoc jira → org
           (text (go-jira-markup--pandoc-convert text "jira" "org"))
           ;; Post-process: restore placeholders, fix _nolang_
           (text (go-jira-markup--jira-post-process-org text))
           ;; Trim trailing whitespace
           (text (string-trim-right text)))
      text)))

;;;###autoload
(defun go-jira-markup-from-org (org-text)
  "Convert ORG-TEXT (Org-mode markup) to Jira wiki markup format.
Uses Pandoc for the heavy lifting, with pre/post-processing for
elements Pandoc doesn't handle well.  Org headings in ORG-TEXT are
converted to Jira headings (h1. through h6.); the caller should
normalize levels before calling if the text was extracted from a
nested Org subtree.
Returns the converted text as a string."
  (when (and org-text (not (string-empty-p org-text)))
    (let* (;; Pre-process: protect pandoc-lossy org elements
           (text (go-jira-markup--org-pre-process org-text))
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
