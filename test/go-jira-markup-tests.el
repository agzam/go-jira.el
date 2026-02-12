;;; go-jira-markup-tests.el --- Tests for go-jira-markup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: December 06, 2024
;; Keywords: tools jira
;; Homepage: https://github.com/agzam/go-jira.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for JIRA markup to Org-mode conversion (Pandoc-based)
;;
;;; Code:

(require 'buttercup)
(require 'go-jira-markup)

(describe "go-jira-markup-to-org"

  (describe "nested lists - numbered with bullets"

    (it "converts numbered list with nested bullets"
      (let ((input "# Item 1
# Item 2
# Item 3 with sub-items:
#* Sub-item A
#* Sub-item B
# Item 4
# Item 5"))
        (let ((result (go-jira-markup-to-org input)))
          ;; Pandoc separates list items with blank lines (standard Org style)
          (expect result :to-match "^1\\. Item 1")
          (expect result :to-match "^2\\. Item 2")
          (expect result :to-match "^3\\. Item 3 with sub-items:")
          (expect result :to-match "Sub-item A")
          (expect result :to-match "Sub-item B")
          (expect result :to-match "^4\\. Item 4")
          (expect result :to-match "^5\\. Item 5"))))

    (it "preserves nested bullet items under numbered items"
      (let* ((input "# Top item
#* Sub A
#* Sub B
# Next item")
             (result (go-jira-markup-to-org input)))
        (expect result :to-match "1\\. Top item")
        (expect result :to-match "Sub A")
        (expect result :to-match "Sub B")
        (expect result :to-match "2\\. Next item")))

    (it "handles deep nesting with ##"
      (let* ((input "# Level 1
## Level 2
## Level 2b
# Level 1b")
             (result (go-jira-markup-to-org input)))
        (expect result :to-match "1\\. Level 1")
        (expect result :to-match "Level 2")
        (expect result :to-match "Level 2b")
        (expect result :to-match "Level 1b"))))

  (describe "deeply nested lists"

    (it "converts three-level nested bulleted list"
      (let ((result (go-jira-markup-to-org "* One\n** Two\n*** Three")))
        (expect result :to-match "^- One")
        (expect result :to-match "- Two")
        (expect result :to-match "- Three")))

    (it "does not confuse three asterisks with org headings"
      (let ((result (go-jira-markup-to-org "* Item\n** Sub-item\n*** Sub-sub-item")))
        (expect result :to-match "^- Item")
        (expect result :to-match "- Sub-item")
        (expect result :to-match "- Sub-sub-item")
        ;; Should NOT have raw *** that look like headings
        (expect result :not :to-match "^\\*\\*\\*"))))

  (describe "basic inline formatting"

    (it "converts inline code"
      ;; Pandoc uses =code= (Org verbatim) instead of ~code~ (Org code)
      ;; Both are valid Org-mode inline code markup
      (expect (go-jira-markup-to-org "Some {{code}} here")
              :to-match "=code="))

    (it "converts bold text"
      (expect (go-jira-markup-to-org "Some *bold* text")
              :to-match "\\*bold\\*"))

    (it "converts italic text"
      (expect (go-jira-markup-to-org "Some _italic_ text")
              :to-match "/italic/"))

    (it "converts strikethrough text"
      (expect (go-jira-markup-to-org "Some -strikethrough- text")
              :to-match "\\+strikethrough\\+"))

    (it "converts underline/insert text"
      (expect (go-jira-markup-to-org "Some +underlined+ text")
              :to-match "_underlined_")))

  (describe "headings"

    (it "converts h1 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h1. Main heading")))
        (expect result :to-match "Main heading")
        (expect (get-text-property 0 'jira-heading result) :to-equal 1)))

    (it "converts h2 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h2. Sub heading")))
        (expect result :to-match "Sub heading")
        (expect (get-text-property 0 'jira-heading result) :to-equal 2)))

    (it "converts h3 with jira-heading property"
      (let ((result (go-jira-markup-to-org "h3. Run linter")))
        (expect result :to-match "Run linter")
        (expect (get-text-property 0 'jira-heading result) :to-equal 3)))

    (it "converts h4-h6 with jira-heading properties"
      (let ((result4 (go-jira-markup-to-org "h4. Details"))
            (result5 (go-jira-markup-to-org "h5. More"))
            (result6 (go-jira-markup-to-org "h6. Even more")))
        (expect (get-text-property 0 'jira-heading result4) :to-equal 4)
        (expect (get-text-property 0 'jira-heading result5) :to-equal 5)
        (expect (get-text-property 0 'jira-heading result6) :to-equal 6))))

  (describe "code blocks"

    (it "converts code blocks with language"
      (let ((result (go-jira-markup-to-org "{code:java}\npublic class Test {}\n{code}")))
        (expect result :to-match "#\\+begin_src java")
        (expect result :to-match "public class Test {}")
        (expect result :to-match "#\\+end_src")))

    (it "converts code blocks without language"
      (let ((result (go-jira-markup-to-org "{code}\nsome code\n{code}")))
        ;; Pandoc guesses java for unspecified language, but the important
        ;; thing is it becomes a src block
        (expect result :to-match "#\\+begin_src")
        (expect result :to-match "some code")
        (expect result :to-match "#\\+end_src")))

    (it "converts noformat blocks to example blocks"
      (let ((result (go-jira-markup-to-org "{noformat}\nsome text\n{noformat}")))
        (expect result :to-match "#\\+begin_example")
        (expect result :to-match "some text")
        (expect result :to-match "#\\+end_example"))))

  (describe "quote blocks"

    (it "converts {quote} blocks"
      (let ((result (go-jira-markup-to-org "{quote}\nsome quote\n{quote}")))
        (expect result :to-match "#\\+begin_quote")
        (expect result :to-match "some quote")
        (expect result :to-match "#\\+end_quote")))

    (it "converts bq. syntax"
      (let ((result (go-jira-markup-to-org "bq. single line quote")))
        (expect result :to-match "#\\+begin_quote")
        (expect result :to-match "single line quote")
        (expect result :to-match "#\\+end_quote")))

    (it "handles quote-wrapped noformat blocks"
      ;; Pandoc faithfully nests these (quote containing example)
      ;; which is correct Org structure
      (let ((result (go-jira-markup-to-org "{quote}\n{noformat}\ncode here\n{noformat}\n{quote}")))
        (expect result :to-match "#\\+begin_quote")
        (expect result :to-match "#\\+begin_example")
        (expect result :to-match "code here")
        (expect result :to-match "#\\+end_example")
        (expect result :to-match "#\\+end_quote")))

    (it "handles quote-wrapped code blocks"
      (let ((result (go-jira-markup-to-org "{quote}\n{code:python}\nprint('hello')\n{code}\n{quote}")))
        (expect result :to-match "#\\+begin_quote")
        (expect result :to-match "#\\+begin_src python")
        (expect result :to-match "print('hello')")
        (expect result :to-match "#\\+end_src")
        (expect result :to-match "#\\+end_quote"))))

  (describe "block content escaping"

    (it "escapes asterisk lines in noformat blocks"
      (let ((result (go-jira-markup-to-org "{noformat}\n* line 1\n* line 2\n{noformat}")))
        (expect result :to-match "#\\+begin_example")
        (expect result :to-match ",\\* line 1")
        (expect result :to-match ",\\* line 2")
        (expect result :to-match "#\\+end_example")))

    (it "escapes asterisk lines in code blocks"
      (let ((result (go-jira-markup-to-org "{code:python}\n* item1\n** item2\n{code}")))
        (expect result :to-match ",\\* item1")
        (expect result :to-match ",\\*\\* item2")))

    (it "escapes #+ lines in noformat blocks"
      (let ((result (go-jira-markup-to-org "{noformat}\n#+begin_src python\nfoo\n#+end_src\n{noformat}")))
        (expect result :to-match "#\\+begin_example")
        (expect result :to-match ",#\\+begin_src python")
        (expect result :to-match ",#\\+end_src")
        (expect result :to-match "#\\+end_example"))))

  (describe "links and images"

    (it "converts links with text"
      (let ((result (go-jira-markup-to-org "[link text|https://example.com]")))
        (expect result :to-match "\\[\\[https://example.com\\]\\[link text\\]\\]")))

    (it "converts plain URL links"
      (let ((result (go-jira-markup-to-org "[https://example.com]")))
        (expect result :to-match "https://example.com")))

    (it "converts images"
      (let ((result (go-jira-markup-to-org "!image.png!")))
        (expect result :to-match "\\[\\[file:image.png\\]\\]"))))

  (describe "tables"

    (it "converts tables with headers"
      (let ((result (go-jira-markup-to-org "||Header 1||Header 2||\n|Cell 1|Cell 2|")))
        (expect result :to-match "Header 1")
        (expect result :to-match "Header 2")
        (expect result :to-match "Cell 1")
        (expect result :to-match "Cell 2")
        ;; Should have separator line
        (expect result :to-match "---"))))

  (describe "color markup"

    (it "preserves color markup through round-trip"
      (let ((result (go-jira-markup-to-org "{color:red}colored text{color}")))
        (expect result :to-match "colored text")
        ;; Color is preserved as literal text for round-trip fidelity
        (expect result :to-match "{color:red}")
        (expect result :to-match "{color}")))))

(describe "go-jira-markup-from-org"

  (describe "headings"

    (it "converts jira-heading properties to Jira headings"
      (let ((input (propertize "Main heading" 'jira-heading 1)))
        (expect (go-jira-markup-from-org input) :to-match "^h1\\. Main heading")))

    (it "converts multiple heading levels"
      (let ((input (concat (propertize "Title" 'jira-heading 1) "\n"
                           (propertize "Subtitle" 'jira-heading 2))))
        (let ((result (go-jira-markup-from-org input)))
          (expect result :to-match "^h1\\. Title")
          (expect result :to-match "^h2\\. Subtitle"))))

    (it "does not add {anchor:} markers"
      (let ((input (propertize "Heading" 'jira-heading 1)))
        (expect (go-jira-markup-from-org input) :not :to-match "{anchor:"))))

  (describe "inline formatting"

    (it "converts bold"
      (expect (go-jira-markup-from-org "Some *bold* text") :to-match "\\*bold\\*"))

    (it "converts italic"
      (expect (go-jira-markup-from-org "Some /italic/ text") :to-match "_italic_"))

    (it "converts strikethrough"
      (expect (go-jira-markup-from-org "Some +strikethrough+ text") :to-match "-strikethrough-"))

    (it "converts inline verbatim"
      (expect (go-jira-markup-from-org "Some =code= text") :to-match "{{code}}")))

  (describe "code blocks"

    (it "converts src blocks with language"
      (let ((result (go-jira-markup-from-org "#+begin_src python\ndef hello():\n    pass\n#+end_src")))
        (expect result :to-match "{code:python}")
        (expect result :to-match "def hello")
        (expect result :to-match "{code}")))

    (it "converts example blocks to noformat"
      (let ((result (go-jira-markup-from-org "#+begin_example\nsome text\n#+end_example")))
        (expect result :to-match "{noformat}")
        (expect result :to-match "some text"))))

  (describe "lists"

    (it "converts numbered lists"
      (let ((result (go-jira-markup-from-org "1. Item 1\n2. Item 2\n3. Item 3")))
        (expect result :to-match "^# Item 1")
        (expect result :to-match "^# Item 2")
        (expect result :to-match "^# Item 3")))

    (it "converts bulleted lists"
      (let ((result (go-jira-markup-from-org "- Item A\n- Item B")))
        (expect result :to-match "^\\* Item A")
        (expect result :to-match "^\\* Item B")))

    (it "converts nested bulleted lists"
      (let ((result (go-jira-markup-from-org "- One\n  - Two\n    - Three")))
        (expect result :to-match "^\\* One")
        (expect result :to-match "^\\*\\* Two")
        (expect result :to-match "^\\*\\*\\* Three"))))

  (describe "links"

    (it "converts org links to Jira links"
      (let ((result (go-jira-markup-from-org "[[https://example.com][link text]]")))
        (expect result :to-match "\\[link text|https://example.com\\]")))

    (it "converts plain org links"
      (let ((result (go-jira-markup-from-org "[[https://example.com]]")))
        (expect result :to-match "https://example.com"))))

  (describe "tables"

    (it "converts org tables to Jira tables"
      (let ((result (go-jira-markup-from-org "| H1 | H2 |\n|----+----|\n| C1 | C2 |")))
        (expect result :to-match "||.*H1.*||.*H2.*||")
        (expect result :to-match "|.*C1.*|.*C2.*|")))))

(describe "go-jira-markup round-trip (jira → org → jira)"

  (it "round-trips bold text"
    (let* ((input "Some *bold* text")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips numbered lists"
    (let* ((input "# Item 1\n# Item 2\n# Item 3")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips noformat blocks with asterisk lines"
    (let* ((input "{noformat}\n* could use incrementalLoadError state\n* another line\n{noformat}")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal (string-trim input))))

  (it "round-trips noformat blocks with #+ lines"
    (let* ((input "{noformat}\n#+begin_src python\nfoo\n#+end_src\n{noformat}")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal (string-trim input))))

  (it "round-trips code blocks with language"
    (let* ((input "{code:python}\ndef hello():\n    pass\n{code}")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      ;; Pandoc may add trailing newline before {code} - normalize
      (expect (replace-regexp-in-string "\n\n{code}" "\n{code}" back)
              :to-equal (string-trim input))))

  (it "round-trips links"
    (let* ((input "[link text|https://example.com]")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips plain text unchanged"
    (let* ((input "Just plain text with no markup")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips tables"
    (let* ((input "||Header 1||Header 2||\n|Cell 1|Cell 2|\n|Cell 3|Cell 4|")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      ;; Pandoc may add spaces around cell content
      (expect back :to-match "||.*Header 1.*||.*Header 2.*||")
      (expect back :to-match "|.*Cell 1.*|.*Cell 2.*|")
      (expect back :to-match "|.*Cell 3.*|.*Cell 4.*|")))

  (it "round-trips headings via jira-heading properties"
    (let* ((input "h1. Main heading\nh2. Sub heading")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-match "^h1\\. Main heading")
      (expect back :to-match "^h2\\. Sub heading")))

  (it "round-trips bulleted lists"
    (let* ((input "* Item A\n* Item B\n* Item C")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips nested bulleted lists"
    (let* ((input "* One\n** Two\n*** Three")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips numbered lists with nested bullets"
    (let* ((input "# Top\n#* Sub A\n#* Sub B\n# Next")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips italic text"
    (let* ((input "Some _italic_ text")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips strikethrough text"
    (let* ((input "Some -strikethrough- text")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips superscript and subscript"
    (let* ((input "x^2^ and H~2~O")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips citation text"
    (let* ((input "Some ??citation text?? here")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips color markup"
    (let* ((input "{color:red}colored text{color}")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal input)))

  (it "round-trips code blocks without language"
    (let* ((input "{code}\nsome code\n{code}")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-equal (string-trim input))))

  (it "is idempotent after first pass (no continued drift)"
    ;; The critical property: after one round-trip, subsequent trips
    ;; produce the same output (the conversion stabilizes)
    (let* ((input "h1. Title\n\nSome *bold*, _italic_, -strike-, +under+, {{code}} text.\n^super^ and ~sub~ and ??cite??.\n{color:red}red{color}.\n\n# Item 1\n#* Sub A\n# Item 2\n\n{code:python}\ndef test():\n    pass\n{code}\n\n{code}\nplain code\n{code}\n\n||H1||H2||\n|C1|C2|")
           (org1 (go-jira-markup-to-org input))
           (jira1 (go-jira-markup-from-org org1))
           (org2 (go-jira-markup-to-org jira1))
           (jira2 (go-jira-markup-from-org org2)))
      (expect jira2 :to-equal jira1))))

(describe "go-jira-markup--find-pandoc"

  (it "finds pandoc executable"
    (expect (go-jira-markup--find-pandoc) :to-match "pandoc"))

  (it "caches the result"
    (let ((go-jira-markup--pandoc-exe nil))
      (go-jira-markup--find-pandoc)
      (expect go-jira-markup--pandoc-exe :not :to-be nil))))

(provide 'go-jira-markup-tests)
;;; go-jira-markup-tests.el ends here
