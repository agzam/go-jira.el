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

    (it "converts h1 to level-1 Org heading"
      (let ((result (go-jira-markup-to-org "h1. Main heading")))
        (expect result :to-match "^\\* Main heading")))

    (it "converts h2 to level-2 Org heading"
      (let ((result (go-jira-markup-to-org "h2. Sub heading")))
        (expect result :to-match "^\\*\\* Sub heading")))

    (it "converts h3 to level-3 Org heading"
      (let ((result (go-jira-markup-to-org "h3. Run linter")))
        (expect result :to-match "^\\*\\*\\* Run linter")))

    (it "converts h4-h6 to Org headings"
      (let ((result4 (go-jira-markup-to-org "h4. Details"))
            (result5 (go-jira-markup-to-org "h5. More"))
            (result6 (go-jira-markup-to-org "h6. Even more")))
        (expect result4 :to-match "^\\*\\*\\*\\* Details")
        (expect result5 :to-match "^\\*\\*\\*\\*\\* More")
        (expect result6 :to-match "^\\*\\*\\*\\*\\*\\* Even more"))))

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

    (it "converts Org headings to Jira headings"
      (expect (go-jira-markup-from-org "* Main heading")
              :to-match "^h1\\. .*Main heading"))

    (it "converts multiple heading levels"
      (let ((result (go-jira-markup-from-org "* Title\n** Subtitle")))
        (expect result :to-match "^h1\\. .*Title")
        (expect result :to-match "^h2\\. .*Subtitle")))

    (it "does not add {anchor:} markers"
      (expect (go-jira-markup-from-org "* Heading")
              :not :to-match "{anchor:")))

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

  (it "round-trips headings via Org headings"
    (let* ((input "h1. Main heading\nh2. Sub heading")
           (org (go-jira-markup-to-org input))
           (back (go-jira-markup-from-org org)))
      (expect back :to-match "^h1\\. .*Main heading")
      (expect back :to-match "^h2\\. .*Sub heading")))

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

(describe "Code blocks with complex JSON content"

  (describe "JSON with nested objects and escaped quotes"

    (it "round-trips JSON code block with nested structure"
      (let* ((input "{code:json}\n{\n  \"completionState\": \"FAILED\",\n  \"datasetStatuses\": {\n    \"48728\": {\n      \"extractionEndMode\": \"fullLoadEnd\",\n      \"completionState\": \"COMPLETED\"\n    },\n    \"48729\": {\n      \"extractionEndMode\": \"fullLoadPartial\",\n      \"completionState\": \"FAILED\"\n    }\n  }\n}\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        ;; Normalize trailing newlines that pandoc may add/remove
        (expect (string-trim back) :to-equal (string-trim input))))

    (it "preserves JSON structure through multiple round-trips"
      (let* ((input "{code:json}\n{\n  \"test\": \"value\",\n  \"nested\": {\n    \"key\": \"data\"\n  }\n}\n{code}")
             (org1 (go-jira-markup-to-org input))
             (jira1 (go-jira-markup-from-org org1))
             (org2 (go-jira-markup-to-org jira1))
             (jira2 (go-jira-markup-from-org org2))
             (org3 (go-jira-markup-to-org jira2))
             (jira3 (go-jira-markup-from-org org3)))
        ;; After multiple round-trips, output should stabilize
        (expect (string-trim jira3) :to-equal (string-trim jira2))
        (expect (string-trim jira2) :to-equal (string-trim jira1))))

    (it "does not corrupt escaped quotes in JSON"
      (let* ((input "{code:json}\n{\"key\": \"value with \\\"quotes\\\"\"}\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "\\\"quotes\\\\\"")))

    (it "preserves newlines in JSON code blocks"
      (let* ((input "{code:json}\n{\n  \"line1\": \"value1\",\n  \"line2\": \"value2\"\n}\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        ;; Should have real newlines, not escaped ones like \n or \\n
        (expect (string-trim back) :to-equal (string-trim input))
        (expect back :not :to-match "\\\\n")  ; No double-escaped newlines
        (expect back :not :to-match "\\\\\\\\")  ; No escaped backslashes
        )))

  (describe "Code blocks without language (NOLANG marker handling)"

    (it "removes NOLANG marker from final output"
      (let* ((input "{code}\nplain code\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        ;; NOLANG is an internal marker - should never appear in output
        (expect back :not :to-match "NOLANG")
        (expect (string-trim back) :to-equal (string-trim input))))

    (it "handles NOLANG through multiple round-trips"
      (let* ((input "{code}\nsome code\nmore lines\n{code}")
             (org1 (go-jira-markup-to-org input))
             (jira1 (go-jira-markup-from-org org1))
             (org2 (go-jira-markup-to-org jira1))
             (jira2 (go-jira-markup-from-org org2))
             (org3 (go-jira-markup-to-org jira2))
             (jira3 (go-jira-markup-from-org org3)))
        ;; NOLANG should NEVER leak into any output
        (expect jira1 :not :to-match "NOLANG")
        (expect jira2 :not :to-match "NOLANG")
        (expect jira3 :not :to-match "NOLANG")
        ;; Output should stabilize
        (expect (string-trim jira3) :to-equal (string-trim jira2)))))

  (describe "Edge cases with special characters"

    (it "handles code blocks with curly braces"
      (let* ((input "{code:javascript}\nfunction test() { return {}; }\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "{ return {}; }")
        (expect (string-trim back) :to-equal (string-trim input))))

    (it "handles code blocks with backslashes"
      (let* ((input "{code:bash}\necho \"path\\\\to\\\\file\"\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "path\\\\\\\\to\\\\\\\\file")
        ;; Should not have triple or quadruple backslashes
        (expect back :not :to-match "\\\\\\\\\\\\\\\\")))

    (it "handles empty code blocks"
      (let* ((input "{code:python}\n\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect (string-trim back) :to-match "{code:python}")
        (expect back :not :to-match "NOLANG")))

    (it "handles code blocks with only whitespace"
      (let* ((input "{code}\n   \n\t\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :not :to-match "NOLANG"))))

  (describe "Simulated edit flow with JSON encoding"

    (it "survives JSON encoding and decoding"
      (let* ((org-text "#+begin_src json\n{\n  \"test\": \"value\"\n}\n#+end_src")
             (jira-text (go-jira-markup-from-org org-text))
             ;; Simulate JSON encoding (what happens in go-jira-edit-submit)
             (json-string (json-encode (list :description jira-text)))
             ;; Simulate JSON decoding (what go-jira CLI would do)
             (decoded (let ((json-object-type 'plist))
                        (plist-get (json-read-from-string json-string) :description))))
        ;; Decoded text should match original jira text
        (expect decoded :to-equal jira-text)
        ;; Should not have escaped newlines or backslashes
        (expect decoded :not :to-match "\\\\n")
        (expect decoded :not :to-match "\\\\\\\\")
        ;; Should not have NOLANG
        (expect decoded :not :to-match "NOLANG")))

    (it "handles complex JSON through full edit cycle"
      (let* ((org-text "#+begin_src json\n{\n  \"completionState\": \"FAILED\",\n  \"nested\": {\n    \"key\": \"value\"\n  }\n}\n#+end_src")
             (jira-text (go-jira-markup-from-org org-text))
             (json-string (json-encode (list :fields (list :description jira-text))))
             (decoded (let ((json-object-type 'plist))
                        (plist-get (plist-get (json-read-from-string json-string) :fields) :description)))
             ;; Convert back to org (simulating next edit)
             (org-again (go-jira-markup-to-org decoded))
             ;; Convert to jira again
             (jira-again (go-jira-markup-from-org org-again)))
        ;; Should not have any corruption
        (expect jira-again :not :to-match "\\\\n")
        (expect jira-again :not :to-match "\\\\\\\\")
        (expect jira-again :not :to-match "NOLANG")
        ;; Should match original (normalized)
        (expect (string-trim jira-again) :to-equal (string-trim jira-text))))))

(describe "Underscores in words (variable names, file paths, identifiers)"

  (describe "Basic underscore preservation"

    (it "preserves underscores in variable names"
      (let* ((input "Variable names: my_variable, another_var, some_function_name")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_variable")
        (expect back :to-match "another_var")
        (expect back :to-match "some_function_name")
        (expect back :not :to-match "{~}")  ; No subscript markup
        (expect back :to-equal input)))  ; Perfect round-trip

    (it "preserves underscores in file paths"
      (let* ((input "File: /path/to/my_file.txt and another_file_name.json")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_file")
        (expect back :to-match "another_file_name")
        (expect back :to-equal input)))

    (it "preserves underscores in code identifiers"
      (let* ((input "Constants: MAX_SIZE, MIN_VALUE, DEFAULT_CONFIG")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "MAX_SIZE")
        (expect back :to-match "MIN_VALUE")
        (expect back :to-match "DEFAULT_CONFIG")
        (expect back :to-equal input)))

    (it "handles multiple underscores in one word"
      (let* ((input "Complex: this_is_a_long_variable_name")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "this_is_a_long_variable_name")
        (expect back :not :to-match "{~}")
        (expect back :to-equal input))))

  (describe "Distinguishing underscores from emphasis"

    (it "distinguishes between underscore words and actual emphasis"
      (let* ((input "Variable: my_var but also _actual italic_ text")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_var")
        (expect back :to-match "_actual italic_")
        (expect back :to-equal input)))

    (it "distinguishes between underscore words and actual subscript"
      (let* ((input "Variable: my_var but subscript: H~2~O")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_var")
        (expect back :to-match "H~2~O")
        (expect back :to-equal input))))

  (describe "Underscores in code blocks"

    (it "preserves underscores in code blocks"
      (let* ((input "{code:python}\ndef my_function(param_name):\n    result_value = process_data(param_name)\n    return result_value\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_function")
        (expect back :to-match "param_name")
        (expect back :to-match "result_value")
        (expect back :to-match "process_data")))

    (it "preserves underscores in bash commands"
      (let* ((input "{code:bash}\ncd /path/to/my_project\nnpm run build_production\n./scripts/deploy_to_staging.sh\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_project")
        (expect back :to-match "build_production")
        (expect back :to-match "deploy_to_staging")))

    (it "preserves underscores in SQL"
      (let* ((input "{code:sql}\nSELECT user_id, user_name, created_at\nFROM user_table\nWHERE status_code = 'ACTIVE'\n{code}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "user_id")
        (expect back :to-match "user_name")
        (expect back :to-match "created_at")
        (expect back :to-match "user_table")
        (expect back :to-match "status_code"))))

  (describe "Edge cases"

    (it "handles URLs with underscores"
      (let* ((input "Link: https://example.com/api/user_profile?param_name=value")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "user_profile")
        (expect back :to-match "param_name")))

    (it "handles email addresses with underscores"
      (let* ((input "Contact: john_doe@example.com and jane_smith@test.org")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "john_doe")
        (expect back :to-match "jane_smith")))

    (it "handles mixed underscores and dashes"
      (let* ((input "Variables: my-kebab-case and my_snake_case and camelCase")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my-kebab-case")
        (expect back :to-match "my_snake_case")))



    (it "handles underscores in inline code"
      (let* ((input "Use {{my_function()}} to call {{another_func}}")
             (org (go-jira-markup-to-org input))
             (back (go-jira-markup-from-org org)))
        (expect back :to-match "my_function")
        (expect back :to-match "another_func"))))

  (describe "Multiple round-trips with underscores"

    (it "is idempotent with underscored variables"
      (let* ((input "Content with my_variable and *bold* and _italic_")
             (org1 (go-jira-markup-to-org input))
             (jira1 (go-jira-markup-from-org org1))
             (org2 (go-jira-markup-to-org jira1))
             (jira2 (go-jira-markup-from-org org2))
             (org3 (go-jira-markup-to-org jira2))
             (jira3 (go-jira-markup-from-org org3)))
        ;; Should stabilize immediately
        (expect jira1 :to-equal jira2)
        (expect jira2 :to-equal jira3)
        ;; Should preserve content
        (expect jira1 :to-match "my_variable")
        (expect jira1 :to-match "\\*bold\\*")
        (expect jira1 :to-match "_italic_")))))

(describe "go-jira-markup--find-pandoc"

  (it "finds pandoc executable"
    (expect (go-jira-markup--find-pandoc) :to-match "pandoc"))

  (it "caches the result"
    (let ((go-jira-markup--pandoc-exe nil))
      (go-jira-markup--find-pandoc)
      (expect go-jira-markup--pandoc-exe :not :to-be nil))))

(provide 'go-jira-markup-tests)
;;; go-jira-markup-tests.el ends here
