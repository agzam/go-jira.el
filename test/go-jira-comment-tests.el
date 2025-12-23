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
;;  Tests for Org-mode to JIRA markup conversion
;;
;;; Code:

(require 'buttercup)
(require 'go-jira-markup)

(describe "go-jira-markup-from-org"
  
  (describe "headings"
    (it "converts org headings to jira headings"
      (expect (go-jira-markup-from-org "*** Heading 1")
              :to-equal "h1. Heading 1"))
    
    (it "converts multiple heading levels"
      (expect (go-jira-markup-from-org "*** H1\n**** H2\n***** H3")
              :to-equal "h1. H1\nh2. H2\nh3. H3")))
  
  (describe "inline formatting"
    (it "converts italic to jira underline"
      (expect (go-jira-markup-from-org "This is /italic/ text")
              :to-equal "This is _italic_ text"))
    
    (it "converts underline to jira insert"
      (expect (go-jira-markup-from-org "This is _underlined_ text")
              :to-equal "This is +underlined+ text"))
    
    (it "converts strikethrough"
      (expect (go-jira-markup-from-org "This is +struck+ text")
              :to-equal "This is -struck- text"))
    
    (it "converts inline code"
      (expect (go-jira-markup-from-org "Use ~code~ here")
              :to-match "{{code}}")))
  
  (describe "lists"
    (it "converts simple bulleted list"
      (expect (go-jira-markup-from-org "- Item 1\n- Item 2\n- Item 3")
              :to-equal "* Item 1\n* Item 2\n* Item 3"))
    
    (it "converts simple numbered list"
      (expect (go-jira-markup-from-org "1. First\n2. Second\n3. Third")
              :to-equal "# First\n# Second\n# Third"))
    
    (it "converts nested bulleted list"
      (expect (go-jira-markup-from-org "- Item 1\n  - Sub 1\n  - Sub 2")
              :to-equal "* Item 1\n** Sub 1\n** Sub 2"))
    
    (it "converts nested numbered list"
      (expect (go-jira-markup-from-org "1. First\n  1. Sub first\n  2. Sub second")
              :to-equal "# First\n## Sub first\n## Sub second")))
  
  (describe "links"
    (it "converts links with description"
      (expect (go-jira-markup-from-org "[[https://example.com][Example]]")
              :to-equal "[Example|https://example.com]"))
    
    (it "converts links without description"
      (expect (go-jira-markup-from-org "[[https://example.com]]")
              :to-equal "[https://example.com]")))
  
  (describe "code blocks"
    (it "converts code block with language"
      (expect (go-jira-markup-from-org "#+begin_src python\nprint('hello')\n#+end_src")
              :to-equal "{code:python}\nprint('hello')\n{code}"))
    
    (it "converts code block without language"
      (expect (go-jira-markup-from-org "#+begin_src\nsome code\n#+end_src")
              :to-equal "{code}\nsome code\n{code}"))
    
    (it "converts example block"
      (expect (go-jira-markup-from-org "#+begin_example\nsome example\n#+end_example")
              :to-equal "{noformat}\nsome example\n{noformat}")))
  
  (describe "complex content"
    (it "converts a realistic comment"
      (let ((input "*** Analysis

Here's what I found:

1. The *first* issue is /critical/
2. The second needs ~refactoring~
  - Check the database
  - Review the logs

Code example:

#+begin_src clojure
(defn example []
  (println \"test\"))
#+end_src

See [[https://docs.example.com][the docs]] for more info."))
        (let ((result (go-jira-markup-from-org input)))
          (expect result :to-match "h1\\. Analysis")
          (expect result :to-match "# The \\*first\\* issue")
          (expect result :to-match "_critical_")
          (expect result :to-match "{{refactoring}}")
          (expect result :to-match "\\* Check the database")
          (expect result :to-match "{code:clojure}")
          (expect result :to-match "\\[the docs\\|https://docs.example.com\\]"))))))

(provide 'go-jira-comment-tests)
;;; go-jira-comment-tests.el ends here
