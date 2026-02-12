;; go-jira-e2e-test.el --- E2E round-trip tests against real Jira -*- lexical-binding: t; -*-
;;
;; Tests the ENTIRE editing flow against SAC-30264:
;;   fetch → parse/convert → display → edit → submit → fetch → verify
;;
;; Covers: title, description, comment add, comment edit, all markup types.
;;
;; Run:  emacs -Q --batch -L . -l test/go-jira-e2e-test.el
;;
;; Cleanup: test content is wiped after a successful run.
;;          Set go-jira-debug to t to keep it for inspection.

(require 'go-jira-markup)
(require 'json)

(defvar go-jira-debug nil)

(defvar e2e-ticket "SAC-30264")
(defvar e2e-clean-title "This is a dummy ticket for e2e testing. Ignore!")
(defvar e2e-fails 0)
(defvar e2e-total 0)
(defvar e2e-comment-ids nil "Comment IDs created during this run, for cleanup.")

;; ── helpers ────────────────────────────────────────────────────────

(defun e2e-check (name test)
  (setq e2e-total (1+ e2e-total))
  (if test
      (princ (format "  ✅ %s\n" name))
    (setq e2e-fails (1+ e2e-fails))
    (princ (format "  ❌ FAIL: %s\n" name))))

(defun e2e-jira-edit (ticket json-payload)
  "Submit JSON-PAYLOAD to TICKET via jira edit. Return t on success."
  (let ((jf (make-temp-file "e2e-" nil ".json"))
        (sf (make-temp-file "e2e-" nil ".sh")))
    (unwind-protect
        (progn
          (with-temp-file jf (insert json-payload))
          (with-temp-file sf
            (insert (format "#!/bin/bash\ncat %s > \"$1\"\n"
                            (shell-quote-argument jf))))
          (set-file-modes sf #o755)
          (with-temp-buffer
            (call-process "bash" nil (current-buffer) nil "-c"
                          (format "echo y | jira edit %s --editor %s --template json 2>&1"
                                  (shell-quote-argument ticket)
                                  (shell-quote-argument sf)))
            (let ((out (buffer-string)))
              (when go-jira-debug (princ (format "    jira edit: %s\n" (string-trim out))))
              (string-match-p "^OK" out))))
      (ignore-errors (delete-file jf))
      (ignore-errors (delete-file sf)))))

(defun e2e-fetch ()
  "Fetch TICKET fields. Return (summary description comments-alist)."
  (with-temp-buffer
    (call-process "jira" nil (current-buffer) nil
                  "view" e2e-ticket "--template" "debug")
    (let* ((data (json-read-from-string (buffer-string)))
           (flds (cdr (assoc 'fields data)))
           (sum  (cdr (assoc 'summary flds)))
           (desc (or (cdr (assoc 'description flds)) ""))
           (cobj (cdr (assoc 'comment flds)))
           (carr (cdr (assoc 'comments cobj)))
           (clst (mapcar (lambda (c)
                           (list :id   (cdr (assoc 'id c))
                                 :body (or (cdr (assoc 'body c)) "")))
                         carr)))
      (list :summary sum :description desc :comments clst))))

(defun e2e-update-fields (summary description)
  "Update title + description. Return t on success."
  (e2e-jira-edit e2e-ticket
                 (json-encode (list :fields (list :summary summary
                                                  :description description)))))

(defun e2e-add-comment (body-jira)
  "Add a comment with BODY-JIRA markup. Return t on success."
  (e2e-jira-edit e2e-ticket
                 (json-encode
                  (list :update
                        (list :comment
                              (vector (list :add (list :body body-jira))))))))

(defun e2e-edit-comment (id body-jira)
  "Edit comment ID with new BODY-JIRA. Return t on success."
  (e2e-jira-edit e2e-ticket
                 (json-encode
                  (list :update
                        (list :comment
                              (vector (list :edit (list :id id
                                                       :body body-jira))))))))

(defun e2e-delete-comment (id)
  "Delete comment ID. Return t on success."
  (with-temp-buffer
    (call-process "jira" nil (current-buffer) nil
                  "req" "-M" "DELETE"
                  (format "/rest/api/2/issue/%s/comment/%s" e2e-ticket id))
    t))

(defun e2e-cleanup ()
  "Reset ticket: clear description, restore title, delete test comments."
  (princ "\n── CLEANUP ──\n")
  ;; Delete all comments we created
  (dolist (id e2e-comment-ids)
    (princ (format "  Deleting comment %s... " id))
    (condition-case err
        (progn (e2e-delete-comment id) (princ "ok\n"))
      (error (princ (format "FAILED: %s\n" err)))))
  ;; Reset title and description (Jira ignores empty string, so use a space)
  (princ "  Resetting title & clearing description... ")
  (if (e2e-update-fields e2e-clean-title " ")
      (princ "ok\n")
    (princ "FAILED\n")))

;; ── test content ───────────────────────────────────────────────────

(defvar e2e-test-description
  "h2. Overview

This is a test with my_variable and another_var and file_name.txt.

Some *bold text* and _italic text_ and also underscored_words.

h3. Code Block with Underscores

{code:python}
def my_function(param_name):
    result_value = process_data(param_name)
    return result_value
{code}

h3. JSON Block

{code:json}
{
  \"completionState\": \"FAILED\",
  \"datasetStatuses\": {
    \"48728\": {
      \"extractionEndMode\": \"fullLoadEnd\",
      \"completionState\": \"COMPLETED\"
    }
  }
}
{code}

h3. Lists

* Item with my_variable reference
* Another item with file_name.txt
* Code inline: {{my_function()}}

# Numbered one
# Numbered two

h3. Special Cases

Chemical formula: H~2~O
Variable name: water_volume
Math: x^2^
Variable: max_value")

(defvar e2e-test-title "E2E Test: my_variable title with *bold* and _italic_")

(defvar e2e-comment-1
  "This comment has my_variable and code:

{code:python}
def comment_func(arg_one):
    return arg_one
{code}

And a list:
* bullet_one
* bullet_two")

(defvar e2e-comment-edit-1
  "EDITED comment has updated_variable and new code:

{code:python}
def edited_func(new_arg):
    return new_arg
{code}

Changed list:
* edited_bullet_one
* edited_bullet_two")

;; ── main ───────────────────────────────────────────────────────────

(princ "\n══════════════════════════════════════════\n")
(princ " E2E ROUND-TRIP TEST: SAC-30264\n")
(princ " Title + Description + Comments\n")
(princ "══════════════════════════════════════════\n")

;; ===================================================================
;; PHASE 1 — TITLE + DESCRIPTION
;; ===================================================================
(princ "\n── PHASE 1: Title + Description ──\n\n")

(princ "1a) Push test title + description\n")
(unless (e2e-update-fields e2e-test-title e2e-test-description)
  (error "FATAL: could not set initial content"))
(sleep-for 2)

(princ "1b) Fetch back from Jira\n")
(let* ((data (e2e-fetch))
       (fetched-title (plist-get data :summary))
       (fetched-desc  (plist-get data :description)))

  (princ "1c) Jira→Org→Jira round-trip\n")
  (let* ((org-desc  (go-jira-markup-to-org fetched-desc))
         (jira-desc (go-jira-markup-from-org org-desc)))

    (princ "1d) Submit round-tripped content\n")
    ;; Title passes through as-is (no Pandoc), description is converted
    (unless (e2e-update-fields fetched-title jira-desc)
      (error "FATAL: could not submit round-tripped content"))
    (sleep-for 2)

    (princ "1e) Fetch final & verify\n\n")
    (let* ((final (e2e-fetch))
           (f-title (plist-get final :summary))
           (f-desc  (plist-get final :description)))

      (princ "[Title]\n")
      (e2e-check "title preserved" (string= fetched-title f-title))
      (e2e-check "title has my_variable" (string-match-p "my_variable" f-title))
      (e2e-check "title has *bold*" (string-match-p "\\*bold\\*" f-title))
      (e2e-check "title has _italic_" (string-match-p "_italic_" f-title))

      (princ "\n[Underscores in description]\n")
      (e2e-check "my_variable" (string-match-p "my_variable" f-desc))
      (e2e-check "another_var" (string-match-p "another_var" f-desc))
      (e2e-check "file_name" (string-match-p "file_name" f-desc))
      (e2e-check "underscored_words" (string-match-p "underscored_words" f-desc))
      (e2e-check "water_volume" (string-match-p "water_volume" f-desc))
      (e2e-check "max_value" (string-match-p "max_value" f-desc))
      (e2e-check "no {~} subscript" (not (string-match-p "{~}" f-desc)))

      (princ "\n[Code blocks]\n")
      (e2e-check "{code:python}" (string-match-p "{code:python}" f-desc))
      (e2e-check "{code:json}" (string-match-p "{code:json}" f-desc))
      (e2e-check "my_function in code" (string-match-p "my_function" f-desc))
      (e2e-check "param_name in code" (string-match-p "param_name" f-desc))
      (e2e-check "result_value in code" (string-match-p "result_value" f-desc))
      (e2e-check "process_data in code" (string-match-p "process_data" f-desc))
      (e2e-check "no NOLANG" (not (string-match-p "NOLANG" f-desc)))

      (princ "\n[JSON block]\n")
      (e2e-check "completionState" (string-match-p "completionState" f-desc))
      (e2e-check "datasetStatuses" (string-match-p "datasetStatuses" f-desc))
      (e2e-check "extractionEndMode" (string-match-p "extractionEndMode" f-desc))
      (e2e-check "no {code:json}{" (not (string-match-p "{code:json}{" f-desc)))
      (e2e-check "no \\\\" (not (string-match-p "\\\\\\\\" f-desc)))

      (princ "\n[Formatting]\n")
      (e2e-check "*bold text*" (string-match-p "\\*bold text\\*" f-desc))
      (e2e-check "_italic text_" (string-match-p "_italic text_" f-desc))
      (e2e-check "h2. heading" (string-match-p "h2\\." f-desc))
      (e2e-check "h3. heading" (string-match-p "h3\\." f-desc))

      (princ "\n[Lists]\n")
      (e2e-check "bullet list" (string-match-p "^\\* " f-desc))
      (e2e-check "numbered list" (string-match-p "^# " f-desc))
      (e2e-check "{{my_function()}}" (string-match-p "{{my_function()}}" f-desc))

      (princ "\n[Sub/Superscript]\n")
      (e2e-check "H~2~O" (string-match-p "H~2~O" f-desc))
      (e2e-check "x^2^" (string-match-p "x\\^2\\^" f-desc))

      (princ "\n[Idempotency]\n")
      (let* ((org2  (go-jira-markup-to-org f-desc))
             (jira2 (go-jira-markup-from-org org2)))
        (e2e-check "2nd round-trip == 1st" (string= (string-trim jira-desc) (string-trim jira2)))))))

;; ===================================================================
;; PHASE 2 — ADD COMMENT
;; ===================================================================
(princ "\n── PHASE 2: Add Comment ──\n\n")

(princ "2a) Add comment with underscores + code block\n")
(unless (e2e-add-comment e2e-comment-1)
  (error "FATAL: could not add comment"))
(sleep-for 2)

(princ "2b) Fetch & verify comment\n")
(let* ((data (e2e-fetch))
       (comments (plist-get data :comments))
       (c1 (car (last comments)))  ;; newest comment
       (c1-id (plist-get c1 :id))
       (c1-body (plist-get c1 :body)))

  ;; Track for cleanup
  (push c1-id e2e-comment-ids)

  (princ (format "    Comment ID: %s  (%d chars)\n\n" c1-id (length c1-body)))

  (princ "[Comment content]\n")
  (e2e-check "comment has my_variable" (string-match-p "my_variable" c1-body))
  (e2e-check "comment has comment_func" (string-match-p "comment_func" c1-body))
  (e2e-check "comment has arg_one" (string-match-p "arg_one" c1-body))
  (e2e-check "comment has {code:python}" (string-match-p "{code:python}" c1-body))
  (e2e-check "comment has bullet_one" (string-match-p "bullet_one" c1-body))
  (e2e-check "comment has bullet_two" (string-match-p "bullet_two" c1-body))

  ;; ===================================================================
  ;; PHASE 3 — ROUND-TRIP COMMENT (Jira→Org→Jira)
  ;; ===================================================================
  (princ "\n── PHASE 3: Round-trip Comment ──\n\n")

  (princ "3a) Jira→Org→Jira on comment body\n")
  (let* ((org-comment  (go-jira-markup-to-org c1-body))
         (jira-comment (go-jira-markup-from-org org-comment)))

    (princ "3b) Submit round-tripped comment via edit\n")
    (unless (e2e-edit-comment c1-id jira-comment)
      (error "FATAL: could not edit comment"))
    (sleep-for 2)

    (princ "3c) Fetch & verify round-tripped comment\n\n")
    (let* ((data2 (e2e-fetch))
           (comments2 (plist-get data2 :comments))
           (c1-rt (cl-find-if (lambda (c) (string= (plist-get c :id) c1-id)) comments2))
           (c1-rt-body (plist-get c1-rt :body)))

      (princ "[Comment after round-trip]\n")
      (e2e-check "my_variable" (string-match-p "my_variable" c1-rt-body))
      (e2e-check "comment_func" (string-match-p "comment_func" c1-rt-body))
      (e2e-check "arg_one" (string-match-p "arg_one" c1-rt-body))
      (e2e-check "{code:python}" (string-match-p "{code:python}" c1-rt-body))
      (e2e-check "bullet_one" (string-match-p "bullet_one" c1-rt-body))
      (e2e-check "no {~}" (not (string-match-p "{~}" c1-rt-body)))
      (e2e-check "no NOLANG" (not (string-match-p "NOLANG" c1-rt-body)))

      ;; ===================================================================
      ;; PHASE 4 — EDIT COMMENT (replace content entirely)
      ;; ===================================================================
      (princ "\n── PHASE 4: Edit Comment with New Content ──\n\n")

      (princ "4a) Org→Jira on NEW comment content\n")
      ;; Simulate user writing new Org content and converting to Jira
      (let* ((new-org "EDITED comment has updated_variable and new code:\n\n#+begin_src python\ndef edited_func(new_arg):\n    return new_arg\n#+end_src\n\nChanged list:\n- edited_bullet_one\n- edited_bullet_two")
             (new-jira (go-jira-markup-from-org new-org)))

        (princ "4b) Submit edited comment\n")
        (unless (e2e-edit-comment c1-id new-jira)
          (error "FATAL: could not edit comment with new content"))
        (sleep-for 2)

        (princ "4c) Fetch & verify edited comment\n\n")
        (let* ((data3 (e2e-fetch))
               (comments3 (plist-get data3 :comments))
               (c1-ed (cl-find-if (lambda (c) (string= (plist-get c :id) c1-id)) comments3))
               (c1-ed-body (plist-get c1-ed :body)))

          (princ "[Edited comment]\n")
          (e2e-check "updated_variable" (string-match-p "updated_variable" c1-ed-body))
          (e2e-check "edited_func" (string-match-p "edited_func" c1-ed-body))
          (e2e-check "new_arg" (string-match-p "new_arg" c1-ed-body))
          (e2e-check "{code:python}" (string-match-p "{code:python}" c1-ed-body))
          (e2e-check "edited_bullet_one" (string-match-p "edited_bullet_one" c1-ed-body))
          (e2e-check "edited_bullet_two" (string-match-p "edited_bullet_two" c1-ed-body))
          (e2e-check "no {~}" (not (string-match-p "{~}" c1-ed-body)))
          (e2e-check "no NOLANG" (not (string-match-p "NOLANG" c1-ed-body)))

          ;; Round-trip the edited comment one more time
          (princ "\n[Edited comment idempotency]\n")
          (let* ((org-ed  (go-jira-markup-to-org c1-ed-body))
                 (jira-ed (go-jira-markup-from-org org-ed)))
            (e2e-check "2nd round-trip stable" (string= (string-trim new-jira) (string-trim jira-ed)))))))))

;; ===================================================================
;; PHASE 5 — COMPACT CODE BLOCKS (SAC-30260 pattern)
;; ===================================================================
;; Real Jira tickets sometimes have code blocks with NO newline between
;; the tag and content: {code:json}{"key":"val"}{code}.
;; This is the exact pattern from SAC-30260 that broke viewing.
;; Push it as a comment, fetch, round-trip, verify nothing is garbled.

(princ "\n── PHASE 5: Compact Code Blocks (no newlines) ──\n\n")

(let* ((compact-comment
        (concat "Compact code blocks from SAC-30260:\n\n"
                ;; JSON with no newlines around tags
                "{code:json}{\"completionState\": \"FAILED\", \"exitCode\": 1}{code}\n\n"
                "Normal paragraph between blocks.\n\n"
                ;; Python with no newline after opener
                "{code:python}def my_func(exit_status):\n    return exit_status{code}\n\n"
                ;; Plain {code} with no newlines
                "{code}plain block no lang{code}\n\n"
                ;; Normal code block (with newlines) to make sure we don't break those
                "{code:bash}\necho \"normal_block\"\n{code}\n\n"
                "End of test with my_variable.")))

  (princ "5a) Add comment with compact code blocks\n")
  (unless (e2e-add-comment compact-comment)
    (error "FATAL: could not add compact code block comment"))
  (sleep-for 2)

  (princ "5b) Fetch & verify raw content\n\n")
  (let* ((data (e2e-fetch))
         (comments (plist-get data :comments))
         (c (car (last comments)))
         (c-id (plist-get c :id))
         (c-body (plist-get c :body)))

    (push c-id e2e-comment-ids)
    (princ (format "    Comment ID: %s  (%d chars)\n\n" c-id (length c-body)))

    (princ "[Raw content present]\n")
    (e2e-check "completionState" (string-match-p "completionState" c-body))
    (e2e-check "exitCode" (string-match-p "exitCode" c-body))
    (e2e-check "my_func" (string-match-p "my_func" c-body))
    (e2e-check "exit_status" (string-match-p "exit_status" c-body))
    (e2e-check "plain block" (string-match-p "plain block" c-body))
    (e2e-check "normal_block" (string-match-p "normal_block" c-body))
    (e2e-check "my_variable" (string-match-p "my_variable" c-body))

    ;; Round-trip through Org and back
    (princ "\n5c) Jira→Org→Jira round-trip\n")
    (let* ((org-text  (go-jira-markup-to-org c-body))
           (jira-text (go-jira-markup-from-org org-text)))

      (princ "\n[Org intermediate - sanity]\n")
      (e2e-check "org has begin_src" (string-match-p "#\\+begin_src" org-text))
      (e2e-check "org has completionState" (string-match-p "completionState" org-text))
      (e2e-check "org has my_func" (string-match-p "my_func" org-text))
      (e2e-check "org has no {code" (not (string-match-p "{code" org-text)))

      (princ "\n[Round-tripped Jira content]\n")
      (e2e-check "completionState" (string-match-p "completionState" jira-text))
      (e2e-check "exitCode" (string-match-p "exitCode" jira-text))
      (e2e-check "my_func" (string-match-p "my_func" jira-text))
      (e2e-check "exit_status" (string-match-p "exit_status" jira-text))
      (e2e-check "plain block" (string-match-p "plain block" jira-text))
      (e2e-check "normal_block" (string-match-p "normal_block" jira-text))
      (e2e-check "my_variable" (string-match-p "my_variable" jira-text))
      (e2e-check "no NOLANG" (not (string-match-p "NOLANG" jira-text)))
      (e2e-check "no {~}" (not (string-match-p "{~}" jira-text)))
      (e2e-check "no \\\\n" (not (string-match-p "\\\\n" jira-text)))
      (e2e-check "no garbled {code" (not (string-match-p "{code:NOLANG}" jira-text)))
      (e2e-check "has {code:json}" (string-match-p "{code:json}" jira-text))
      (e2e-check "has {code:python}" (string-match-p "{code:python}" jira-text))
      (e2e-check "has {code:bash}" (string-match-p "{code:bash}" jira-text))

      ;; Push the round-tripped content back and verify it survives
      (princ "\n5d) Submit round-tripped content\n")
      (unless (e2e-edit-comment c-id jira-text)
        (error "FATAL: could not submit round-tripped compact code block comment"))
      (sleep-for 2)

      (princ "5e) Fetch final & verify\n\n")
      (let* ((data2 (e2e-fetch))
             (comments2 (plist-get data2 :comments))
             (c-final (cl-find-if (lambda (c) (string= (plist-get c :id) c-id)) comments2))
             (c-final-body (plist-get c-final :body)))

        (princ "[Final content after push+fetch]\n")
        (e2e-check "completionState" (string-match-p "completionState" c-final-body))
        (e2e-check "my_func" (string-match-p "my_func" c-final-body))
        (e2e-check "exit_status" (string-match-p "exit_status" c-final-body))
        (e2e-check "plain block" (string-match-p "plain block" c-final-body))
        (e2e-check "normal_block" (string-match-p "normal_block" c-final-body))
        (e2e-check "my_variable" (string-match-p "my_variable" c-final-body))
        (e2e-check "no NOLANG" (not (string-match-p "NOLANG" c-final-body)))
        (e2e-check "no {~}" (not (string-match-p "{~}" c-final-body)))

        (princ "\n[Idempotency]\n")
        (let* ((org2  (go-jira-markup-to-org c-final-body))
               (jira2 (go-jira-markup-from-org org2)))
          (e2e-check "2nd round-trip stable" (string= (string-trim jira-text) (string-trim jira2))))))))

;; ===================================================================
;; RESULT
;; ===================================================================
(princ (format "\n══════════════════════════════════════════\n"))
(princ (format " RESULT: %d/%d PASSED\n" (- e2e-total e2e-fails) e2e-total))
(princ (format "══════════════════════════════════════════\n"))

(if (> e2e-fails 0)
    (progn
      (princ (format "\n!!! %d TESTS FAILED — keeping content for inspection !!!\n" e2e-fails))
      (kill-emacs 1))
  ;; All passed — clean up unless debug
  (if go-jira-debug
      (princ "\ngo-jira-debug is t — keeping test content on ticket.\n")
    (e2e-cleanup))
  (princ "\nDone.\n"))
