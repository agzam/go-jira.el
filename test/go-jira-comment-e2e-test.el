;; go-jira-comment-e2e-test.el --- Comment identity round-trip against real Jira -*- lexical-binding: t; -*-
;;
;; Drives the whole buffer flow - render, draft, submit, edit, submit - and
;; checks Jira afterwards.  The failure this guards against is a second copy
;; of a comment appearing where the first should have been amended.
;;
;; Run:  make e2e-comments
;;
;; Test comments are deleted on the way out, pass or fail.

(require 'go-jira)
(require 'seq)
(require 'cl-lib)

(defvar go-jira-comment-e2e-ticket "SAC-30264")
(defvar go-jira-comment-e2e-fails 0)
(defvar go-jira-comment-e2e-created '())
(defvar go-jira-comment-e2e-baseline nil)

(defun go-jira-comment-e2e-check (name ok)
  (if ok
      (princ (format "  ✅ %s\n" name))
    (setq go-jira-comment-e2e-fails (1+ go-jira-comment-e2e-fails))
    (princ (format "  ❌ FAIL: %s\n" name))))

(defun go-jira-comment-e2e-comments ()
  (gethash 'comments
           (go-jira-comment--request
            "GET" (format "rest/api/2/issue/%s/comment" go-jira-comment-e2e-ticket))))

(defun go-jira-comment-e2e-ids ()
  (mapcar (lambda (c) (format "%s" (gethash 'id c))) (go-jira-comment-e2e-comments)))

(defun go-jira-comment-e2e-body (id)
  (when-let* ((c (car (seq-filter
                       (lambda (c) (equal id (format "%s" (gethash 'id c))))
                       (go-jira-comment-e2e-comments)))))
    (gethash 'body c)))

(defun go-jira-comment-e2e-parent (id)
  (when-let* ((c (car (seq-filter
                       (lambda (c) (equal id (format "%s" (gethash 'id c))))
                       (go-jira-comment-e2e-comments)))))
    (when (gethash 'parentId c) (format "%s" (gethash 'parentId c)))))

(defun go-jira-comment-e2e-render ()
  "Render the ticket without disturbing window layout, return its buffer."
  (let ((go-jira-display-images nil))
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'get-buffer-window) #'ignore))
      (go-jira-view-ticket go-jira-comment-e2e-ticket)))
  (get-buffer (format "*Jira: %s*" go-jira-comment-e2e-ticket)))

(defun go-jira-comment-e2e-cleanup ()
  (princ "\n── CLEANUP ──\n")
  (dolist (id (reverse go-jira-comment-e2e-created))
    (princ (format "  Deleting comment %s... " id))
    (condition-case err
        (progn
          (go-jira-comment--request
           "DELETE" (format "rest/api/2/issue/%s/comment/%s"
                            go-jira-comment-e2e-ticket id))
          (princ "ok\n"))
      (error (princ (format "FAILED: %s\n" err))))))

(princ "\n══════════════════════════════════════════\n")
(princ " E2E: COMMENT IDENTITY AND REPLIES\n")
(princ "══════════════════════════════════════════\n")

(setq go-jira-comment-e2e-baseline (go-jira-comment-e2e-ids))
(princ (format "\nBaseline comments: %S\n" go-jira-comment-e2e-baseline))

(unwind-protect
    (progn
      ;; ── PHASE 1: add ────────────────────────────────────────────
      (princ "\n── PHASE 1: Add a comment ──\n\n")
      (with-current-buffer (go-jira-comment-e2e-render)
        (go-jira-comment-add)
        (insert "gj e2e first body")
        (go-jira-edit-submit)
        (let ((new (seq-difference (go-jira-comment-e2e-ids)
                                   go-jira-comment-e2e-baseline)))
          (go-jira-comment-e2e-check "exactly one comment added" (= 1 (length new)))
          (setq go-jira-comment-e2e-created (append go-jira-comment-e2e-created new))
          (go-jira-comment-e2e-check
           "body reached Jira"
           (equal "gj e2e first body" (go-jira-comment-e2e-body (car new))))
          ;; Without this the next edit posts a copy instead of amending.
          (go-jira-comment-e2e-check
           "Jira's comment ID was recorded in the buffer"
           (save-excursion
             (goto-char (point-min))
             (and (re-search-forward "New comment" nil t)
                  (equal (car new) (go-jira-comment-id-at)))))))

      ;; ── PHASE 2: edit, the reported bug ─────────────────────────
      (princ "\n── PHASE 2: Edit that comment ──\n\n")
      (with-current-buffer (go-jira-comment-e2e-render)
        (goto-char (point-min))
        (re-search-forward "gj e2e first body")
        (go-jira-edit)
        (goto-char (point-min))
        (re-search-forward "gj e2e first body")
        (replace-match "gj e2e first body EDITED" t t)
        (go-jira-edit-submit)
        (go-jira-comment-e2e-check
         "no duplicate created"
         (= (length (go-jira-comment-e2e-ids))
            (1+ (length go-jira-comment-e2e-baseline))))
        (go-jira-comment-e2e-check
         "the original was amended"
         (equal "gj e2e first body EDITED"
                (go-jira-comment-e2e-body (car go-jira-comment-e2e-created)))))

      ;; ── PHASE 3: reply ──────────────────────────────────────────
      (princ "\n── PHASE 3: Reply to it ──\n\n")
      (with-current-buffer (go-jira-comment-e2e-render)
        (goto-char (point-min))
        (re-search-forward "gj e2e first body EDITED")
        (go-jira-comment-reply)
        (insert "gj e2e reply body")
        (go-jira-edit-submit)
        (let ((new (seq-difference (go-jira-comment-e2e-ids)
                                   (append go-jira-comment-e2e-baseline
                                           go-jira-comment-e2e-created))))
          (go-jira-comment-e2e-check "exactly one reply added" (= 1 (length new)))
          (when new
            (go-jira-comment-e2e-check
             "reply carries the right parentId"
             (equal (go-jira-comment-e2e-parent (car new))
                    (car go-jira-comment-e2e-created)))
            ;; Org counts a property drawer as heading contents, so an
            ;; unguarded body read ships the drawer to Jira as prose.
            (go-jira-comment-e2e-check
             "reply body has no drawer text"
             (not (string-match-p ":PROPERTIES:\\|JIRA_"
                                  (go-jira-comment-e2e-body (car new)))))
            (setq go-jira-comment-e2e-created
                  (append go-jira-comment-e2e-created new)))))

      ;; ── PHASE 4: heading inside a comment body ──────────────────
      (princ "\n── PHASE 4: A heading inside a comment body ──\n\n")
      (with-current-buffer (go-jira-comment-e2e-render)
        (goto-char (point-min))
        (re-search-forward "gj e2e first body EDITED")
        (go-jira-edit)
        (goto-char (point-min))
        (re-search-forward "gj e2e first body EDITED")
        (replace-match
         "gj e2e first body EDITED\n**** A heading in the body\nunder it" t t)
        (go-jira-edit-submit)
        ;; A sub-heading without the parent property is body text.  It used
        ;; to be posted a second time as a headless reply.
        (go-jira-comment-e2e-check
         "no phantom reply created"
         (= (length (go-jira-comment-e2e-ids))
            (+ 2 (length go-jira-comment-e2e-baseline))))
        (go-jira-comment-e2e-check
         "the heading landed in the body"
         (string-match-p "A heading in the body"
                         (go-jira-comment-e2e-body
                          (car go-jira-comment-e2e-created))))))
  (go-jira-comment-e2e-cleanup))

(go-jira-comment-e2e-check
 "ticket back to baseline"
 (equal (sort (go-jira-comment-e2e-ids) #'string<)
        (sort (copy-sequence go-jira-comment-e2e-baseline) #'string<)))

(princ "\n══════════════════════════════════════════\n")
(princ (format " RESULT: %s\n"
               (if (zerop go-jira-comment-e2e-fails)
                   "ALL PASSED"
                 (format "%d FAILED" go-jira-comment-e2e-fails))))
(princ "══════════════════════════════════════════\n")

(kill-emacs (if (zerop go-jira-comment-e2e-fails) 0 1))
