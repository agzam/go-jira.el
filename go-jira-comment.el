;;; go-jira-comment.el --- Jira comment identity and rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comment identity for go-jira buffers.
;;
;; A comment's Jira ID and its parent live in an Org property drawer rather
;; than in the heading text.  The heading is prose the user is invited to
;; edit; a drawer is structured data Org keeps intact, which is what lets
;; `go-jira-edit-submit' amend a comment instead of posting a copy of it.
;;
;; The parent property is also the only signal that separates a reply from a
;; heading that happens to sit inside a comment body, since Jira comment
;; bodies may contain headings of their own.

;;; Code:

(require 'org)
(require 'json)

(declare-function go-jira--find-exe "go-jira")

(defconst go-jira-comment-id-property "JIRA_COMMENT_ID"
  "Org property holding a comment's Jira ID.")

(defconst go-jira-comment-parent-property "JIRA_PARENT_ID"
  "Org property holding the Jira ID of a reply's parent comment.")

(defconst go-jira-comment-draft-heading "New comment"
  "Heading text of a comment drafted in the buffer but not yet posted.")

(defun go-jira-comment--anchor-url (base-url comment-id)
  "Return the browse URL of COMMENT-ID under BASE-URL, or nil.
Nil when either argument is missing, in which case the heading carries no
link and the drawer is the only record of the comment's identity."
  (when (and base-url comment-id)
    (format (concat "%s?focusedCommentId=%s"
                    "&page=com.atlassian.jira.plugin.system.issuetabpanels"
                    ":comment-tabpanel#comment-%s")
            base-url comment-id comment-id)))

(defun go-jira-comment--timestamp (created)
  "Format Jira's CREATED string for display, or return nil.
Deliberately unbracketed: Org ends a link description at the first \"]]\",
so an inactive timestamp inside one truncates the description and leaves a
stray bracket in the heading."
  (when created
    (condition-case nil
        (format-time-string "%Y-%m-%d %a %H:%M" (date-to-time created))
      (error created))))

(defun go-jira-comment--blank-p (value)
  "Return non-nil when VALUE is nil or an empty string."
  (or (null value) (string-empty-p (format "%s" value))))

(defun go-jira-comment-insert-heading (level author created comment-id parent-id base-url)
  "Insert a comment heading at LEVEL, followed by its property drawer.
AUTHOR is a display name, CREATED a Jira timestamp string, COMMENT-ID the
comment's Jira ID, PARENT-ID the parent's ID for a reply, and BASE-URL the
issue's browse URL.  Returns the position the heading starts at."
  (let* ((start (point))
         (timestamp (go-jira-comment--timestamp created))
         (url (go-jira-comment--anchor-url base-url comment-id))
         (stamp (cond ((and url timestamp) (format "[[%s][%s]]" url timestamp))
                      (timestamp timestamp)
                      (t ""))))
    (insert (format "%s %s - %s\n" (make-string level ?*) (or author "Unknown") stamp))
    (go-jira-comment-insert-drawer comment-id parent-id)
    start))

(defun go-jira-comment-insert-drawer (comment-id parent-id)
  "Insert a property drawer for COMMENT-ID and PARENT-ID at point.
Blank values are omitted rather than written empty, so that a failed
parent lookup does not leave behind a heading that reads as a reply to
nothing."
  (let ((props (append
                (unless (go-jira-comment--blank-p comment-id)
                  (list (cons go-jira-comment-id-property comment-id)))
                (unless (go-jira-comment--blank-p parent-id)
                  (list (cons go-jira-comment-parent-property parent-id))))))
    (when props
      (insert ":PROPERTIES:\n")
      (dolist (prop props)
        (insert (format ":%s: %s\n" (car prop) (cdr prop))))
      (insert ":END:\n"))))

(defun go-jira-comment-id-at (&optional pos)
  "Return the Jira comment ID recorded at POS, or nil.
Falls back to the comment anchor in the heading text, so that buffers
rendered before comments carried a drawer still edit in place."
  (save-excursion
    (when pos (goto-char pos))
    (or (let ((id (org-entry-get (point) go-jira-comment-id-property)))
          (unless (go-jira-comment--blank-p id) id))
        (when (ignore-errors (org-back-to-heading t) t)
          (let ((heading (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
            (when (string-match "focusedCommentId=\\([0-9]+\\)" heading)
              (match-string 1 heading)))))))

(defun go-jira-comment-parent-id-at (&optional pos)
  "Return the Jira ID of the parent comment recorded at POS, or nil.
A heading carries this property only when a reply command wrote it, which
is what distinguishes a reply from a heading inside a comment body."
  (save-excursion
    (when pos (goto-char pos))
    (let ((id (org-entry-get (point) go-jira-comment-parent-property)))
      (unless (go-jira-comment--blank-p id) id))))

(defun go-jira-comment-body-start (element)
  "Return the position where the body of comment ELEMENT begins.
Org counts a property drawer as part of a heading's contents, so reading
from `:contents-begin' would send the drawer text to Jira as prose.
Anchors on the heading rather than on the contents, because
`org-end-of-meta-data' re-anchors to whatever heading point sits on, and
a comment whose body opens with a heading would otherwise lose it."
  (when (org-element-property :contents-begin element)
    (save-excursion
      (goto-char (org-element-property :begin element))
      (org-end-of-meta-data t)
      (point))))

;;; Drafting comments in the buffer

(defun go-jira-comment-insert-draft (level &optional parent-id)
  "Insert an empty comment draft at LEVEL and leave point in its body.
PARENT-ID makes the draft a reply.  The draft carries no comment ID,
which is what marks it as unsent."
  (unless (bolp) (insert "\n"))
  (insert (format "%s %s\n" (make-string level ?*) go-jira-comment-draft-heading))
  (go-jira-comment-insert-drawer nil parent-id)
  (let ((body (point)))
    (insert "\n\n")
    (goto-char body)))

(defun go-jira-comment-thread-root-id ()
  "Return the Jira ID of the comment thread point sits in, or nil.
Point inside a reply resolves to the reply's parent: Jira threads are one
level deep, so a reply to a reply joins the same thread."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t) t)
      (or (go-jira-comment-parent-id-at)
          (go-jira-comment-id-at)))))

(defun go-jira-comment-goto-id (id bound)
  "Move point to the heading whose comment ID is ID, searching up to BOUND.
Returns the position, or nil when no such heading exists.  Callers resolve
the ID before starting an edit session, which may narrow the buffer and
move point."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (re-search-forward org-heading-regexp bound t))
      (goto-char (line-beginning-position))
      (if (equal id (go-jira-comment-id-at))
          (setq found (point))
        (end-of-line)))
    found))

;;; Writing comments to Jira

;; The issue-edit payload can express adding and editing a comment, but not
;; replying to one, and it never reports the ID of what it created.  The
;; comment endpoints do both, so all three operations go through them.

(defun go-jira-comment--request (method endpoint &optional payload)
  "Send METHOD to Jira ENDPOINT with optional PAYLOAD.
Returns the parsed response, or nil when the response is not JSON.
Signals an error when the CLI fails or Jira reports one.  Failure is read
off the parsed response rather than matched in the raw text, which would
also fire on a comment body that merely mentions an error."
  (let ((j (go-jira--find-exe)))
    (with-temp-buffer
      (let* ((args (append (list "request" endpoint)
                           (when payload (list payload))
                           (list "--method" method)))
             (exit-code (apply #'call-process j nil (current-buffer) nil args))
             (result (buffer-string))
             (parsed (condition-case nil
                         (let ((json-object-type 'hash-table)
                               (json-key-type 'symbol)
                               (json-array-type 'list))
                           (json-read-from-string result))
                       (error nil))))
        (when (or (not (zerop exit-code))
                  (and (hash-table-p parsed)
                       (or (gethash 'errorMessages parsed)
                           (gethash 'errors parsed))))
          (error "Jira %s %s failed: %s" method endpoint (string-trim result)))
        parsed))))

(defun go-jira-comment-post (key body &optional parent-id)
  "Add BODY as a comment on issue KEY, replying to PARENT-ID when given.
Returns the Jira ID of the created comment."
  (let* ((payload (json-encode
                   (if (go-jira-comment--blank-p parent-id)
                       (list :body body)
                     (list :body body
                           :parentId (string-to-number (format "%s" parent-id))))))
         (response (go-jira-comment--request
                    "POST" (format "rest/api/2/issue/%s/comment" key) payload)))
    (when (hash-table-p response)
      (when-let* ((id (gethash 'id response)))
        (format "%s" id)))))

(defun go-jira-comment-put (key id body)
  "Replace the body of comment ID on issue KEY with BODY."
  (go-jira-comment--request
   "PUT" (format "rest/api/2/issue/%s/comment/%s" key id)
   (json-encode (list :body body)))
  t)

(defun go-jira-comment-record-id (marker id)
  "Write ID into the property drawer of the comment heading at MARKER.
A comment posted during an edit session carries no ID in the buffer until
it is recorded, and editing it again would post a second copy."
  (when (and marker id (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (org-entry-put (point) go-jira-comment-id-property (format "%s" id)))))))

(provide 'go-jira-comment)
;;; go-jira-comment.el ends here

;; Local Variables:
;; package-lint-main-file: "go-jira.el"
;; End:
