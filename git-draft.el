
(require 'rx)
(require 's)
(require 'dash)

(defun git-draft/diff ()
  "Diff the git index and parse the result.

Returns an ordered (as found in the source diff) list of diff
elements which are p-lists beginning with either `:file' or
`:hunk'.

`:file' elements contain a `:from' indicating the from file and a
`:to' indicating the to file.

`:hunk' elements contain a `:from-start', `:from-count',
`:to-start' and an optional `:to-count' integer."
  (reverse
   (let (result)
     (with-temp-buffer
       (let ((pager (getenv "GIT_PAGER")))
         (setenv "GIT_PAGER" "")
         (unwind-protect
              (shell-command "git diff -U --cached ." (current-buffer))
           (when pager 
             (setenv "GIT_PAGER" pager))))
       (goto-char (point-min))
       (while (re-search-forward 
               (rx (or (and line-start ; diff start
                            (group-n 1 "diff --git ") 
                            "a/" (group-n 2 (1+ (any "a-zA-Z0-9._-"))) " "
                            "b/" (group-n 3 (1+ (any "a-zA-Z0-9._-")))
                            line-end)
                       (and line-start  ; hunk start
                            (group-n 1 "@@ -")
                            (group-n 2 (1+ (any "0-9")))
                            "," (group-n 3 (1+ (any "0-9")))
                            " +" (group-n 4 (1+ (any "0-9")))
                            (* (and "," (group-n 5 (1+ (any "0-9")))))
                            " @@"  (* not-newline) line-end)))
               nil t)
         (if (equal (match-string 1) "@@ -")
             (push (list :hunk
                         :from-start (string-to-int (match-string 2))
                         :from-count (string-to-int (match-string 3))
                         :to-start (string-to-int (match-string 4))
                         :to-count (string-to-int (match-string 5)))  result)
             ;; Else it's a diff start
             (push (list :file
                         :from (match-string 2)
                         :to (match-string 3)) result)))
       result))))

(defun git-draft/current-defun ()
  "Maybe an alternative to `add-log-current-defun'.

This is very simple, it just looks for things with a face
indicator of defn-ness."
  (beginning-of-defun)
  (while (not (memq (get-text-property (point) 'face)
                    '(font-lock-variable-name-face
                      font-lock-function-name-face)))
    (goto-char
     (next-single-property-change
      (point) 'face nil (line-end-position))))
  (buffer-substring-no-properties
   (point)
   (next-single-property-change
    (point) 'face nil (line-end-position))))

(defun git-draft/make-draft ()
  "Make a draft git commit message."
  (let ((buf (current-buffer))
        result)
    (unwind-protect
         (let ((lst (git-draft/diff)))
           (dolist (e lst)
             (if (equal (car e) :file)
                 (set-buffer (find-file-noselect (plist-get (cdr e) :from)))
                 ;; Else it's a hunk
                 (save-excursion
                   (goto-line 
                    (+ (plist-get (cdr e) :from-start)
                       (or (plist-get (cdr e) :from-count) 0)))
                   (push (git-draft/current-defun) result)))))
      (set-buffer buf))
    (reverse result)))

(defun git-draft ()
  "Insert a draft commit message into the current-buffer.

Presumably the current buffer is a commit buffer."
  (interactive)
  (insert
   (s-join "\n" (--map (concat it ": ") (git-draft/make-draft)))))

;; git-draft.el ends here
