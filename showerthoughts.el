;;; showerthoughts.el --- fortune file creation -*- lexical-binding: t; -*-

;;; Commentary:

;; Download the reddit submission archive here:

;;   http://files.pushshift.io/reddit/

;; Gather up all the /r/Showerthoughts posts using jq:

;;  $ cat *.bz2 | \
;;      lbunzip2 | \
;;      grep -a Showerthoughts | \
;;      jq -c 'select(.subreddit == "Showerthoughts") |
;;               {title, score, author, created_utc}' \
;;      > showerthoughts.json

;; The "grep -a" greatly speeds up the process by reducing the number
;; of JSON objects for jq to parse. The second part of the jq filter
;; trims it to just the relevant fields, reducing Emacs' workload.

;; Next run it through Emacs to turn it into a file formatted for
;; fortune:

;;  $ emacs -Q -batch -l showerthoughts.elc -f showerthoughts-batch \
;;           showerthoughts.json showerthoughts

;; Finally use strfile to file an index (showerthoughts.dat):

;;  $ strfile showerthoughts

;; Now it's ready for use:

;;  $ fortune showerthoughts

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'avl-tree)

(defun showerthoughts-< (a b)
  "Return non-nil if thought A is preferred to thought B."
  (let ((score-a (car a))
        (score-b (car b)))
    ;; Prefer higher scores
    (if (= score-a score-b)
        ;; Prefer earlier submissions
        (let ((utc-a (cl-cadddr a))
              (utc-b (cl-cadddr b)))
          ;; Otherwise sort by title
          (if (= utc-a utc-b)
              (let ((title-a (cadr a))
                    (title-b (cadr b)))
                (string< title-a title-b))
            (< utc-a utc-b)))
      (> score-a score-b))))

(cl-defun showerthoughts-gather (file &optional (limit 10000))
  "Return the top LIMIT thoughts from FILE."
  (with-temp-buffer
    (let ((json-object-type 'plist)
          (count 0)
          (thought nil)
          (thoughts (avl-tree-create #'showerthoughts-<)))
      (insert-file-contents file)
      (while (setf thought (ignore-errors (json-read)))
        (let* ((score (plist-get thought :score))
               (title (plist-get thought :title))
               (author (plist-get thought :author))
               (utc (plist-get thought :created_utc))
               (candidate (list score title author
                                (if (stringp utc) 
                                    (string-to-number utc)
                                  utc))))
          (if (= count limit)
              (let ((last (avl-tree-last thoughts)))
                (unless (showerthoughts-< last candidate)
                  (avl-tree-enter thoughts candidate)
                  (avl-tree-delete thoughts last)))
            (avl-tree-enter thoughts candidate)
            (cl-incf count))))
      (avl-tree-flatten thoughts))))

(defun showerthoughts-write (thought)
  "Write THOUGHT to buffer, paragraph-filled and in fortune format."
  (cl-destructuring-bind (_ title author utc) thought
    (let ((buffer (current-buffer))
          (date (format-time-string "%b %Y" (seconds-to-time utc) t)))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer))
              (fill-column 78))
          (insert title)
          (fill-paragraph)
          (with-current-buffer buffer
            (insert-buffer-substring temp-buffer))))
      (insert (format "\n\t―%s, %s\n%%\n" author date)))))

(defun showerthoughts-batch ()
  "Read from first unprocessed arg, write to second unprocessed arg."
  (let ((input (pop command-line-args-left))
        (output (pop command-line-args-left)))
    (with-temp-file output
      (mapc #'showerthoughts-write
            (showerthoughts-gather input)))))

(provide 'showerthoughts)

;;; showerthoughts.el ends here
