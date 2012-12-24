
;; Write a file, overwriting it if it exists.
(define write-file
  (lambda (file out)
    (printf "Writing file for post: ~s~n" file)
    (if (file-exists? file) (delete-file file))
    (call-with-output-file file
      (lambda (p)
        (display out p)))
    (string-append "Successfully wrote " file)))

;; Reads a file in as a string (actually a list of chars).
(define parse-file-as-string
  (lambda (file)
    (with-input-from-file file
      (lambda ()
        (let f ((x (read-char)))
          (if (eof-object? x)
              '()
              (cons x (f (read-char)))))))))

(define read-file-reasonably
  (lambda (file)
    (map 
      (lambda (x) (cons (car x) (trim (cdr x)))) 
      (map convert-car-to-symbol 
        (split-on-parens 
          (parse-file-as-string file))))))

(define extract-page-tags
  (lambda (ls)
    (map convert-string-to-symbol (split-on #\space (get-tag 'tags ls)))))

(define extract-date
  (lambda (ls)
    (let* ((date (get-tag 'date ls))
           (date (remove-leading-symbols date '(#\( #\newline #\space)))
           (date (split-on #\space date))
           (date (map list->string date))
           (date (map string->number date)))
      (apply make-date date))))

(define extract-title
  (lambda (ls)
    (let* ((title (get-tag 'title ls))
           (title (remove-leading-symbols title '(#\( #\newline #\space)))
           (title (list->string title)))
      title)))

(define extract-type
  (lambda (ls)
    (string->symbol (list->string (get-tag 'type ls)))))

(define get-tag
  (lambda (tag ls)
    (cdr (assq tag ls))))

(define extract-tag
  (lambda (tag ls)
    (let* ((ret (get-tag tag ls))
           (ret (remove-leading-symbols ret '(#\( #\newline #\space)))
           (ret (list->string ret)))
      ret)))
