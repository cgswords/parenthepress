;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| TEXT POSTS                                                           |-+
;; +-| TYPE TAG: text                                                       |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| POST TAGS                                                            |-+
;; +-| - type                                                               |-+
;; +-| - date                                                               |-+
;; +-| - title                                                              |-+
;; +-| - text                                                               |-+
;; +-| - tags                                                               |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| Plain vanilla text posts here. No, really...                         |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

(define html-break
    (string->list "\n<br />\n"))

(define run-markdown
  (lambda (str)
    (let ((str (write-file "tmp.text" str)))
      (system "perl Markdown.pl tmp.text > out")
      (list->string (parse-file-as-string "out")))))

(define make-title-and-date
  (lambda (title date)
    (string-append
      "<div align=\"left\">\n"
      (make-title title)
      (parse-date date)
      "</div>\n\n<br />\n\n")))

(define parse-text-post
  (lambda (post) 
    (let* ((date (extract-date post))
           (text (list->string 
                   (remove-leading-symbols 
                     (get-tag 'text post)
                     '(#\( #\newline #\space))))
           (text (run-markdown text))
           (title (extract-title post)))
      (string-append
        (make-title-and-date title date)
        (make-content-div text)
        ))))

(set! post-types (cons `(text . ,parse-text-post) post-types))

