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

(define parse-text-post
  (lambda (post) 
    (let* ((date (extract-date post))
           (text (list->string 
                   (replace 
                     (remove-leading-symbols 
                       (get-tag 'text post)
                       '(#\( #\newline #\space))
                     #\newline
                     html-break)))
           (title (extract-title post))
           (tags  (extract-tags post)))
      (string-append
        (make-title-and-date title date)
        (make-content-div text)
        ))))

(set! post-types (cons `(text . ,parse-text-post) post-types))

