;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| QUOTE POSTS                                                          |-+
;; +-| TYPE TAG: quote                                                      |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| POST TAGS                                                            |-+
;; +-| - type                                                               |-+
;; +-| - date                                                               |-+
;; +-| - quote                                                              |-+
;; +-| - source                                                             |-+
;; +-| - tags                                                               |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| For posting quotes as the body of the post with nothing else; the    |-+
;; +-| title and date are included as one might expect.                     |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

(define quote-body
  (lambda (my-quote source)
      (string-append 
        "<div id=\"quote\">\n"
        my-quote
        "\n</div>\n"
        "&mdash;<i>"
        source
        "</i>\n\n")))

(trace-define parse-quote-post
  (lambda (post)
    (let ((my-quote (extract-tag 'quote post))
          (source (extract-tag 'source post))
          (date (extract-date post))
          (tags (extract-tags post)))
      (string-append
        (make-content-div (quote-body my-quote source))
        "<div align=\"left\">"
        (parse-date date)
        "</div>"
        ))))

(set! post-types (cons `(quote . ,parse-quote-post) post-types))
