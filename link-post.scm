;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| LINK POSTS                                                           |-+
;; +-| TYPE TAG: link                                                       |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| POST TAGS                                                            |-+
;; +-| - type                                                               |-+
;; +-| - date                                                               |-+
;; +-| - link                                                               |-+
;; +-| - title                                                              |-+
;; +-| - desc                                                               |-+
;; +-| - tags                                                               |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| For posting images as the body of the post with nothing else; the    |-+
;; +-| title and date are included as one might expect.                     |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

(define parse-link-post
  (lambda (post) 
    (let* ((date  (extract-date post))
           (title (extract-title post))
           (link  (extract-tag 'link post))
           (desc  (extract-tag 'desc post))
           (tags  (extract-tags post)))
      (string-append
        (make-content-div (link-body title link desc))
        "<div align=\"left\">"
        (parse-date date)
        "</div>"
        ))))

(define link-body
  (lambda (title link desc)
    (string-append "<u><a href=\"" link "\">" (make-title title) "</a></u>\n\n"
        "<i>"
        desc
        "</i>\n\n")))

(set! post-types (cons `(link . ,parse-link-post) post-types))
