;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| IMAGE POSTS                                                          |-+
;; +-| TYPE TAG: picture                                                    |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| POST TAGS                                                            |-+
;; +-| - type                                                               |-+
;; +-| - date                                                               |-+
;; +-| - image                                                              |-+
;; +-| - tags                                                               |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| For posting images as the body of the post with nothing else; the    |-+
;; +-| title and date are included as one might expect.                     |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

(define parse-image-post
  (lambda (post) 
    (let* ((date (extract-date post))
           (image (list->string (cdr (assq 'image post)))))
      (string-append
        (make-content-div (picture-body image))
        "<div align=\"left\">"
        (parse-date date)
        "</div>"
        ))))

(define picture-body
  (lambda (img)
    (string-append 
        "<div id=\"image\"><a href=\"img/" 
        img
        "\"> <img src=\"img/"
        img
        "\" /></a></div>")))

(set! post-types (cons `(picture . ,parse-image-post) post-types))
