;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| Generation Code and Helpers. This is where new post types get loaded |-+
;; +-| too, so it may be worth knowing about! The variable post-types       |-+
;; +-| should be consed onto with a pair (type . parse-function) for each   |-+
;; +-| post type added. This list is used to deal with posts by type tag.   |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+
(load "file-io.scm")
(load "string-parsing.scm")

(define post-types '())
(define blog-title "Bit | Hacker")
(define banner
  (string-append
    "<div id=\"title\">\n"
    "<div id=\"titleinner\">\n"
    "<h1>BIT <b id=\"color\">|</b> HACKER</h1>\n"
    "The long road to the Ph.D.\n"
    "</div>\n"
    "</div>\n\n"))

(define blog-desc
  "Cameron Swords is a Ph.D.-track graduate student at Indiana University, keeping this blog to document his life as he pursues his degree. More academic information can be found <a href=\"http://www.cs.indiana.edu/~cswords/\">here</a>.")

(define posts-per-page 10)

(define make-navigation
  (lambda (ls)
    (cond
      [(null? ls) ""]
      [(null? (cdr ls))
        (string-append
          "<a href=\"" (cdar ls)
          "\" id=\"topnav\">" (caar ls) "</a>")]
      [else
        (string-append
          "<a href=\"" (cdar ls)
          "\" id=\"topnav\">" (caar ls) "</a>"
          " <i style=\"font-size: 28px; padding-left: 5px; padding-right: 5px;\">|</i> "
          (make-navigation (cdr ls)))])))

(define preamble
  (string-append
    "<html>\n"
    "<head>\n"
    "<title>"
    blog-title
    "</title>\n"
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" />\n"
    "</head>\n"
    "<body>\n"
    "<div align=\"center\">\n"
    banner
    "<div id=\"nav\">\n\n"
    (make-navigation '(("Latest" . "index.html") 
                       ("Academic" . "http://www.cs.indiana.edu/~cswords/")
                       ("Github" . "http://github.com/cgswords/")))
    "\n\n"
    "</div>"))

(define postamble
  (string-append
    "\n<br /><br />\n\n"
    "<div id=\"footer\">\n\n"
    blog-desc
    "\n<br /><br />\n</div>\n</div>\n</body>\n</html>"))

;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| Just some helper functions for general parsing and whatnot. They are |-+
;; +-| mostly self-explainatory, but any comments that I want later will be |-+
;; +-| added to this block here, explaining their usage and stuff.          |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| assqd                                                                |-+
;; +-|   This simply calls cdr on the result of an assq, and returns #f if  |-+
;; +-|   it cannot (as expected from assq). For compact code.               |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| substring?                                                           |-+
;; +-|   Returns #t if the first argument occurs in the second, else #f.    |-+
;; +-+----------------------------------------------------------------------+-+
;; +-| sublist?                                                             |-+
;; +-|   Returns #t if the first argument occurs in the second, else #f.    |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(memq (car ls1) ls2) (union (cdr ls1) ls2)]
      [else (cons (car ls1) (union (cdr ls1) ls2))])))

(define assqd
  (lambda (x ls)
    (let ((a (assq x ls)))
      (if a (cdr a) #f))))

(define substring?
  (lambda (pat str)
    (sublist? (string->list pat) (string->list str))))

(define sublist?
  (lambda (pat ls)
    (define (sublist-check pat ls pat-length ls-length)
      (cond
        [(> pat-length ls-length) #f]
        [(equal? pat (list-head ls pat-length)) #t]
        [else (sublist-check pat (cdr ls) pat-length (sub1 ls-length))]))
    (sublist-check pat ls (length pat) (length ls))))

(define substring-index
  (lambda (pat str)
    (sublist-index (string->list pat) (string->list str))))

(define sublist-index
  (lambda (pat ls)
    (define (sublist-check pat ls pat-length ls-length index)
      (cond
        [(> pat-length ls-length) #f]
        [(equal? pat (list-head ls pat-length)) index]
        [else (sublist-check pat (cdr ls) 
                             pat-length (sub1 ls-length) (add1 index))]))
    (sublist-check pat ls (length pat) (length ls) 0)))

(define zip
  (lambda (ls1 ls2)
    (map (lambda (x y) (cons x y)) ls1 ls2)))

(define (rac ls)
  (car (reverse ls)))

;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| This is where we load all of our post types; each one does its own   |-+
;; +-| parsing and whatnot. Below is a short summary of what each type is.  |-+
;; +-+----------------------------------------------------------------------+-+
;; | FILE NAME  | POST TAG | DESCRIPTION OF POST TYPE                         |
;; +------------+----------+--------------------------------------------------+
;; | image-post | picture  | This is for displaying a picture as a post.      |
;; +--------------------------------------------------------------------------+
;; +--------------------------------------------------------------------------+
(load "image-post.scm")
(load "text-post.scm")
(load "quote-post.scm")
(load "link-post.scm")

;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| Here be file parsing and interaction. It ain't pretty, but it gets   |-+
;; +-| the job done, my good friend! read-posts is the point of entry here. |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

;;(define write-file
;;  (lambda (file out)
;;    (with-output-to-file file
;;      (lambda ()
;;        (display out (current-output-port))))))
(define get-post-list
  (lambda ()
    (directory-list ".")))

(define (read-posts)
  (cd "posts")
  (let* ((post-list (get-post-list))
         (post-list (filter (lambda (x) (not (substring? ".swp" x))) post-list))
         (posts (map read-file-reasonably post-list)))
    (cd "..")
    (zip post-list posts)))

;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| These functions build out the HTML mark up itself. It relies mostly  |-+
;; +-| on string-append and recursion. Soome of these functions are used    |-+
;; +-| in the post types loaded above instead of in build-posts itself.     |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

;; Take a string body and produces a content div
(define make-content-div
  (lambda (b)
    (string-append
      "<div id=\"content\">\n"
      b
      "\n</div>\n\n")))

;; Produces the title, correctly formatted
(define make-title
  (lambda (title)
    (string-append
      "<h2>"
      title
      "</h2>\n")))

;; Parses a date and returns a string
(define parse-date
  (lambda (date)
    (string-append
      "<h4>"
      (date-and-time date)
      "</h4>\n")))

;; Builds a title/date div for a post
(define make-title-and-date
  (lambda (title date)
    (string-append
      "<div align=\"left\">\n"
      (make-title title)
      (parse-date date)
      "</div>\n\n<br />\n\n")))

(define (build-file-name filename)
  (string-append 
    ;;"pages/"
    (substring filename 0 (- (string-length filename) 5))
    ".html"))


(define build-post-body
  (lambda (post)
    (let ((type (extract-type post)))
      ((assqd type post-types) post))))
    
(define build-tag-links
  (lambda (tags)
    (cond
      [(null? tags) ""]
      [else 
        (let ((tag-str (symbol->string (car tags))))
          (string-append
            "<a href=\"" tag-str "-tag-page.html\">"
            tag-str "</a> "
            (build-tag-links (cdr tags))))])))

(define build-post
  (lambda (post link)
    (printf "Building page ~s~n" link)
    (let ((tags (extract-page-tags post)))
      (string-append
        "<div id=\"post\">\n\n"
        (build-post-body post)
        "<div id=\"tags\">"
        "Tags: "
        (build-tag-links tags)
        " | <a href=\""
        "\">Permalink</a> "
        "</div>"
        "\n\n</div>\n\n<hr />\n"))))

(define build-posts
  (lambda (posts)
    (map (lambda (x) (build-post (cdr x) (car x))) posts)))

(define generate-post-page
  (lambda (post)
    (string-append
      preamble
      post
      postamble)))

(define build-next-and-previous
  (lambda (index max-index)
    (cond
      [(and (< index 2) (= index max-index)) ""]
      [(< index 2) "\n<a href=\"page2.html\" id=\"oldnew\">Older</a>\n"]
      [(= index max-index) 
        (string-append 
          "\n<a href=\"" 
          (page-file-names (sub1 index))
          "\" id=\"oldnew\">Newer</a>\n")]
      [else 
        (string-append 
          "<a href=\""
          (page-file-names (add1 index))
          "\" id=\"oldnew\">Older</a> <i style=\"font-size: 28px;\">|</i> <a href=\""
          (page-file-names (sub1 index))
          "\" id=\"oldnew\">Newer</a>")])))

(define generate-posts-page
  (lambda (post index max-index)
    (string-append
      preamble
      post
      (build-next-and-previous index max-index)
      postamble)))

;; (build-posts) builds the post

(define post-sort
  (lambda (x y)
    (let ((x-t (extract-date (cdr x)))
          (y-t (extract-date (cdr y))))
      (time>? (date->time-utc x-t) (date->time-utc y-t)))))

(define sort-posts
  (lambda (x) (sort post-sort x)))

(define page-file-names
  (lambda (n)
    (cond
      [(< n 2) "index.html"]
      [else (string-append "page" (number->string n) ".html")])))

(define generate-pages
  (lambda (posts len index max-index)
    (cond
      [(<= len posts-per-page) `((,(page-file-names index) . 
                                 ,(generate-posts-page (apply string-append posts) index max-index)))]
      [else (cons 
              (cons (page-file-names index) 
                    (generate-posts-page 
                      (apply string-append (list-head posts posts-per-page)) index max-index))
              (generate-pages (list-tail posts posts-per-page) (- len posts-per-page) (add1 index) max-index))])))

(define get-page-tags
  (lambda (ls)
    (let ((posts (map car ls)))
      (fold-left union '() (map extract-page-tags (map cdr posts))))))

(define generate-tag-page
  (lambda (ls tags)
    (cond
      [(null? tags) '()]
      [else   
        (let* ((tag (car tags))
               (posts (filter (lambda (x) (memq tag (car x))) ls)))
          (cons 
            `(,(string-append (symbol->string tag) "-tag-page.html")
              ,(apply string-append (map cddr posts)))
            (generate-tag-page ls (cdr tags))))])))

(define generate-tag-pages
  (lambda (ls tags)
    (let* ((page-tag (map (lambda (x) (extract-page-tags (cdar x))) ls))
           (ls (zip page-tag ls)))
      (generate-tag-page ls tags))))

(define (generate-posts)
  (let* ((posts (read-posts))
         (posts (sort-posts posts))
         (post-bodies (map cdr posts))
         (post-file-names (map build-file-name (map car posts)))
         (post-bodies (build-posts (zip post-file-names post-bodies)))
         (tags (get-page-tags (zip posts post-bodies)))
         (tag-pages (generate-tag-pages (zip posts post-bodies) tags))
         (post-pages (map generate-post-page post-bodies)))
    (let ((output (append 
                    (zip post-file-names post-pages)
                    (map (lambda (x) (cons (car x) (generate-post-page (cadr x)))) tag-pages)
                    (let ((len (length post-bodies)))
                      (generate-pages post-bodies len 1 (ceiling (/ len posts-per-page)))))))
      (map (lambda (x) (write-file (car x) (cdr x))) output))))

;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-|----------------------------------------------------------------------|-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+
