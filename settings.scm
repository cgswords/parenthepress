;; +--------------------------------------------------------------------------+
;; +-+----------------------------------------------------------------------+-+
;; +-| General settings for the blog generation code to reference when it   |-+
;; +-| runs. Each has a small explaination by it, describing its role in    |-+
;; +-| the system. These also have default values associated with them that |-+
;; +-| you may want to change, so you should read through it all before     |-+
;; +-| using the software.                                                  |-+
;; +-+----------------------------------------------------------------------+-+
;; +--------------------------------------------------------------------------+

;; This defines what text will be put in the <title> ... </title> tag.
(define blog-title "Parenthepress Blog")

;; This defines if tag pages are created and links to them are made.
;; TODO: This is not currently implemented. Tag pages will always be built!
(define enable-tag-pages #t)

;; This defines the number of posts on each 'page' of the blog, for where
;; "Older" and "Newer" will be built.
(define posts-per-page 10)

;; This is the 'banner' of every page, included as the first thing.
(define banner
  (string-append
      "<div id=\"title\">\n"
      "<div id=\"titleinner\">\n"
      "<h1>BLOG TITLE</h1>\n"
      "Subtitle\n"
      "</div>\n"
      "</div>\n\n"))

;; This 'navigation' is included right after the banner on every page.
;; It is an association list of text to use and where to link.
(define page-navigation 
  '(("Latest" . "index.html")
    ("Link1" . "#")
    ("Linke2" . "#")))

;; This is called on each navigation link, providing the formatting
;; it should be created with.
(define build-individual-nav-link
  (lambda (text url)
    (string-append
      "<a href=\"" url "\" id=\"topnav\">" text "</a>")))

;; This is called to describe how the navigation looks. Note that it calls
;; build-individual-nav-link.
(define make-navigation
  (lambda (ls)
    (cond
      [(null? ls) ""]
      [(null? (cdr ls)) (build-individual-nav-link (caar ls) (cdar ls))]
      [else
        (string-append
          (build-individual-nav-link (caar ls) (cdar ls))
          " <i style=\"font-size: 28px; padding-left: 5px; padding-right: 5px;\">|</i> "
          (make-navigation (cdr ls)))])))

;; This determines what the body content looks like. b here is the string 
;; contaning whatever goes in the body of the post.
(define make-content-div
  (lambda (b)
    (string-append
      "<div id=\"content\">\n"
      b
      "\n</div>\n\n")))

;; This is how titles for title-using posts are built.
(define make-title
  (lambda (title)
    (string-append "<h2>" title "</h2>\n")))

;; This is how the date string is built, and the output is what is used anywhere
;; a date appears.
(define parse-date
  (lambda (date)
    (string-append "<h4>" (date-and-time date) "</h4>\n")))

;; This text is currently included at the footer of every page, but may be
;; changed as part of the layout.
(define blog-desc
  "This is a short description that appears in the footer of each page. If you want something else in the footer, this might be a good place for it, too.")

;; This contains all of the 'preamble' stuff for each page, including the above
;; described above needed to make it all work.
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
    (make-navigation page-navigation)
    "\n\n</div>"))
    
;; This contains all of the 'postamble' stuff for each page, including the stuff
;; described above needed to make it all work.
(define postamble
  (string-append
    "\n<br /><br />\n\n"
    "<div id=\"footer\">\n\n"
    blog-desc
    "\n<br /><br />\n</div>\n</div>\n</body>\n</html>"))
 

