(define (rac ls)
  (car (reverse ls)))

(define matching-closing-paren
  (lambda (ls)
    (letrec ((recur (lambda (ls pcount index)
                      (cond
                        [(null? ls) (error 'matching-closing-paren "Invalid string input.")]
                        [(eq? (car ls) #\() (recur (cdr ls) (add1 pcount) (add1 index))]
                        [(and (eq? (car ls) #\)) (zero? pcount)) index]
                        [(eq? (car ls) #\)) (recur (cdr ls) (sub1 pcount) (add1 index))]
                        [else (recur (cdr ls) pcount (add1 index))]))))
      (recur ls 0 0))))

(define next-open-paren
  (lambda (ls)
    (letrec ((recur (lambda (ls index)
                      (cond
                        [(null? ls) (error 'next-open-paren "Invalid string input.")]
                        [(eq? (car ls) #\() index]
                        [else (recur (cdr ls) (add1 index))]))))
      (recur ls 0))))

(define replace
  (lambda (ls symbol string)
    (cond
      [(null? ls) '()]
      [(eq? (car ls) symbol) (append string (replace (cdr ls) symbol string))]
      [else (cons (car ls) (replace (cdr ls) symbol string))])))

(define split-on-parens
  (lambda (ls)
    (letrec ((splitter (lambda (ls)
                      (cond
                        [(null? ls) '()]
                        [else 
                          (let* ((start-index (next-open-paren ls))
                                 (ls (list-tail ls start-index))
                                 (end-index   (+ 2 (matching-closing-paren (cdr ls)))))
                            (cons 
                              (cdr (list-head ls end-index)) 
                              (splitter (list-tail ls end-index))))]))))
      (splitter (trim ls)))))

(define split-on
    (lambda (x ls)
      (cond
        [(null? ls) '()]
        [else
         (let ((index (next-occurance x ls)))
           (if index
               (cons 
                 (list-head ls index) 
                 (split-on x (cdr (list-tail ls index))))
               (list ls)))])))

(define next-occurance
    (lambda (x ls)
      (define find-index
        (lambda (x ls index)
          (cond
            [(null? ls) #f]
            [(eq? (car ls) x) index]
            [else (find-index x (cdr ls) (add1 index))])))
      (find-index x ls 0)))

(define get-string-until-symbol
  (lambda (ls sym)
    (cond
      [(null? ls) '()]
      [(eq? (car ls) sym) '()]
      [else (cons (car ls) (get-string-until-symbol (cdr ls) sym))])))

(define convert-car-to-symbol
  (lambda (ls)
    (let* ((sym (filter (lambda (x) (not (eq? x #\space))) (get-string-until-symbol ls #\.)))
           (sym (string->symbol (list->string sym))))
      (cons sym (remove-trailing-parens (cdr (memq #\. ls)))))))

(define convert-string-to-symbol
  (lambda (str)
    (string->symbol (list->string (remove-leading-symbols str '(#\space #\. #\())))))

(define (remove-trailing-parens ls)
  (remove-trailing-symbols ls '(#\) #\newline)))

(define remove-trailing-symbols
  (lambda (ls symbols)
    (reverse (remove-leading-symbols (reverse ls) symbols))))

;; '(#\space #\. #\()
(define remove-leading-symbols
  (lambda (ls symbols)
    (cond
      [(memq (car ls) symbols) (remove-leading-symbols (cdr ls) symbols)]
      [else ls])))

(define trim
  (lambda (ls)
    (remove-leading-symbols (remove-trailing-symbols ls '(#\space #\newline)) '(#\space #\newline))))
