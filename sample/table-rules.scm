;; When converting html tables to latex longtables, you need to 
;;  - extract colspec from tr elems having th elems and/or most td elems, 
;;  - specify last td elems to avoid last &, and
;;  - negate multirows to avoid overdrawing.

(use xmltex.latex)
(use sxml.sxpath)
(use sxml.tools)
(use xmltex.cnvr)
(use text.tree)
(use srfi-13)
(use util.list)

(define-simple-rules through
  *TOP* *PI*)

(define xml-entities
  (list
   '(amp  . "&")
   '(quot . "'")
   '(lt   . "<")
   '(gt   . ">")
   '(apos . "'")
   '(ouml . "\\\"o")
   ))

(define-tag body
  (define-rule
    '("\\documentclass{book}\n"
      "\\usepackage[table]{xcolor}\n"
      "\\usepackage{multirow}\n"
      "\\begin{document}\n"
      )
    list
    "\\end{document}"))

(define-tag table
  (define-rule
    (lambda ()
      (list #`"\\begin{tabular}{|,($@ 'colspec)|}\n"))
    trim
    "\\end{tabular}"
    :pre
    (lambda (body root)
      (let1 trs ((node-closure (ntype-names?? '(tr))) body)
        (let1 trs-tds (map (node-closure (ntype-names?? '(td th))) trs)
          (let* ((colnums (sum-colspan trs-tds))
                 (max-colnum (apply max colnums))
                 (colspec (make-colspec trs-tds)))
            (sxml:set-attr
              (cons (sxml:name body)
                    (append ((compose set-col-width last-td negate-multirow) (map (cut cons 'tr <>) trs-tds))
                            (sxml:aux-as-list body)
                            (sxml:content
                              (filter (sxml:invert (ntype-names?? '(tr))) body))))
              (list 'colspec colspec))))))
))

(define (set-col-width trstds)
  (define (max-colwidths trstds)
    (map max-width
      (apply zip
        (map (lambda (tr)
               (map (lambda (td)
                      (let ((colspan (x->integer (or (sxml:attr-u td 'colspan) "1")))
                            (width (x->integer (string-filter (or (sxml:attr-u td 'width) "0") #[0-9]))))
                        (x->string (x->integer (if colspan (ceiling (/. width colspan)) width)))))
                    (expand ((node-closure (ntype-names?? '(td th))) tr))))
             trstds))))
  (define (max-width ls)
    (apply max (map (compose string->number (cut string-filter <> #[0-9])) ls)))
  (define (apply-width widths tr)
    (let1 colspans (map (compose x->integer (lambda (td) (or (sxml:attr-u td 'colspan) "1")))
                        ((node-closure (ntype-names?? '(td th))) tr))
      (make-content
        (sxml:name tr) (sxml:attr-as-list tr)
          (reverse (fold
            (lambda (c td s)
              (let1 w (number->string (apply + (take* widths c)))
                (set! widths (drop* widths c))
                (cons (sxml:set-attr td (list 'width #`",|w|zw")) s)))
            '() colspans (sxml:content tr))))))
  (let1 colwidths (max-colwidths trstds)
    (map (pa$ apply-width colwidths) trstds)))

(define (expand tds)
  (fold-right (lambda (head rest)
                (append (make-list (x->integer (or (sxml:attr-u head 'colspan) "1")) head)
                        rest))
              '() tds))

(define (negate-multirow trs-tds)
  ;; td -> #f|Int
  (define (positive-rowspan? td)
    (cond ((sxml:attr-u td 'rowspan)
           => (lambda (rows) (if (< 0 (x->integer rows)) (x->integer rows) #f)))
          (else #f)))
  (define (crash-positive-rowspan tds)
    (fold-right (lambda (head rest)
                  (cons (if (and (pair? head) (positive-rowspan? head))
                            (crash-content head) head)
                        rest))
                '() tds))
  (define (crash-content td)
    (sxml:change-attr (sxml:change-content td (list "")) (list 'rowspan "1")))
  ;; [[td]] -> [[(#f|Int):td]]
  ;; The integer indicates a tr to which we set negative rowspan.
  (define (having-rowspan tr)
    (map (lambda (td) (cons (and (pair? td) (positive-rowspan? td)) td))
         (expand ((node-closure (ntype-names?? '(td th))) tr))))
  (define (set-negative-rowcol row-tds trs-tds)
    (unzip2
      (map (lambda (tds-i)
             (for-each (lambda (row-td)
                         (if (and (car row-td) (= (car row-td) (cadr tds-i)))
                             (set! tds-i
                               (list (replace-positive-rowcol (car tds-i) row-tds) (cadr tds-i)))
                             tds-i))
                       row-tds)
             tds-i)
           (zip trs-tds (iota (length trs-tds) 2)))))
  (define (replace-positive-rowcol tds-i row-tds)
    (make-content
      (sxml:name tds-i) (sxml:attr-as-list tds-i)
      (map (lambda (td-i row-td)
             (if (car row-td)
                 (sxml:set-attr (cdr row-td);(sxml:change-content td-i (sxml:content (cdr row-td)))
                   (list 'rowspan (x->string (- (car row-td)))))
                 td-i))
           (sxml:content tds-i) row-tds)))
  (unfold null?
          (lambda (seed)
            (crash-positive-rowspan (car seed)))
          (lambda (seed)
            (let1 row-td (having-rowspan (car seed))
              (set-negative-rowcol row-td (cdr seed))))
          trs-tds))

(define (last-td trs-tds)
  (map (lambda (tr)
         (append (drop-right tr 1) (list (sxml:set-attr (last tr) (list 'last "yes")))))
       trs-tds))
(define (make-content name attr cont)
  (cons name (append attr cont)))

(define-tag tr
  (define-rule
    ""
    trim
    (lambda ()
      (let ((terminator (if (null? ($child 'th)) "" "\\hline"))
            (underline  (parse-cline ($@ 'cline))))
        #`"\\\\,|underline|,|terminator|\n"))))

(define (parse-cline str)
  (cond ((not str) "")
        ((string=? str "full") "\\hline")
        (else (string-join (map (cut string-append "\\cline{" <> "}") (string-split str ","))))))

(define (cellcolor bgcolor)
  (define (hex->digit str)
    (x->string (/. (string->number str 16) 255)))
  (define (parce bgcolor)
    (if (not bgcolor) ""
        (cond ((string-prefix? "#" bgcolor)
               (unfold string-null? (compose hex->digit (cut string-take <> 2)) (cut string-drop <> 2) 
                  (string-drop bgcolor 1)))
              (else bgcolor))))
  (define (rgb-str rgb)
    (string-join rgb "," 'strict-infix))
  (if bgcolor #`"\\columncolor[rgb]{,(rgb-str (parce bgcolor))}" ""))

(define (make-td type)
  (define-rule 
    (lambda ()
      (let ((rows ($@ 'rowspan))
            (cols (or ($@ 'colspan) "1"))
            (width (or ($@ 'width) "*"))
            (align (if (eq? 'th type) "c" (string-take (or ($@ 'align) "l") 1)))
            (bgcolor (if (eq? 'th type) "#cccccc" ($@ 'bgcolor)))
            (last ($@ 'last)))
        (list
          (if cols #`"\\multicolumn{,|cols|}{|>{,(cellcolor bgcolor)},|align|,(if last \"|\" \"\")}{" "")
          (if rows #`"\\multirow{,|rows|}{,|width|}{" ""))))
    trim
    (lambda ()
      (list
        (if ($@ 'rowspan) "}" "")
        (if ($@ 'last) "}" "}& ")))))
(define-tag th (make-td 'th))
(define-tag td (make-td 'td))

;; [th]:[[td]] -> String
(define (make-colspec thtds)
  (string-join 
    (map (lambda (th)
           (let ((colspan (sxml:attr-u th 'colspan))
                 (width (sxml:attr-u th 'width)) (align (sxml:attr-u th 'align))
                 (bgcolor (if (eq? 'th (sxml:name th)) (sxml:attr-u th 'bgcolor) #f)))
             (cond (colspan (string-join 
                              (make-list (x->integer colspan) 
                                         #`">{,(cellcolor bgcolor)},(string-take (or align \"c\") 1)")))
                   (width   #`">{,(cellcolor bgcolor)},(string-take (or align \"m\") 1){,|width|}")
                   (else    "c"))))
         (car thtds))
    ""
    'strict-infix))

;; [[td]] -> [n]
(define (sum-colspan tdss)
  (define (sum ls) (fold + 0 ls))
  (map (lambda (tds)
         (sum (map (lambda (td) 
                     (x->integer (or (sxml:attr-u td 'colspan) 1)))
              tds)))
       tdss))
