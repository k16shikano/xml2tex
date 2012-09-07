;; rules for HTML

(use srfi-1)
(use sxml.sxpath)
(use sxml.tools)
(use text.tree)
(use xmltex.cnvr)
(use xmltex.latex)


(define accented-chars
  (append-map (lambda (h)
                (map (lambda (t)
                       (cons (string->symbol (string-append h (car t)))
                             (string-append "\\" (cdr t) "{" h "}")))
                     '(("acute" . "'")
                       ("uml"   . "\"")
                       ("circ"  . "^")
                       ("grave" . "`")
                       ("tilde" . "~")
                       ("ring"  . "r")
                       ("cedil" . "c"))))
              '("a" "e" "i" "u" "o" "n" "c" "y" "A" "E" "I" "U" "O" "N" "C" "Y")))

(define xml-entities
  `((amp  . "&") (nbsp . "\, ")
    (lt   . "<") (gt   . ">")
    (quot . "\"") (rsquo . "'") (lsquo . "`") (rdquo . "''") (ldquo . "``") 
    (ndash . " -- ") (mdash . " --- ") (bull . "・")
    ,@accented-chars
    (AElig . "\\AE") (aelig . "\\ae") (szlig . "\\ss") (ETH . "\\DH") (eth . "\\dh") (THORN . "\\Thorn") (thorn . "\\thorn")
    (cent . "\\textcent") (pound . "\\pounds") (yen . "\\yen")
    ))

(define-tag body
  (define-rule
    '("\\documentclass{book}\n"
      "\\usepackage[table]{xcolor}\n"
      "\\usepackage{multirow}\n"
      "\\usepackage{graphicx}\n"
      "\\usepackage{alltt}\n"
      "\\begin{document}\n"
      )
    list
    "\\end{document}"))

(define-simple-rules through
  *TOP* html meta title style span a footnote br div arrow)

(define-simple-rules ignore
  *PI* head)

(define-macro (define-by-tag tag str)
  `(define-tag ,tag
     (define-rule ,(string-append "\\" str "{") trim "}")))

(define-by-tag b  "textbf")
(define-by-tag em "emph")
(define-by-tag strong "emph")
(define-by-tag tt "texttt")
(define-by-tag code "texttt")
(define-by-tag i "textit")
(define-by-tag kbd "textsc")

(define-by-tag h1 "chapter")
(define-by-tag h2 "section")
(define-by-tag h3 "subsection")
(define-by-tag h4 "subsubsection")

(define-simple-rules make-latex-env
  myquote biblio)

(define-simple-rules make-latex-cmd-without-tex-escape
  eq)

(define-tag p
  (define-rule "" trim "\n\n"))

(define-tag img
  (define-rule
    (lambda ()
      (receive (src width)
          (values (format #f "{~a}" ( (ifstr ($@ 'src))))
                  (format #f "[width=~a]" (ifstr ($@ 'width) else "100truemm")))
        (list "\\includegraphics" width src)))
    trim
    ""))

(define-simple-rules ignore
  img)

(define-tag pre
  (define-rule
    (lambda () 
      (list #`"\\begin{alltt}"))
    kick-comment
    (lambda () 
      (list "\\end{alltt}"))))

(define-tag dl
  (define-rule
    (lambda ()
      (list "\\begin{description}"))
    kick-comment
    (lambda ()
      (list "\\end{description}"))))

(define-tag dt
  (define-rule "\\item[" trim "]"))

(define-tag dd
  (define-rule "" trim "\n\n"))

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
                    (append (last-td (negate-multirow ((node-closure (ntype-names?? '(tr))) body)))
                            (sxml:content
                              (filter (sxml:invert (ntype-names?? '(tr))) body))))
              (list 'colspec colspec))))))
))

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
         (expand tr)))
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
                 (sxml:set-attr (cdr row-td)
                   (list 'rowspan (x->string (- (car row-td)))))
                 td-i))
           (padding-null-td (sxml:content tds-i) row-tds) row-tds)))
  (define (make-content name attr cont)
    (cons name (append attr cont)))
  (define (padding-null-td tds row-tds)
    (take* tds (rownum row-tds) #t '(td "")))
  (define (rownum row-tds)
    (if (null? row-tds) 0
        (fold (lambda (a b) (if a (+ a b) b)) 0 (map car row-tds))))
  ;; repeat td colspan times
  (define (expand tr)
    (let1 tds ((node-closure (ntype-names?? '(td th))) tr)
      (fold-right (lambda (head rest)
                    (append (take* (list head) (x->integer (or (sxml:attr-u head 'colspan) "1")) #t '())
                            rest))
                  '() tds)))
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
