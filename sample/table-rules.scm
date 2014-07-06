;; When converting html tables to latex longtables, you need to 
;;  - extract colspec from tr elems having th elems and/or most td elems, 
;;  - specify last td elems to avoid last &, and
;;  - negate multirows to avoid overdrawing.

(use sxml.sxpath)
(use sxml.tools)
(use xmltex.latex)
(use xmltex.latex-table)
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
      "\\usepackage{calc}\n"
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
      (let* ((trs ((node-closure (ntype-names?? '(tr))) body))
	     (tds (map (node-closure (ntype-names?? '(td th))) trs))
	     (tr-attrs (map sxml:attr-as-list trs))
	     (colspec (make-colspec tds)))
	(sxml:set-attr
	 (cons (sxml:name body)
	       (append 
		(transform-trs tds tr-attrs)
		(sxml:aux-as-list body)
		(sxml:content
		 (filter (sxml:invert (ntype-names?? '(tr))) body))))
	 (list 'colspec colspec))))
    ))

(define (transform-trs trs tr-attrs)
  (define (make-content name attr cont)
    (cons name (append attr cont)))
  (map 
   (lambda (tr)
     (cons
      'tr 
      (append tr-attrs tr)))
   ((compose set-col-width tr-last-td negate-multirow) trs)))

(define-tag tr
  (define-rule
    ""
    trim
    (lambda ()
      (let ((terminator (if (null? ($child 'th)) "" "\\hline"))
            (underline  (cline ($@ 'cline))))
        #`"\\\\,|underline|,|terminator|\n"))))

(define-tag th (make-td 'th))
(define-tag td (make-td 'td))

