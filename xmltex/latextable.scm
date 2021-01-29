;; xmltex/latex.scm -- Some utilities for SXML to LaTeX converter
;;              intended to use with cnvr.scm

;; Copyright (c) 2014 SHIKANO keiichirou <k16.shikano@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(define-module xmltex.latextable
  (use srfi-1)
  (use util.list)
  (use srfi-13)
  (use sxml.tools)
  (use xmltex.cnvr)
  (use xmltex.latex)
  (export set-col-width
	  tr-last-td
	  negate-multirow
	  make-colspec 
	  make-td
	  cline
	  cellcolor)
  )

(select-module xmltex.latextable)

; Int -> String
(define (recalc-relative-width w)
  (if (string=? w "0") "0"
    (format #f "~a\\textwidth" (* 0.01 (string->number w)))))

(define (latex-relative-width w)
  (format #f "~a\\textwidth" (* 0.01 (string->number (string-delete w #\%)))))

; [TR] -> [TR]
; seek the broadest TD in the column
(define (set-col-width trs)
  (define (max-colwidths trs)
    (deside-colwidth-from-left 0
      (apply zip ; to transpose the row into the column
        (map (lambda (tr)
	       (map 
		(lambda (td) (let* ((colspan (x->integer (or (sxml:attr-u td 'colspan) "1")))
				    (width   (sxml:attr-u td 'width))
				    (wvalue  (if width (string->number (string-delete width #\%)) #f))) ; width must be a relative value ended with "%"
			       (cons wvalue colspan))) ; (Int/#f . Int)
		(expand tr)))
	     trs))))
  (define (deside-colwidth-from-left accum columns)
    (cond ((null? columns) '())	  
	  (else (let1 w (col-width (car columns))
		      (cons w
			    (deside-colwidth-from-left 
			     (+ accum (or w 0))
			     (reduce-by-left w accum columns)))))))
  (define (reduce-by-left w a cols)
    (if (null? (cdr cols)) '()
	(cons (map (lambda (a b)
		     (if (> (cdr a) 1) ; this col is grouped with the previous col
			 (if w 
			     (cons (- (car b) w) (- (cdr b) 1)) 
			     (cons (car b) (- (cdr b) 1)))
			 b))
		   (car cols) (cadr cols))
	      (cddr cols))))
  ; [col] -> Int/#f
  (define (col-width col)
    (let1 ls (remove (lambda (x) (or (> (cdr x) 1) (not (car x)))) col)
	  (if (null? ls) #f (apply max (map car ls)))))
  ; replicate cell as many times as its colspan value
  (define (expand tds)
    (fold-right 
     (lambda (head rest)
       (append (make-list (x->integer (or (sxml:attr-u head 'colspan) "1")) head)
	       rest))
     '() tds))

  (define (apply-width widths tr)
    (let1 colspans 
	  (map (compose 
		x->integer 
		(lambda (td) (or (sxml:attr-u td 'colspan) "1")))
	       tr)
          (reverse 
	   (fold
	    (lambda (c td s)
	      (let1 w (number->string (apply + (filter values (take* widths c))))
		    (set! widths (drop* widths c))
		    (cons (sxml:set-attr td (list 'width (recalc-relative-width w))) s)))
	    '() colspans (sxml:content tr)))))
  (let1 colwidths (max-colwidths trs)
	(map (pa$ apply-width colwidths) trs)))

(define (tr-last-td trs)
  (map (lambda (tr)
         (append (drop-right tr 1) (list (sxml:set-attr (last tr) (list 'last "yes")))))
       trs))

(define (negate-multirow trs)
  ;; td -> #f|Int
  (define (positive-rowspan? td)
    (cond ((sxml:attr-u td 'rowspan)
           => (lambda (rows) (if (< 0 (x->integer rows)) (x->integer rows) #f)))
          (else #f)))
  (define (crash-positive-rowspan tds)
    (fold-right (lambda (head rest)
                  (cons (if (positive-rowspan? head)
                            (crash-content head) head)
                        rest))
                '() tds))
  (define (crash-content td)
    (sxml:change-attr (sxml:change-content td (list "")) (list 'rowspan "1")))
  ;; [[td]] -> [[(#f|Int):td]]
  ;; The integer indicates a tr to which we set negative rowspan.
  (define (having-rowspan tr)
    (map (lambda (td) (cons (positive-rowspan? td) td))
         tr))
  (define (fill-col row-tds tdss)
    (map 
      (lambda (tds-i)
	(let R ((row-tds row-tds)
		(tds (car tds-i)))
	  (if (null? row-tds)
	      '()
	      (let ((i (cadr tds-i))
		    (content (cdr (car row-tds)))
		    (rowspan (car (car row-tds))))
		(cond ((null? row-tds) '())
		      ((null? tds) (list content))
		      ((not rowspan)
		       (cons (car tds)
			     (R (cdr row-tds) (cdr tds))))
		      ((and rowspan (> rowspan i))
		       (cons (sxml:set-attr content (list 'rowspan "1"))
			     (R (cdr row-tds) tds)))
		      ((and rowspan (= rowspan i))
		       (cons (sxml:set-attr content (list 'rowspan (x->string (- rowspan))))
			     (R (cdr row-tds) tds)))
		      (else 
		       (cons (car tds) (R (cdr row-tds) (cdr tds)))))))))
    (zip tdss (iota (length tdss) 2))))

  (unfold null?
          (lambda (seed)
            (crash-positive-rowspan (car seed)))
          (lambda (seed)
            (let1 row-tds (having-rowspan (car seed))
              (fill-col row-tds (cdr seed))))
          trs))

(define (cline str)
  (cond ((not str) "")
        ((string=? str "full") "\\hline")
        (else (string-join 
	       (map (cut string-append "\\cline{" <> "}") 
		    (string-split str ","))))))

(define (cellcolor bgcolor)
  (define (hex->digit str)
    (x->string (/. (string->number str 16) 255)))
  (define (parce bgcolor)
    (if (not bgcolor) ""
        (cond ((string-prefix? "#" bgcolor)
               (unfold 
		string-null? 
		(compose hex->digit (cut string-take <> 2)) 
		(cut string-drop <> 2) 
		(string-drop bgcolor 1)))
              (else bgcolor))))
  (define (rgb-str rgb)
    (string-join rgb "," 'strict-infix))
  (if bgcolor #`"\\columncolor[rgb]{,(rgb-str (parce bgcolor))}" ""))

(define (make-td type . options)
  (define (calc-span cols)
    (let1 cols (x->number cols)
      (if (> cols 1) (format "~a\\tabcolsep+~apt" (* 2 (- cols 1)) (* 0.4 (- cols 1))) "0pt")))
  (let-keywords options ((trimer trim))
   (define-rule 
    (lambda ()
      (let* ((rows ($@ 'rowspan))
             (cols (or ($@ 'colspan) "1"))
             (width (if (and ($@ 'width) (string=? "0" ($@ 'width))) ""
                        #`"{,($@ 'width)+,(calc-span cols)}"))
             (hfil (if (or (eq? 'th type) ($@ 'align)) "" ""))
             (align (cond ((and (eq? 'th type) (not (string=? "" width)))
                           "\\centering\\arraybackslash\\hspace{0pt}")
                          ((or (not ($@ 'align)) (string=? "left" ($@ 'align)))
                           "\\raggedright\\arraybackslash")
                          ((or (not ($@ 'align)) (string=? "justify" ($@ 'align)))
                           "\\raggedright\\arraybackslash\\rightskip0pt")
                          ((string=? "center" ($@ 'align))
                           "\\centering\\arraybackslash\\hspace{0pt}")
                          ((string=? "right" ($@ 'align))
                           "\\raggedleft\\arraybackslash\\hspace{0pt}")
                          (else "")))
             (style (if (string=? "" width)
                        (cond ((and (eq? 'th type) (not (string=? "" width)))
                               "c")
                              ((or (not ($@ 'align)) (string=? "left" ($@ 'align)))
                               "l")
                              ((string=? "center" ($@ 'align))
                               "c")
                              ((string=? "right" ($@ 'align))
                               "r")
                              (else ""))
                        "m"))
             (bgcolor (if (eq? 'th type) "\\columncolor[rgb]{0.9,0.9,0.9}" (cellcolor ($@ 'bgcolor))))
             (last ($@ 'last)))
        (list
          (if cols #`"\\multicolumn{,|cols|}{,(ifstr ($@ 'lsep))>{,bgcolor,|align|},|style|,|width|,(ifstr ($@ 'rsep))}{,hfil" "")
          (if rows #`"\\multirow{,|rows|}{,(or ($@ 'width) \"*\")}{" ""))))
    trimer
    (lambda ()
      (let ((hfil (if (or (eq? 'th type) (and ($@ 'align) (string=? "c" ($@ 'align)))) "" "")))
        (list
          (if ($@ 'rowspan) "}" "")
          (if ($@ 'last) #`",hfil}" "}& ")))))))

;; [th]:[[td]] -> String
(define (make-colspec thtds)
  (string-join
    (map 
     (lambda (th)
       (let ((colspan (sxml:attr-u th 'colspan))
	     (width (sxml:attr-u th 'width))
	     (align (sxml:attr-u th 'align))
	     (bgcolor (if (eq? 'th (sxml:name th)) (sxml:attr-u th 'bgcolor) #f)))
	 (cond 
	  (colspan 
	   (string-join 
	    (make-list (x->integer colspan)
		       #`">{,(cellcolor bgcolor)},(string-take (or align \"c\") 1)")))
	  (else    "c"))))
     (car thtds))
    ""
    'strict-infix))

(provide "xmltex/latextable")
