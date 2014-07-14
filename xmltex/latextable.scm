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
  (format #f "~a\\textwidth" (* 0.01 (string->number w))))

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
  (define (set-negative-rowcol row-tds tdss)
    (unzip2
      (map (lambda (tds-i)
             (for-each 
	      (lambda (row-td)
		(if (and (car row-td) (= (car row-td) (cadr tds-i)))
		    (set! tds-i (list (fill-rowcol (car tds-i) row-tds) (cadr tds-i)))
		    tds-i))
	      row-tds)
             tds-i)
           (zip tdss (iota (length tdss) 2)))))
  (define (fill-rowcol tds-i row-tds)
    (let R ((row-tds row-tds)
	    (tds-i   (sxml:content tds-i)))
      (cond ((null? row-tds) '())
	    ((car (car row-tds))
	     (cons (sxml:set-attr
		    (cdr (car row-tds))
		    (list 'rowspan (x->string (- (car (car row-tds))))))
		   (R (cdr row-tds) tds-i)))
	    ((null? tds-i)   (R (cdr row-tds) tds-i))
	    (else (cons (car tds-i) (R (cdr row-tds) (cdr tds-i)))))))

  (unfold null?
          (lambda (seed)
            (crash-positive-rowspan (car seed)))
          (lambda (seed)
            (let1 row-td (having-rowspan (car seed))
              (set-negative-rowcol row-td (cdr seed))))
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

(define (make-td type)
  (define (calc-span cols)
    (let1 cols (x->number cols)
      (if (> cols 1) (format "~a\\tabcolsep+~apt" (* 2 (- cols 1)) (* 0.4 (- cols 1))) "0pt")))
  (define-rule 
    (lambda ()
      (let* ((rows ($@ 'rowspan))
             (cols (or ($@ 'colspan) "1"))
             (width (if (string=? "0zw" ($@ 'width)) ""
                        #`"{,($@ 'width)+,(calc-span cols)}"))
             (hfil (if (or (eq? 'th type) ($@ 'align)) "" ""))
             (align (cond ((and (eq? 'th type) (not (string=? "" width)))
                           "\\centering\\hspace{0pt}")
                          ((or (not ($@ 'align)) (string=? "left" ($@ 'align)))
                           "\\raggedright\\hspace{0pt}")
                          ((string=? "center" ($@ 'align))
                           "\\centering\\hspace{0pt}")
                          ((string=? "right" ($@ 'align))
                           "\\raggedleft\\hspace{0pt}")
                          (else "")))
             (style (if (string=? "" width)
                        (if (eq? 'th type) "c" "l")
                        "m"))
             (bgcolor (if (eq? 'th type) "\\columncolor{10gray}" (cellcolor ($@ 'bgcolor))))
             (last ($@ 'last)))
        (list
          (if cols #`"\\multicolumn{,|cols|}{,(ifstr ($@ 'lsep))>{,bgcolor,|align|},|style|,|width|,(ifstr ($@ 'rsep)) }{,hfil" "")
          (if rows #`"\\multirow{,|rows|}{,(or ($@ 'width) \"*\")}{" ""))))
    trim
    (lambda ()
      (let ((hfil (if (or (eq? 'th type) (and ($@ 'align) (string=? "c" ($@ 'align)))) "" "")))
        (list
          (if ($@ 'rowspan) "}" "")
          (if ($@ 'last) #`",hfil}" "}& "))))))

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
	  (width   #`">{,(cellcolor bgcolor)},(string-take (or align \"m\") 1){,(relative-width width)}")
	  (else    "c"))))
     (car thtds))
    ""
    'strict-infix))

(provide "xmltex/latextable")
