;; xmltex/latex.scm -- Some utilities for SXML to LaTeX converter
;;              intended to use with cnvr.scm

;; Copyright (c) 2011 SHIKANO keiichirou <k16.shikano@gmail.com>

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

(define-module xmltex.latex
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use sxml.tools)
  (use xmltex.cnvr)
  (export make-latex-env
          make-latex-cmd
          make-latex-cmd-without-tex-escape
          make-latex-group
	  define-by-tag
	  relative-width          
	  ignore
          through
	  doc-class
	  usepackage
          without-white
          kick-comment
          dis-ligature
          trim
          index-trim)
  )

(select-module xmltex.latex)


;;--------------
;; making rules

(define (latex-opt-args args)
  (let ((opt (get-keyword :opt args ""))
        (args (get-keyword :args args '())))
    (values (if (string=? opt "") '()
		(format #f "[~a]" opt))
            (if (null? args) '()
                (map (pa$ format #f "{~a}") args)))))

(define (make-latex-env name . args)
  (let ((name (format #f "{~a}" name)))
    (receive (opt args) (latex-opt-args args)
      (define-rule
        (lambda ()
          (list "\n\\begin" name opt args))
        (lambda (str)
          (without-white (kick-comment str)))
        (lambda () 
          (list "\\end" name))))))

(define (make-latex-cmd name . args)
  (receive (opt args) (latex-opt-args args)
    (define-rule
      (lambda ()
        (list "\\" name opt args "{"))
      (lambda (str)
        (list (without-white (kick-comment str))))
      (lambda () (list "} ")))))

(define (make-latex-cmd-without-tex-escape name . args)
  (receive (opt args) (latex-opt-args args)
    (define-rule
      (lambda ()
        (list "\\" name opt args "{"))
      (lambda (str) str)
      (lambda () (list "} ")))))

(define (make-latex-group group)
  (define-rule
    (lambda () (list "{"))
    (lambda (str)
      (without-white (kick-comment str)))
    (lambda () (list "}\n"))))

(define (ignore . name)
  (lambda (body root) '()))

(define (through . name)
  (define-rule
    (lambda () '())
    (lambda (s) s)
    (lambda () '())))

(define (doc-class name . args)
  (let1 opts (if (null? args) "" (format #f "[~a]" (string-join args "," 'strict-infix)))
	(string-join `("\\documentclass" ,opts "{" ,name "}\n"
		       "\\usepackage[T1]{fontenc}" ;; must
		       ) "")))

(define (usepackage name . args)
  (let1 opts (if (null? args) "" (format #f "[~a]" (string-join args "," 'strict-infix)))
	(string-join `("\\usepackage" ,opts "{" ,name "}\n") "")))

; String -> String
(define (relative-width w)
  (format #f "~a\\textwidth" (* 0.01 (string->number (string-delete w #\%)))))

(define-macro (define-simple-rules builder . tags)
  (let R ((tags tags)
          (rest '()))
    (if (null? tags) `(begin ,@(reverse rest))
        (R (cdr tags)
           (list* `(define-tag ,(car tags)
                     (,builder ',(car tags)))
                  rest)))))

(define-macro (define-tag-replace tagname proc-or-str)
  `(define-tag ,tagname
     (define-rule
       (lambda ()
         (if (string? ,proc-or-str) '(,proc-or-str)
             '("")))
       (lambda (str)
         (if (procedure? ,proc-or-str) (,proc-or-str str)
             str))
       "")))

(define-macro (define-by-tag tag str . while-proc)
  `(define-tag ,tag
     (define-rule 
       ,(string-append "\\" str "{")
       ,(if (null? while-proc) trim (car while-proc))
       "}")))


(define-syntax ifstr
  (syntax-rules (else)
    ((_ con proc-or-str)
      (if (and con (string? con))
          (if (procedure? proc-or-str)
              (proc-or-str con)
              proc-or-str)
          ""))
     ((_ con)
      (if con con ""))
     ((_ con else str)
      (if con con str))
     ((_ con proc-or-str1 else str)
      (if (and con (string? con))
          (if (procedure? proc-or-str1)
              (proc-or-str1 con)
              proc-or-str1)
          str))))


(define-syntax when-lang
  (syntax-rules (else)
    ((_) "")
    ((_ else b) b)
    ((_ language builder)
     (call/cc (lambda (k)
       (lambda (body root)
         (let1 attr-lang (sxml:attr-u body 'lang)
           (if (and attr-lang (string=? attr-lang language))
               (k builder)
               ""))))))
    ((_ language1 builder1 language2 builder2 ...)
     (call/cc (lambda (k)
       (lambda (body root)
         (let1 attr-lang (sxml:attr-u body 'lang)
           (if (and attr-lang (string=? attr-lang language1))
               (k builder1)
               (k (when-lang language2 builder2 ...))))))))))

;;--------------
;; escapes

(define (without-white str)
  (regexp-replace-all* str
    #/[ ]{2,}|　/     " "
    #/\n{2,}/         ""
    #/(?:^|\n)[ \t]/  ""
    #/[ \t](?:^|\n)/  ""
    ))

(define (kick-comment str)
  (regexp-replace-all 
   #/[(]symbol (\d{1,3})[)]/
   (regexp-replace-all* str
      #/\\/ "(symbol 92)"
      #/{/  "(symbol 123)"
      #/}/  "(symbol 125)"
      #/\#/ "(symbol 35)"
      #/\$/ "(symbol 36)"
      #/\%/ "(symbol 37)"
      #/\&/ "(symbol 38)"
      #/\_/ "(symbol 95)"
      #/</  "(symbol 60)"
      #/>/  "(symbol 62)"
      #/\^/ "(symbol 94)"
      #/\~/ "(symbol 126)")
   "{\\\\symbol{\\1}}"))

(define (dis-ligature string)
  (regexp-replace-all* string
    #/--/ "-{}-"
    #/fi/ "f{}i"
    #/ff/ "f{}f"
    #/fl/ "f{}l"
    ))

(define trim
  (compose without-white kick-comment))

(define (index-trim str)
  (regexp-replace-all* str
      #/[\"!@|]/ "\"\\0"))

(provide "xmltex/latex")
