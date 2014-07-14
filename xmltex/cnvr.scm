;; xmltex/cnvr.scm --  Rule based SXML converter generator

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

(define-module xmltex.cnvr
  (use sxml.sxpath)
  (use sxml.tools)
  (export cnvr
          define-tag
          define-rule
          define-simple-rules
          define-tag-replace
	  xml-entities
          ifstr
          when-lang
          has-siblings?)
  )
(select-module xmltex.cnvr)

(define (cnvr sxml root)
  (let ((tag (car sxml))
        (body sxml))
    (tag body root)))

(define-macro (define-tag tagname rule)
  ;; tagname: symbol
  ;; rule: define-rule
  `(define (,tagname body root)
       (,rule body root)))

(define-method object-apply ((tag <symbol>) (body <list>) (root <list>))
  (guard (exc
	  ((<error> exc)
	   (map (lambda (b)
		  (cond (((ntype?? '*) b) (cnvr b root))
			(((ntype?? '@) b) '())
			(((ntype?? '*text*) b) b)
			(else '())))
		(cdr body)))  ;; default is 'through'
	  (else 'othre-error))
	 ((global-variable-ref 'user tag) body root)))

(define-macro (define-rule begin while end . options)
  (let-keywords options ((pre values) (post values))
    `(lambda (body root)
       (let1 body (,pre body root)
           (let* (($body body)
                  ($root root)
                  ($parents (lambda ()
                              (((sxml:parent (ntype?? '*any*)) root) body)))
                  ($parent (lambda ()
                             (sxml:node-name (if (null? ($parents)) '() (car ($parents))))))
                  ($parent? (lambda (name)
                             (eq? name ($parent))))
                  ($childs (lambda ()
                            ((sxml:child (ntype?? '*any*)) body)))
                  ($child (lambda (name)
                            (filter (lambda (kid)
                                      (eq? (sxml:node-name kid) name))
                                    ($childs))))
                  ($following-siblings (lambda ()
                               (((sxml:following-sibling (ntype?? '*any*)) root) body)))
                  ($siblings (lambda ()
                               ((node-or ((sxml:following-sibling (ntype?? '*any*)) root)
                                         ((sxml:preceding-sibling (ntype?? '*any*)) root)) body)))
                  (|$@| (lambda (name)
                          (cond ((sxml:attr-u body name)
                                 => values)
                                (else #f))))
                  ($under? (lambda (list)
                             (not (null? (((sxml:ancestor (ntype-names?? list)) root) body)))))
                  ($ancestor-of? (lambda (list)
                             (not (null? ((node-closure (ntype-names?? list)) body))))))
             (append (cond ((string? ,begin) (list ,begin))
                           ((procedure? ,begin) (,begin))
                           (else ,begin))
                     (map (lambda (b)
                            (cond ((,(ntype?? '*) b) (cnvr b root))
                                  ((,(ntype?? '@) b) '())
                                  ((,(ntype?? '*text*) b) (,while b))
                                  (else '())))
                          (cdr body))
                     (cond ((string? ,end) (list ,end))
                           ((procedure? ,end) (,end))
                           (else ,end))))))))

(define (has-siblings? name siblings)
  (any (lambda (e) (eq? name (sxml:name e))) siblings))

(define xml-entities
  (list
   '(amp  . "&")
   '(lt   . "<")
   '(gt   . ">")
   ))

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

(provide "xmltex/cnvr")
