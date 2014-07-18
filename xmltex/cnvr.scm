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
  (use srfi-1)
  (export cnvr
          define-tag
          define-rule)
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
  (cond ((global-variable-bound? 'gauche tag)
         (error #`",(string-color 31 (symbol->string tag)) is not allowed for tag name.\n "))
        ((global-variable-bound? 'user tag)
         ((global-variable-ref 'user tag) body root))
        (else
         (let ()
         (display #`"Not knowing tha LaTeX syntax for <,(string-color 32 (symbol->string tag))>, ... applyed (through). \n" 
           (standard-error-port))
         (map (lambda (b)
                 (cond (((ntype?? '*) b) (cnvr b root))
                       (((ntype?? '@) b) '())
                       (((ntype?? '*text*) b) b)
                       (else '())))
              (cdr body))))))  ;; default is 'through'

(define (string-color n str)
  (string-append 
    (apply string 
      `(#\escape #\[ ,@(string->list (x->string n)) #\m))
    str
    (apply string `(#\escape #\[ #\0 #\m))))

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

(provide "xmltex/cnvr")
