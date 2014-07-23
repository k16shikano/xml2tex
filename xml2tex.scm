#!/bin/sh
:; exec gosh -I. -- $0 "$@" 

;; xml2tex.scm -- XML to LaTeX converter

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

(use srfi-1)
(use sxml.ssax)
(use sxml.sxpath)
(use text.tree)
(use gauche.parseopt)
(use xmltex.cnvr)

(require "r7rs")
(import scheme.base scheme.r5rs (gauche.base :except (div)))
(extend)

(define (to-sxml filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((local-xml (port->string port))
            (xml-entities xml-entities))
        (if (equal? "" local-xml)
            '()
            (call-with-input-string local-xml
              (lambda (port)
                (with-module sxml.ssax
                  (fluid-let ((ssax:predefined-parsed-entities
                               xml-entities)
                              (ssax:reverse-collect-str-drop-ws ssax:reverse-collect-str))
                    (ssax:xml->sxml port '()))))))))))

(define (main args)
  (let-args (cdr args)
    ((rulefile "r|rule=s" #f)
     (help     "h|help" => (cut show-help (car args)))
     . restargs)
    (if (null? restargs)
        (show-help (car args))
        (begin 
	  (load "default.rules")
          (if rulefile (load rulefile))
          (let ((sxml (to-sxml (last restargs))))
            (write-tree (cnvr sxml sxml))))))
  0)

(define (show-help p)
  (display "usage: gosh xml2tex.scm -r|rule <default.rule> input.xml > output.tex\n"))
