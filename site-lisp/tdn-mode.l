;;; -*- mode: lisp; package: editor -*-

;;; tdn-mode.l --- TDN mode
;;
;; Copyright (c) 2008 MIYAMUKO Katsuyuki.
;;
;; Author: MIYAMUKO Katsuyuki <miyamuko@gmail.com>
;; Version: 0.1
;; Keywords: joke, tdn, アッーーー
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; * tdd-minor-mode にして C-c m するたびに TDN がランダムにポップアップします。
;;
;; * 依存ライブラリ
;;   * tdd-minor-mode
;;     http://coderepos.org/share/browser/lang/xyzzy/tdd-minor-mode/site-lisp/tdd-minor-mode.l
;;   * xml-http-request
;;     http://miyamuko.s56.xrea.com/xyzzy/xml-http-request/intro.htm
;;


;;; Code:

(in-package "editor")

(require "xml-http-request")
(require "tdd-minor-mode")

(defvar *tdn-aa-url* "http://enjo.eek.jp/base_aa/index.php?%C2%BF%C5%C4%CC%EE%BF%F4%BF%CD")
(defvar *tdn-aa-list* nil)

(defun tdn-aa-list ()
  (or *tdn-aa-list*
      (setf *tdn-aa-list* (xhr:xhr-get *tdn-aa-url* :key 'scan-tdn-aa))))

(defun scan-tdn-aa (res)
  (mapcar 'html-unescape (scan-pre res)))

(defun scan-pre (res)
  (let ((html (xhr:xhr-response-text res))
        (start 0)
        s e r)
    (while (and (setf s (string-match "<pre>" html start))
                (setf e (string-match "</pre>" html s)))
      (push (substring html (+ s 5) e) r)
      (setf start (1+ e)))
    (nreverse r)))

(defun html-unescape (html)
  (reduce #'(lambda (r pair)
              (substitute-string r (car pair) (string (cdr pair))))
          '(("&lt;" . #\<) ("&gt;" . #\>) ("&quot;" . #\") ("&amp;" . #\&))
          :initial-value html))

(defun choice-tdn-aa ()
  (let ((lst (tdn-aa-list)))
    (nth (random (length lst)) lst)))

(defun popup-tdn ()
  (popup-string (choice-tdn-aa) (point)))

(add-hook '*tdd-minor-mode-change-state-hook* 'popup-tdn)


(provide "tdn-mode")

;;; tdn-mode.l ends here
