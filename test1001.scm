;; -*- coding: utf-8 -*-
;;
;; numinfixのテスト
;; 2014-10-29
;;
(use gauche.test)
(test-start "numinfix")

(use numinfix)
(test-module 'numinfix)

(define-syntax expr-test
  (syntax-rules ()
    ((_ txt ans expr)
     (test* (string-append txt " : " (x->string (quote expr))) ans expr))
    ((_ txt ans expr chk)
     (test* (string-append txt " : " (x->string (quote expr))) ans expr chk))))

(test-section "numinfix-on")
(numinfix-on)
(expr-test "in-and" #t    (1 in-and 2))
(expr-test "in-or " #t    (1 in-or 2))
(expr-test "logand" 3     (7 logand 3))
(expr-test "logior" 7     (7 logior 3))
(expr-test "logxor" 4     (7 logxor 3))
(expr-test "<<"     8     (1 << 3))
(expr-test ">>"     1     (8 >> 3))
(expr-test "< "     #t    (1 < 3))
(expr-test "<="     #t    (1 <= 3))
(expr-test "> "     #f    (1 > 3))
(expr-test ">="     #f    (1 >= 3))
(expr-test "=="     #f    (1 == 3))
(expr-test "!="     #t    (1 != 3))
(expr-test "+ "     3     (1 + 2))
(expr-test "- "     -1    (1 - 2))
(expr-test "+."     3.5   (1.2 + 2.3) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(expr-test "-."     -1.1  (1.2 - 2.3) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(expr-test "* "     6     (2 * 3))
(expr-test "/ "     2/3   (2 / 3))
(expr-test "*."     4.62  (2.1 * 2.2) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(expr-test "/."     1.1   (2.2 / 2)   (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(expr-test "\\ "    3     (10 \ 3))
(expr-test "% "     1     (10 % 3))
(expr-test "**"     8     (2 ** 3))

(test-section "expression")
(expr-test "expr"   11    (1 + 2 * 3 + 4))
(expr-test "expr"   4     (1 logand 2 ** 3 logior 4))
(expr-test "expr"   5     (8 - 4 / 2 - 1))
(expr-test "expr"   5     (8 - 2 - 1))
(expr-test "expr"   -5/2  (1 + 2 * 3 / 4 - 5))
(expr-test "expr"   -981  (1 + 2 * 3 ** 2 - 1000))
(define x 100)
(expr-test "expr"   10111 (x + 1 + x * x + 30 / 3))
(expr-test "expr"   20    (5 * (+ 1 1) * 2))
(expr-test "expr"   #f    ((1 < 2) in-and (1 >= 3)))
(expr-test "expr"   #t    ((1 < 2) in-or  (1 >= 3)))

(test-section "error")
(expr-test "err"    (test-error <error>) (1 2))
(expr-test "err"    (test-error <error>) (1 2 3))
(expr-test "err"    (test-error <error>) (1 + 2 3 4))
(expr-test "err"    (test-error <error>) (- 1 + 2))
(expr-test "err"    1                    (-1 + 2))

(test-section "numinfix-off")
(numinfix-off)
(expr-test "err"    (test-error <error>) (1 in-and 2))
(expr-test "err"    (test-error <error>) (2 ** 3))

(test-end)

