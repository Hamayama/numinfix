;; -*- coding: utf-8 -*-
;;
;; noqlistのテスト
;; 2014-8-29
;;
(use gauche.test)
(test-start "numinfix")

(use numinfix)
(test-module 'numinfix)

(test-section "numinfix-on")
(numinfix-on)
(test* "logand" 3    (7 logand 3))
(test* "logior" 7    (7 logior 3))
(test* "logxor" 4    (7 logxor 3))
(test* "<<"     8    (1 << 3))
(test* ">>"     1    (8 >> 3))
(test* "+"      3    (1 + 2))
(test* "-"      -1   (1 - 2))
(test* "+."     3.5  (1.2 + 2.3) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(test* "-."     -1.1 (1.2 - 2.3) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(test* "*"      6    (2 * 3))
(test* "/"      2/3  (2 / 3))
(test* "*."     4.62 (2.1 * 2.2) (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(test* "/."     1.1  (2.2 / 2)   (lambda (expected result) (< (abs (- expected result)) 1.0e-10)))
(test* "\\"     3    (10 \ 3))
(test* "%"      1    (10 % 3))
(test* "**"     8    (2 ** 3))

(test-section "expression")
(test* "(1 + 2 * 3 + 4)"            11    (1 + 2 * 3 + 4))
(test* "(1 logand 2 ** 3 logior 4)" 4     (1 logand 2 ** 3 logior 4))
(test* "(8 - 4 / 2 - 1)"            5     (8 - 4 / 2 - 1))
(test* "(8 - 2 - 1)"                5     (8 - 2 - 1))
(test* "(1 + 2 * 3 / 4 - 5)"        -5/2  (1 + 2 * 3 / 4 - 5))
(define x 100)
(test* "(x + 1 + x * x + 30 / 3)"   10111 (x + 1 + x * x + 10))
(test* "(5 * (+ 1 1) * 2)"          20    (5 * (+ 1 1) * 2))

(test-section "error")
(test* "(1 2)"       (test-error <error>) (1 2))
(test* "(1 2 3)"     (test-error <error>) (1 2 3))
(test* "(1 + 2 3 4)" (test-error <error>) (1 + 2 3 4))
(test* "(- 1 + 2)"   (test-error <error>) (- 1 + 2))
(test* "(-1 + 2)"    1                    (-1 + 2))

(test-section "numinfix-off")
(numinfix-off)
(test* "logand" (test-error <error>) (7 logand 3))
(test* "**"     (test-error <error>) (2 ** 3))

(test-end)

