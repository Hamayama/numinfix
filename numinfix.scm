;; -*- coding: utf-8 -*-
;;
;; numinfix.scm
;; 2014-11-26 v1.12
;;
;; ＜内容＞
;;   Gaucheで中置記法による数値演算を可能にするためのモジュールです。
;;   例えば (1 + 2 * 3) などと書けるようになります。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/numinfix
;;
(define-module numinfix
  (export
    numinfix-on numinfix-off
    numinfix-operator
    ** \ % << >> == != in-and in-or))
(select-module numinfix)

;; 2項演算子の記号の定義
(define **     expt)
(define \      quotient)
(define %      modulo)
(define <<     ash)
(define >>     (lambda (n1 n2) (ash n1 (- n2))))
(define ==     =)
(define !=     (lambda (n1 n2) (not (= n1 n2))))
(define in-and (lambda (n1 n2) (if (and n1 n2) #t #f)))
(define in-or  (lambda (n1 n2) (if (or  n1 n2) #t #f)))

;; 2項演算子の優先順位の定義 (数値が大きいほど優先順位が高い)
(define numinfix-operator
  (hash-table 'eq?
    `(,**     . 70)
    `(,*      . 60) `(,/      . 60) `(,*.     . 60) `(,/.     . 60)
    `(,\      . 60) `(,%      . 60)
    `(,+      . 50) `(,-      . 50) `(,+.     . 50) `(,-.     . 50)
    `(,<<     . 40) `(,>>     . 40)
    `(,<      . 30) `(,<=     . 30) `(,>      . 30) `(,>=     . 30)
    `(,==     . 30) `(,!=     . 30)
    `(,logand . 20) `(,logior . 20) `(,logxor . 20)
    `(,in-and . 10) `(,in-or  . 10)))

;; 中置記法による数値演算 (操車場アルゴリズムに近いもの)
;; 引数
;;   ops   演算子のスタック
;;   vals  値のスタック
;;   rest  読み込むトークンの残り
(define (calc ops vals rest)
  (if (null? rest)
    ;; 読み込むトークンがもうない場合
    (let loop ((ops ops) (vals vals))
      ;(print ops ":" vals)
      (if (null? ops)
        (car vals)
        (loop (cdr ops)
              (cons ((car ops) (cadr vals) (car vals)) (cddr vals)))))
    ;; 読み込むトークンがまだある場合
    (let* ((op    (car rest))
           (pr    (hash-table-get numinfix-operator op -1))
           (rest2 (cdr rest)))
      (if (< pr 0) (errorf "unknown binary operator: ~s" op))
      (if (null? rest2) (error "invalid expression"))
      (if (null? ops)
        (calc (cons op ops)
              (cons (car rest2) vals)
              (cdr rest2))
        (let* ((op2 (car ops))
               (pr2 (hash-table-get numinfix-operator op2 -1)))
          (if (<= pr pr2)
            (calc (cdr ops)
                  (cons (op2 (cadr vals) (car vals)) (cddr vals))
                  rest)
            (calc (cons op ops)
                  (cons (car rest2) vals)
                  (cdr rest2))))))))


;; 中置記法による数値演算を可能にするモードに入る
(define (numinfix-on)
  (rlet1 mold (list
               (get-gf-method object-apply 1 #t `(,<number>))
               (get-gf-method object-apply 1 #t `(,<boolean>)))
    (define-method object-apply ((n <number>) . rest)
      (calc () (list n) rest))
    (define-method object-apply ((n <boolean>) . rest)
      (calc () (list n) rest))))

;; 中置記法による数値演算を可能にするモードを抜ける
(define (numinfix-off :optional (mold #f))
  (cond ((and (list? mold) (= (length mold) 2))
         (let ((mold1 (car  mold))
               (mold2 (cadr mold)))
           (if (is-a? mold1 <method>)
             (add-method! object-apply mold1)
             (delete-gf-method object-apply 1 #t `(,<number>)))
           (if (is-a? mold2 <method>)
             (add-method! object-apply mold2)
             (delete-gf-method object-apply 1 #t `(,<boolean>)))))
        (else
         (delete-gf-method object-apply 1 #t `(,<number>))
         (delete-gf-method object-apply 1 #t `(,<boolean>)))))


;; ジェネリック関数のメソッドを種別を指定して取得する
;; 引数
;;   gf            ジェネリック関数(例えば object-apply 等)
;;   required      メソッドの引数の数(省略可能引数は除く)
;;   optional      メソッドの省略可能引数の有無(#tまたは#f)
;;   specializers  メソッドの引数の型を示す特定化子リスト(例えば `(,<number> ,<string>) 等)
(define (get-gf-method gf required optional specializers)
  (find
   (lambda (m)
     (and (equal? required (slot-ref m 'required))
          (equal? optional (slot-ref m 'optional))
          (equal? specializers (slot-ref m 'specializers))))
   (slot-ref gf 'methods)))

;; ジェネリック関数のメソッドを種別を指定して削除する
;; 引数
;;   gf            ジェネリック関数(例えば object-apply 等)
;;   required      メソッドの引数の数(省略可能引数は除く)
;;   optional      メソッドの省略可能引数の有無(#tまたは#f)
;;   specializers  メソッドの引数の型を示す特定化子リスト(例えば `(,<number> ,<string>) 等)
(define (delete-gf-method gf required optional specializers)
  (let1 m (get-gf-method gf required optional specializers)
    (if m (delete-method! gf m))))

