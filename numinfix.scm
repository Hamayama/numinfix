;; -*- coding: utf-8 -*-
;;
;; numinfix.scm
;; 2014-8-30 v1.01
;;
;; ＜内容＞
;;   Gaucheで中置記法による数値演算を可能にするためのモジュールです。
;;   例えば (1 + 2 * 3) などと書けるようになります。
;;
;; ＜インストール方法＞
;;   numinfix.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;; ＜使い方＞
;;   中置記法による数値演算を可能にするモードに入る場合
;;     (use numinfix)
;;     (numinfix-on)
;;
;;   中置記法による数値演算を可能にするモードを抜ける場合
;;     (numinfix-off)
;;
;;   使用可能な演算子 (レベルが小さいほど優先順位が高い)
;;     レベル1 : **             (べき乗)
;;     レベル2 : * / *. /. \ %  (乗算,除算,不正確数乗算,不正確数除算,整数除算,剰余)
;;     レベル3 : + - +. -.      (加算,減算,不正確数加算,不正確数減算)
;;     レベル4 : << >>          (左ビットシフト,右ビットシフト(符号ありシフトのみ))
;;     レベル5 : logand logior logxor (ビットAND,ビットOR,ビットXOR)
;;
;;   ジェネリック関数のメソッドの競合について
;;     もし、ジェネリック関数 object-apply の以下のメソッド
;;       (define-method object-apply ((n <number>) . rest) ...
;;     が、すでに定義されていた場合には、上書きしてしまいます。
;;     そのような場合は、モードに入るときに
;;       (define mold (numinfix-on))
;;     として、元のメソッドを保存してください。
;;     そして、モードを抜けるときに
;;       (numinfix-off mold)
;;     として、元のメソッドに戻してください。
;;     (ただし、モードに入っている間は、元のメソッドは使用できないので注意)
;;
;; ＜注意事項＞
;;   (1)リストの先頭が数値であった場合に、中置記法を処理するように
;;      ジェネリック関数 object-apply に数値用のメソッドを追加しています。
;;      ジェネリック関数 object-apply はグローバルであるため、競合が発生する可能性が
;;      あります。例えば、他のモジュールでもジェネリック関数 object-apply に
;;      数値用のメソッドを追加していたりすると、正常に動作しなくなります。
;;
;;   (2)数値と演算子の間にはスペースが必要です。
;;
;;   (3)演算子の優先順位比較等の計算コストがかかります。
;;
;;   (4)他のモジュールとの演算子記号のバッティングに注意が必要です。
;;
;;   (5)中置記法は、2項演算子のみ対応です。
;;      例えば、単項演算の - を混ぜて記述することはできません。
;;      すなわち (- 1 + 2) はエラーになります
;;      ただし   (-1 + 2)  のように - と数字の間にスペースを入れずに書けば、
;;      ひとつの数値と解釈されるため計算可能です。
;;
;;   (6)数値のみ対応です(ベクタや文字列等は非対応)。
;;
;;   (7)括弧でくくれば、通常の前置記法と混在が可能です。
;;      例えば (5 * (+ 1 1) * 2) や ((cos 0) + 1) のように記述できます。
;;
(define-module numinfix
  (export numinfix-on numinfix-off numinfix-operator << >> \ % **))
(select-module numinfix)

;; 2項演算子の記号の定義
;(define logand logand)
;(define logior logior)
;(define logxor logxor)
(define <<  ash)
(define >>  (lambda (x n) (ash x (- n))))
;(define +   +)
;(define -   -)
;(define +.  +.)
;(define -.  -.)
;(define *   *)
;(define /   /)
;(define *.  *.)
;(define /.  /.)
(define \   quotient)
(define %   modulo)
(define **  expt)

;; 2項演算子の優先順位の定義 (数値が大きいほど優先順位が高い)
(define numinfix-operator
  `((,logand . 10)
    (,logior . 10)
    (,logxor . 10)
    (,<<     . 20)
    (,>>     . 20)
    (,+      . 30)
    (,-      . 30)
    (,+.     . 30)
    (,-.     . 30)
    (,*      . 40)
    (,/      . 40)
    (,*.     . 40)
    (,/.     . 40)
    (,\      . 40)
    (,%      . 40)
    (,**     . 50)))

;; 中置記法による数値演算 (操車場アルゴリズムに近いもの)
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
    (let ((op1   (car rest))
          (pr1   -1)
          (val   0)
          (rest2 (cdr rest)))
      (set! pr1 (assoc-ref numinfix-operator op1 -1))
      (if (< pr1 0) (errorf "unknown binary operator: ~s" op1))
      (if (null? rest2) (error "invalid expression"))
      (set! val   (car rest2))
      (set! rest2 (cdr rest2))
      (if (null? ops)
        (calc (cons op1 ops)
              (cons val vals)
              rest2)
        (let* ((op2 (car ops))
               (pr2 (assoc-ref numinfix-operator op2 -1)))
          (if (<= pr1 pr2)
            (calc (cdr ops)
                  (cons (op2 (cadr vals) (car vals)) (cddr vals))
                  rest)
            (calc (cons op1 ops)
                  (cons val vals)
                  rest2)))))))


;; 中置記法による数値演算を可能にするモードに入る
(define (numinfix-on)
  (rlet1 mold (get-gf-method object-apply 1 #t `(,<number>))
    (define-method object-apply ((n <number>) . rest)
      (calc () (list n) rest))))

;; 中置記法による数値演算を可能にするモードを抜ける
(define (numinfix-off :optional (mold #f))
  (if (is-a? mold <method>)
    (add-method! object-apply mold)
    (delete-gf-method object-apply 1 #t `(,<number>))))


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

