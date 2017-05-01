#lang racket

(require "building-images.rkt")

(provide rules run-list get-piece castle-state kings count1)

(define kings (mcons (mcons 7 4) (mcons 0 4)))

(define-syntax myfor
  (syntax-rules (:)
    [(myfor init : condition : step : statements)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements step (iter))]))
       (iter))]))

(define-syntax while
  (syntax-rules ()
    [ (while boolean-expression one-or-more-statements ...)
      (begin (define (iter) (cond
                              [boolean-expression (begin one-or-more-statements ... (iter))]))
             (iter))]))

(define-syntax all
  (syntax-rules ()
    [(all (b1 s1) ...)
     (begin (cond [b1 s1]) ...)]))

(define (run-list l f)
  (cond [ (not (null? l)) (begin (f (car l)) (run-list (cdr l) f))]))

;(define current-position initial-vector)

(define castle-state (mcons (mcons #t #t) (mcons #t #t)))
;(define castle-state (mcons (mcons #f #f) (mcons #f #f)))

(define (in-range posi)
  (and (>= (car posi) 0) (<= (car posi) 7) (>= (cdr posi) 0) (<= (cdr posi) 7)))

(define (get-piece i j board)
  (if (in-range (cons i j)) (vector-ref (vector-ref board i) j) -1))

(define rules
  (lambda(i j piece variable-position)
    (define answer '())

    (define (rook-vertical-white ii jj op)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! ii (op ii 1)))
      (cond [ (>= (get-piece ii jj variable-position ) 7) (set! answer (cons (cons ii jj) answer))]))

    (define (rook-horizontal-white ii jj op)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! jj (op jj 1)))
      (cond [ (>= (get-piece ii jj variable-position ) 7) (set! answer (cons (cons ii jj) answer))]))

    (define (bishop-white ii jj op1 op2)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! ii (op1 ii 1)) (set! jj (op2 jj 1)))
      (cond [ (>= (get-piece ii jj variable-position ) 7) (set! answer (cons (cons ii jj) answer))]))
     
    (define (bishop-black ii jj op1 op2)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! ii (op1 ii 1)) (set! jj (op2 jj 1)))
      (cond [ (let ((obj (get-piece ii jj variable-position ))) (and (>= obj 0) (<= obj 6))) (set! answer (cons (cons ii jj) answer))]))
     
    (define (rook-vertical-black ii jj op)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! ii (op ii 1)))
      (cond [ (let ((obj (get-piece ii jj variable-position ))) (and (>= obj 0) (<= obj 6))) (set! answer (cons (cons ii jj) answer))]))

    (define (rook-horizontal-black ii jj op)
      (while (= (get-piece ii jj variable-position ) 0) (set! answer (cons (cons ii jj) answer))
             (set! jj (op jj 1)))
      (cond [ (let ((obj (get-piece ii jj variable-position ))) (and (>= obj 0) (<= obj 6))) (set! answer (cons (cons ii jj) answer))]))

    (define (knight-helper-white ii jj op1 op2)
      (let ((obj (get-piece (op1 i 1) (op2 j 2) variable-position))) (cond [(or (= obj 0) (>= obj 7)) (set! answer (cons (cons (op1 i 1) (op2 j 2)) answer))]))
      (let ((obj (get-piece (op1 i 2) (op2 j 1) variable-position))) (cond [(or (= obj 0) (>= obj 7)) (set! answer (cons (cons (op1 i 2) (op2 j 1)) answer))])))      

    (define (knight-helper-black ii jj op1 op2)
      (let ((obj (get-piece (op1 i 1) (op2 j 2) variable-position))) (cond [(and (>= obj 0) (< obj 7)) (set! answer (cons (cons (op1 i 1) (op2 j 2)) answer))]))
      (let ((obj (get-piece (op1 i 2) (op2 j 1) variable-position))) (cond [(and (>= obj 0) (< obj 7)) (set! answer (cons (cons (op1 i 2) (op2 j 1)) answer))])))      

    (define (white-king-helper ii jj a b)
      (let ((obj (get-piece (+ i a) (+ j b) variable-position))) (cond [(or (= obj 0) (>= obj 7)) (set! answer (cons (cons (+ i a) (+ j b)) answer))])))
    
    (define (black-king-helper ii jj a b)
      (let ((obj (get-piece (+ i a) (+ j b) variable-position))) (cond [(and (>= obj 0) (< obj 7)) (set! answer (cons (cons (+ i a) (+ j b)) answer))])))
    
    (cond [(= piece 1) (all [(= (get-piece (- i 1) j variable-position) 0)
                             (set! answer (cons (cons (- i 1) j) answer))]
                            [(and (= i 6) (= (get-piece (- i 2) j variable-position) 0) (= (get-piece (- i 1) j variable-position) 0))
                             (set! answer (cons (cons (- i 2) j) answer))]
                            [(>= (get-piece (- i 1) (- j 1) variable-position) 7)
                             (set! answer (cons (cons (- i 1) (- j 1)) answer))]
                            [(>= (get-piece (- i 1) (+ j 1) variable-position) 7)
                             (set! answer (cons (cons (- i 1) (+ j 1)) answer))])]
           
          [(= piece 7)  (all [(= (get-piece (+ i 1) j variable-position) 0)
                              (set! answer (cons (cons (+ i 1) j) answer))]
                             [(and (= i 1) (= (get-piece (+ i 2) j variable-position) 0) (= (get-piece (+ i 1) j variable-position) 0))
                              (set! answer (cons (cons (+ i 2) j) answer))]
                             [( = (ceiling (/ (get-piece (+ i 1) (- j 1) variable-position) 6)) 1)
                              (set! answer (cons (cons (+ i 1) (- j 1)) answer))]
                             [ ( = (ceiling (/ (get-piece (+ i 1) (+ j 1) variable-position) 6)) 1)
                               (set! answer (cons (cons (+ i 1) (+ j 1)) answer))])]
           
          [(= piece 2) (knight-helper-white i j + +) (knight-helper-white i j + -) (knight-helper-white i j - +) (knight-helper-white i j - -)]
           
          [(= piece 8) (knight-helper-black i j + +) (knight-helper-black i j + -) (knight-helper-black i j - +) (knight-helper-black i j - -)]
           
          [(= piece 6) (white-king-helper i j 0 1) (white-king-helper i j 1 0) (white-king-helper i j 0 -1) (white-king-helper i j -1 0)
                       (white-king-helper i j 1 1) (white-king-helper i j -1 1) (white-king-helper i j 1 -1) (white-king-helper i j -1 -1)
                       (all [ (let ((obj1 (get-piece 7 3 variable-position))
                                    (obj2 (get-piece 7 2 variable-position))
                                    (obj3 (get-piece 7 1 variable-position)))
                                (and (mcar (mcar castle-state)) (= (+ obj1 obj2 obj3) 0) (not (check? 0 variable-position (mcons (mcons 7 4) (mcdr kings))))
                                     (not (check? 0 variable-position (mcons (mcons 7 3) (mcdr kings))))))
                              (set! answer (cons (cons 7 2) answer))]
                            [ (let ((obj1 (get-piece 7 5 variable-position))
                                    (obj2 (get-piece 7 6 variable-position)))
                                (and (mcdr (mcar castle-state)) (= (+ obj1 obj2) 0) (not (check? 0 variable-position (mcons (mcons 7 4) (mcdr kings))))
                                     (not (check? 0 variable-position (mcons (mcons 7 5) (mcdr kings))))))
                              (set! answer (cons (cons 7 6) answer))])]
                                                                      
           
          [(= piece 12) (black-king-helper i j 0 1) (black-king-helper i j 1 0) (black-king-helper i j 0 -1) (black-king-helper i j -1 0)
                        (black-king-helper i j 1 1) (black-king-helper i j -1 1) (black-king-helper i j 1 -1) (black-king-helper i j -1 -1)
                        (all [ (begin (set! count1 (add1 count1)) (let ((obj1 (get-piece 0 3 variable-position))
                                                                        (obj2 (get-piece 0 2 variable-position))
                                                                        (obj3 (get-piece 0 1 variable-position)))
                                                                    (and (mcar (mcdr castle-state)) (= (+ obj1 obj2 obj3) 0) (not (check? 1 variable-position (mcons (mcar kings) (mcons 0 4))))
                                                                         (not (check? 1 variable-position (mcons (mcar kings) (mcons 0 3)))))))
                               (set! answer (cons (cons 0 2) answer))]
                             [ (let ((obj1 (get-piece 0 5 variable-position))
                                    (obj2 (get-piece 0 6 variable-position)))
                                 (and (mcdr (mcdr castle-state)) (= (+ obj1 obj2) 0) (not (check? 1 variable-position (mcons (mcar kings) (mcons 0 5))))
                                      (not (check? 1 variable-position (mcons (mcar kings) (mcons 0 4))))))
                               (set! answer (cons (cons 0 6) answer))])]

[(= piece 4) (begin
                         (rook-vertical-white (+ i 1) j +)
                         (rook-vertical-white (- i 1) j -)
                         (rook-horizontal-white i (+ j 1) +)
                         (rook-horizontal-white i (- j 1) -))]

          [(= piece 10) (begin
                          (rook-vertical-black (+ i 1) j +)
                          (rook-vertical-black (- i 1) j -)
                          (rook-horizontal-black i (+ j 1) +)
                          (rook-horizontal-black i (- j 1) -))]
           
          [(= piece 3) (begin
                         (bishop-white (+ i 1) (+ j 1) + +)
                         (bishop-white (+ i 1) (- j 1) + -)
                         (bishop-white (- i 1) (+ j 1) - +)
                         (bishop-white (- i 1) (- j 1) - -))]

          [(= piece 9) (begin
                         (bishop-black (+ i 1) (+ j 1) + +)
                         (bishop-black (+ i 1) (- j 1) + -)
                         (bishop-black (- i 1) (+ j 1) - +)
                         (bishop-black (- i 1) (- j 1) - -))]
           
          [(= piece 5) (set! answer (append (rules i j 4 variable-position) (rules i j 3 variable-position)))]

          [(= piece 11) (set! answer (append (rules i j 10 variable-position) (rules i j 9 variable-position)))]
           
          ) answer))

(define (check-rules i j val board)
  (define answer '())
  (cond [(and (not (= val 6)) (not (= val 12)))
         (set! answer (rules i j val board))]
        [(= val 6) (all [  (let ((obj (get-piece (+ i 1) j variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (+ i 1) j) answer))]
                            [ (let ((obj (get-piece (- i 1) j variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (- i 1) j) answer))]
                            [ (let ((obj (get-piece (+ i 1) (+ j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (+ i 1) (+ j 1)) answer))]
                            [ (let ((obj (get-piece i (+ j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons i (+ j 1)) answer))]
                            [ (let ((obj (get-piece (- i 1) (+ j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (- i 1) (+ j 1)) answer))]
                            [ (let ((obj (get-piece (+ i 1) (- j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (+ i 1) (- j 1)) answer))]
                            [ (let ((obj (get-piece i (- j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons i (- j 1)) answer))]
                            [ (let ((obj (get-piece (- i 1) (- j 1) variable-position))) (or (= obj 0) (>= obj 7))) (set! answer (cons (cons (- i 1) (- j 1)) answer))])]
        [(= val 12) (all [ (let ((obj (get-piece (+ i 1) j variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (+ i 1) j) answer))]
                             [ (let ((obj (get-piece (- i 1) j variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (- i 1) j) answer))]
                             [ (let ((obj (get-piece (+ i 1) (+ j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (+ i 1) (+ j 1)) answer))]
                             [ (let ((obj (get-piece i (+ j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons i (+ j 1)) answer))]
                             [ (let ((obj (get-piece (- i 1) (+ j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (- i 1) (+ j 1)) answer))]
                             [ (let ((obj (get-piece (+ i 1) (- j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (+ i 1) (- j 1)) answer))]
                             [ (let ((obj (get-piece i (- j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons i (- j 1)) answer))]
                             [ (let ((obj (get-piece (- i 1) (- j 1) variable-position))) (and (<= obj 6) (>= obj 0))) (set! answer (cons (cons (- i 1) (- j 1)) answer))])])
  answer)



(define (check? colour variable-position kings1)
  (define i 0)
  (define j 0)
  (define check #f)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
         (myfor (set! j 0) : (< j 8) : (set! j (+ j 1)) :
                (cond [ (and (not check) (= (ceiling (/ (get-piece i j variable-position) 6)) (- 2 colour)))
                        (run-list (check-rules i j (get-piece i j variable-position) variable-position)
                                  (lambda (posi) (cond [ (equal? posi (get-king-position colour kings1))
                                                         (begin (set! check #t))])))])))
  check)

(define (get-king-position colour kings)
  (if (= colour 0) (cons (mcar (mcar kings)) (mcdr (mcar kings))) (cons (mcar (mcdr kings)) (mcdr (mcdr kings)))))




(define count1 0)