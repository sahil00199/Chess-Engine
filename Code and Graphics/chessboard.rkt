#lang racket
(require racket/draw)
(require racket/gui)
(require racket/mpair)
(require "building-images.rkt")
(require "rules.rkt")
(provide set-variable-board canvas dc-chess frame which-square possible-moves in-list? highlight get-og-piece)


(define frame (new frame%
                   [label "Chessboard"]
                   [width 1300]
                   [height 710]))

(define frame1 (new frame%
                   [label "One-Player-Chessboard"]
                   [width 1300]
                   [height 710]))

(define promotion-frame (new frame%
                             [label "Promotion"]
                             [width 100]
                             [height 200]))


(define menu-frame (new frame%
                        [label "Welcome to the game of chess"]
                        [width 867]
                        [height 600]))

(define (menu-click frame)
  (class canvas%
    (define/override (on-event mouse-event)
      (cond [(eq? (send mouse-event get-event-type) 'left-down)
             (if (< (send mouse-event get-x) 348) (begin (one-player) (send menu-frame show #f))
                 (begin (two-player) (send menu-frame show #f)))]))
  (super-new [parent menu-frame])))

(define menu-canvas (new (menu-click menu-frame)
                     [paint-callback
                      (lambda (canvas1 dc)    
                        (send dc draw-bitmap
        (make-object bitmap% "abc.jpg")
        0 0)
        (send dc draw-bitmap
        (make-object bitmap% "def.jpg")
        348 0))]))

(define end-frame (new frame%
                   [label "Game Over!"]
                   [width 1300]
                   [height 300]))
(new canvas% [parent end-frame]
             [paint-callback
              (lambda (end-canvas end-dc)
                (send end-dc set-scale 3 3)
                (send end-dc set-text-foreground "blue")
                (send end-dc draw-text game-termination-text1 0 0)
                (send end-dc draw-text game-termination-text2 0 30))])

(define game-termination-text1 0)
(define game-termination-text2 0)

(define (in-range posi)
  (and (>= (car posi) 0) (<= (car posi) 7) (>= (cdr posi) 0) (<= (cdr posi) 7)))

(define l 70)
(define x-offset 370)
(define y-offset 80)

(define current-position initial-vector)
(define previous-position initial-vector)
(define whose-move 0)
(define possible-moves '())

(define captured-by-black '())
(define captured-by-white '())

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

(define (in-list? l a)
  (if (null? l) #f
      (or (equal? (car l) a) (in-list? (cdr l) a))))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define (colour p)
  (ceiling (/ p 6)))

(define (rem l1 l2)
  (if (null? l1) l2
      (rem (cdr l1) (remove (car l1) l2))))

(define (checkmate? colour)
  (set-variable-board)
  (and (check? colour) (not (any-legal? current-position colour))))

(define (stalemate? colour)
  (set-variable-board)
  (and (not (check? colour)) (not (any-legal? current-position colour))))

(define (make-board canv b)
  (define i 0)
  (define j 0)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (myfor (set! j 0) : (< j 8) : (set! j (+ j 1)) :
      (send (send canv get-dc) draw-bitmap (list-ref (list-ref image-list
                                                     (get-og-piece j i))
                                           (remainder (+ i j) 2))
            (+ x-offset (* i l)) (+ y-offset (* j l)))))
  (cond [(check? (b whose-move)) (let* ((a (get-king-position whose-move))
                                                 (i (car a))
                                                 (j (cdr a)))
                                            (send (send canv get-dc) draw-bitmap (cadddr (list-ref image-list
                                                                                         (vector-ref (vector-ref current-position i) j)))
                                                  (+ x-offset (* j l)) (+ y-offset (* i l))))])
  (for ([i captured-by-black] [j (range 20)])
  (cond [(> i 0) (send (send canv get-dc) draw-bitmap (list-ref plain-list (- i 1))
                                                  250 (+ 100 (* j 35)))]))
  (for ([i captured-by-white] [j (range 20)])
  (cond [(> i 0) (send (send canv get-dc) draw-bitmap (list-ref plain-list (- i 1))
                                                  1000 (- 600 (* j 35)))])))

(define (get-og-piece i j)
  (if (in-range (cons i j)) (vector-ref (vector-ref current-position i) j) -1))

(define (get-king-position colour)
  (if (= colour 0) (cons (mcar (mcar kings)) (mcdr (mcar kings))) (cons (mcar (mcdr kings)) (mcdr (mcdr kings)))))

(define (check? colour)
  (define i 0)
  (define j 0)
  (define check #f)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (myfor (set! j 0) : (< j 8) : (set! j (+ j 1)) :
      (cond [ (and (not check) (= (ceiling (/ (get-piece i j variable-position) 6)) (- 2 colour)))
              (run-list (rules i j (get-piece i j variable-position) variable-position) (lambda (posi) (cond [ (equal? posi (get-king-position colour))
                                                                            (set! check #t)])))])))
  check)

(define (set-variable-board)
  (define i 0)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (vector-copy! (vector-ref variable-position i) 0 (vector-ref current-position i))))

(define (set-previous-board)
  (define i 0)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (vector-copy! (vector-ref previous-position i) 0 (vector-ref current-position i))))

(define (set-prev)
   (define i 0)
  (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (vector-copy! (vector-ref prev-pos i) 0 (vector-ref current-position i))))

(define (diff-bet a b)
  (define c (make-vector 8 (make-vector 8 0)))
  (for ([i (range 8)])
    (for ([j (range 8)])
      (vector-set! (vector-ref c i) j) (- (vector-set! (vector-ref a i) j) (vector-set! (vector-ref b i) j))))
  c)
 
(define (legal-moves pos)
  (set-variable-board)
  (define king? #f)
  (define i (car pos))
  (define j (cdr pos))
  (define p (get-og-piece (car pos) (cdr pos)))
  (define addition (cons #f 0))
  (cond [(and (= p 1) (= i 3)  (= (get-og-piece i (- j 1)) 7) (= (get-og-piece (- i 2) (- j 1)) 0)
              (= (vector-ref (vector-ref prev-pos i) (- j 1)) 0) (= (vector-ref (vector-ref prev-pos (- i 2)) (- j 1)) 7))
         (set! addition (cons #t (cons (- i 1) (- j 1))))])
  
  (cond [(and (= p 1) (= i 3)  (= (get-og-piece i (+ j 1)) 7) (= (get-og-piece (- i 2) (+ j 1)) 0)
              (= (vector-ref (vector-ref prev-pos i) (+ j 1)) 0) (= (vector-ref (vector-ref prev-pos (- i 2)) (+ j 1)) 7))
         (set! addition (cons #t (cons (- i 1) (+ j 1))))])
  
  (cond [(and (= p 7) (= i 4)  (= (get-og-piece i (- j 1)) 1) (= (get-og-piece (+ i 2) (- j 1)) 0)
              (= (vector-ref (vector-ref prev-pos i) (- j 1)) 0) (= (vector-ref (vector-ref prev-pos (+ i 2)) (- j 1)) 1))
         (set! addition (cons #t (cons (+ i 1) (- j 1))))])
  
  (cond [(and (= p 7) (= i 4)  (= (get-og-piece i (+ j 1)) 1) (= (get-og-piece (+ i 2) (+ j 1)) 0)
              (= (vector-ref (vector-ref prev-pos i) (+ j 1)) 0) (= (vector-ref (vector-ref prev-pos (+ i 2)) (+ j 1)) 1))
         (set! addition (cons #t (cons (+ i 1) (+ j 1))))])
  
               (define base (lc x : x <- (rules (car pos) (cdr pos) (get-og-piece (car pos) (cdr pos)) variable-position) @ (begin
                                                                                                    
                                                           (cond [(and (equal? pos (cons 7 4)) (equal? x (cons 7 2)) (mcar (mcar castle-state)))
                                                                  (vector-set! (vector-ref variable-position (car pos)) (cdr pos) 0)
                                                                  (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                               (get-og-piece (car pos) (cdr pos)))
                                                                  (vector-set! (vector-ref variable-position 7) 0 0)
                                                                  (vector-set! (vector-ref variable-position 7) 3 4)]
                                                                 [(and (equal? pos (cons 7 4)) (equal? x (cons 7 6)) (mcdr (mcar castle-state)))
                                                                  (vector-set! (vector-ref variable-position (car pos)) (cdr pos) 0)
                                                                  (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                               (get-og-piece (car pos) (cdr pos)))
                                                                  (vector-set! (vector-ref variable-position 7) 7 0)
                                                                  (vector-set! (vector-ref variable-position 7) 5 4)]
                                                                 [(and (equal? pos (cons 0 4)) (equal? x (cons 0 2)) (mcar (mcdr castle-state)))
                                                                  (vector-set! (vector-ref variable-position (car pos)) (cdr pos) 0)
                                                                  (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                               (get-og-piece (car pos) (cdr pos)))
                                                                  (vector-set! (vector-ref variable-position 0) 0 0)
                                                                  (vector-set! (vector-ref variable-position 0) 3 10)]
                                                                 [(and (equal? pos (cons 0 4)) (equal? x (cons 0 6)) (mcdr (mcdr castle-state)))
                                                                  (vector-set! (vector-ref variable-position (car pos)) (cdr pos) 0)
                                                                  (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                               (get-og-piece (car pos) (cdr pos)))
                                                                  (vector-set! (vector-ref variable-position 0) 7 0)
                                                                  (vector-set! (vector-ref variable-position 0) 5 10)])
                                                           
                                                           
                                                                                  (vector-set! (vector-ref variable-position (car pos)) (cdr pos) 0)
                                                                                  (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                               (get-og-piece (car pos) (cdr pos)))
                                                                                  (cond [(equal? pos (get-king-position whose-move))
                                                                                         (begin (set! king? #t)
                                                                                                (if (= whose-move 0) (set-mcar! kings
                                                                                                                            (mcons (car x) (cdr x)))
                                                                                                    (set-mcdr! kings  (mcons (car x) (cdr x)))))])
                                                                                 (let ( (b (not (check? whose-move))))
                                                                                   (cond [king?
                                                                                           (begin (set! king? #f)
                                                                                                  (if (= whose-move 0) (set-mcar! kings
                                                                                                                            (mcons (car pos) (cdr pos)))
                                                                                                      (set-mcdr! kings
                                                                                                                            (mcons (car pos) (cdr pos)))))])
                                                                                    (vector-set! (vector-ref variable-position (car pos)) (cdr pos)
                                                                                                 (get-og-piece (car pos) (cdr pos))) 
                                                                                    (vector-set! (vector-ref variable-position (car x)) (cdr x)
                                                                                                 (get-og-piece (car x) (cdr x)))
                                                                                    b))))
  (if (car addition) (cons (cdr addition) base) base))


(define  (which-square x y)
  (cons (floor (/ (- x x-offset) l)) (floor (/ (- y y-offset) l))))

(define (which-colour x y)
  (- (ceiling (/ (get-og-piece x y) 6)) 1))

(define (highlight square)
  (define i (cdr square))
  (define j (car square))
  (send dc-chess clear)
  (make-board canvas identity)
  (cond [ (= (which-colour i j) whose-move) (begin
                                              (send dc-chess draw-bitmap (caddr (list-ref image-list
                                                                                          (vector-ref (vector-ref current-position i) j)))
                                                    (+ x-offset (* j l)) (+ y-offset (* i l)))
                                              (set! possible-moves (cons (cons i j) (legal-moves (cons i j))))
                                              (run-list (legal-moves (cons i j)) (lambda (posi)
                                                                                   (send dc-chess draw-bitmap
                                                                                         (cadddr (list-ref image-list
                                                                                                           (get-piece (car posi)
                                                                                                                      (cdr posi) variable-position)))
                                                                                         (+ x-offset (* (cdr posi) l)) (+ y-offset
                                                                                                                          (* (car posi) l))))))]))

(define (highlight1 square)
  (define i (cdr square))
  (define j (car square))
  (send (send canvas1 get-dc) clear)
  (make-board canvas1 (lambda(x) (- 1 x)))
  (cond [ (= (which-colour i j) whose-move) (begin
                                              (send (send canvas1 get-dc) draw-bitmap (caddr (list-ref image-list
                                                                                          (vector-ref (vector-ref current-position i) j)))
                                                    (+ x-offset (* j l)) (+ y-offset (* i l)))
                                              (set! possible-moves (cons (cons i j) (legal-moves (cons i j))))
                                              (run-list (legal-moves (cons i j)) (lambda (posi)
                                                                                   (send (send canvas1 get-dc) draw-bitmap
                                                                                         (cadddr (list-ref image-list
                                                                                                           (get-piece (car posi)
                                                                                                                      (cdr posi) variable-position)))
                                                                                         (+ x-offset (* (cdr posi) l)) (+ y-offset
                                                                                                                          (* (car posi) l))))))]))



(define (left-click frame)
  (class canvas%
    (define/override (on-event mouse-event)
      (cond [(eq? (send mouse-event get-event-type) 'left-down)
             (let* ((pos1 (which-square (send mouse-event get-x) (send mouse-event get-y)))
                    (pos (cons (cdr pos1) (car pos1))))
               (cond [(or (null? possible-moves) (not (in-list? (cdr possible-moves) pos))) (highlight (which-square (send mouse-event get-x)
                                                                                                                     (send mouse-event get-y)))]
                     [ else (begin
                              
                              (for ([i (range 8)])
                                (for ([j (range 8)])
                                  (vector-set! (vector-ref prev-pos i) j (vector-ref (vector-ref current-position i) j))))
                              (set-variable-board)
                              (let ((obj (get-og-piece (car pos) (cdr pos))))
                                (cond[(and (>= obj 1) (<= obj 5)) (set! captured-by-black (cons obj captured-by-black))]
                                     [(and (>= obj 7) (<= obj 11)) (set! captured-by-white (cons obj captured-by-white))]))
                              
                              (set! whose-move (- 1 whose-move))
                              (cond [(equal? (car possible-moves) (cons 7 0)) (set-mcar! (mcar castle-state) #f)])
                              (cond [(equal? (car possible-moves) (cons 7 7)) (set-mcdr! (mcar castle-state) #f)])
                              (cond [(equal? (car possible-moves) (cons 0 0)) (set-mcar! (mcdr castle-state) #f)])
                              (cond [(equal? (car possible-moves) (cons 0 7)) (set-mcdr! (mcdr castle-state) #f)])
                              (cond [(= (get-og-piece (caar possible-moves) (cdar possible-moves)) 6)
                                     (begin
                                       (set-mcar! castle-state (mcons #f #f))
                                       (set-mcar! kings  (mcons (car pos) (cdr pos))))]
                                    [(= (get-og-piece (caar possible-moves) (cdar possible-moves)) 12)
                                     (begin (set-mcdr! castle-state (mcons #f #f))
                                            (set-mcdr! kings (mcons (car pos) (cdr pos))))])
                              (cond [(and (= (get-og-piece (caar possible-moves) (cdar possible-moves))(+ 1 (* 6 (- 1 whose-move))))
                                          (= (car pos) (* 7 (- 1 whose-move))))
                                     (promo (car pos) (cdr pos))]
                                    [else (vector-set! (vector-ref current-position (car pos)) (cdr pos) (get-og-piece
                                                                                                          (caar possible-moves) (cdar possible-moves)))])
                                    
                              (set-previous-board) 
                              
                              (cond [(and (equal? (car possible-moves) (cons 7 4)) (equal? (get-og-piece 7 2) 6) (equal? pos (cons 7 2)))
                                     (begin (vector-set! (vector-ref current-position 7) 0 0)
                                            (vector-set! (vector-ref current-position 7) 3 4))])
                              (cond [(and (equal? (car possible-moves) (cons 7 4)) (equal? (get-og-piece 7 6) 6) (equal? pos (cons 7 6)))
                                     (begin (vector-set! (vector-ref current-position 7) 7 0)
                                            (vector-set! (vector-ref current-position 7) 5 4))])
                              (cond [(and (equal? (car possible-moves) (cons 0 4)) (equal? (get-og-piece 0 2) 12) (equal? pos (cons 0 2)))
                                     (begin (vector-set! (vector-ref current-position 0) 0 0)
                                            (vector-set! (vector-ref current-position 0) 3 10))])
                              (cond [(and (equal? (car possible-moves) (cons 0 4)) (equal? (get-og-piece 0 6) 12) (equal? pos (cons 0 6)))
                                     (begin (vector-set! (vector-ref current-position 0) 7 0)
                                            (vector-set! (vector-ref current-position 0) 5 10))])

                              (cond [(and (= (caar possible-moves) 3) (= (get-og-piece (caar possible-moves) (cdar possible-moves)) 1))
                                     (higher-order-en-passent (car possible-moves) pos - +)
                                     (higher-order-en-passent (car possible-moves) pos - -)]
                                    [(and (= (caar possible-moves) 4) (= (get-og-piece (caar possible-moves) (cdar possible-moves)) 7))
                                     (higher-order-en-passent (car possible-moves) pos + +)
                                     (higher-order-en-passent (car possible-moves) pos + -)])
                            
                              (cond [(checkmate? whose-move) (if (= whose-move 0)
                                                                 (begin (set! game-termination-text1 "Congratulations Black, you win!")
                                                                        (set! game-termination-text2 "Better luck next time, White")
                                                                        (send end-frame show #t))
                                                                 (begin (set! game-termination-text1 "Congratulations White, you win!")
                                                                        (set! game-termination-text2 "Better luck next time, Black")
                                                                        (send end-frame show #t)))]
                                    [(stalemate? whose-move) (set! game-termination-text1 "This match is a draw")
                                                             (set! game-termination-text2 "")
                                                             (send end-frame show #t)])
                              (set-variable-board)
                              (vector-set! (vector-ref current-position (caar possible-moves)) (cdar possible-moves) 0)
                              (set! possible-moves '())
                              (make-board canvas identity)                              
                              )
                            ]))]))
    (super-new [parent frame])))

(define (higher-order-en-passent a b f1 f2)
  (cond [(and (= (f1 (car a) 1) (car b)) (= (f2 (cdr a) 1) (cdr b)))
         (vector-set! (vector-ref current-position (car a)) (f2 (cdr a) 1) 0)]))

(define canvas (new (left-click frame)
                    [paint-callback
                     (lambda (canvas dc)
               
                       (make-board canvas identity))]))

(define (promo i j)
  (define promotion-canvas (begin (new button% [parent promotion-frame]
                                       [label "Queen"]
                                       [callback (lambda (button event) 
                                                   (vector-set! (vector-ref current-position i) j (+ 5 (* 6 (- 1 whose-move))))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  
                                  (new button% [parent promotion-frame]
                                       [label "Rook"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 4 (* 6 (- 1 whose-move))))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  
                                  (new button% [parent promotion-frame]
                                       [label "Bishop"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 3 (* 6 (- 1 whose-move))))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  (new button% [parent promotion-frame]
                                       [label "Knight"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 2 (* 6 (- 1 whose-move))))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])))
  (send promotion-frame show #t))

(define (promo2 i j)
  (define promotion-canvas (begin (new button% [parent promotion-frame]
                                       [label "Queen"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 5 (* 6 whose-move)))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  
                                  (new button% [parent promotion-frame]
                                       [label "Rook"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 4 (* 6 whose-move)))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  
                                  (new button% [parent promotion-frame]
                                       [label "Bishop"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 3 (* 6 whose-move)))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])
                                  (new button% [parent promotion-frame]
                                       [label "Knight"]
                                       [callback (lambda (button event)
                                                   (vector-set! (vector-ref current-position i) j (+ 2 (* 6 whose-move)))
                                                   (send promotion-frame show #f)
                                                   (make-board canvas identity))])))
  (send promotion-frame show #t)
  
  )
(define dc-chess (send canvas get-dc))

(define (two-player) (send frame show #t))
(define (one-player) (send frame1 show #t))













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vector-foldr op id vec)
  (define (helper i)
    (if (= i (vector-length vec)) id
        (op (vector-ref vec i) (helper (+ i 1)))))
  (helper 0))

(define (score board)
 
  (define (ind-score x y) (+ y (individual-score x)))
 
  (define score1 (vector-foldr + 0 (vector-map (lambda(x) (vector-foldr ind-score 0 x)) board)))
 
  (define white-pawns (parse-pawns 0 board))
  (define black-pawns (parse-pawns 1 board))
  (set! score1 (+ score1 (- (score-pawns white-pawns black-pawns #t)
                            (score-pawns black-pawns white-pawns #f))
                  (score-of-number-of-moves board) (score-castle board)))
  score1)

(define (score-castle board)
  (define cscore 0)
  (cond [(or (and (= (vector-ref (vector-ref board 0 ) 6) 12)
                  (= (vector-ref (vector-ref board 0 ) 5) 10))
             (and (= (vector-ref (vector-ref board 0 ) 2) 12)
                  (= (vector-ref (vector-ref board 0 ) 3) 10))) (set! cscore (- cscore 1.2))])
 
  (cond      [(or (and (= (vector-ref (vector-ref board 7 ) 6) 6)
                  (= (vector-ref (vector-ref board 7 ) 5) 4))
             (and (= (vector-ref (vector-ref board 7 ) 2) 6)
                  (= (vector-ref (vector-ref board 7 ) 3) 4))) (set! cscore (+ cscore 1.2))])
  cscore)

(define (parse-pawns n b)
  (define pawns (make-vector 8 '()))
  (define (f x) (if (= n 1) x (- 7 x)))
  (for ([i (build-list 8 f)])
    (for ([j (build-list 8 f)])
      (cond [(= (vector-ref (vector-ref b i) j) (+ 1 (* 6 n))) (vector-set! pawns j (cons i (vector-ref pawns j)))])))
  pawns)

(define (score-pawns white black n)
  (define g (if n >= <=))
  (define (rank xx) (if n (- 7 xx) xx)) 
  (define isolated #f)
  (define passed #f)
  (define score (foldr (lambda(xx yy) (begin 
  (if (null? (vector-ref white 1)) (set! isolated #t) (set! isolated #f))
  (if (and (null? (vector-ref black 0)) (or (null? (vector-ref black 1)) (g (car (vector-ref black 1)) xx)))
      (set! passed #t)
      (set! passed #f))
  (+ yy (pawn-individual isolated passed (rank xx))))) 0 (vector-ref white 0)))
  
  (for ([i (list 1 2 3 4 5 6)])
    (set! score (+ score (foldr (lambda(xx yy) (begin 
  (if (and (null? (vector-ref white (- i 1))) (null? (vector-ref white (+ i 1)))) (set! isolated #t) (set! isolated #f))
  (if (and (null? (vector-ref black i)) (or (null? (vector-ref black (+ i 1))) (g (car (vector-ref black (+ i 1))) xx))
           (or (null? (vector-ref black (- i 1))) (g (car (vector-ref black (- i 1))) xx)))
      (set! passed #t)
      (set! passed #f))
  (+ yy (pawn-individual isolated passed (rank xx))))) 0 (vector-ref white i)))))
  
  (set! score (+ score (foldr (lambda(xx yy) (begin 
  (if (null? (vector-ref white 6)) (set! isolated #t) (set! isolated #f))
  (if (and (null? (vector-ref black 7)) (or (null? (vector-ref black 6)) (g (car (vector-ref black 6)) xx)))
      (set! passed #t)
      (set! passed #f))
  (+ yy (pawn-individual isolated passed (rank xx))))) 0 (vector-ref white 7))))

  (set! score (- score (penalty-for-double white)))
  
  score)

(define (penalty-for-double vec)
  (define score 0)
  (cond [(> (length (vector-ref vec 0)) 1) (set! score (+ score (* 0.65 (- (length (vector-ref vec 0)) 1))))])
  (for ([i (range 1 7)])
  (cond [(> (length (vector-ref vec i)) 1) (set! score (+ score (* 0.5 (- (length (vector-ref vec i)) 1))))]))
  (cond [(> (length (vector-ref vec 7)) 1) (set! score (+ score (* 0.65 (- (length (vector-ref vec 7)) 1))))])
  score)
  
(define (pawn-individual isolated passed rank)
  (cond [(= rank 3)
         (if isolated
             (if passed 1.3 1.05)
             (if passed 1.55 1.15))]
        [(= rank 4)
         (if isolated
             (if passed 1.55 1.3)
             (if passed 2.3 1.35))]
        [(= rank 5)
         (if isolated
             (if passed 2.4 2.1)
             (if passed 3.5 2.15))]
        [(= rank 6) 3]                      ; Change this later
        [else 1]))

(define (individual-score x)
  (cond [ (or (= x 0) (= x 1) (= x 7)) 0]
        [ (= x 2) 3]
        [ (= x 3) 3]
        [ (= x 4) 5]
        [ (= x 5) 9]
        [ (= x 6) 10000]
        [ (= x 8) -3]
        [ (= x 9) -3]
        [ (= x 10) -5]
        [ (= x 11) -9]
        [ (= x 12) -10000]))

(define (make-all-possible board colour)
  (define answer '())
  (define go-through (if (= colour 1) (range 8) (reverse (range 8))))
  (for ([i go-through])
    (for ([j go-through])
      (cond [(= colour (- (ceiling (/ (get-piece i j board) 6)) 1))
             (set! answer (append (moves-for-a-piece i j board) answer))])))
  answer)

(define (make-all-legal-possible board colour)
  (define answer '())
  (define go-through (if (= colour 1) (range 8) (reverse (range 8))))
  (for ([i go-through])
    (for ([j go-through])
      (cond [(= colour (- (ceiling (/ (get-piece i j board) 6)) 1))
             (set! answer (append (legal-moves-for-a-piece i j board) answer))])))
  answer)

(define (any-legal? board colour)
  (define answer #t)
  (define go-through (if (= colour 1) (range 8) (reverse (range 8))))
  (for ([i go-through])
    (for ([j go-through])
      (cond [(= colour (- (ceiling (/ (get-piece i j board) 6)) 1))
             (set! answer (and answer (= (length (legal-moves-for-a-piece i j board)) 0)))])))
  (not answer))
(define (score-of-number-of-moves board)
  (define score 0)
  (for ([i (range 8)])
    (for ([j (range 8)])
      (let (( x (vector-ref (vector-ref board i ) j)))
       
      (cond [(or (= x 0) (= x 7) (= x 1)) (set! score score)]
            [(= x 4) (if (< number-of-moves 10) (set! score (+ score (* (/ 1 20) (length (rules i j x board)))))
                      (if (< number-of-moves 20) (set! score (+ score (* (/ 1 13) (length (rules i j x board)))))
                         (set! score (+ score (* (/ 1 10) (length (rules i j x board)))))))]
            [(= x 5) (if (< number-of-moves 10) (set! score (+ score (* (/ 1 25) (length (rules i j x board)))))
                      (if (< number-of-moves 20) (set! score (+ score (* (/ 1 15) (length (rules i j x board)))))
                         (set! score (+ score (* (/ 1 10) (length (rules i j x board)))))))]
            [(= x 10) (if (< number-of-moves 10) (set! score (- score (* (/ 1 20) (length (rules i j x board)))))
                      (if (< number-of-moves 20) (set! score (- score (* (/ 1 13) (length (rules i j x board)))))
                         (set! score (- score (* (/ 1 10) (length (rules i j x board)))))))]
            [(= x 11) (if (< number-of-moves 10) (set! score (- score (* (/ 1 25) (length (rules i j x board)))))
                      (if (< number-of-moves 20) (set! score (- score (* (/ 1 15) (length (rules i j x board)))))
                         (set! score (- score (* (/ 1 10) (length (rules i j x board)))))))]
           
            [(>= x 7) (begin   (set! score (- score (* (/ 1 10) (length (rules i j x board))))))]
            [else (set! score (+ score (* (/ 1 10) (length (rules i j x board)))))]))))
  score)
(define (moves-for-a-piece i j board)
  (map (lambda(pos)
(begin
  (define k 0)
  (define object-board (vector (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)))
  (myfor (set! k 0) : (< k 8) : (set! k (+ k 1)) :
    (vector-copy! (vector-ref object-board k) 0 (vector-ref board k)))
  (vector-set! (vector-ref object-board i) j 0)
  (vector-set! (vector-ref object-board (car pos)) (cdr pos) (get-piece i j board))
  (cond [(and (= i 7) (= j 4) (equal? pos (cons 7 2)) (equal? (get-piece 7 2 object-board) 6)) (begin
                                                              (vector-set! (vector-ref object-board 7) 0 0)
                                                              (vector-set! (vector-ref object-board 7) 2 4))]
        [(and (= i 7) (= j 4) (equal? pos (cons 7 6)) (equal? (get-piece 7 6 object-board) 6)) (begin
                                                              (vector-set! (vector-ref object-board 7) 7 0)
                                                              (vector-set! (vector-ref object-board 7) 5 4))]
        [(and (= i 0) (= j 4) (equal? pos (cons 0 2)) (equal? (get-piece 0 2 object-board) 12)) (begin
                                                              (vector-set! (vector-ref object-board 0) 0 0)
                                                              (vector-set! (vector-ref object-board 0) 2 10))]
        [(and (= i 0) (= j 4) (equal? pos (cons 0 6)) (equal? (get-piece 0 6 object-board) 12)) (begin
                                                              (vector-set! (vector-ref object-board 0) 7 0)
                                                              (vector-set! (vector-ref object-board 0) 5 10))])
  object-board))
         
         (rules i j (get-piece i j board) board)))

(define (legal-moves-for-a-piece i j board)
  (map (lambda(pos)
(begin
  (define k 0)
  (define object-board (vector (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)
                                  (make-vector 8 0)))
  (myfor (set! k 0) : (< k 8) : (set! k (+ k 1)) :
    (vector-copy! (vector-ref object-board k) 0 (vector-ref board k)))
  (vector-set! (vector-ref object-board i) j 0)
  (vector-set! (vector-ref object-board (car pos)) (cdr pos) (get-piece i j board))
  (cond [(and (= i 7) (= j 4) (equal? pos (cons 7 2)) (equal? (get-piece 7 2 object-board) 6)) (begin
                                                              (vector-set! (vector-ref object-board 7) 0 0)
                                                              (vector-set! (vector-ref object-board 7) 2 4))]
        [(and (= i 7) (= j 4) (equal? pos (cons 7 6)) (equal? (get-piece 7 6 object-board) 6)) (begin
                                                              (vector-set! (vector-ref object-board 7) 7 0)
                                                              (vector-set! (vector-ref object-board 7) 5 4))]
        [(and (= i 0) (= j 4) (equal? pos (cons 0 2)) (equal? (get-piece 0 2 object-board) 12)) (begin
                                                              (vector-set! (vector-ref object-board 0) 0 0)
                                                              (vector-set! (vector-ref object-board 0) 2 10))]
        [(and (= i 0) (= j 4) (equal? pos (cons 0 6)) (equal? (get-piece 0 6 object-board) 12)) (begin
                                                              (vector-set! (vector-ref object-board 0) 7 0)
                                                              (vector-set! (vector-ref object-board 0) 5 10))])
  object-board))
         
         (legal-moves (cons i j))))


(define (minimax depth alpha beta MaximisingPlayer current-board next-board f)
  
  (set-variable-board)
  
  (cond [(< depth 1) (begin (set! count (+ 1 count)) (mcons (score current-board) next-board) )]
        [MaximisingPlayer
         (begin
           (define v (mcons -100000 next-board))
            
           (define all-possible (f current-board 0))
           
           (define (helper i)
             (if (= i (length all-possible)) v
                 (if (>= alpha beta) v
                     (begin (if (= depth d) (let ((obj (minimax (- depth 1) alpha beta #f (list-ref all-possible i) (list-ref all-possible i)
                                                                make-all-possible)))
                                                  (cond [ (< (mcar v) (mcar obj)) (set! v (mcons (mcar obj) (list-ref all-possible i)))]))
                                (let ((obj (minimax (- depth 1) alpha beta #f (list-ref all-possible i) next-board f)))
                                                  (cond [(< (mcar v) (mcar obj)) (set! v obj)])))
                            (set! alpha (max alpha (mcar v)))
                            (helper (+ i 1))))))
           (helper 0))]
        [else (begin (define v (mcons 100000 next-board))
                     
                     
                     (define all-possible (f current-board 1))
                     
                     (define (helper i)
                       (if (= i (length all-possible)) v
                           (if (>= alpha beta) v
                              (begin (if (= depth d) (let ((obj (minimax (- depth 1) alpha beta #t (list-ref all-possible i) (list-ref all-possible i)
                                                                         make-all-possible)))
                                                       (cond [(> (mcar v) (mcar obj)) (set! v (mcons (mcar obj) (list-ref all-possible i)))]))
                                         (let ((obj (minimax (- depth 1) alpha beta #t (list-ref all-possible i) next-board f)))
                                           (cond [(> (mcar v) (mcar obj)) (set! v obj)])))
                                     (set! beta (min beta (mcar v)))
                                     (helper (+ i 1))))))
                     (helper 0))]))

(define (left-click1 frame)
  (if (= whose-move 0)
  (begin
    (cond [(checkmate? 0) (set! game-termination-text1 "The Computer wins!")
                          (set! game-termination-text2 "Better luck next time, White")
                          (send end-frame show #t)]
          [(stalemate? whose-move) (set! game-termination-text1 "This match is a draw")
                                   (set! game-termination-text2 "")
                                   (send end-frame show #t)])      
    (class canvas%
      (define/override (on-event mouse-event)
        (cond [(eq? (send mouse-event get-event-type) 'left-down)
               (let* ((pos1 (which-square (send mouse-event get-x) (send mouse-event get-y)))
                      (pos (cons (cdr pos1) (car pos1))))
                 (cond [(or (null? possible-moves) (not (in-list? (cdr possible-moves) pos))) (highlight1 (which-square (send mouse-event get-x)
                                                                                                                        (send mouse-event get-y)))]
                       [ else (begin
                                (let ((obj (get-og-piece (car pos) (cdr pos))))
                                (cond[(and (>= obj 7) (<= obj 11)) (set! captured-by-white (cons obj captured-by-white))]))
                                (cond [(equal? (car possible-moves) (cons 7 0)) (set-mcar! (mcar castle-state) #f)])
                                (cond [(equal? (car possible-moves) (cons 7 7)) (set-mcdr! (mcar castle-state) #f)])
                                (cond [(equal? (car possible-moves) (cons 0 0)) (set-mcar! (mcdr castle-state) #f)])
                                (cond [(equal? (car possible-moves) (cons 0 7)) (set-mcdr! (mcdr castle-state) #f)])
                                (cond [(= (get-og-piece (caar possible-moves) (cdar possible-moves)) 6)
                                       (begin
                                       (set-mcar! castle-state (mcons #f #f))
                                       (set-mcar! kings (mcons (car pos) (cdr pos))))]
                                      [(= (get-og-piece (caar possible-moves) (cdar possible-moves)) 12)
                                       (begin (set-mcdr! castle-state (mcons #f #f))
                                              (set-mcdr! kings (mcons (car pos) (cdr pos))))])
                                (cond [(and (= (get-og-piece (caar possible-moves) (cdar possible-moves))(+ 1 (* 6 whose-move)))
                                            (= (car pos) (* 7 whose-move)))
                                       ;(promo2 (car pos) (cdr pos))]
                                       (vector-set! (vector-ref current-position (car pos)) (cdr pos) (+ 5 (* 6 whose-move)))]
                                      ;(make-board1)
                                      
                                    [else (vector-set! (vector-ref current-position (car pos)) (cdr pos) (get-og-piece
                                                                                                          (caar possible-moves) (cdar possible-moves)))])
                                (set-previous-board)
                                (cond [(and (equal? (car possible-moves) (cons 7 4)) (equal? (get-og-piece 7 2) 6) (equal? pos (cons 7 2)))
                                       (begin (vector-set! (vector-ref current-position 7) 0 0)
                                              (vector-set! (vector-ref current-position 7) 3 4))])
                             
                                (cond [(and (equal? (car possible-moves) (cons 7 4)) (equal? (get-og-piece 7 6) 6) (equal? pos (cons 7 6)))
                                       (begin 
                                         (vector-set! (vector-ref current-position 7) 7 0)
                                         (vector-set! (vector-ref current-position 7) 5 4))])
                                (cond [(and (equal? (car possible-moves) (cons 0 4)) (equal? (get-og-piece 0 2) 12) (equal? pos (cons 0 2)))
                                       (begin (vector-set! (vector-ref current-position 0) 0 0)
                                            (vector-set! (vector-ref current-position 0) 3 10))])
                                (cond [(and (equal? (car possible-moves) (cons 0 4)) (equal? (get-og-piece 0 6) 12) (equal? pos (cons 0 6)))
                                       (begin (vector-set! (vector-ref current-position 0) 7 0)
                                              (vector-set! (vector-ref current-position 0) 5 10))])

                                (cond [(and (= (caar possible-moves) 3) (= (get-og-piece (caar possible-moves) (cdar possible-moves)) 1))
                                 (higher-order-en-passent (car possible-moves) pos - +)
                                 (higher-order-en-passent (car possible-moves) pos - -)])
                                
                                (vector-set! (vector-ref current-position (caar possible-moves)) (cdar possible-moves) 0)
                                (set! possible-moves '())
                                (make-board canvas1 (lambda(x) (- 1 x)))
                                (set! whose-move (- 1 whose-move))
                              (left-click1 frame1))
                              ]))]))
           (super-new [parent frame])))
  (let* ((i 0)
         (j 0)
         (i1 0)
         (j1 0))
    (set-variable-board)
    (let* ((next-board (if (not (check? 1)) (begin 
                                                   (mcdr (minimax d -100000 100000 #f current-position current-position make-all-legal-possible)))
                    (begin  (mcdr (minimax d -100000 100000 #f current-position current-position make-all-legal-possible))))))
    (begin 
    (set! number-of-moves (add1 number-of-moves))
     (display "You have played ") (display number-of-moves) (display " moves and have a current score of ") (display (score current-position)) 
     (newline)
     (cond [(checkmate? 1) (set! game-termination-text1 "You win!")
                           (set! game-termination-text2 "We'll make sure the computer tries harder next time")
                           (send end-frame show #t)]
           [(stalemate? whose-move) (set! game-termination-text1 "This match is a draw")
                                    (set! game-termination-text2 "")
                                    (send end-frame show #t)])
     (set! whose-move (- 1 whose-move))
    (myfor (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (myfor (set! j 0) : (< j 8) : (set! j (+ j 1)) :
      (cond [(not (= (vector-ref (vector-ref next-board i) j) (get-og-piece i j )))
             (make-changed-piece i j next-board)])))
    (let* ((original (find-white-pieces current-position))
           (final (find-white-pieces next-board)))
      (cond [(not (equal? original final)) (set! captured-by-black (cons (difference-in-lists original final)  captured-by-black))]))
    (myfor (set! i1 0) : (< i1 8) : (set! i1 (+ i1 1)) :
    (myfor (set! j1 0) : (< j1 8) : (set! j1 (+ j1 1)) :
      (vector-set! (vector-ref current-position i1) j1 (vector-ref (vector-ref next-board i1) j1))))
    (cond [(not (= (get-og-piece 0 4) 12)) (set-mcdr! castle-state (mcons #f #f))]
          [(not (= (get-og-piece 0 0) 10)) (set-mcar! (mcdr castle-state) #f)]
          [(not (= (get-og-piece 0 7) 10)) (set-mcdr! (mcdr castle-state) #f)])
    (set-kings)
    (for ([i (range 8)])
      (cond [(= (vector-ref (vector-ref current-position 7) i) 7)
             (begin (vector-set! (vector-ref current-position 7) i 11) (make-changed-piece 7 i current-position))]))
    (set-variable-board)
    (cond [(check? whose-move) (let* ((a (get-king-position whose-move))
                                                 (i (car a))
                                                 (j (cdr a)))
                                            (send (send canvas1 get-dc) draw-bitmap (cadddr (list-ref image-list
                                                                                         (vector-ref (vector-ref current-position i) j)))
                                                  (+ x-offset (* j l)) (+ y-offset (* i l))))])
    (left-click1 frame1))))))

(define (find-white-pieces board)
  (define pawn 0)
  (define k 0)
  (define b 0)
  (define r 0)
  (define q 0)
  (for ([i (range 8)])
    (for ([j (range 8)])
      (cond [ (= (vector-ref (vector-ref board i ) j) 1) (set! pawn (+ pawn 1))]
            [ (= (vector-ref (vector-ref board i ) j) 2) (set! k (+ k 1))]
            [ (= (vector-ref (vector-ref board i ) j) 3) (set! b (+ b 1))]
            [ (= (vector-ref (vector-ref board i ) j) 4) (set! r (+ r 1))]
            [ (= (vector-ref (vector-ref board i ) j) 5) (set! q (+ q 1))])))
  (list pawn k b r q))

(define (difference-in-lists a b)
  (define ans 0)
  (for ([i (range 5)]) (cond [(not (= (list-ref a i) (list-ref b i))) (set! ans i)]))
  (+ ans 1))

(define (set-kings)
  (define i 0)
  (define j 0)
  (while (not (= (get-og-piece i j) 12)) (cond [(= j 7) (begin (set! i (+ i 1)) (set! j 0))]
                                               [else (set! j (+ j 1))]))
  (set-mcdr! kings (mcons i j)))
  

(define canvas1 (new (left-click1 frame1)
                     [paint-callback
                      (lambda (canvas1 dc)    
                        (make-board canvas1 (lambda(x) (- 1 x))))]))

(define (make-changed-piece i j b)
  (if (= (get-piece i j b) 12)
      (send (send canvas1 get-dc) draw-bitmap (cadddr (cdr (list-ref image-list
                                                     (vector-ref (vector-ref b i) j))))
            (+ x-offset (* j l)) (+ y-offset (* i l)))
  (send (send canvas1 get-dc) draw-bitmap (cadddr (list-ref image-list
                                                     (vector-ref (vector-ref b i) j)))
            (+ x-offset (* j l)) (+ y-offset (* i l)))))

(define count 0)
(define d 4)
(define number-of-moves 0)

(send menu-frame show #t)