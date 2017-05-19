#!/usr/bin/racket

#lang racket/gui
(require math/matrix)
(require math/number-theory)

(define alpha->num (hash #\a 0 #\b 1 #\c 2 #\d 3 #\e 4
                         #\f 5 #\g 6 #\h 7 #\i 8 #\j 9
                         #\k 10 #\l 11 #\m 12 #\n 13 #\o 14
                         #\p 15 #\q 16 #\r 17 #\s 18 #\t 19
                         #\u 20 #\v 21 #\w 22 #\x 23 #\y 24
                         #\z 25 #\space  26 #\! 27 #\. 28 #\, 29
                         #\; 30 #\: 31 #\? 32 #\( 33 #\) 34
                         #\& 35 #\/ 36 #\' 37 #\^ 38 #\* 39
                         #\% 40 #\A 41 #\B 42 #\C 43 #\D 44
                         #\E 45 #\F 46 #\G 47 #\H 48 #\I 49
                         #\J 50 #\K 51 #\L 52 #\M 53 #\N 54
                         #\O 55 #\P 56 #\Q 57 #\R 58 #\S 59
                         #\T 60 #\U 61 #\V 62 #\W 63 #\X 64
                         #\Y 65 #\Z 66 #\` 67 #\æ 68 #\ø 69
                         #\å 70 #\- 71 #\£ 72))

(define num->alpha (hash 0 #\a 1 #\b 2 #\c 3 #\d 4 #\e
                         5 #\f 6 #\g 7 #\h 8 #\i 9 #\j
                         10 #\k 11 #\l 12 #\m 13 #\n 14 #\o
                         15 #\p 16 #\q 17 #\r 18 #\s 19 #\t
                         20 #\u 21 #\v 22 #\w 23 #\x 24 #\y
                         25 #\z 26 #\space  27 #\! 28 #\. 29 #\,
                         30 #\; 31 #\: 32 #\? 33 #\( 34 #\)
                         35 #\& 36 #\/ 37 #\' 38 #\^ 39 #\*
                         40 #\% 41 #\A 42 #\B 43 #\C 44 #\D
                         45 #\E 46 #\F 47 #\G 48 #\H 49 #\I
                         50 #\J 51 #\K 52 #\L 53 #\M 54 #\N
                         55 #\O 56 #\P 57 #\Q 58 #\R 59 #\S
                         60 #\T 61 #\U 62 #\V 63 #\W 64 #\X
                         65 #\Y 66 #\Z 67 #\` 68 #\æ 69 #\ø
                         70 #\å 71 #\- 72 #\£))

(define (cipher str key)
  (define num-list (map (λ (x) (hash-ref alpha->num x)) (string->list str)))
  (define enc-list (build-list (length key) (λ (x) (modulo (foldl + 0 (map * (list-ref key x) num-list)) (hash-count alpha->num)))))
  (define ret-list (map (λ (x) (hash-ref num->alpha x)) enc-list))
  (list->string ret-list))
  
(define (apply-cipher msg key)
  (define correction (make-string (modulo (- (length key) (modulo (string-length msg) (length key))) (length key)) #\space))
  (define (apply-cipher-aux msg enc-msg key)
    (if (zero? (string-length msg))
        enc-msg
        (apply-cipher-aux (substring msg (length key))
                          (string-append enc-msg (cipher (substring msg 0 (length key)) key))
                          key)))
  (apply-cipher-aux (string-append msg correction) "" key))

(define (make-inverse-key key)
  (define key-matrix (build-matrix (length key) (length key) (λ (m n) (list-ref (list-ref key m) n))))
  (define key-det (matrix-determinant key-matrix))
  (define inv-key (matrix-map (λ (x) (modulo x (hash-count alpha->num)))
                              (matrix-map (λ (x) (* x (* key-det (modular-inverse key-det (hash-count alpha->num)))))
                                          (matrix-inverse key-matrix))))
  (matrix->list* inv-key))

(define (check-key key)
  (define key-matrix (build-matrix (length key) (length key) (λ (m n) (list-ref (list-ref key m) n))))
  (define key-det (matrix-determinant key-matrix))
  (cond
    [(zero? key-det) #f]
    [(not (coprime? key-det (hash-count alpha->num))) #f]
    [else #t]))

;############ Build GUI.############

;;Callbacks
(define (set-key-button-callback button event)
  (if (check-key (read (open-input-string (send key-entry get-value))))
      (message-box "Key Check" "Key OK ")
      (message-box "Key Check" "Invalid Key ")))

(define (encrypt-button-callback button event)
  (send encrypted-entry delete 0 1000)
  (send encrypted-entry insert (apply-cipher (string-normalize-spaces (send plain-entry get-text))
                                             (read (open-input-string (send key-entry get-value))))))

(define (decrypt-button-callback button event)
  (send plain-entry delete 0 1000)
  (send plain-entry insert (apply-cipher (string-normalize-spaces (send encrypted-entry get-text))
                                         (make-inverse-key (read (open-input-string (send key-entry get-value)))))))

;;Window, text areas and buttons
(define window (new frame%
                    [label "Hill Cipher"]
                    [width 600]
                    [height 400]))
                    
(define key-entry (new text-field% [label " Key: "]
                       [parent window]
                       [init-value "((321 123 212) (242 412 187) (112 197 216))"])) ;hardcode in a default key. 
                       
(define plain-entry (new text% [auto-wrap #t]))

(define encrypted-entry (new text% [auto-wrap #t]))

(define plain-canvas (new editor-canvas% [parent window]
                          [editor plain-entry]
                          [min-height 180]
                          [style '(no-hscroll no-vscroll)]))

(define encrypted-canvas (new editor-canvas% [parent window]
                              [editor encrypted-entry]
                              [min-height 180]
                              [style '(no-hscroll no-vscroll)]))

(define button-panel (new horizontal-panel% [parent window]
                          [alignment '(center top)]))

(define encrypt-button (new button% [label "Encrypt!"]
                            [parent button-panel]
                            [callback encrypt-button-callback]))

(define decrypt-button (new button% [label "Decrypt!"]
                            [parent button-panel]
                            [callback decrypt-button-callback]))

(define key-check-button (new button% [label "Check Key"]
                              [parent button-panel]
                              [callback set-key-button-callback]))

;;Default values in text areas
(send plain-entry insert "Enter plain text here...")
(send encrypted-entry insert "Or enter cipher text here...")

;;Initialise. 
(send window show #t)
