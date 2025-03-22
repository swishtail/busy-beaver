;;; Busy Beaver

(define (tape-read  t)   (cadr t))
(define (tape-write t s) (list (car t) s (caddr t)))
(define (tape-left  t)   (list (cdar t) (caar t) (cons (cadr t) (caddr t))))
(define (tape-right t)   (list (cons (cadr t) (car t)) (caaddr t) (cdaddr t)))

(define (card-number c) (car c))
(define (zero-branch c) (cadr c))
(define (one-branch  c) (caddr c))

(define (branch-symbol    b) (car b))
(define (branch-next-card b) (caddr b))
(define (branch-move      b) (if (zero? (cadr b)) tape-left tape-right))

(define (head-exec t c)
  (define (output b)
    (list ((branch-move b)
           (tape-write t (branch-symbol b)))
          (branch-next-card b)))
  (if (zero? (tape-read t))
      (output (zero-branch c))
      (output (one-branch c))))

(define (run-machine head tape cards)
  (begin (display-tape tape)
         (let iter ((next-tape tape)
                    (next-card (cadr cards)))
           (if (zero? (card-number next-card))
               (begin (display 'halt) (newline))
               (let ((run-head (head next-tape next-card)))
                 (begin (display-tape (car run-head))
                        (iter (car run-head)
                              (list-ref cards (cadr run-head)))))))))

(define (make-tape n)
  (define (zeros n)
    (if (zero? n)
        '()
        (cons 0 (zeros (- n 1)))))
  (let ((halfway (floor (/ n 2))))
    (if (odd? n)
        (list (zeros halfway) 0 (zeros halfway))
        (list (zeros halfway) 0 (zeros (- halfway 1))))))

(define (display-tape t)
  (display
   (append (reverse (car t))
           (cons (list (cadr t)) (caddr t))))
  (newline))

;;; Examples:
(define tape (make-tape 21))

(define 2-cards
  '((0)
    (1 (1 1 2) (1 0 2))
    (2 (1 0 1) (1 1 0))))

(define 3-cards
  '((0)
    (1 (1 1 2) (1 1 0))
    (2 (0 1 3) (1 1 2))
    (3 (1 0 3) (1 0 1))))

(define 4-cards
  '((0)
    (1 (1 1 2) (1 0 2))
    (2 (1 0 1) (0 0 3))
    (3 (1 1 0) (1 0 4))
    (4 (1 1 4) (0 1 1))))

(run-machine head-exec tape 2-cards)
(run-machine head-exec tape 3-cards)
(run-machine head-exec tape 4-cards)