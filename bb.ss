;;; Busy Beaver
(import (rnrs))

(define make-tape
  (lambda (n)
    (letrec ((zeros
              (lambda (n)
                (if (zero? n)
                    '()
                    (cons 0 (zeros (- n 1)))))))
      (let ((halfway (floor (/ n 2))))
        (if (odd? n)
            (list (zeros halfway) 0 (zeros halfway))
            (list (zeros halfway) 0 (zeros (- halfway 1))))))))

(define display-tape
  (lambda (t)
    (display
     (append (reverse (car t))
             (cons (list (cadr t)) (caddr t))))
    (newline)))

(define tape-read
  (lambda (t)
    (cadr t)))

(define tape-write
  (lambda (t s)
    (list (car t) s (caddr t))))

(define tape-left
  (lambda (t)
    (list (cdar t) (caar t) (cons (cadr t) (caddr t)))))

(define tape-right
  (lambda (t)
    (list (cons (cadr t) (car t)) (caaddr t) (cdaddr t))))

(define card-number
  (lambda (c)
    (car c)))

(define zero-branch
  (lambda (c)
    (cadr c)))

(define one-branch
  (lambda (c)
    (caddr c)))

(define branch-symbol
  (lambda (b)
    (car b)))

(define branch-next-card
  (lambda (b)
    (caddr b)))

(define branch-move
  (lambda (b)
    (if (zero? (cadr b))
        tape-left
        tape-right)))

(define head-exec
  (lambda (t c)
    (letrec ((output
              (lambda (b)
                (list ((branch-move b)
                       (tape-write t (branch-symbol b)))
                      (branch-next-card b)))))
      (if (zero? (tape-read t))
          (output (zero-branch c))
          (output (one-branch c))))))

(define run-machine
  (lambda (head tape cards)
    (begin
      (display-tape tape)
      (let machine-loop ((next-tape tape) (next-card (cadr cards)))
        (if (zero? (card-number next-card))
            (begin
              (display "halt")
              (newline))
            (let ((run-head (head next-tape next-card)))
              (begin
                (display-tape (car run-head))
                (machine-loop (car run-head)
                              (list-ref cards (cadr run-head))))))))))


;; Examples

(define the-tape (make-tape 21))

(define two-cards
  '((0)
    (1 (1 1 2) (1 0 2))
    (2 (1 0 1) (1 1 0))))

(define three-cards
  '((0)
    (1 (1 1 2) (1 1 0))
    (2 (0 1 3) (1 1 2))
    (3 (1 0 3) (1 0 1))))

(define four-cards
  '((0)
    (1 (1 1 2) (1 0 2))
    (2 (1 0 1) (0 0 3))
    (3 (1 1 0) (1 0 4))
    (4 (1 1 4) (0 1 1))))

(run-machine head-exec the-tape two-cards)
(run-machine head-exec the-tape three-cards)
(run-machine head-exec the-tape four-cards)
