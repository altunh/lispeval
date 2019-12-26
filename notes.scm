;;; Accumulator
(define (accumulator starting)
  (lambda (incr)
    (begin
      (set! starting (+ starting incr))
      starting)))
(define acc (accumulator 0))

;;; Stateful Function as Object
(define (make-account balance)
  (lambda (msg amt)
    (cond ((eq? msg 'deposit) (begin (set! balance (+ balance amt))
                              balance))
          ((eq? msg 'withdraw) (begin (set! balance (- balance amt))
                              balance)))))

(define act (make-account 100)) ;Value: act
(act 'deposit 10) ;Value: 110
(act 'deposit 10) ;Value: 120
(act 'withdraw 50) ;Value: 70

(define (make-account balance)
  (lambda (msg)
    (cond ((eq? msg 'deposit) (lambda (amt)
                                (begin (set! balance (+ balance amt))
                                       balance)))
          ((eq? msg 'withdraw) (lambda (amt)
                                 (begin (set! balance (- balance amt))
                                        balance))))))

(define act (make-account 100)) ;Value: act
((act 'deposit) 10) ;Value: 110
((act 'withdraw) 100) ;Value: 10
