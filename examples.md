```scheme
(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...) (void)))))

(let ((i 0))
  (when (= i 0)
    (display "i == 0")
    (newline)))
; i == 0

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))


(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (1+ i)))))))

(let ((i 0))
  (while (< i 10)
    (display i)
    (display #\Space)
    (set! i (+ i 1))))
;0 1 2 3 4 5 6 7 8 9

(for (i 0 10)
  (display i)
  (display #\Space))
;0 1 2 3 4 5 6 7 8 9

(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))
    ((_ x i) (begin (set! x (+ x i)) x))))

(let ((i 0) (j 0))
  (incf i)
  (incf j 3)
  (display (list 'i '= i))
  (newline)
  (display (list 'j '= j)))
```