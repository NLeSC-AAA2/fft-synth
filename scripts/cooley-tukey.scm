#| Code generator for Cooley-Tukey FFT algorithm
 |#

(import (rnrs (6))
        (format)
        (arrays)
        (utility cut)
        (utility algorithms)
        (pfds sequences)
        (call-graphs))

(define pi 3.14159265359)

(define (format-symbol fmt . args)
  (string->symbol (apply format fmt args)))

(define (:* . args) (cons* 'call '× args))
(define (:+ . args) (cons* 'call '+ args))
(define (:- . args) (cons* 'call '- args))
(define (:wave-number k n) (list 'const (format-symbol "w{}" k)))

(define (wave-number k n) (exp (* -2i pi (/ k n))))

(define (fft! in out)
  (let* ((n (array-size in))
         (m (/ n 2)))
    (assert (= n (array-size out)))
    (if (= 1 n)
      (array-copy! in out)
      (begin
        (fft! (array-slice in 0 2 n)
              (array-slice out 0 1 m))
        (fft! (array-slice in 1 2 n)
              (array-slice out m 1 n))
        (do ((k 0 (+ k 1)))
            ((= k m) '())
          (let* ((t (array-ref out k))
                 (u (array-ref out (+ k m)))
                 (w (if (zero? k)
                        u
                        (:* (:wave-number k n) u))))
            (array-set! out k       (:+ t w))
            (array-set! out (+ k m) (:- t w))))))))

(define (fft x)
  (let* ((n   (vector-length x))
         (in  (make-array x n 1 0))
         (out (make-array (make-vector n) n 1 0)))
    (fft! in out)
    (array-data out)))

(define (symbolic-input-range name n)
  (list->vector
    (map (lambda (i) `(input ,name ,i)) (range n))))

(let* ((fft-expr  (fft (symbolic-input-range 'input 8)))
       (fft-graph (expression->call-graph
                    '((input 8))
                     (cons* 'output (vector->list fft-expr)))))
  (display (call-graph->dot fft-graph)) (newline))
