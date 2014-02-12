(define heap (make-vector 0))

(define empty
  (lambda (index)
    (= (length (vector-ref heap index)) 0)))

(define insert
  (lambda (val)
    (set! heap (list->vector (append (vector->list heap) (list val))))
    (upheap (- (vector-length heap) 1))))

(define startItUp
  (lambda ()
    (insert '((2 2) 1))
    (insert '((3 2) 1))
    (insert '((4 2) 1))
    (insert '((7 2) 1))
    (insert '((6 2) 1))
    (insert '((10 2) 1))
    (insert '((9 2) 1))
    (insert '((5 2) 1))
    (insert '((8 2) 1))
    (insert '((1 2) 1))))

(define front
  (lambda ()
    (vector-ref heap 0)))

(define taxidist
  (lambda (pt1)
    (+ (floor (abs (- (car goal) (car pt1)))) 
       (floor (abs (- (cadr goal) (cadr pt1)))))))

(define heap-empty?
  (lambda ()
    (= (vector-length heap) 0)))

(define priority
  (lambda (val)
    ;(display (cadr val))
    ;(newline) (newline)
    (if (equal? (car val) goal)
        -1
        (+ (length (cadr val)) (taxidist (car val))))))

(define upheap
  (lambda (index)
    (let ((parent (floor (abs (/ (- index 1) 2)))))
      (if (and (not (= index 0))
               (< (priority (vector-ref heap index))
                  (priority (vector-ref heap parent))))
          (begin
            (swap index parent)
            (upheap parent))))))

(define find-end
  (lambda (index)
    (if (empty index)
        index
        (find-end (- index 1)))))

(define depqueue
  (lambda () 
    (let ((ret (vector-ref heap 0))
          (last (- (vector-length heap) 1)))
      (swap 0 last)
      (vector-set! heap last '())
      (set! heap (list->vector (remove '() (vector->list heap))))
      (if (> (vector-length heap) 0)
        (downheap 0))
      ret)))

(define downheap
  (lambda (index)
    (let ((children (filter 
                     (lambda (val)
                       (not (eq? val -1)))
                     (list (if (< (+ (* index 2) 1) (vector-length heap))
                               (+ (* index 2) 1)
                               -1)
                           (if (< (+ (* index 2) 2) (vector-length heap))
                               (+ (* index 2) 2)
                               -1)))))
      (cond 
        ((= (length children) 0)
           #t)
        ((= (length children) 1)
           (if (< (priority (vector-ref heap (car children)))
                  (priority (vector-ref heap index)))
               (swap (car children) index)))
        ((= (length children) 2)
          (let ((nextIndex 
                 (if (< (priority (vector-ref heap (car children)))
                        (priority (vector-ref heap (cadr children))))
                     (+ (* index 2) 1)
                     (+ (* index 2) 2))))
            (if (< (priority (vector-ref heap nextIndex))
                   (priority (vector-ref heap index)))
                (begin
                  (swap nextIndex index)
                  (downheap nextIndex)))))))))
            
(define swap
  (lambda (child parent)
    (let ((temp (vector-ref heap parent)))
      (vector-set! heap parent (vector-ref heap child))
      (vector-set! heap child temp))))