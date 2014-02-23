(define get-next-robot 
  (lambda (point)
    (let ((val (r-minimax 3 #t point goal)))
      (set! robot point)
      val)))

(define r-get-frontiers
  (lambda (x y)
    (let ((frontiers '()))
      (filter
       (lambda (n)
         (not (= (length n) 0)))
       (append
        (list
         (if (and (> x 0) (not (= (get-node grid (- x 1) y) obstacle)))
             (list (- x 1) y)
             '())
         (if (and (< x (- num-col-row 1)) (not (= (get-node grid (+ x 1) y) obstacle)))
             (list (+ x 1) y)
             '()))
        (list
         (if (and (> y 0) (not (= (get-node grid x (- y 1)) obstacle)))
             (list x (- y 1))
             '())
         (if (and (< y (- num-col-row 1)) (not (= (get-node grid x (+ y 1)) obstacle)))
             (list x (+ y 1))
             '())))))))

(define expand
  (lambda (plyes max pos targ frontiers)
    (let ((newPos
           (if max
               (append (list

(define r-minimax
  (lambda (plyes max pos targ)
    (if (= plyes 0)
        (begin
          (set! robot pos)
          (set! target targ)
          (set! r-visited '())
          (set! r-heap (make-vector 0))
          (length (r-searchbb 20000)))
        (let ((children (expand plyes max pos targ (r-get-frontiers (car pos) (cadr pos)))))

(define r-visited '())
(define r-target goal)

(define r-searchbb
  (lambda (stop-count)
    (car (r-search2bb 0 stop-count 0))))

(define r-get-frontiersbb
  (lambda (x y)
    (let ((frontiers '()))
      (filter
       (lambda (n)
         (not (= (length n) 0)))
       (append
        (list
         (if (and (> x 0) (not (= (get-node grid (- x 1) y) obstacle)) (not (r-contains (list (- x 1) y))))
             (list (- x 1) y)
             '())
         (if (and (< x (- num-col-row 1)) (not (= (get-node grid (+ x 1) y) obstacle)) (not (r-contains (list (+ x 1) y))))
             (list (+ x 1) y)
             '()))
        (list
         (if (and (> y 0) (not (= (get-node grid x (- y 1)) obstacle)) (not (r-contains (list x (- y 1)))))
             (list x (- y 1))
             '())
         (if (and (< y (- num-col-row 1)) (not (= (get-node grid x (+ y 1)) obstacle)) (not (r-contains (list x (+ y 1)))))
             (list x (+ y 1))
             '())))))))

(define r-load-frontiersbb
  (lambda (frontiers path)
    (if (not (null? frontiers))
        (begin
          (r-insert (list (car frontiers) (+ path 1)))
          (r-load-frontiersbb (cdr frontiers) path)))))

(define r-cleanhousebb
  (lambda (x y path route)
    (list (append (list (list x y)) route) path)))

(define r-contains
  (lambda (lst val)
    (not (equal? (find
                  (lambda (x)
                    (equal? x val)) lst) #f))))

(define r-search2bb
  (lambda (count stop-count path)
    (let ((x (robot-x))
          (y (robot-y)))
      (set! r-visited (append (list (list x y)) r-visited))
      (if (or (equal? robot r-target)
              (>= count stop-count))
          (begin
            (r-cleanhousebb x y path '()))
          (let ((frontiers (r-get-frontiersbb x y)))
            (r-load-frontiersbb frontiers path)
            (if (< (vector-length heap) 1)
                (begin
                  (display "NO PATH FOUND IN BB")
                  #f)
                (let ((next (r-depqueue)))
                  (set! robot (car next))
                  (let ((ref (r-search2bb (+ count 1) stop-count (cadr next))))
                    (if (and (list? ref)
                             (r-contains frontiers (caar ref))
                             (< path (cadr ref)))
                        (r-cleanhousebb x y path (car ref))
                        ref)))))))))

(define r-heap (make-vector 0))

(define r-empty
  (lambda (index)
    (= (length (vector-ref r-heap index)) 0)))

(define r-insert
  (lambda (val)
    (set! r-heap (list->vector (append (vector->list r-heap) (list val))))
    (r-upheap (- (vector-length r-heap) 1))))

(define r-front
  (lambda ()
    (vector-ref r-heap 0)))

(define r-taxidist
  (lambda (pt1)
    (+ (floor (abs (- (car r-target) (car pt1)))) 
       (floor (abs (- (cadr r-target) (cadr pt1)))))))

(define r-heap-empty?
  (lambda ()
    (= (vector-length r-heap) 0)))

(define r-priority
  (lambda (val)
    (if (equal? (car val) r-target)
        -1
        (+ (cadr val) (r-taxidist (car val))))))

(define r-upheap
  (lambda (index)
    (let ((parent (floor (abs (/ (- index 1) 2)))))
      (if (and (not (= index 0))
               (or (< (r-priority (vector-ref r-heap index))
                      (r-priority (vector-ref r-heap parent)))
                   (and (= (r-priority (vector-ref r-heap index))
                           (r-priority (vector-ref r-heap parent)))
                        (< (r-taxidist (car (vector-ref r-heap index)))
                           (r-taxidist (car (vector-ref r-heap parent)))))))
          (begin
            (r-swap index parent)
            (r-upheap parent))))))

(define r-find-end
  (lambda (index)
    (if (empty index)
        index
        (r-find-end (- index 1)))))

(define r-depqueue
  (lambda () 
    (let ((ret (vector-ref r-heap 0))
          (last (- (vector-length r-heap) 1)))
      (r-swap 0 last)
      (vector-set! r-heap last '())
      (set! r-heap (list->vector (remove '() (vector->list r-heap))))
      (if (> (vector-length r-heap) 0)
        (r-downheap 0))
      ret)))

(define r-downheap
  (lambda (index)
    (let ((children (filter 
                     (lambda (val)
                       (not (eq? val -1)))
                     (list (if (< (+ (* index 2) 1) (vector-length r-heap))
                               (+ (* index 2) 1)
                               -1)
                           (if (< (+ (* index 2) 2) (vector-length r-heap))
                               (+ (* index 2) 2)
                               -1)))))
      (cond 
        ((= (length children) 0)
           #t)
        ((= (length children) 1)
           (if (< (r-priority (vector-ref r-heap (car children)))
                  (r-priority (vector-ref r-heap index)))
               (r-swap (car children) index)))
        ((= (length children) 2)
          (let ((nextIndex 
                 (if (< (r-priority (vector-ref r-heap (car children)))
                        (r-priority (vector-ref r-heap (cadr children))))
                     (+ (* index 2) 1)
                     (+ (* index 2) 2))))
            (if (or (< (r-priority (vector-ref r-heap nextIndex))
                       (r-priority (vector-ref r-heap index)))
                    (and (= (r-priority (vector-ref r-heap nextIndex))
                            (r-priority (vector-ref r-heap index)))
                         (< (r-taxidist (car (vector-ref r-heap nextIndex)))
                            (r-taxidist (car (vector-ref r-heap index))))))
                (begin
                  (r-swap nextIndex index)
                  (r-downheap nextIndex)))))))))
            
(define r-swap
  (lambda (child parent)
    (let ((temp (vector-ref r-heap parent)))
      (vector-set! r-heap parent (vector-ref heap child))
      (vector-set! r-heap child temp))))