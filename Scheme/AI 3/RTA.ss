(define target '())
(define bigFrontiers (make-vector 0))

(define search
  (lambda (grid stop-count)
    (set! target goal)
    (search2 grid 0 stop-count)))

(define get-frontiers
  (lambda (grid x y)
    (let ((frontiers '()))
      (filter
       (lambda (n)
         (not (= (length n) 0)))
       (append
        (list
         (if (and (> x 0) (< (get-node grid (- x 1) y) 1))
             (list (- x 1) y)
             '())
         (if (and (< x (- num-col-row 1)) (< (get-node grid (+ x 1) y) 1))
             (list (+ x 1) y)
             '()))
        (list
         (if (and (> y 0) (< (get-node grid x (- y 1)) 1))
             (list x (- y 1))
             '())
         (if (and (< y (- num-col-row 1)) (< (get-node grid x (+ y 1)) 1))
             (list x (+ y 1))
             '())))))))

(define load-frontiers
  (lambda (frontiers)
    (if (not (null? frontiers))
        (begin
          (insert (list (car frontiers) 1))
          (draw-frontier (caar frontiers) (cadar frontiers))
          (load-frontiers (cdr frontiers)))
        (begin
          (set! bigFrontiers heap)
          (set! heap (make-vector 0))))))

(define reheap
  (lambda (x y grid stop-count)
    (let ((in (vector->list bigFrontiers)))
      (set! bigFrontiers (make-vector 0))
      (reheap2 in x y grid stop-count))))

(define reheap2
  (lambda (lst x y grid stop-count)
    (if (not (null? lst))
        (begin
          (set! heap (make-vector 0))
          (set! target (caar lst))
          (set! robot (list x y))
          (let ((oldVal (get-node grid (car target) (cadr target))))
            (set-node! grid (car target) (cadr target) 1)
            (let ((dist (length (searchbb grid stop-count))))
              (set-node! grid (car target) (cadr target) oldVal)
              (set! heap bigFrontiers)
              (set! target goal)
              (if (not (contains (vector->list bigFrontiers) (list (caar lst) dist)))
                  (insert (list (caar lst) dist)))
              (set! bigFrontiers heap)
              (reheap2 (cdr lst) x y grid stop-count)))))))

(define cleanhouse
  (lambda (count x y)
    (draw-path-node x y)
    (list (list x y) count)))

(define search2
  (lambda (grid count stop-count)
    (let ((x (robot-x))
          (y (robot-y)))
      (pause pause-num)
      (draw-visited x y)
      (set-node! grid x y 1)
      (draw-moved-robot x y)
      (if (or (equal? robot goal)
              (>= count stop-count))
          (cleanhouse count x y)
          (let ((frontiers (get-frontiers grid x y)))
            (set! heap bigFrontiers)
            (load-frontiers frontiers)
            (reheap x y grid stop-count)
            (set! robot (list x y))
            (if (< (vector-length bigFrontiers) 1)
                (begin
                  (display "NO PATH FOUND")
                  #f)
                (let ((next (depqueue)))
                  (set! bigFrontiers heap)
                  (if (not (contains frontiers (car next)))
                      (begin
                        (set! heap (make-vector 0))
                        (set! target (car next))
                        (set-node! grid (car target) (cadr target) 1)
                        (let ((path (searchbb grid stop-count)))
                          (slow-move-just-cuz path)
                          (set! heap bigFrontiers)
                          (set! target goal)
                          (set! robot (car next))
                          (let ((ref (search2 grid (+ count (length path)) stop-count)))
                            (if (and (list? ref)
                                     (contains frontiers (car ref))
                                     (< count (cadr ref)))
                                (begin
                                  (markFullPath path)
                                  (cleanhouse count x y))
                                ref))))
                      (begin
                        (set! robot (car next))
                        (let ((ref (search2 grid (+ count 1) stop-count)))
                          (if (and (list? ref)
                                   (contains frontiers (car ref))
                                   (< count (cadr ref)))
                              (cleanhouse count x y)
                              ref)))))))))))

(define markFullPath
  (lambda (path)
    (if (not (null? path))
        (begin
          (draw-path-node (caar path) (cadar path))
          (markFullPath (cdr path))))))

(define slow-move-just-cuz
  (lambda (path)
    (if (not (null? path))
        (let ((next (car path)))
          (pause 1000000)
          (draw-moved-robot (car next) (cadr next))
          (slow-move-just-cuz (cdr path))))))