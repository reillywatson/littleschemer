#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))



(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (plus (add1 a) (sub1 b)))))) 

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (plus a (times a (sub1 b)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define greater
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (greater (sub1 m) (sub1 n))))))

(define less
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (less (sub1 m) (sub1 n))))))

(define numequals
  (lambda (m n)
    (cond
      ((less m n) #f)
      ((greater m n) #f)
      (else #t))))

(define exp
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (times m (exp m (sub1 n)))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (numequals a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (numequals n 1)))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (plus (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
        (eq? a (car l))
        (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost* (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (x y)
    (cond
      ((and (atom? x) (atom? y)) (eqan? x y))
      ((or (atom? x) (atom? y)) #f)
      (else (eqlist? x y)))))

(define rembers
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (rembers s (cdr l)))
      (else (cons (rembers s (car l)) (rembers s (cdr l)))))))

(define numberedatom?
  (lambda (a)
    (or (equal? a 'plus) (equal? a 'times) (equal? a 'exp) (number? a))))


(define numbered
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? l) (numberedatom? l))
      (else (and (numbered (car l)) (numbered (cdr l)))))))

(define firstsubexpr
  (lambda (expr)
    (car expr)))

(define secondsubexpr
  (lambda (expr)
    (car (cdr (cdr expr)))))

(define operatorofexpr
  (lambda (expr)
    (car (cdr expr))))

(define value
  (lambda (expr)
    (cond
      ((atom? expr) expr)
      ((null? (atom-to-function (operatorofexpr expr))) 0)
      (else ((atom-to-function (operatorofexpr expr)) (value (firstsubexpr expr)) (value (secondsubexpr expr)))))))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a 'plus) plus)
      ((eq? a 'times) times)
      ((eq? a 'exp) exp)
      (else #f))))

(define prefixop
  (lambda (expr)
    (car expr)))

(define prefixfirst
  (lambda (expr)
    (car (cdr expr))))

(define prefixsecond
  (lambda (expr)
    (car (cdr (cdr expr)))))

(define valuef
  (lambda (expr getop getfirst getsecond)
    (cond
      ((atom? expr) expr)
      ((eq? (getop expr) 'plus)
       (plus (valuef (getfirst expr) getop getfirst getsecond) (valuef (getsecond expr) getop getfirst getsecond)))
      ((eq? (getop expr) 'times)
       (times (valuef (getfirst expr) getop getfirst getsecond) (valuef (getsecond expr) getop getfirst getsecond)))
      ((eq? (getop expr) 'exp)
       (exp (valuef (getfirst expr) getop getfirst getsecond) (valuef (getsecond expr) getop getfirst getsecond)))
      (else 0))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define othermakeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (othermakeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (l)
    (set? (firsts l))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

; up to chapter 8, Lambda the Ultimate


;(define rember-f
 ; (lambda (test? a l)
  ;  (cond
   ;   ((null? l) '())
    ;  ((test? a (car l)) (cdr l))
     ; (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons new lat))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))


(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))

(define insert-g
  (lambda (inserter test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (inserter new old (cdr lat)))
        (else (cons (car lat) ((insert-g inserter test?) new old (cdr lat))))))))

;(define insertL-new (insert-g (lambda (new old lat) (cons new (cons old lat)))))
(define insertL-new
  (insert-g
   (lambda (new old l)
     (cons new (cons old l))) eq?))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-new
  (insert-g seqS eq?))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq (multirember-f eq?))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define eq?-tuna (eq?-c 'tuna))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

;(define multiinsertR
 ; (lambda (new old lat)
  ;  (cond
   ;   ((null? lat) '())
    ;  ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     ; (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL oldR) lat)
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L R)))))))

(define even?
  (lambda (n)
    (zero? (modulo n 2))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (times (car l) p) s))))
         (else (evens-only*&co (cdr l)
                              (lambda (newl p s)
                                (col newl p (plus (car l) s)))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl)
                                                     (times ap dp)
                                                     (plus as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))

; chapter 9: ...and again, and again, and again...

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))


(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (plus (length* (first pora)) (length* (second pora)))))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (plus (times (weight* (first pora)) 2)
             (weight* (second pora)))))))

; Ackermann function
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))