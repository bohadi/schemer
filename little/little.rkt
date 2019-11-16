#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(atom? (quote ()))
(atom? (quote atom))
(atom? 'atom)
(atom? 'turkey)
(atom? 1492)
(atom? 'u)
(atom? '*abc$)
(atom? '(harry had a heap of apples))

(list? '(atom))
(list? '((atom turkey) or))

'(((how) are) ((you) (doing so)) far)

(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdogs)) (and) (pickle) relish)))
(cdr '(1 2 3))
(cdr '(123))

(cons 'peanut '(butter and jelly))

(null? '())

(eq? 'Harry 'Harry)
(eq? '(a b) '(a b))
(eq? 5 5)
(eq? (car '(Mary had a)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)

(define lat?
  (lambda (x)
    (or (null? x) (and (atom? (car x)) (lat? (cdr x))))))
(lat? '())
(lat? '(1))
(lat? '(1 2))
(lat? '(1 2 (3)))
(lat? '((1)))
(lat? '(1 (2 3) 4))
(lat? '(1 2 3 4))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))
(member? 4 '(1 2 3))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat))) ))))
(rember 0 '())
(rember 0 '(1 2 3))
(rember 1 '(1 2 3))
(rember 2 '(1 2 3))
(rember 3 '(1 2 3))
(rember 1 '(1 2 1 3 1))
(rember 'sauce '(soy sauce and tomato sauce))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (car l)) (firsts (cdr l)))
      (else (cons (car (car l)) (firsts (cdr l)))))))
(firsts '())
(firsts '((a b) (1 2)))
(firsts '((a b) (0) (10 20)))
(firsts '((a b) () (10 20)))
(firsts '((1 2) (10 20) (100 200)))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )))
(insertR 'cone 'cream '(ice cream with fudge))
(insertR 'pepper 'and '(tacos tamales and salsa))
(insertR 3 2 '(1 2 4 5))
(insertR 3 2 '(1 2 2 1))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat))))
      )))
(insertL 'pepper 'and '(tacos tamales and salsa))
(insertL 2 3 '(1 3 4 5))
(insertL 2 3 '(1 3 3 2 1)) 
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat))))
      )))
(subst 20 0 '(1 2 3))
(subst 20 2 '(1 2 3))
(subst 20 2 '(1 2 3 2 1))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (or (eq? o1 (car lat)) (eq? o2 (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))
      )))
(subst2 200 0 10 '(1 2 3))
(subst2 200 2 20 '(1 2 20))
(subst2 200 2 20 '(1 20 2))
(subst2 200 2 20 '(1 3 5 20 2))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat))) ))))
(multirember 0 '())
(multirember 0 '(1 2 3))
(multirember 1 '(1 2 3))
(multirember 2 '(1 2 3))
(multirember 3 '(1 2 3))
(multirember 1 '(1 2 1 3 1))
(multirember 'sauce '(soy sauce and tomato sauce))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat))))
      )))
(multiinsertR 3 0 '(1 2 3))
(multiinsertR 3 2 '(1 2 4 5))
(multiinsertR 3 2 '(1 2 2))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat))))
      )))
(multiinsertL 3 0 '(1 2 3))
(multiinsertL 3 2 '(1 2 4 5))
(multiinsertL 3 2 '(1 2 2))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat))))
      )))
(multisubst 10 0 '(1 2 3))
(multisubst 10 1 '(1 2 3))
(multisubst 10 1 '(1 2 1 3))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add (add1 n) (sub1 m)))
      )))
(add 0 0)
(add 3 0)
(add 3 5)
(add -5 1)

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub (sub1 n) (sub1 m)))
      )))
(sub 0 0)
(sub 3 0)
(sub 5 3)
(sub -5 1)
#;(sub -5 -1)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup))))
      )))
(addtup '())
(addtup '(1 'a 3))
(addtup '(1 2 3))

(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m))))
      )))
(mult 0 0)
(mult 0 1)
(mult 1 0)
(mult 1 1)
(mult 2 2)
(mult 2 3)
(mult 5 2)
(mult 8 6)

(define tup+
  (lambda (u v)
    (cond
      ((null? u) v)
      ((null? v) u)
      (else (cons (add (car u) (car v)) (tup+ (cdr u) (cdr v))))
      )))
(tup+ '() '())
(tup+ '() '(1 2))
(tup+ '(1) '())
(tup+ '(1 4) '(8))
(tup+ '(1 2 3) '(10 20 30 40))
(tup+ '(1 -1 3) '(6 8 4 7))

(define gt?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt? (sub1 n) (sub1 m)))
      )))
(gt? 0 0)
(gt? 0 1)
(gt? 1 0)
(gt? 1 1)
(gt? 100 1)
(gt? 1 100)

(define lt?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt? (sub1 n) (sub1 m)))
      )))
(lt? 0 0)
(lt? 0 1)
(lt? 1 0)
(lt? 1 1)
(lt? 100 1)
(lt? 1 100)

(define =?
  (lambda (n m)
    (cond
      ((gt? n m) #f)
      ((lt? n m) #f)
      (else #t))))
(=? 0 0)
(=? 0 1)
(=? 1 0)
(=? 13 13)
(=? 10 100)

(define expo
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (expo n (sub1 m))))
      )))
(expo 1 1)
(expo 2 3)
(expo 5 3)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
(length '())
(length '(hello world))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(pick 0 '(hello world))
(pick 1 '(hello world))
(pick 2 '(hello world))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat))))
      )))
(rempick 1 '(a b))
(rempick 3 '(hotdog with ketchup mustard relish))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat))))
      )))
(no-nums '(1 2 3))
(no-nums '(a b c))
(no-nums '(a 1 2 b c 3))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat)))
      )))
(all-nums '(1 2 3))
(all-nums '(a b c))
(all-nums '(a 1 2 b c 3))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
(eqan? 1 2)
(eqan? 'a 'a)
(eqan? '(1 2) '(1 2))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add 1 (occur a (cdr lat))))
      (else (occur a (cdr lat)))
      )))
(occur 0 '(1 2 3))
(occur 1 '(1 2 3))
(occur 1 '(1 2 3 1 1))

(define one?
  (lambda (n)
    (= n 1)))
(one? 0)
(one? 1)
(one? 2)

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat))))
      )))
(rempick 1 '(a b))
(rempick 3 '(hotdog with ketchup mustard relish))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))
          ))
      (else (cons (rember* a (car l)) (rember* a (cdr l))))
      )))
(rember* 1 '(1 2 3 1 1))
(rember* 1 '(1 (1) 2 (3 1 1)))
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
    )))
(insertR* '1 2 '(2 2 2))
(insertR* 10 1 '(1 2 1 (1) (2 1) ((1))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))
        ))
      (else (add (occur* a (car l)) (occur* a (cdr l))))
      )))
(occur* 1 '(0 (((1)))))
(occur* 1 '(1 0 3 1 2 1 1))
(occur* 1 '(1 0 (3 1) 2 (((1 1)))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))
          ))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l))))
      )))
(subst* 'orange 'banana '((choc) banana and ((banana) sundae)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l))))
          ))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
      )))
(insertL* 0 1 '(5 1 ((1))))
(insertL* 2 3 '(1 3 (3 (3 3))))
