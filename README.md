# August 1st, 2017

I'm reading "The Little Schemer", its making me a better programmer!  I'm trying to find the last version of this function that i wrote, its much worse than this one.

```racket
#lang racket

(require rackunit)

(define list-to-pairs
  (λ (l)
    (cond
      [(null? l)        '()]
      [(null? (rest l)) '()]
      [else (cons (list (first l) (second l))
                  (list-to-pairs (rest l)))])))

(module+ test
  (check-equal? (list-to-pairs '())        '() ) 
  (check-equal? (list-to-pairs '(a))       '() ) 
  (check-equal? (list-to-pairs '(a b))     '((a b)) )
  (check-equal? (list-to-pairs '(a b c))   '((a b) (b c)) )
  (check-equal? (list-to-pairs '(a b c d)) '((a b) (b c) (c d)) ))
```

# April 10th, 2017

A stack machine for building sentences using the [axiom-of-specification](https://github.com/jerry-james/axiom-of-specification)

```racket
Russels-Paradox
            B x A bt x x bt not st y B bt y A bt y y bt not and iff y swap fa ift sto 
(stack: #&(Russels-Paradox))
(stack: #&(B Russels-Paradox))
(stack: #&(x B Russels-Paradox))
(stack: #&(A x B Russels-Paradox))
(stack: #&((x ∈ A) B Russels-Paradox))
(stack: #&(x (x ∈ A) B Russels-Paradox))
(stack: #&(x x (x ∈ A) B Russels-Paradox))
(stack: #&((x ∈ x) (x ∈ A) B Russels-Paradox))
(stack: #&((¬ (x ∈ x)) (x ∈ A) B Russels-Paradox))
(stack: #&((B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(y (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(B y (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&((y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(y (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(A y (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&((y ∈ A) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(y (y ∈ A) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(y y (y ∈ A) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&((y ∈ y) (y ∈ A) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&((¬ (y ∈ y)) (y ∈ A) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(((y ∈ A) ∧ (¬ (y ∈ y))) (y ∈ B) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(((y ∈ B) ↔ ((y ∈ A) ∧ (¬ (y ∈ y)))) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(y ((y ∈ B) ↔ ((y ∈ A) ∧ (¬ (y ∈ y)))) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(((y ∈ B) ↔ ((y ∈ A) ∧ (¬ (y ∈ y)))) y (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&((∀ y ((y ∈ B) ↔ ((y ∈ A) ∧ (¬ (y ∈ y))))) (B = (x ∈ A) : (¬ (x ∈ x))) Russels-Paradox))
(stack: #&(((B = (x ∈ A) : (¬ (x ∈ x))) → (∀ y ((y ∈ B) ↔ ((y ∈ A) ∧ (¬ (y ∈ y)))))) Russels-Paradox))
(stack: #&())
```

# March 10th, 2017

I added the basis for a [front-end](https://github.com/jerry-james/cek/blob/master/src/main/antlr4/Sexp.g4) to the CEK machine.  I pulled in the grammar from the last time I tried to do something like this, not really sure how to coax antlr into doing what I want.  I'm trying to decide between only having the sexp in the grammar, or adding in stuff for parsing out concrete syntactic forms like variable and function definitions like this. 

```
sexps : sexp* EOF;

sexp : vardefn | fundefn | atom | list ;

vardefn : '(' 'define' SYMBOL sexp ')';
fundefn : '(' 'define' '(' fundefnsymbol SYMBOL* ')' sexp* ')';

fundefnsymbol : SYMBOL;

list : '(' sexp* ')';

atom : STRING | SYMBOL | NUMBER ;
```

# February 24th, 2017

1. I added a repo for my [Intel HEX file format library for Racket](https://github.com/jerry-james/intel-hex)
2. I found out that racket syntax highlighting works, but coq causes a page build failure.

```racket
(define (test) 0)
```

# February 23rd, 2017

Today I played around with Coq and relations by copying stuff from http://www.cis.upenn.edu/~bcpierce/sf/current/Smallstep.html and trying to apply it to the first chapter of SEwPR.

```
Definition relation (X: Type) := X -> X -> Prop.

(* 1.1 *)
Inductive B : Set :=
  | t  : B
  | f  : B
  | or : B -> B -> B.

Reserved Notation " t '==>' t' " (at level 40).

Inductive r : B -> B -> Prop :=
  | r_a   : forall b  , (or f b) ==> b
  | r_b   : forall b  , (or t b) ==> t
  | r_r   : forall b  ,        b ==> b
  | r_s   : forall a b,   a ==> b -> b ==> a
  | r_t   : forall a b c, a ==> b -> b ==> c -> a ==> c
  where " t '==>' t' " := (r t t').

Definition reflexive  {A : Type} (R : relation A) :=
  forall x : A, R x x.
Definition symmetric {A : Type} (R : relation A) :=
  forall x y : A, R x y -> R y x.
Definition transitive {A : Type} (R : relation A) :=
  forall x y z : A, R x y -> R y z -> R x z.
  
Theorem r_is_reflexive: 
  reflexive r.
Proof.
  unfold reflexive. intros [].
  - (* t ==> t *) apply r_r.
  - (* f ==> f *) apply r_r.
  - intros. (* or b b0 ==> or b b0 *) apply r_r.
Restart.
  intros; eapply r_r; eauto. Qed.
Qed.

Theorem r_is_symmetric:
  symmetric r.
Proof.
  unfold symmetric.
  destruct x,y.
  - intros. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
  - intros. apply r_s in H. apply H.
Restart.
  unfold symmetric.
  destruct x, y; intros; apply r_s in H; apply H.
Restart.
  unfold symmetric.
  intros; eapply r_s; eauto. 
Qed.


Theorem r_is_transitive:
  transitive r.
Proof.
  unfold transitive. 
  intros; eapply r_t; eauto.
Qed.
  
```


# February 21st, 2017
I added my [iswim](https://github.com/jerry-james/iswim) models.

# February 19th, 2017
I added my implementation of [The CEK Machine](https://github.com/jerry-james/cek).


# November 27th, 2016
I added my Racket implementation of the [COBS](https://github.com/jerry-james/cobs) algorithm. 
