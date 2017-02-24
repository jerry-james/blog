# jerry.fyi blog


```racket
(define (test) 0)
```
## February 24th, 2017

I added a repo for my [Intel HEX file format library for Racket](https://github.com/jerry-james/intel-hex)

## February 23rd, 2017

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


## February 21st, 2017
I added my [iswim](https://github.com/jerry-james/iswim) models.

## February 19th, 2017
I added my implementation of [The CEK Machine](https://github.com/jerry-james/cek).


## November 27th, 2016
I added my Racket implementation of the [COBS](https://github.com/jerry-james/cobs) algorithm. 
