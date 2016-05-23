-- Ceci est du code Haskell
module Stdlib (And, Or, Bot, Neg, and_intro, and_elim_l, and_elim_r, or_intro_l, or_intro_r, or_elim, neg_elim, neg_intro, negneg_elim) where

data And a b = And_stuff (a,b)
data Or a b = Or_left a | Or_right b
data Bot = Bot_stuff
data Neg a = Neg_stuff (a -> Bot)

and_intro x y = (And_stuff (x,y))
and_elim_l (And_stuff (x,y)) =  x
and_elim_r (And_stuff (x,y)) =  y

or_intro_l x = Or_left x
or_intro_r x = Or_right x

or_elim f g (Or_left  x) = f x
or_elim f g (Or_right y) = g y

neg_elim x (Neg_stuff f) = f x
neg_intro f = Neg_stuff f

negneg_elim :: Neg (Neg f) -> f
negneg_elim f = negneg_elim f