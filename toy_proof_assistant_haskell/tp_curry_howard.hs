-- Dans ce pseudo-TP, on va utiliser un "assistant de preuve"
-- rudimentaire basé sur le système de type de Haskell.

-- Le terme "assistant de preuve" est très mal choisi, car le système
-- ne vous assiste pas. Au contraire, il vous embête jusqu'à ce que
-- votre "preuve" soit CORRECTE. Dans ce sens-la, il vous assiste à
-- faire des preuves correctes.

--  Le coeur de cet assistant de preuve jouet est contenu dans le
--  module Stdlib, qui est fourni.

--  Dans l'interpréteur GHCi, vous pouvez charger ce module avec la
--  commande suivante :
--  :l tp_curry_howard.hs

--  Ensuite vous pouvez voir le contenu de ce module, avec les types,
--  en faisant :
--  :browse

import Stdlib

-- pour commencer en douceur, on va démontrer que le ET est
-- commutatif : pour écrire une preuve de A /\ B => B /\ A, on écrit
-- une fonction de type And A B -> And B A.

et_commutatif :: And a b -> And b a
et_commutatif x =
  let a = and_elim_l x in
  let b = and_elim_r x in
    and_intro b a

-- On peut vérifier qu'on ne s'est pas trompé avec la commande
-- :t et_commutatif

-- Si le type de la fonction n'est pas conforme, le typeur de
-- Haskell vous le dira tout de suite.

-- Même topo avec l'associativité : on cherche une preuve de
-- A /\ (B /\ C) => (A /\ B) /\ C :

et_associatif :: And a (And b c) -> And (And a b) c
et_associatif x =
  let a = and_elim_l x in
  let bc = and_elim_r x in
  let b = and_elim_l bc in
  let c = and_elim_r bc in
  let ab = and_intro a b in
    and_intro ab c

-- Traitons ensemble un petit exemple avec le OU
-- On veut écrire une preuve de A \/ (A /\ B) => A.

ou_bizarre :: Or a (And a b) -> a
ou_bizarre x =
  let given_a  a = a in                -- transforme une preuve de A en... preuve de A
  let given_ab ab = and_elim_l ab in   -- transforme une preuve de A /\ B en preuve de A
    or_elim given_a given_ab x         -- transforme la preuve de A \/ (A /\ B) en preuve de A
 
-- EXERCICE : voici une sélection de petites choses "faciles" à prouver :
--   A /\ B => A
exo_1 :: (And a b) -> a
exo_1 x =
  let t = and_elim_l x in
    t

--   (A /\ (A => B)) => B     [Socrate est un homme, les hommes sont mortels, Socrate est mortel]
exo_2 :: (And a (a -> b)) -> b
exo_2 x =
  let a = and_elim_l x in
  let ab = and_elim_r x in
    ab a

--   (A \/ A) => A
exo_3 :: (Or a a) -> a
exo_3 x =
  let given_a a = a in
  or_elim given_a given_a x
    
--   (A \/ B) => (B \/ A)
exo_4 :: (Or a b) -> (Or b a)
exo_4 = or_elim or_intro_r or_intro_l 

-- La négation est toujours un peu délicate, à cause des raisonnements
-- par l'absurde qu'elle introduit et qui ne sont pas toujours naturels.
-- Démontrons A => ¬¬A

negneg_intro :: a -> Neg (Neg a)
negneg_intro a =
  let nega_to_bot nega = neg_elim a nega in -- ceci prend une preuve de ¬A et la combine avec la preuve de A
                                            -- reçue en argument pour produire une contradiction.
  neg_intro nega_to_bot                     -- de ¬A -> Bot, on déduit une preuve de ¬¬A.

-- EXERCICE : maintenant les choses dures.
-- Il y a deux sources de difficulté, le "OU" qui oblige à des
-- raisonnements par cas, et la négation qui oblige à faire des
-- "raisonnements par l'absurde".

-- associativité du OU :
--   A \/ (B \/ C) => (A \/ B) \/ C
--or_associatif :: Or a (Or b c) -> Or (Or a b) c 
or_associatif x =
  let given_a x = or_intro_l (or_intro_l x) 
      given_bc x = 
        let given_b x = or_intro_l (or_intro_r x) 
            given_c x = or_intro_r x
        in 
            or_elim given_b given_c x
  in
    or_elim given_a given_bc x

-- distributivité du OU
--   A \/ (B /\ C) => (A \/ C) /\ (A \/ C) ainsi que (A \/ C) /\ (A \/ C) => A \/ (B /\ C)

-- distributivité du ET
--   A /\ (B \/ C) => (A /\ B) \/ (A /\ C) ainsi que (A /\ B) \/ (A /\ C) => A /\ (B \/ C)

-- Lois de De Morgan :
--   A /\ B => ¬(¬A \/ ¬B)
--   A \/ B => ¬(¬A /\ ¬B) 
--   ¬A /\ ¬B => ¬(A \/ B) ainsi que ¬(A \/ B) => ¬A /\ ¬B

-- Formes équivalentes de l'implication :
--    A \/ B => (¬A => B)
--a_ou_b :: (Or a b) -> ((Neg a) -> b)


--   ¬A \/ B => ( A => B)
--non_a_ou_b :: (Or (Neg a) b) -> (a -> b)


-- Et pour finir les "horribles" : ceux où l'emploi de la fonction
-- negneg_elim est indispensable (personne ne sait trop ce qui peut
-- se passer si on l'exécute...)

-- EXERCICE : Démontrez la validité du raisonnement par l'absurde :
--   (¬A => ⊥) => A
-- non_a_bottom :: ((Neg a) -> Bot) -> a

-- Démontrez le principe du tiers-exclu
--   A \/ ¬A

-- Démontrez la loi de Pierce :
--   ((A => B) => A) => A