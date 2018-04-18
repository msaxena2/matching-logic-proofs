{-# LANGUAGE OverloadedStrings #-}

module ProofUtils where
import Data.Text.Prettyprint.Doc hiding (equals) 

type Name = String 

data Pattern =    Variable Name 
              |   Exists Pattern Pattern
              |   Forall Pattern Pattern
              |   Equals Pattern Pattern
              |   Plus Pattern Pattern
              |   Succ Pattern
              |   Zero  


instance Pretty Pattern where
  pretty p = case p of
              (Exists p1 p2) -> "\\exists{Nat{}}"        <> (tupled ([pretty p1, pretty p2]))
              (Variable v)   -> (pretty v)               <> ":Nat{}"
              (Forall p1 p2) -> "\\forall{Nat{}}"        <> (tupled [(pretty p1), (pretty p2)])
              (Equals p1 p2) -> "\\equals{Nat{}, Nat{}}" <> (tupled [(pretty p1), (pretty p2)])
              (Plus p1 p2)   -> "plus{}"                 <> (tupled [(pretty p1), (pretty p2)])
              (Succ p1)      -> "succ{}"                 <> (tupled [pretty p1])
              (Zero)         -> "zero{}()"


equals :: Pattern -> Pattern -> Pattern

equals a b = Equals a b 

plus :: Pattern -> Pattern -> Pattern

plus a b = Plus a b 

type Id = Int

data Line = Line Id

instance Pretty Line where
  pretty (Line id) = (pretty id) <+> ":" 


data Claim =   Claim Line Pattern 


data Rule =   FuncSub Id Id 
            | VarSubst Pattern Id Pattern 
            | EqSymmetric Id 
            | EqTrans Id Id  
            | EqSubst Id Pattern Int

instance Pretty Rule where
  pretty (FuncSub i1 i2)     = "funsubst-rule" <> (tupled ([pretty i1, pretty i2]))
  pretty (VarSubst p1 i1 p2) = "varsubst"      <> (tupled [pretty p1, pretty i1, pretty p2]) 
  pretty (EqTrans i1 i2)     = "eq-trans"      <> (tupled ([pretty i1, pretty i2]))
  pretty (EqSymmetric i1)    = "eq-comm"       <> (tupled ([pretty i1]))
  pretty (EqSubst i1 p ps)   = "eqsubst-rule"  <> (tupled $ [pretty i1, pretty p, pretty ps])

data Formula = Formula Line Pattern Rule 


data Proof = Proof [Claim] [Formula] 

instance Pretty Claim where
  pretty (Claim (Line id) pattern) = (pretty id) <+> ":" <+> pretty pattern 

instance Pretty Formula where
  pretty (Formula (Line id) pattern rule) = (pretty id) <+> ":" <+> pretty pattern 
                                                <+> "by" <+> pretty rule 


instance Pretty Proof where 
  pretty (Proof claims formulae) = vsep $ (pretty <$> claims) ++ (pretty <$> formulae)

varT = Variable "T"

varW :: Pattern
varW = Variable "W"

varY :: Pattern
varY = Variable "Y"

varX :: Pattern
varX = Variable "X"

varZ :: Pattern
varZ = Variable "Z"

-- Functional Pattern Axioms
functionalZero :: Claim
functionalZero = Claim (Line 1) $ Exists varT (Zero `equals` varT)

functionalSuc :: Claim
functionalSuc = Claim (Line 2) $ Forall (varX) (Exists varT ( Succ varX `equals` varT))

functionalPlus :: Claim
functionalPlus = Claim (Line 3) $ Forall varX $ 
                                    Forall varY $
                                      Exists varT $
                                        varX `plus`varY `equals` varT
-- Nat Plus and Zero Axioms
identity :: Claim
identity = Claim (Line 4) $ Forall varX $ 
                             (varX `plus` Zero) `equals` varX

sPropagation :: Claim
sPropagation = Claim (Line 5) $ Forall varX $ 
                                  Forall varY $
                                    (varX `plus` (Succ varY)) `equals` Succ (varX `plus` varY)
 
-- Formulae for proof
f1 :: Formula 
f1 = Formula (Line 6)
      (Exists varT $ Succ Zero `equals` varT)
        (FuncSub 1 2)

f2 :: Formula
f2 = Formula (Line 7) 
              (Forall varY  
                ((Succ Zero) `plus` Succ(varY))
                  `equals` 
                (Succ ((Succ Zero) `plus` varY)))
                (FuncSub 6 5)

f3 :: Formula
f3 = Formula (Line 8) 
                (((Succ Zero) `plus` Succ(Zero))
                  `equals` 
                 (Succ ((Succ Zero) `plus` Zero)))
                (FuncSub 1 7)

      

f4 :: Formula
f4 = Formula (Line 9)
        (((Succ Zero) `plus` Zero) `equals` (Succ Zero))
        (FuncSub 6 4)

f5 :: Formula
f5 = Formula (Line 10)
        (Succ ((Succ Zero) `plus` Zero) `equals` (Succ (Succ Zero)))
        (EqSubst 9 (Succ (varX)) 0)

f6 :: Formula
f6 =  Formula (Line 11) 
        (((Succ Zero) `plus` Succ(Zero))
          `equals` 
         (Succ (Succ Zero)))
        (EqTrans 8 10)


axioms :: [Claim]

formulae :: [Formula]

axioms = [ functionalZero
 , functionalSuc 
 , functionalPlus
 , identity
 , sPropagation
 ]

formulae = [f1, f2, f3, f4, f5, f6]

onePlusOneProof :: Proof 

onePlusOneProof = Proof axioms formulae

-- renderProof p = pageWidth Unbounded (pretty p) 
