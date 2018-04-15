module ProofUtils where

type Name = String 

data Pattern =    Variable Name 
              |   Exists Pattern Pattern
              |   Forall Pattern Pattern
              |   Equals Pattern Pattern
              |   Plus Pattern Pattern
              |   Succ Pattern
              |   Zero  deriving (Show)


equals :: Pattern -> Pattern -> Pattern

equals a b = Equals a b 

plus :: Pattern -> Pattern -> Pattern

plus a b = Plus a b 

type Id = Int


data Line = Line Id

instance Show Line where
  show (Line id) = (show id) ++ [':']

data Claim =   Claim Line Pattern 

instance Show Claim where
  show (Claim l p) = show l ++ show p 

data Rule =   FuncSub Id Id 
            | VarSubst Pattern Id Pattern 
            | EqSymmetric Id 
            | EqTrans Id Id  deriving Show

data Formula = Formula Line Pattern Rule 

instance Show Formula where
  show (Formula l p r) = show l ++ show p ++ " by " ++  show r


data Proof = Proof [Claim] [Formula] 


instance Show Proof where 
  show (Proof claims formulae) = 
      do 
        a <- total
        show a ++ ['\n']
        where 
          claimsStr = fmap (show) claims  
          formulaeStr = fmap (show) formulae  
          total = claimsStr ++ formulaeStr  


varT = Variable "t"

varW :: Pattern
varW = Variable "w"

varY :: Pattern
varY = Variable "y"

varX :: Pattern
varX = Variable "x"

varZ :: Pattern
varZ = Variable "z"

functionalZero :: Claim
functionalZero = Claim (Line 1) $ Exists varT (Zero `equals` varT)

functionalSuc :: Claim
functionalSuc = Claim (Line 2) $ Forall (varX) (Exists varZ ( Succ varX `equals` varZ))

sPropagation :: Claim
sPropagation = Claim (Line 3) $ Forall varX $ 
                          Forall varY $
                            Succ  (varX `plus`varY) `equals` (varX `plus` (Succ varY))
 
identity :: Claim
identity = Claim (Line 4) $ Forall varX $ 
                            (varX `plus` Zero) `equals` varX

sExists :: Claim
sExists = Claim (Line 5) $ Exists varT $
                            (Succ varX) `equals` varT

pExists :: Claim
pExists = Claim (Line 6) $ Exists varT $
                        (varX `plus` varY) `equals` varT


f1 :: Formula 
f1 = Formula (Line 7)
      (Exists varZ $ Succ Zero `equals` varZ)
        (FuncSub 1 2)

f2 :: Formula
f2 = Formula (Line 8) 
              (Forall varY ((Succ (Zero `plus` varY)) `equals` 
                (Zero `plus` Succ(varY))))
                (FuncSub 1 3)
      
f3 :: Formula
f3 = Formula (Line 9) 
              (Succ (Zero `plus` Succ (Zero)) `equals` 
                (Zero `plus` Succ(Succ(Zero))))
                (FuncSub 7 3)


f4 :: Formula
f4 = Formula (Line 10)
        (Exists varW $ Succ Zero `equals` varW)
        (VarSubst varZ 7 varW)

f5 :: Formula
f5 = Formula (Line 11)
              (Exists varW $ Succ (Succ Zero) `equals` varW)
              (FuncSub 10 2) 

f6 :: Formula
f6 = Formula (Line 12)
              ((Succ (Succ Zero)) `equals` (Zero `plus` (Succ (Succ Zero))))
              (FuncSub 11 4)

-- Equality Symmetricity

f7 :: Formula
f7 = Formula (Line 13)
              ((Zero `plus` (Succ (Succ Zero))) `equals` (Succ (Succ Zero)))
              (EqSymmetric 12)

f8 :: Formula
f8 = Formula (Line 14) 
              (Succ (Zero `plus` Succ (Zero)) `equals` Succ (Succ Zero))
              (EqTrans 9 13)

f9 :: Formula
f9 = Formula (Line 15) 
          (Forall varY $ (Succ (Succ Zero `plus` varY)) `equals` (Succ Zero `plus` (Succ varY)))
          (FuncSub 7 3)

f10 :: Formula
f10 = Formula (Line 16) 
          ((Succ (Succ Zero `plus` Zero)) `equals` (Succ Zero `plus` (Succ Zero)))
          (FuncSub 1 15)

f11 :: Formula
f11 = Formula (Line 17) 
          ((Succ Zero `plus` (Succ Zero)) `equals` (Succ (Succ Zero `plus` Zero)))
          (EqSymmetric 16)

f12 :: Formula
f12 = Formula (Line 18) 
        ((Succ Zero `plus` (Succ Zero)) `equals` (Succ (Succ Zero)))
        (EqTrans 17 13)

axioms :: [Claim]

formulae :: [Formula]

axioms = [ functionalZero
 , functionalSuc 
 , sPropagation
 , identity
 , sExists
 , pExists
 ]

formulae = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11]

onePlusOneProof :: Proof 

onePlusOneProof = Proof axioms formulae
