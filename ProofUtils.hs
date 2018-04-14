import Data.Foldable (mapM_)

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

data Rule = MyRule deriving Show

data Line = Line Id deriving Show

data Claim =   Claim Line Pattern deriving Show

data Formula = Formula Line Pattern deriving Show
 

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

varY :: Pattern
varY = Variable "y"

varX :: Pattern
varX = Variable "x"

functionalZero :: Claim
functionalZero = Claim (Line 1) $ Exists varT (Zero `equals` varT)

functionalSuc :: Claim
functionalSuc = Claim (Line 2) $ Forall (varX) (Exists varY ( Succ (varX `equals` varY)))

sPropagation :: Claim
sPropagation = Claim (Line 3) $ Forall varX $ 
                          Forall varY $
                            Succ  (varX `plus`varY) `equals` (varX `plus` (Succ varY))
 
identity :: Claim
identity = Claim (Line 4) $ Forall varX $ 
                            (varX`plus` Zero) `equals` varX

sExists :: Claim
sExists = Claim (Line 5) $ Exists varT $
                            (Succ varX) `equals` varT

pExists :: Claim
pExists = Claim (Line 6) $ Exists varT $
                        (varX `plus` varY) `equals` varT
 
-- s(0 + s(0)) = 0 + s(s(0))

axioms :: [Claim]

formulae :: [Formula]

axioms = [ functionalZero
 , functionalSuc 
 , sPropagation
 , identity
 , sExists
 , pExists
 ]

formulae = []

onePlusOneProof :: Proof 

onePlusOneProof = Proof axioms formulae





