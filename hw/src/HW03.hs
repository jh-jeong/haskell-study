module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s i = st'
  where
    st' a
      | (a == s) = i
      | otherwise = (st a)

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op expr1 op expr2) =
  case op of
    Plus   -> val1 + val2
    Minus  -> val1 - val2
    Times  -> val1 * val2
    Divide -> val1 `div` val2
    Gt     -> i
      where
        i
          | val1 > val2 = 1
          | otherwise   = 0
    Ge     -> i
      where
        i
          | val1 >= val2 = 1
          | otherwise    = 0
    Lt     -> i
      where
        i
          | val1 < val2 = 1
          | otherwise   = 0
    Le     -> i
      where
        i
          | val1 <= val2 = 1
          | otherwise    = 0
    Eql    -> i
      where
        i
          | val1 == val2 = 1
          | otherwise    = 0
    where
      val1 = evalE st expr1
      val2 = evalE st expr2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar st = case st of
    Incr s -> DAssign s (Op (Var s) Plus (Val 1))
    For sInit exp sUpdt sLoop ->  DSequence
                                    (DSequence
                                        (DSequence (desugar sInit) (desugar sLoop)) (desugar sUpdt))
                                    (DWhile exp (DSequence (desugar sLoop) (desugar sUpdt)))
    Assign s exp -> DAssign s exp
    If exp st1 st2 -> DIf exp (desugar st1) (desugar st2)
    While exp st -> DWhile exp (desugar st)
    Sequence st1 st2 -> DSequence (desugar st1) (desugar st2)
    Skip -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s exp) = extend st s (evalE st exp)
evalSimple st (DIf exp dst1 dst2)
  | cond == 0 = evalSimple st dst2
  | otherwise = evalSimple st dst1
  where
    cond = evalE st exp
evalSimple st dwhile@(DWhile exp dst)
  | cond == 0 = st
  | otherwise = evalSimple (evalSimple st dst) dwhile
  where
    cond = evalE st exp
evalSimple st (DSequence dst1 dst2) = evalSimple (evalSimple st dst1) dst2
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st dstmt
  where
    dstmt = desugar stmt

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
