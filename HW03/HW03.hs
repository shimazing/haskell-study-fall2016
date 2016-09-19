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
extend st var val = (\x -> if x == var then val else (st x))

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st ex =
    case ex of
        Var s -> (st s)
        Val n -> n
        Op e1 bop e2 -> evalBop bop (evalE st e1) (evalE st e2)

evalBop :: Bop -> Int -> Int -> Int
evalBop bop n1 n2 =
    case bop of
        Plus    -> n1 + n2
        Minus   -> n1 - n2
        Times   -> n1 * n2
        Divide  -> n1 `div` n2
        Gt      -> if n1 > n2 then 1 else 0
        Ge      -> if n1 >= n2 then 1 else 0
        Lt      -> if n1 < n2 then 1 else 0
        Le      -> if n1 <= n2 then 1 else 0
        Eql     -> if n1 == n2 then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar stmt =
    case stmt of
        Assign s ex              -> DAssign s ex
        Incr s                   -> DAssign s (Op (Var s) Plus (Val 1))
        If ex stmt1 stmt2        -> DIf ex (desugar stmt1) (desugar stmt2)
        While ex stmt            -> DWhile ex (desugar stmt)
        For stmt1 ex stmt2 stmt3 -> DSequence (desugar stmt1) (DWhile ex (DSequence (desugar stmt3) (desugar stmt2)))
        Sequence stmt1 stmt2     -> DSequence (desugar stmt1) (desugar stmt2)
        Skip                     -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st dstmt =
    case dstmt of
        DAssign s ex            -> extend st s (evalE st ex)
        DIf ex dstmt1 dstmt2    -> if (evalE st ex) == 1 then (evalSimple st dstmt1) else (evalSimple st dstmt2)
        DWhile ex dstmt         -> if (evalE st ex) == 1 then (evalSimple (evalSimple st dstmt) (DWhile ex dstmt)) else st
        DSequence dstmt1 dstmt2 -> evalSimple (evalSimple st dstmt1) dstmt2
        DSkip                   -> st


run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

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
