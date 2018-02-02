module Main where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Language
import qualified Data.Functor.Identity (Identity)
import qualified Data.List.Split as Split

main :: IO ()
main = putStrLn "hello world"

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

extend :: Main.State -> String -> Int -> Main.State
extend start var value = \k -> if k == var
                                    then value
                                    else start k

empty :: Main.State
empty _ = 0

-- Part 02 -----------------------------------------

-- boolean
--
boolToInt :: Bool -> Int
boolToInt b
  | b = 1
  | otherwise = 0

evalE :: Main.State -> Expression -> Int
evalE state (Var x) = state x
evalE state (Val x) = x

-- int
evalE state (Op exp1 Plus exp2) = (evalE state exp1) + (evalE state exp2)
evalE state (Op exp1 Minus exp2) = (evalE state exp1) - (evalE state exp2)
evalE state (Op exp1 Times exp2) = (evalE state exp1) * (evalE state exp2)
evalE state (Op exp1 Divide exp2) = (evalE state exp1) `div` (evalE state exp2)

-- bool
evalE state (Op exp1 Gt exp2) = boolToInt (evalE state exp1 > evalE state exp2)
evalE state (Op exp1 Ge exp2) = boolToInt (evalE state exp1 >= evalE state exp2)
evalE state (Op exp1 Lt exp2) = boolToInt (evalE state exp1 < evalE state exp2)
evalE state (Op exp1 Le exp2) = boolToInt (evalE state exp1 <= evalE state exp2)
evalE state (Op exp1 Eql exp2) = boolToInt (evalE state exp1 == evalE state exp2)

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Incr x) = DAssign x (Op (Var x) Plus (Val 1)) 
desugar (Assign s e) = DAssign s e
desugar (If e sif selse) = DIf e (desugar sif) (desugar selse)
desugar (While exp s) = DWhile exp (desugar s)
desugar (Skip) = DSkip
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar (For s1 exp s2 s3) = DSequence init (DWhile exp update)
    where init = desugar s1
          -- s2 is increment and s3 should be the body of for
          -- increment goes after the body
          update = DSequence (desugar s3) (desugar s2)

-- Part 04 -----------------------------------------

evalSimple :: Main.State -> DietStatement -> Main.State
evalSimple state (DAssign s exp) = extend state s (evalE state exp)

evalSimple state (DIf exp ds1 ds2)
  | (evalE state exp) /= 0 = evalSimple state ds1 
  | otherwise              = evalSimple state ds2

evalSimple state (DSequence ds1 ds2) = evalSimple (evalSimple state ds1) ds2
evalSimple state (DWhile exp ds)
  | (evalE state exp) /= 0 = evalSimple state (DSequence ds (DWhile exp ds))
  | otherwise = state

evalSimple state (DSkip) = state

run :: Main.State -> Statement -> Main.State
run state statement = evalSimple state (desugar statement)

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse str = Just x
    where (Right x) = getStatement str

getStatement :: String -> Either ParseError Statement
getStatement str = Text.Parsec.parse sequenceOfStatement "" str

def :: LanguageDef st
def = emptyDef{ Token.commentStart = "{-"
              , Token.commentEnd = "-}"
              , Token.identStart = letter
              , Token.identLetter = alphaNum
              , Token.opStart = oneOf "+-/*=<>:"
              , Token.opLetter = oneOf "+-/*=<>:"
              , Token.reservedOpNames = ["+", "-", ":=", "/", "*", ">=", ">", "==", "<=", "<", "++", "--"]
              , Token.reservedNames = ["for", "if", "else", "skip", "while", "do" ]
              }

lexer = Token.makeTokenParser def

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer 
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
braces   = Token.braces   lexer 

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table term <?> "expression"

table :: [[Operator String a Data.Functor.Identity.Identity Expression]]
table = [ [Infix (Main.reservedOp "/" >> return ((flip Op) Divide)) AssocLeft]
        , [Infix (Main.reservedOp "*" >> return ((flip Op) Times)) AssocLeft]
        , [Infix (Main.reservedOp "+" >> return ((flip Op) Plus)) AssocLeft]
        , [Infix (Main.reservedOp "-" >> return ((flip Op) Minus)) AssocLeft]
        , [Infix (Main.reservedOp ">" >> return ((flip Op) Gt)) AssocLeft]
        , [Infix (Main.reservedOp "==" >> return ((flip Op) Eql)) AssocLeft]
        , [Infix (Main.reservedOp "<=" >> return ((flip Op) Le)) AssocLeft]
        , [Infix (Main.reservedOp "<" >> return ((flip Op) Lt)) AssocLeft]
        , [Infix (Main.reservedOp ">=" >> return ((flip Op) Ge)) AssocLeft]
        ]

lexemeD :: Parser a -> Parser a
lexemeD p = do
           x <- p
           whiteSpace
           return x

term :: Parsec String () Expression
term = Main.parens expressionParser
        <|> fmap Var Main.identifier
        -- making a custom parser since integer returns Integer 
        -- instead of Int
        <|> do
                n <- lexemeD $ many1 digit
                return $ Val $ read n

statement :: Parser Statement
statement = sequenceOfStatement
            <|> parens statement

sequenceOfStatement :: Parsec String () Statement
sequenceOfStatement =
  do list <- (sepBy1 statementParser semi)
     return (slist list)

statementParser :: Parser Statement
statementParser = try (do
                    v <- Main.identifier
                    Main.reservedOp ":="
                    e <- expressionParser
                    return (Assign v e))
              <|>  try (do
                    v <- Main.identifier
                    Main.reservedOp "++"
                    return (Incr v))
              <|> do
                    Main.reserved "if"
                    cond <- expressionParser
                    thenVar <- braces statement
                    Main.reserved "else"
                    elseVar <- braces statement
                    return (If cond thenVar elseVar)
              <|> do
                    Main.reserved "while"
                    cond <- expressionParser
                    loop <- braces statement
                    return (While cond loop)
              <|> do
                    Main.reserved "for"
                    char '('
                    init <- statement
                    semi
                    cond <- expressionParser 
                    semi
                    lastForCommand <- statement
                    char ')'
                    loop <- braces statement
                    return (For init cond lastForCommand loop)
              <|> do
                    Main.reserved "skip" 
                    return Skip

 
-- Programs ----------------------------------------

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


{-
  Calculate the floor of the square root of the input
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

{-
  Calculate the nth Fibonacci number

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



-- full tests

-- B := 0
-- while (A >= B * B){
--     B ++
-- };
-- B := B - 1

testSquareRoot = "B:= 0; while (A >= B * B)  \
                  \ {B++};  \                    
                  \ B := B - 1 "

(Just parsedSqroot) = Main.parse testSquareRoot
endToEndSquareRoot num = run (extend empty "A" num) parsedSqroot
test1 = (endToEndSquareRoot 5) "B" == 2
test2 = (endToEndSquareRoot 16) "B" == 4

-----
(Just parsedFor) = Main.parse "for((a:=0) ; a < 5; a++ ){b := a * 2}"
endToEndForLoop = run empty parsedFor
testFor = (endToEndForLoop) "a" == 5 && endToEndForLoop "b" == 8
-----

testFactorial = "for((Out := 1); In > 0; In := In - 1){(Out := In * Out)}"
(Just parsedFact) = Main.parse testFactorial
endToEndFactorial num = run (extend empty "In" num) parsedFact
  
testFact = (run (extend empty "In" 5) parsedFact) "Out" == 120

-----
testFibo = "F0 := 1;  F1 := 1 ; \ 
            \ if (In == 0) { \
            \      Out := F0 \
            \ } else {       \
            \      if (In == 1) { \
            \           Out := F1 \
            \      } \
            \      else { \
            \        for ((C:=2) ; C <= In; C++){\
            \            T      := F0 + F1; \
            \            F0     := F1;     \
            \            F1     := T;      \
            \            Out    := T}    \
            \      } \
            \ }"
(Just parsedFibo) = Main.parse testFibo
endToEndFibo num = run (extend empty "In" num) parsedFibo

test3 = (endToEndFibo 10) "Out" == 89

arbitraryExpressionWithTrace exp = Text.Parsec.parse statementParser "" exp
