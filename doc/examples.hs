module Acrid.Examples where

import Data.Char (digitToInt)
import Acrid.Parser
import Acrid.Common


-- R -> a | R b | R c
data R = R0 Char | R1 R Char | R2 R Char
  deriving Show

pR =
      R0 <$> tok 'a'
  </> r1 <$> tok 'b'
  <|> r2 <$> tok 'c'

r1 = flip R1
r2 = flip R2



-- V -> a | z V z | V V | b V
data V = V0 Char | V1 Char V Char | V2 V V | V3 V Char V
  deriving Show

pV =
      V0 <$> tok 'a'
  <|> V1 <$> tok 'z' <*> pV <*> tok 'z'
  </> v2 <$> pV
  <|> v3 <$> tok 'b' <*> pV

v2 = flip V2
v3 = (flip .) . flip $ V3



-- E -> EzE | z | eta
data E = E0 E Char E | E1 | E2 Char
  deriving Show

pE = (E2 <$> tok 'z' <|> eta E1) </> e0 <$> tok 'e' <*> pE

e0 = (flip .) . flip $ E0



-- A -> Ba | x
-- B -> Ab | y
data A = A0 B Char | A1 Char deriving Show
data B = B0 A Char | B1 Char deriving Show

leftA  = flip A0 <$> tok 'a'
rightA = A1 <$> tok 'x'
leftB  = flip B0 <$> tok 'b'
rightB = B1 <$> tok 'y' 

pA = rightA <|> rightB <**> leftA </> leftB .> leftA
pB = rightB <|> rightA <**> leftB </> leftA .> leftB



-- Expr -> Expr ('+' | '-') Term | Term
-- Term -> Term ('*' | '/') Factor | Factor
-- Factor -> Number | (Expr)
-- Number -> Number Char | Char

data Expr = Expr0 Expr Char Term | Expr1 Term     deriving Show
data Term = Term0 Term Char Factor | Term1 Factor deriving Show
data Factor = Factor0 Number | Factor1 Expr       deriving Show
data Number = Number0 Number Char | Number1 Char  deriving Show

pExpr =
      Expr1 <$> pTerm
  </> expr0 <$> (tok '+' <|> tok '-') <*> pTerm
--

pTerm =
      Term1 <$> pFactor
  </> term0 <$> (tok '*' <|> tok '/') <*> pFactor
--

pFactor =
      Factor0 <$> pNumber
  <|> (tok '(' *>) (Factor1 <$> pExpr <* tok ')')
--

pNumber =
      Number1 <$> pDigit
  </> number0 <$> pDigit
--

pDigit = anyIn ['0'..'9']

expr0 = (flip .) . flip $ Expr0
term0 = (flip .) . flip $ Term0
number0 = flip Number0

evalMath :: [Char] -> Maybe Int
evalMath = liftA evalExpr . first . parse pExpr

evalOp :: Char -> Int -> Int -> Int
evalOp '+' = (+)
evalOp '-' = (-)
evalOp '/' = quot
evalOp '*' = (*)

evalExpr :: Expr -> Int
evalExpr (Expr0 left op term) = evalOp op (evalExpr left) (evalTerm term)
evalExpr (Expr1 term) = evalTerm term

evalTerm :: Term -> Int
evalTerm (Term0 left op factor) = evalOp op (evalTerm left) (evalFactor factor)
evalTerm (Term1 factor) = evalFactor factor

evalFactor :: Factor -> Int
evalFactor (Factor0 number) = evalNumber number
evalFactor (Factor1 expr) = evalExpr expr

evalNumber :: Number -> Int
evalNumber (Number0 left digit) = 10*(evalNumber left) + (digitToInt digit)
evalNumber (Number1 digit) = digitToInt digit
