{-# LANGUAGE DataKinds #-}
import Prelude
import Data.List
import Data.Maybe

-- Tem que programar o teste?
-- 

-- Operadores ; | U | * | ?
-- Uma expressão pode ser terminal, um teste, uma escolha ou um interador
--  Exemplo de programa : b1?;π;(1?;πs)*;1?
data Expression = Binary Operator Expression Expression
                | Uni Operator Expression
                | Nuclear Identifier
  deriving (Show, Eq, Read)

data ExpressionAnalisys = Result {
  isOnProgram :: Bool,
  stateTransition :: Maybe [Transition]
}

-- Um identificador no programa
type Identifier = String

instance Show Operator where
  show :: Operator -> String
  show Sequencial = ";"
  show Choice = "U"
  show Iteration = "*"
  show Test = "?"

readOperator :: String -> Operator
readOperator ";" = Sequencial
readOperator "U" = Choice
readOperator "*" = Iteration
readOperator "?" = Test

data Operator = Sequencial
              | Choice
              | Iteration
              | Test
    deriving (Eq, Read)

-- Uma transição entre estados e a expressão que revela essa transição
type Transition = (State, State, Expression, Bool) -- Uma Transição só pode ser feita com expressões nucleares (simples)

-- Um estado de uma transição
type State = String


validateProgram :: [Transition] -> Expression -> Bool
validateProgram [] _ = True
-- validateProgram t e = checkForExpression t e

-- Vê se uma certa transição está representada por uma expressão
-- Comando any
-- Pattern matching a partir de um estado.
-- checkForExpression :: Transition -> Expression -> Bool
-- checkForExpression [] _ = False
-- checkForExpression ((a, b, Nuclear i1, _):ts) (Nuclear i2) = i1 == i2
-- checkForExpression t (Uni Test _) = True
-- checkForExpression t (Binary Test _ i) = checkForExpression t i
-- checkForExpression t (Binary Choice i1 i2) = checkForExpression t i1 || checkForExpression t i2
-- checkForExpression t (Uni Iteration i) =  checkForExpression t i
-- checkForExpression (t:ts) (Binary Sequencial i1 i2) = checkForExpression (t:ts) i1 && checkForExpression ts i2
checkForExpression :: [Transition] -> Expression -> State -> ExpressionAnalisys
checkForExpression [] _ _ = Result False Nothing
checkForExpression t e s = 
  let 
    ((a, b, Nuclear idTransition, _):ts) = t
    (Nuclear idExpression) = e
    isTransitionValid = idTransition == idExpression
    next = checkForNextTransition t e s 
  in if not isTransitionValid || isNothing next
      then Result False Nothing
      else let
        newTransitions = deleteTransition t next
        in
          Result True (Just newTransitions)
          

updateTransitions :: Int -> [Transition] -> Transition -> [Transition] -- Index List New Result
updateTransitions i y newValue = let (ys, zs) = Prelude.splitAt (i - 1) y
                      in ys ++ [newValue] ++ tail zs

deleteTransition :: [Transition] -> Maybe Transition -> [Transition]
deleteTransition tx Nothing = tx
deleteTransition tx (Just t) = delete t tx


checkForNextTransition :: [Transition] -> Expression -> State -> Maybe Transition
checkForNextTransition [] _ _ = Nothing
checkForNextTransition t e s =
  let ((c, n, i, b):ts) = t
  in if c == s && i == e && not b
     then Just (c, n, i, b)
     else checkForNextTransition t e s



-- 1 -> 2, 3-> 4


main :: IO ()
main = do
  -- A U B
  -- let program = Binary Choice (Nuclear "A") (Nuclear "B")
  -- A U (A ; B)
  -- let program = Binary Choice (Nuclear "A") (Binary Sequencial (Nuclear "A") (Nuclear "B"))
  -- (A?;(A;B))*;B -- O teste seguido por um sequêncial (?;) será traduzido em um único operador binário (?)
  let program = Binary Sequencial (Uni Iteration (Binary Test (Nuclear "A") (Binary Sequencial (Nuclear "A") (Nuclear "B")))) (Nuclear "B")-- Abuso de notação
  -- print "Describe your program:"
  -- rProgram <- getLine
  -- let program = read rProgram :: Expression
  let tree = [("1", "2", Nuclear "A", False),("2", "3", Nuclear "B", False)] -- Não funciona
  -- print "Write your transitions:"
  -- rTree <- getLine
  -- let tree = read rTree :: Transition
  print (validateProgram tree program)