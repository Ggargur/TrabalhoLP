{-# LANGUAGE DataKinds #-}
import           Data.Text

-- Tem que programar o teste?
-- 

-- Operadores ; | U | * | ?
-- Uma expressão pode ser terminal, um teste, uma escolha ou um interador
--  Exemplo de programa : b1?;π;(1?;πs)*;1?
data Expression = Binary Operator Expression Expression
                | Uni Operator Expression
                | Nuclear Identifier
  deriving (Show)

-- Um identificador no programa
type Identifier = String

instance Show Operator where
  show :: Operator -> String
  show Sequencial = ";"
  show Choice = "U"
  show Iteration = "*"
  show Test = "?"


data Operator = Sequencial
              | Choice
              | Iteration
              | Test

-- Uma transição entre estados e a expressão que revela essa transição
type Transition = [(State, State, Expression)] -- Uma Transição só pode ser feita com expressões nucleares (simples)

-- Um estado de uma transição
type State = String


validateProgram :: Transition -> Expression -> Bool
validateProgram [] _ = True
validateProgram t e = checkForExpression t e

-- Vê se uma certa transição está representada por uma expressão
-- Comando any
-- Pattern matching a partir de um estado.
checkForExpression :: Transition -> Expression -> Bool
checkForExpression [] _ = False
checkForExpression ((a, b, Nuclear i1):ts) (Nuclear i2) = i1 == i2
checkForExpression t (Uni Test _) = True
checkForExpression t (Binary Test _ i) = checkForExpression t i
checkForExpression t (Binary Choice i1 i2) = checkForExpression t i1 || checkForExpression t i2
checkForExpression t (Uni Iteration i) =  checkForExpression t i
checkForExpression (t:ts) (Binary Sequencial i1 i2) = checkForExpression (t:ts) i1 && checkForExpression ts i2

readOperator ::  String -> Maybe Operator
readOperator ";" = Just Sequencial
readOperator "?" = Just Test
readOperator "*" = Just Iteration
readOperator "U" = Just Choice
readOperator _ = Nothing

checkForNextTransition :: Transition -> State -> Int -> Maybe Int
checkForNextTransition [] _ _ = Nothing
checkForNextTransition ((a, _, _):ts) s i = if a == s then Just i else checkForNextTransition ts s (i+1)


-- 1 -> 2, 3-> 4

main = do
  -- A U B
  -- let program = Binary Choice (Nuclear "A") (Nuclear "B")
  -- A U (A ; B)
  -- let program = Binary Choice (Nuclear "A") (Binary Sequencial (Nuclear "A") (Nuclear "B"))
  -- (A?;(A;B))*;B -- O teste seguido por um sequêncial (?;) será traduzido em um único operador binário (?)
  let program = Binary Sequencial (Uni Iteration (Binary Test (Nuclear "A") (Binary Sequencial (Nuclear "A") (Nuclear "B")))) (Nuclear "B")-- Abuso de notação
  let tree = [("1", "2", Nuclear "A"),("2", "3", Nuclear "B")] -- Não funciona
  print (validateProgram tree program)