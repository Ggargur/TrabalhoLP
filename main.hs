import           Data.Text

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
newtype Transition = Transitions [(State, State, Expression)]

-- Um estado de uma transição
type State = String

-- Aqui a gente vai separar o programa em programas menores
-- Quando achar os aspas chamanado a função recursivamente
getProgram program = do
  print program

getOperator :: Expression -> Maybe Operator
getOperator (Binary op _ _) = Just op
getOperator (Uni op _) = Just op
getOperator (Nuclear _) = Nothing

main :: IO ()
main = do
  -- A U B
  let programA = Binary Choice (Nuclear "A") (Nuclear "B")
  -- A U (A ; B)
  let programB = Binary
        Choice
        (Nuclear "A")
        (Binary Sequencial (Nuclear "A") (Nuclear "B"))
  putStrLn "State your program:"
  let tree = Transitions [("A", "B", Nuclear "A"), ("B", "C", Nuclear "B")]
  program <- getLine
  getProgram program