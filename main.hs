import           Data.Text

--  Exemplo de programa : b1?;π;(1?;πs)*;1?
-- Operadores ; | U | * | ?
-- Uma expressão pode ser terminal, um teste, uma escolha ou um interador
data Expression = Terminal Identifier
                | Test Expression
                | Choice Expression Expression
                | Iteration Expression

-- Uma transição entre estados e a expressão que revela essa transição
data Transition = Transition State State Expression

-- Um estado no grafo
type State = String

-- Um identificador no programa
type Identifier = String

-- Aqui a gente vai separar o programa em programas menores
-- Quando achar os aspas chamanado a função recursivamente
getProgram program = do
  print program

main :: IO ()
main = do
  putStrLn "State your program:"
  program <- getLine
  getProgram program