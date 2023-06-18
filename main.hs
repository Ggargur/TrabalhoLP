import           Data.Maybe
import           Data.List

-- Operadores ; | U | * | ?
-- Uma expressão pode ser terminal, um teste, uma escolha ou um interador
--  Exemplo de programa : b1?;π;(1?;πs)*;1?
data Expression = Binary Operator Expression Expression
                | Uni Operator Expression
                | Nuclear Identifier
  deriving (Show, Eq, Read)

data ExpressionAnalisys =
  Result { isOnProgram :: Bool, stateTransition :: Maybe [Transition] }
  deriving (Show)

-- Um identificador no programa
type Identifier = String

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

instance Show Operator where
  show :: Operator -> String
  show Sequencial = ";"
  show Choice = "U"
  show Iteration = "*"
  show Test = "?"


-- Uma transição entre estados e a expressão que revela essa transição
type Transition = (State, State, Expression) -- Uma Transição só pode ser feita com expressões nucleares (simples)

-- Um estado de uma transição
type State = String

validateProgram :: [Transition] -> Expression -> Bool
validateProgram [] _ = True
validateProgram ((s0, s1, e):ts) expression = isOnProgram (checkForExpression ((s0, s1, e):ts) expression Nothing)


-- Vê se uma certa transição está representada por uma expressão
checkForExpression :: [Transition] -> Expression -> Maybe State -> ExpressionAnalisys
checkForExpression [] _ _ = Result False Nothing
checkForExpression ((a, b, Nuclear idTransition):ts) (Nuclear idExpression) s =
  let t = ((a, b, Nuclear idTransition):ts)
      e = Nuclear idExpression
      isTransitionValid = idTransition == idExpression
      next = checkForNextTransition t s e
      newTransitions = deleteTransition t next
  in Result ( isTransitionValid &&  isJust next) (Just newTransitions)
checkForExpression t (Binary Sequencial e1 e2) s =
  let currentResult = checkForExpression t e1 s
      nextResult = checkForExpression t e2 Nothing
      isTransitionValid = isOnProgram currentResult && isOnProgram nextResult
      newTransitions = intersectTransitions
        (stateTransition currentResult)
        (stateTransition nextResult)
  in Result isTransitionValid (Just newTransitions)
checkForExpression t (Binary Choice e1 e2) s =
  let result1 = checkForExpression t e1 s
      result2 = checkForExpression t e2 s
      isTransitionValid = isOnProgram result1 || isOnProgram result2
      newTransitions =
        unionTransitions (stateTransition result1) (stateTransition result2)
  in Result isTransitionValid (Just newTransitions)
checkForExpression t (Uni Iteration e1) s =
  let result = checkForExpression t e1 s
      isTransitionValid = isOnProgram result
      newTransitions = stateTransition result
  in Result isTransitionValid newTransitions
checkForExpression ((a,b,i):ts) (Uni Test e1) s =
  Result (a==b) (Just ((a,b,i):ts))

deleteTransition :: [Transition] -> Maybe Transition -> [Transition]
deleteTransition tx Nothing = tx
deleteTransition tx (Just t) = delete t tx

checkForNextTransition :: [Transition] -> Maybe State -> Expression-> Maybe Transition
checkForNextTransition [] _ _ = Nothing
checkForNextTransition (t:ts) Nothing _ = Just t
checkForNextTransition ((current, next, by):ts) (Just s) e =
  if current == s
     then Just (current, next, by)
     else checkForNextTransition ts (Just s) e

getFirstTransition :: [Transition] -> Expression -> Maybe Transition
getFirstTransition [] _ = Nothing
getFirstTransition ((c, n, i):ts) e =
  if i == e
  then Just (c, n, i)
  else getFirstTransition ts e

intersectTransitions :: Maybe [Transition] -> Maybe [Transition] -> [Transition]
intersectTransitions (Just t1) (Just t2) = t1 `intersect` t2
intersectTransitions (Just t) _ = t
intersectTransitions _ (Just t) = t
intersectTransitions _ _ = []

unionTransitions :: Maybe [Transition] -> Maybe [Transition] -> [Transition]
unionTransitions (Just t1) (Just t2) = t1 `union` t2
unionTransitions _ _ = []

-- Tem que programar o teste?
-- 
-- 1 -> 2, 3-> 4
-- main :: IO ()
main = do
  -- A U B
  -- let program = Binary Choice (Nuclear "A") (Nuclear "B")
  -- A U (A ; B)
  -- let program = Binary Choice (Nuclear "A") (Binary Sequencial (Nuclear "A") (Nuclear "B"))
  -- (A?;(A;B))*;B -- O teste seguido por um sequêncial (?;) será traduzido em um único operador binário (?)
  let program =  Binary Sequencial (Nuclear "A") (Nuclear "B")
  -- print "Describe your program:"
  -- print "Describe your program:"
  -- rProgram <- getLine
  -- let program = read rProgram :: Expression
  let tree = [("1", "2", Nuclear "A"), ("2", "3", Nuclear "B")] -- Não funciona
  -- print "Write your transitions:"
  -- rTree <- getLine
  -- let tree = read rTree :: Transition
  print (validateProgram tree program)