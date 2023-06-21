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
type Transition = (State, State, Expression)-- Uma Transição só pode ser feita com expressões nucleares (simples)

-- Um estado de uma transição
type State = String

validateProgram :: [Transition] -> Expression -> Bool
validateProgram [] _ = True
validateProgram ((s0, s1, e):ts) expression =
  let
    t = ((s0, s1, e):ts)
    Just (first:tail) = getFirstTransition e t
  in
     (isOnProgram (checkForExpression t expression (Just first)) || ((tail /= []) && validateProgram tail expression))


-- Vê se uma certa transição está representada por uma expressão
checkForExpression :: [Transition] -> Expression -> Maybe Transition -> ExpressionAnalisys
checkForExpression [] _ _ = Result False Nothing
checkForExpression _ _ Nothing = Result False Nothing
checkForExpression t (Nuclear idExpression) (Just (s,sNext,Nuclear i)) =
  let
    current = Just (s, sNext, Nuclear i)
    e = Nuclear idExpression
    isTransitionValid = i == idExpression
    newTransitions = deleteTransition t current
  in
    Result ( isTransitionValid &&  isJust current) (Just newTransitions)
checkForExpression t (Binary Sequencial e1 e2) (Just (s,sNext, i)) =
  let
    currentResult = checkForExpression t e1 (Just (s,sNext, i))
    Result isOn (Just updatedList) = currentResult
    nextResult = checkForExpression t e2 (getNextTransition updatedList (Just sNext))
    isTransitionValid = isOnProgram currentResult && isOnProgram nextResult
    newTransitions = intersectTransitions (stateTransition currentResult) (stateTransition nextResult)
  in Result isTransitionValid (Just newTransitions)
checkForExpression t (Binary Choice e1 e2) s =
  let
    result1 = checkForExpression t e1 s
    result2 = checkForExpression t e2 s
    isTransitionValid = isOnProgram result1 || isOnProgram result2
    newTransitions =
      unionTransitions (stateTransition result1) (stateTransition result2)
  in Result isTransitionValid (Just newTransitions)
checkForExpression t (Uni Iteration e1) s =
  let
    result = checkForExpression t e1 s
    Result isO (Just newT) = result
    tryAgain = checkForExpression newT (Uni Iteration e1)
  in
    Result isO (Just newT)
checkForExpression ((a,b,i):ts) (Uni Test e1) s =
  Result (a==b) (Just ((a,b,i):ts))

deleteTransition :: [Transition] -> Maybe Transition -> [Transition]
deleteTransition tx Nothing = tx
deleteTransition tx (Just t) = delete t tx

getFirstExpression :: Expression -> Maybe [Identifier]
getFirstExpression (Binary Sequencial e1 _) = getFirstExpression e1
getFirstExpression (Binary Choice e1 e2) =
  let
    (Just firstResult) = getFirstExpression e1
    (Just secondResult) = getFirstExpression e2
  in
    Just (firstResult ++ secondResult)
getFirstExpression (Uni Iteration e1) = getFirstExpression e1
getFirstExpression (Uni Test e1) = getFirstExpression e1
getFirstExpression (Nuclear i) = Just [i]
getFirstExpression _ = Nothing


getFirstTransition :: Expression ->  [Transition] -> Maybe [Transition]
getFirstTransition e [] = Nothing
getFirstTransition e t = getFirstTransitionAux e t []

getFirstTransitionAux :: Expression -> [Transition] -> [Transition] -> Maybe [Transition]
getFirstTransitionAux e [] l = Just l
getFirstTransitionAux e ((current, next, Nuclear by):ts) l=
  let
    Just firstNuclear = getFirstExpression e
  in if by `elem` firstNuclear
     then getFirstTransitionAux e ts (l ++ [(current, next, Nuclear by)])
     else getFirstTransitionAux e ts l

getNextTransition :: [Transition] -> Maybe State -> Maybe Transition
getNextTransition [] _ = Nothing
getNextTransition (t:ts) Nothing = Just t
getNextTransition ((current, next, by):ts) (Just s) =
  if current == s
     then Just (current, next, by)
     else getNextTransition ts (Just s)

intersectTransitions :: Maybe [Transition] -> Maybe [Transition] -> [Transition]
intersectTransitions (Just t1) (Just t2) = t1 `intersect` t2
intersectTransitions (Just t) _ = t
intersectTransitions _ (Just t) = t
intersectTransitions _ _ = []

unionTransitions :: Maybe [Transition] -> Maybe [Transition] -> [Transition]
unionTransitions (Just t1) (Just t2) = t1 `union` t2
unionTransitions Nothing (Just t2) = t2
unionTransitions (Just t1) Nothing = t1
unionTransitions _ _ = []

-- ghc main.hs para compilar
main :: IO ()
main = do
  -- let program = Binary Sequencial (Nuclear "A") (Uni Iteration (Nuclear "B"))  
  -- let tree = [("1", "2", Nuclear "A"),("2", "3", Nuclear "B"),("3", "4", Nuclear "B")]
  print "Describe your program:"
  rProgram <- getLine
  let program = read rProgram :: Expression
  print "Write your transitions:"
  rTree <- getLine
  let tree = read rTree :: [Transition]
  print (validateProgram tree program)