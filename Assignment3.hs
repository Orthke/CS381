--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa, Trevor Byko

-- #################################
--            Excercise 1
-- #################################

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
  deriving(Show)

type Stack = [Int]
 
sem :: Prog -> Maybe Stack -> Maybe Stack
sem [] (Just a)     = (Just a)
sem (x:xs) (Just a) = sem xs (semCmd x (Just a))
sem _ _             = Nothing


semCmd :: Cmd -> Maybe Stack -> Maybe Stack
semCmd (LD i) _ = Just [i]
semCmd (ADD) (s) = case s of
                      Just (i1:i2:xs) -> Just ((i1 + i2):(xs))
                      _               -> Nothing
semCmd (MULT) (s) = case s of
                    Just (i1:i2:xs) -> Just ((i1 * i2):(xs))
                    _               -> Nothing
semCmd (DUP) (s) = case s of
                   Just (i1:xs) -> Just (i1:i1:xs)
                   _            -> Nothing



-- #################################
--            Excercise 2
-- #################################

--data Cmd  = Pen Mode
--          | MoveTo Int Int
--          | Seq Cmd Cmd

data Mode = Up | Down

type State = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]


--semS :: Cmd -> State -> (State,Lines)


--sem' :: Cmd -> Lines