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
sem [] a = a
sem (x:xs) a = sem xs (semCmd x a)


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

