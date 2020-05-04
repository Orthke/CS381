--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa, Trevor Byko

-- #################################
--            Excercise 1
-- #################################

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]
 

sem :: Prog -> Maybe Stack -> Maybe Stack
sem [] (Just a)     = (Just a)
sem (x:xs) (Just a) = sem xs (semCmd x (Just a))
sem _ _             = Nothing

fromJust (Just x) = x

semCmd :: Cmd -> Maybe Stack -> Maybe Stack
semCmd (LD x) _ = Just [x]
semCmd (ADD) st  = case st of
                      Just (s1:s2:xs) -> Just ((s1 + s2):(xs))
                      _               -> Nothing
semCmd (MULT) st = case st of
                      Just (s1:s2:xs) -> Just ((s1 * s2):(xs))
                      _               -> Nothing
semCmd (DUP) st  = case st of
                      Just (s1:xs)    -> Just (s1:s1:xs)
                      _               -> Nothing
