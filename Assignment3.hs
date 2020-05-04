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
semCmd (LD x) st = case st of
                      Just (xs) -> Just ([x]++(xs))
semCmd (ADD) st  = case st of
                      Just (s1:s2:xs) -> Just ((s1 + s2):(xs))
                      _               -> Nothing
semCmd (MULT) st = case st of
                      Just (s1:s2:xs) -> Just ((s1 * s2):(xs))
                      _               -> Nothing
semCmd (DUP) st  = case st of
                      Just (s1:xs)    -> Just (s1:s1:xs)
                      _               -> Nothing


-- #################################
--            Excercise 2
-- #################################

data Cmd2 = Pen Mode
         | MoveTo Int Int
         | Seq Cmd2 Cmd2
  deriving (Eq)

data Mode = Up | Down
  deriving (Eq)

type State = (Mode,Int,Int)
type Line  = (Int,Int,Int,Int)
type Lines = [Line]


semS :: Cmd2 -> State -> (State,Lines)
semS (Pen m1) (m2,i1,i2)        = ((m1,i1,i2),[])
semS (MoveTo i1 i2) (m1,i3,i4)  | m1 == Down    = ((m1,i1,i2),[(i3,i4,i1,i2)])
                                | otherwise     = ((m1,i1,i2),[])
semS (Seq c1 c2) (m1,i1,i2)     = (fst (semS c2 (fst (semS c1 (m1,i1,i2)))), (snd (semS c1 (m1,i1,i2))++(snd (semS c2 (fst (semS c1 (m1,i1,i2)))))) )
 

sem' :: Cmd2 -> Lines
sem' c = snd (semS c (Up,0,0))
