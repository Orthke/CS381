--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa, Trevor Byko

--Probably not a good idea to copy exactly from https://github.com/bramwelt/CS381-SP13/blob/master/assignment3/Homework3.hs

-- #################################
--            Excercise 1
-- #################################

type Prog = [Cmd]

data Cmd = LD Int 
         | ADD 
         | MULT 
         | DUP 
         | INC 
         | SWAP 
         | POP Int
         deriving show


--  Part a

type Rank       = Int
type CmdRank    = (Int, Int)

type Stack      = [Int]
type D          = Stack -> Stack

--Semantics of a single command
semCmd :: Cmd -> D
semCmd (LD y)  xs         = [y] ++ xs
semCmd (ADD)   (x1:x2:xs) = [x1 + x2] ++ xs
semCmd (MULT)  (x1:x2:xs) = [x1 * x2] ++ xs
semCmd (DUP)   (x1:xs)    = [x1, x1] ++ xs
semCmd (INC)   (x1:xs)    = [succ x1] ++ xs
semCmd (SWAP)  (x1:x2:xs) = (x2:x1:xs)
semCmd (POP y) xs         = drop y xs
semCmd _       _          = []

--Semantics of a single program
sem :: Prog -> D
sem [] y = y
sem (x:xs) y = sem xs (semCmd x y)

rankC :: Cmd -> CmdRank
rankC (LD _)  = (0, 1)
rankC ADD     = (2, 1)
rankC MULT    = (2, 1)
rankC DUP     = (1, 2)
rankC INC     = (1, 1)
rankC SWAP    = (2, 2)
rankC (POP x) = (x, 0)

rankP :: Prog -> Maybe Rank
rankP xs = rank xs 0

rank :: Prog -> Rank -> Maybe Rank
rank []         r | r >= 0              = Just r
rank (x:xs)     r | under >= 0          = rank xs (under+adds)
                  where (subs, adds)    = rankC x
                        under           = r - subs
rank _          _                       = Nothing


--  Part b
--Following evalStatTC (TypeCheck.hs), define func that calls rankP
--  to check type correct, only then evaluate. In semStatTC, use sem
--  to do evaluation

--semStatTC :: Prog -> Type

-- #################################
--            Excercise 2
-- #################################

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int, Int)

--  a
--bbox :: Shape -> BBox

--  b
--rect :: Shape -> Maybe BBox


