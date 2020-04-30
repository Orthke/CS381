--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa, Trevor Byko

-- #################################
--            Excercise 1
-- #################################

type Prog   =   [cmd]

data cmd = LD Int
         | ADD
         | MULT
         | DUP

type stack = [Int]

--sem :: Prog -> D
--sem [] = []
--sem (x:xs) = ???

--semcmd :: cmd -> D
--semcmd LD     =
--semcmd ADD    =
--semcmd MULT   =
--semcmd DUP    =


-- #################################
--            Excercise 2
-- #################################

type State = (Mode,Int,Int)

type Line  = (Int,Int,Int,Int)
type Lines = [Line]

--semS :: Cmd -> State -> (State,Lines)
--semS LD   =
--semS ADD  =
--semS MULT =
--semS DUP  =

--should call semS
--sem' :: Cmd -> Lines
