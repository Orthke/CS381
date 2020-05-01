-- #################################
--            Excercise 2
-- #################################

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd

data Mode = Up | Down

type State = (Mode,Int,Int)
type Line  = (Int,Int,Int,Int)
type Lines = [Line]

--semS :: Cmd -> State -> (State,Lines)
--semS (c:cs) (m, x, y) = ((m,x,y),([x]))