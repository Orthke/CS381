-- #################################
--            Excercise 2
-- #################################

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd
  deriving (Eq)

data Mode = Up | Down
  deriving (Eq)

type State = (Mode,Int,Int)
type Line  = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS (Pen m1) (m2,i1,i2)        = ((m1,i1,i2),[])
semS (MoveTo i1 i2) (m1,i3,i4)  | m1 == Down    = ((m1,i1,i2),[(i3,i4,i1,i2)])
                                | otherwise     = ((m1,i1,i2),[])
--semS (Seq c1 c2) (m1,i3,i4)     = semS c1 (m1,i3,i4)
 
--semS (c:cs) (m, x, y) = ((m,x,y),([x]))

--sem' :: Cmd -> Lines
