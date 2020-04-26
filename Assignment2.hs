import Prelude hiding (Num)

type Num = Int
type Name = String


data Cmd = Pen Mode
         | Moveto Pos Pos
         | Define Name [Pars] Cmd
         | Call Name [Name]
  deriving (Eq,Show)


data Mode = Down
          | Up
  deriving (Eq,Show)


data Pos = Num
          | Myname Name
  deriving (Eq,Show)


data Pars = Myname3 Name Pars
          | Myname4 Name
  deriving (Eq,Show)


data Vals = Mynum Num Vals
          | Mynum2 Num
  deriving (Eq,Show)


--End of part a


-- Part b
vector = Def "vector" [x1, y1, x2, y2] [ pen down, Moveto(x1,y1), Moveto(x2,y2), pen up]


-- Part c
steps :: Int -> Cmd
steps 0 = []
steps n = [Pen Up, Moveto(I n, I n), Pen Down] ++ stairs(n)

stepsHelper :: Int -> Cmd
stepsHelper 0 = []
stepsHelper n = [Moveto(I n-1, I n), Moveto(I n-1, I n-1)] ++ stepsHelper(n-1)