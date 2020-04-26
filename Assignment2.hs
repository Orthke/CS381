import Prelude hiding (Num)

type Prog = [Cmd]

data Cmd = Pen Mode
         | Moveto Pos Pos
         | Define String Pars Prog
         | Call String Vals
  deriving (Eq,Show)


data Mode = Down
          | Up
  deriving (Eq,Show)


data Pos = I Int
          | S String
  deriving (Eq,Show)


data Pars = S1 String Pars
          | S2 String
  deriving (Eq,Show)


data Vals = I1 Int Vals
          | I2 Int
  deriving (Eq,Show)

--End of part a


-- Part b
x1 = 2
x2 = 4
y1 = 2
y2 = 3
--vector = Define "vector" [x1, y1, x2, y2] [ Pen Down, Moveto(x1,y1), Moveto(x2,y2), Pen Up]


-- Part c
--steps :: Int -> Cmd
--steps 0 = []
--steps n = [Pen Up, Moveto(I n, I n), Pen Down] ++ stairs(n)

--stepsHelper :: Int -> Cmd
--stepsHelper 0 = []
--stepsHelper n = [Moveto(I n-1, I n), Moveto(I n-1, I n-1)] ++ stepsHelper(n-1)



-- Excercise 2

type Circuit = (Gates, Links)
type Links = [Link]
type Gates = [(Int,Gate)]

data Gate = And | Or | Xor | Not

data Link  = L (Int, Int) (Int, Int)