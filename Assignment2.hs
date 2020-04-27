import Prelude hiding (Num)

type Program = [Cmd]

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Define String Pars Cmd
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
--x1 = 2
--x2 = 4
--y1 = 2
--y2 = 3
vector = Def "vector" [Pos x1, Pos y1, Pos x2, Pos y2] [ Pen Down, Moveto(x1,y1), Moveto(x2,y2), Pen Up]


-- Part c
steps :: Int -> Program
steps 0 = []
steps n = [Pen Up, Moveto(I n, I n), Pen Down] ++ stepsHelper(n)

stepsHelper :: Int -> Program
stepsHelper 0 = []
stepsHelper n = [Moveto(I (n-1), I n), Moveto(I (n-1), I (n-1))] ++ stepsHelper(n-1)



-- Excercise 2

type Links = [Link]

type Gates = [(Int,Gate)]  -- (Throws an error: Illegal type: `[Int, Gate]' Perhaps you intended to use DataKinds)

data Circuit = Gates Links

data Gate = And | Or | Xor | Not

data Link = L (Int, Int) (Int, Int)
