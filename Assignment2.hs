--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa

-- #################################
--            Excercise 1
-- #################################


--  Part a 

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Define String Pars [Cmd]
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


--  Part b 
x1 = I 4
x2 = I 4
y1 = I 2
y2 = I 3
--vector = Define "vector" (S2(1) S2(2) S2(2) S1(4)) [Pen Down, Pen Up]--[Pen Down, Moveto(x1,y1), Moveto(x2,y2), Pen Up]


--  Part c 

steps :: Int -> [Cmd]
steps 0 = []
steps n = [Pen Up, Moveto(I n, I n), Pen Down] ++ stepsHelper(n)

stepsHelper :: Int -> [Cmd]
stepsHelper 0 = []
stepsHelper n = [Moveto(I (n-1), I n), Moveto(I (n-1), I (n-1))] ++ stepsHelper(n-1)




-- #################################
--            Excercise 2
-- #################################


--  Part a 

type Circuit = ((Int,Gate), [Link])
data Gate = And | Or | Xor | Not
data Link  = L (Int, Int) (Int, Int)


--  Part b
