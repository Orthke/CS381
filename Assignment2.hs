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
--vector = Define "vector" (S1 "x1" (S1 "x2" (S1 "y1" (S2 "y2")))) [Pen Down, Moveto(x1,y1), Moveto(x2,y2), Pen Up]


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

type Circuit = (Gates, Links)

type Gates = [(Int, GateFn)]
data GateFn = And | Or | Xor | Not deriving Show

type Links = [Link]
data Link  = L (Int, Int) (Int, Int) deriving Show



--  Part b

halfAdder :: Circuit
halfAdder = ([(1, Xor), (2, And)], [L (1,1) (2,1), L ( 1,2) (2,2)])


--  Part c

