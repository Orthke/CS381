--Contributors: Carl Bohme, Kelton Orth, Timothy Wiliusa, Trevor Byko

-- #################################
--            Excercise 1
-- #################################

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
  deriving(Show)

type Stack = [Int]

type D = Stack -> Stack
 
sem :: Prog -> D
sem [] a = a
sem (x:xs) a = sem xs (semCmd x a)

semcmd :: Cmd -> D
semcmd (LD a)  xs         = [a] ++ xs
semCmd (ADD)   (s1:s2:xs) = [s1+s2] ++ xs
semCmd (MULT)  (s1:s2:xs) = [s1*s2] ++ xs
semCmd (DUP)   (s1:xs)    = [s1,s1] ++ xs
semCmd _       _          = []


