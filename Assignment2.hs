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
          | Myname2 Name
  deriving (Eq,Show)


--End of part a