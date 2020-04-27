data Day = Mon | Tues | Wed | Thur | Fri | Sat | Sun
  deriving (Eq,Show)

isWeekday :: Day -> Bool
isWeekday Mon = True
isWeekday Tues = True
isWeekday Wed = True
isWeekday Thur = True
isWeekday Fri = True
isWeekday _ = False

type Age = Int
type Name = String

data Person = Person Name Age
  deriving (Eq,Show)
