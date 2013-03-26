module Types.User where 

data User = Student String Int String | 
            Admin String Int   
            deriving (Show, Eq, Ord) 