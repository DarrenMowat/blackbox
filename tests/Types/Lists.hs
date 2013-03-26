module Types.Lists where 

data Fwd a = F0 | a :> Fwd a deriving (Show, Eq)

type Cursor a = (Bwd a, a, [a])

data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)
