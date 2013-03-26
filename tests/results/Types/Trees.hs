module Types.Trees where 

data Tree a = Branch {-l-} (Tree a) {-r-} (Tree a) | Leaf {-x-} a

