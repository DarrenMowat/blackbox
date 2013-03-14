module Function.Scope where 

inScope :: Tok -> [[Tok]] -> [String]

inScope :: Tok -> [[Tok]] -> Maybe String
getVariableNameToSplit []     = Nothing
getVariableNameToSplit (l:ls) = case elemToken splitIdentifier l of 
    True  -> (fSplit l)
    False -> getVariableNameToSplit ls
    where 
      fSplit [] = Nothing
      fSplit ((L _ ls) : ts) = case getVariableNameToSplit ls of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((B _ rs):ts)    = case fSplit rs of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((T _ rs):ts)    = case fSplit rs of 
        Just name  -> Just name
        Nothing -> fSplit ts
      fSplit ((Com "{-SPLIT-}") : ts) = gName ts
      fSplit (t:ts) = fSplit ts
      gName [] = Nothing
      gName ((Lid name) : ts) = Just name
      gName (t : ts) = gName ts