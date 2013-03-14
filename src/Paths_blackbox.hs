
{- 
    When we build this project using Cabal it will generate a 
    module called Paths_blackbox which will actually implement these 
    functions & define the correct Paths

    This file is only used in Development so the file can be loaded
    into GHCI
-}

module Paths_blackbox where

import System.FilePath ((</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName file = return ("/Users/darren/proj/blackbox/" </> file)