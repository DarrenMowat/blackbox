![Blackbox](https://raw.github.com/DarrenMowat/blackbox/master/doc/Blackbox.png)

Blackbox is a Haskell source code transformer which allows you to find out interesting things about your program. The main aim of Blackbox is to act as a backend for text editor plugins which helps to expose the type system of Haskell to the user.

Current features include: 

  * Splitting Pattern Variables
  * Listing variables in scope & their types at a given point in a program
  * Inserting missing type lines for functions

Blackbox works by parsing a Haskell file which has special comments in it which tell Blackbox what to do. For example look at the following function

			fn :: [a] -> [a]
			fn xs = undefined
			
Blackbox can be asked to split the xs into all possible combinations of the type by inserting the comment 

			{-SPLIT-}
			
before the variable to be split

			fn :: [a] -> [a]
			fn {-SPLIT-} xs = undefined
			
You can then pass the file over to Blackbox and it will return a modified file, over STDOUT, with all commands satisfied. The above file would become

			fn :: [a] -> [a]
			fn [] = undefined
			fn (x:xs) = undefined
		 

This project has been undertaken as part of my final year degree project.

## Installation

Get a copy of this repository either by 

    git clone https://github.com/DarrenMowat/blackbox.git
    
Or by downloading it as a zip file from https://github.com/DarrenMowat/blackbox/archive/master.zip

    cd blackbox
    cabal configure
    cabal install
   
The blackbox binary should now be available on your PATH somewhere 

## Available Plugins

[Blackbox for Sublime Text 2](https://github.com/DarrenMowat/blackbox-sb2)

## Developing Plugins

If you fancy contributing a compatible text editor plugin to the project read the instructions in [DEVELOPER.md](https://github.com/DarrenMowat/blackbox/blob/master/DEVELOPER.md)
