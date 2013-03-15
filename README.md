![Blackbox](https://raw.github.com/DarrenMowat/blackbox/master/web/Blackbox.png)

Blackbox is a Haskell source code transformer which allows you to find out interseting things about your program. The main aim of Blackbox is to act as a backend for text editor plugins which helps to expose the type system of Haskell to the user.

Current features include: 
* Splitting variables into patterns ([a] -> [] | (x:xs))
* Inserting missing type lines

Blackbox works by parsing a haskell file which has special comments in it which tell Blackbox what to do. For example look at the following fucntion

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

Available Plugins
===========

[Blackbox bindings for Sublime Text 2](https://github.com/DarrenMowat/blackbox-sb2)

Installing from Cabal
=============

I haven’t gotten around to submitting this to cabal yet but I will eventually. In the mean time install it from source.


Building from source 
=============

git clone https://github.com/DarrenMowat/blackbox.git
cd blackbox
cabal configure
cabal build
cabal install 
