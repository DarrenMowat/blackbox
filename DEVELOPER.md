![Blackbox](https://raw.github.com/DarrenMowat/blackbox/master/doc/Blackbox.png)

## Developer Guide

The aim of this guide is to enable developers to create text editor plugins which can interface with blackbox. Developing a plugin for Blackbox is relatively simple as Blackbox does most of the heavy lifting for you.

### 1. Markup

The way a text editor plugin communicates with Blackbox is by inserting special command comments into the file, before sending it off to Blackbox for processing. 
Blackbox currently has support for 3 different commands: SPLIT, SCOPE, TYPELINE.

#### Pattern Splitting  

Split commands should be inserted before the variable that the user wants to split. For example to split xs the function should be marker up as follows:

    foo :: String -> String 
    foo {-SPLIT-} xs = undefined 

#### Scope

Scope commands should be inserted on the right hand side of the function binding that the user wants to determine the scope of. For example: 

    foo :: String -> Maybe Int -> String 
    foo  xs  Nothing = undefined 
    foo  xs  (Just x) = {-SCOPE-} undefined 

Would return a scope of 
    {- xs :: String, x :: Int, RetType :: String -}

#### Inserting Type Signatures

Type signatures can be automatically inserted by tagging a function up as follows 

    foo {-TYPELINE-} xs ys = “”

#### Combinations

Any combination of commands can be issued at once, for example a function definition marked up like so can be processed: 

    foo :: String -> String -> String 
    foo {-SPLIT-} xs {-SPLIT-} ys = undefined 

### 2. Temporary Files

Some text editors allow you to save the file that the user is currently editing, this is useful as you can just save the marked up version of the file to send to Blackbox. However some do not! If your text editor doesn’t support saving the users file then it is perfectly valid to save the marked up version of the file to a temporary directory on the machine. This temporary marked up copy of the file can then be sent to Blackbox. 

### 3. Running Blackbox

The Blackbox binary takes 3 different parameters:

* In file - This is the file on which Blackbox is to be run.
* Markup File - This is the file that contains the marked up version of the file. This can be the same as the infile.
* GHCI Path - This is the path to GHCI on the system.

### 4. Parsing Response 

Blackbox will respond in 2 ways: Success or Failure. Success is denoted by the Blackbox process exiting with a return code of 0. Failure is denoted by a non-zero exit code. 

#### Success 
In the successful case the full contents of the Blackbox processes STDOUT should be inserted into the editors buffer, replacing its current contents.
#### Failure
In the non-successful case the full contents of the Blackbox processes STDERR should be displayed to the user in whatever way is appropriate in the editor. The output on STDERR will contain useful error messages about how to fix the error.



