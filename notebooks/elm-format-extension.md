# Divide the code into files for the user.
If the interface to your code was a diagram like this, 
If you wrote code by clicking on this to change a function
and adding names here before writing the functions, 
you could have it all in one big file, and the elm-review / elm-format 
equivilent could seperate it into files if the structure 
of the names and types made that make sense. 

## First step: Ordering functions
Go top down, simplest parts first. 
So start with the `Prorgam` type, and do the simplest
(probably `init`) first, then the next (`subscriptions`?)
descending to the view. 

Divide the view up by which functions are called in top level, etc.

## Second step: Dividing modules
Maybe you don't need modules at all if you're working this way! 
Maybe you do, and there are good heuristics that can break them out for you.
