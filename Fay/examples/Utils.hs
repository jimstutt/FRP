module Utils (printDbls ) where
import Data.Complex

--import Numeric

--[C-ROADS video tutorials](http://bit.ly/ak4ifw)

--Utility Functions

{-
write :: [Double] -> String -> (IO [String])
write dl outf
  | length dl > 0 = do
      write dl outf
      appendFile outf  (show $ head dl)
      putStrLn (show dl)
      appendFile outf "\n"
      write (tail dl) outf
  | otherwise = putStrLn ""
-}

--Had to add _ <- Just $ to get rid of:

{-
"Warning: A do-notation statement discarded a result of type Maybe".
I don't want to add the then suggested -fno-wrong-do-bind compiler flag.

That tells me the implementation is wrong. TBD Fix.

But then it was crap.
-}
 
printDbls :: Show a => [a] -> IO ()
printDbls x 
  | length x > 0 = do
     putStrLn $ show (head x)
     printDbls $ tail x
  | otherwise =  putStr ""

printCmplxs :: Show a => [Complex a] -> Maybe a
printCmplxs x 
  | length x > 0 = do
     Just $ print (head x)
     printCmplxs $ tail x
  | otherwise = Nothing

secsToYrs s = s/60*60*24*365.25 

