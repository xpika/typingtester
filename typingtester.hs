import Control.Monad
import Data.List
import Data.Time
import System.IO
import Text.Printf
import System.Environment
import System.Directory 
import Data.Monoid

hPutStrFlush h str = putStr str >> hFlush h 

main = do
  a <- getArgs
  let target_phrase = (concat $ intersperse " " a)
  let target_phrase_set = not . null $ target_phrase 
  d <- getAppUserDataDirectory "typingTester"
  createDirectoryIfMissing True d
  let file = (d++"/highscores.txt")
  file_exists <- doesFileExist file
  {-
  a <- if file_exists then read . readFile exists 
                      else return []
 -}
  when target_phrase_set
    (putStrLn ("target phrase: " ++ target_phrase))
  foreverWith (0) $ \(x) -> do
     -- prompt user to type
     hPutStrFlush stdout (show x++": type something:")
     -- grab time 
     t1 <- getCurrentTime
     -- grab input
     line <- getLine
     -- grab time 
     t2 <- getCurrentTime
     --print 2
     let timeinunits = ( realToFrac $ diffUTCTime t2 t1 :: Float) 
     let numWords = ( genericLength . words  $ line :: Float)
     let speed = numWords / (timeinunits/60)
     let match = line == target_phrase
     if target_phrase_set && (not match) then putStrLn "failure to match"
                                         else putStrLn (printf "%d words in %f seconds for a speed of %f WPM (words per minute)" (round numWords :: Int) timeinunits speed )
     return (x+1)

foreverWith a m = do 
   b <- m a
   foreverWith b m
