import Control.Monad
import Data.List
import Data.Time
import System.IO
import Text.Printf

hPutStrFlush h str = hPutStr h str >> hFlush h 


main = 
  forever $ do
     -- prompt user to type
     hPutStrFlush stdout "type something:"
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
     putStrLn (printf "%d words in %f seconds for a speed of %f WPM (words per minute)" (round numWords :: Int) timeinunits speed )
