import System.Posix.Time
import Data.List
import System.Time
import Text.Printf
import Control.Monad
import System.IO

hPutStrFlush h str = hPutStr h str >> hFlush h 

main = forever $ do
         hPutStrFlush stdout "type something:"
	 t1 <- time
	 line <- getLine
	 t2 <- time
	 let numWords = (genericLength . words  $ line :: Float)
	 let tdiff = difftimes t1 t2
	 let timeinunits =  (/100000000000) . fromIntegral . tdPicosec $ tdiff
	 let speed = numWords / (timeinunits/60)
	 putStrLn (printf "%d words in %f seconds for a speed of %f WPM (words per minute)" (round numWords :: Int) timeinunits speed )
	 
time = getClockTime
difftimes t1 t2 = diffClockTimes t1 t2
