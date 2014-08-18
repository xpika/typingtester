{-# Language BangPatterns #-}
{-# Language DoAndIfThenElse #-}


import Control.Monad
import Data.List
import Data.Time
import System.IO
import Text.Printf
import System.Environment
import System.Directory 
import Data.Monoid
import Data.Map

hPutStrFlush h str = putStr str >> hFlush h 

main = do
  a <- getArgs
  if a == ["highscores"] then 
    putStrLn "highscores"
  else do
    let target_phrase = (concat $ intersperse " " a)
    let target_phrase_set = not . Data.List.null $ target_phrase 
    d <- getAppUserDataDirectory "typingTester"
    createDirectoryIfMissing True d
    let fileName = (d++"/highscores.txt")
    file_exists <- doesFileExist fileName
    scores <- if file_exists then readFile fileName >>= return . read
                             else return mempty
    let !scores_strict = scores
    when target_phrase_set
      (putStrLn ("target phrase: " ++ target_phrase))
    foreverWith (0,scores_strict) $ \(x,m) -> do
       -- prompt user to type
       hPutStrFlush stdout ("type something:")
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
       if target_phrase_set && (not match) then do
         putStrLn "failure to match"
         return (x+1, m)
       else do
         putStrLn (printf "%d words in %f seconds for a speed of %f WPM (words per minute)" (round numWords :: Int) timeinunits speed )
         let (maybeExisting,newValue,newMap) = insertOrUpdateWith (\k e -> if speed > e then Just speed else Just e) line speed m
             (newValue',newMap') = if not target_phrase_set && maybeExisting == Nothing then (Nothing,m) else (newValue,newMap)
         case newValue' of 
           Just spe -> do
             let recordStr = case maybeExisting of
                    Just ex -> "new record over " ++ (show ex)
                    _ -> "time recorded"
             putStrLn recordStr
             let newMapStr = show newMap'
             writeFile fileName newMapStr
           Nothing -> return ()
         return (x+1,newMap')

foreverWith a m = do 
   b <- m a
   foreverWith b m

insertOrUpdateWith f k v m = let existingValue = Data.Map.lookup k m
  in case existingValue of 
      Just x -> let (maybeUpdated,latestTable) = updateLookupWithKey f k m
                in (existingValue,if maybeUpdated /= existingValue then existingValue else Nothing,latestTable)
      Nothing -> (Nothing,(Just v),Data.Map.insert k v m)
