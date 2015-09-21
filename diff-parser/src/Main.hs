module Main where

import Options.Applicative
import Options.Applicative.Builder
import Text.Regex.Posix
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.IORef
import Control.Monad.Reader
data Option = Option
             { size :: Int
             , output :: Output
             , euler :: Euler
             , file :: String}

data Output = Cost | Time | Traces

data Euler = Chunk | Stride | None deriving Show

instance Read Euler where
  readsPrec _ value = 
    tryParse [("chunk", Chunk), ("stride", Stride)]
    where tryParse [] = []    -- If there is nothing left to try, fail
          tryParse ((attempt, result):xs) =
            -- Compare the start of the string to be parsed to the
            -- text we are looking for.
            if (take (length attempt) value) == attempt
               -- If we have a match, return the result and the
               -- remaining input
            then [(result, drop (length attempt) value)]
                 -- If we don't have a match, try the next pair
                 -- in the list of attempts.
            else tryParse xs
  
    

instance Read Output where
  readsPrec _ value = 
    tryParse [("cost", Cost), ("time", Time), ("traces", Traces)]
    where tryParse [] = []    -- If there is nothing left to try, fail
          tryParse ((attempt, result):xs) =
            -- Compare the start of the string to be parsed to the
            -- text we are looking for.
            if (take (length attempt) value) == attempt
               -- If we have a match, return the result and the
               -- remaining input
            then [(result, drop (length attempt) value)]
                 -- If we don't have a match, try the next pair
                 -- in the list of attempts.
            else tryParse xs
                 
parser :: Parser Option
parser = Option
         <$> option auto
         ( long "size"
           <> metavar "SIZE"
           <> help "Matrix/euler size" )
         <*> option auto
         ( long "output"
           <> metavar "OUTPUT")
         <*> option auto
         ( long "euler"
           <> metavar "EULER"
           <> value None)
         <*> argument str (metavar "FILE")

costmatch :: String -> (String,String,String,[String])
costmatch s = s =~ "DIFF COSTS: (.*)"

timematch :: String -> (String,String,String,[String])
timematch s  = s =~ "TIME: (.*) microseconds"


tracesmatch :: String -> (String,String,String,[String])
tracesmatch s = s =~ "TRACE COUNT: (.*)"

sizematch :: Euler -> String -> (String, String, String, [String])
sizematch None s = s =~ "([0-9]*)x[0-9]*"
sizematch Chunk s = s =~ "([0-9]*)x[0-9]* chunk"
sizematch Stride s = s =~ "([0-9]*)x[0-9]* stride"


printOutput :: IORef Bool -> Euler -> Output -> IORef Int -> Int -> [B.ByteString] -> IO ()
printOutput skip e o i l (x:xs) =
  let (_,_,_, size) = sizematch e  $ C.unpack x
      (_,_,_,cost)  = costmatch $ C.unpack x
      (_,_,_,time) = timematch $ C.unpack x
      (_,_,_,traces) = tracesmatch $ C.unpack x
  in do
    when (size /= []) (writeIORef i (read (size !! 0)))
    lastSize <- readIORef i
    --putStrLn (show lastSize)

    bskip <- readIORef skip
    --putStrLn (show bskip)
    if (not bskip)
      then (case o of
        Cost -> condOut cost lastSize l e skip
        Time -> condOut time lastSize l e skip
        Traces -> condOut traces lastSize l e skip
        )
      else (case o of
        Cost -> when (cost /= []) (writeIORef skip False)
        Time -> when (time /= []) (writeIORef skip False)
           )
    printOutput skip e o i l xs
printOutput _ _ _ _ _ [] = return ()

condOut s lastSize l e skip = when (s /= [] && lastSize == l) ( putStrLn (s !! 0) >> case e of
                                               None -> return ()
                                               _ -> writeIORef skip True)

main :: IO ()
main = do
  (Option size output euler file)  <- execParser (info parser fullDesc)
  putStrLn (show size)
  putStrLn (show euler)
  contents <- B.readFile file
  i <- newIORef 0
  skip <- newIORef False
  case euler of
   Chunk -> return ()
   Stride -> writeIORef skip True >> putStrLn "FOO"
   None -> return ()
  putStrLn $ "Starting..." ++ (show euler)
  printOutput skip euler output i size (C.lines contents)
  return ()
