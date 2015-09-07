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
             , file :: String}

data Output = Cost | Time

instance Read Output where
  readsPrec _ value = 
    tryParse [("cost", Cost), ("time", Time)]
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
         <*> argument str (metavar "FILE")

costmatch :: String -> (String,String,String,[String])
costmatch s = s =~ "DIFF COSTS: (.*)"

timematch :: String -> (String,String,String,[String])
timematch s  = s =~ "TIME: (.*) microseconds"


sizematch :: String -> (String, String, String, [String])
sizematch s = s =~ "([0-9]*)x[0-9]*"


printOutput :: Output -> IORef Int -> Int -> [B.ByteString] -> IO ()
printOutput o i l (x:xs) =
  let (_,_,_, size) = sizematch $ C.unpack x
      (_,_,_,cost)  = costmatch $ C.unpack x
      (_,_,_,time) = timematch $ C.unpack x
  in do
    when (size /= []) (writeIORef i (read (size !! 0)))
    lastSize <- readIORef i
    case o of
     Cost -> when (cost /= [] && lastSize == l) (putStrLn (cost !! 0))
     Time -> when (time /= [] && lastSize == l) (putStrLn (time !! 0))
    printOutput o i l xs
printOutput _ _  _ [] = return ()

main :: IO ()
main = do
  (Option size output file)  <- execParser (info parser fullDesc)
  putStrLn (show size)
  putStrLn file
  contents <- B.readFile file
  i <- newIORef 0
  printOutput output i size (C.lines contents)
  return ()
