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
             , mandel :: Int
             , file :: String}


data Output = Cost | Time | Traces | Tracing

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
    tryParse [("cost", Cost), ("time", Time), ("traces", Traces), ("tracing", Tracing)]
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
         <*> option auto
         ( long "mandel"
           <> metavar "MANDEL"
           <> value (-1) )
         <*> argument str (metavar "FILE")

costmatch :: String -> (String,String,String,[String])
costmatch s = s =~ "DIFF COSTS: (.*)"

timematch :: String -> (String,String,String,[String])
timematch s  = s =~ "TIME: (.*) microseconds"


tracesmatch :: String -> (String,String,String,[String])
tracesmatch s = s =~ "TRACE COUNT: (.*)"

tracingmatch :: String -> (String,String,String,[String])
tracingmatch s = s =~ "TRACING: (.*)" 

sizematch :: Euler -> String -> (String, String, String, [String])
sizematch None s = s =~ "([0-9]*)x[0-9]*"
sizematch Chunk s = s =~ "([0-9]*)x[0-9]* chunk"
sizematch Stride s = s =~ "([0-9]*)x[0-9]* stride"


mandelmatch :: String -> (String, String, String, [String])
mandelmatch s = s =~ "([0-9]*)x[0-9]*x([0-9]*)"

printOutput :: IORef Bool -> Euler -> Output -> IORef Int -> Int -> [B.ByteString] -> IO ()
printOutput skip e o i l (x:xs) =
  let (_,_,_, size) = sizematch e  $ C.unpack x
      (_,_,_,cost)  = costmatch $ C.unpack x
      (_,_,_,time) = timematch $ C.unpack x
      (_,_,_,traces) = tracesmatch $ C.unpack x
      (_,_,_,tracing) = tracingmatch $ C.unpack x
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
        Tracing -> condOut tracing lastSize l e skip
        )
      else (case o of
        Cost -> when (cost /= []) (writeIORef skip False)
        Time -> when (time /= []) (writeIORef skip False)
        Traces -> when (traces /= []) (writeIORef skip False)
        Tracing -> when (tracing /= []) (writeIORef skip False)
           )
    printOutput skip e o i l xs
printOutput _ _ _ _ _ [] = return ()


printOutputMandel :: Int -> IORef Int -> Output -> IORef Int -> Int -> [B.ByteString] -> IO ()
printOutputMandel chunk lcr o i l (x:xs) =
  let (_,_,_,sizes) = mandelmatch $ C.unpack x
      (_,_,_,cost)  = costmatch $ C.unpack x
      (_,_,_,time) = timematch $ C.unpack x
      (_,_,_,traces) = tracesmatch $ C.unpack x
      (_,_,_,tracing) = tracingmatch $ C.unpack x
  in do
    when (sizes /= []) (writeIORef i (read (sizes !! 0) ) >> writeIORef lcr (read (sizes !! 1)))
    lastSize <- readIORef i
    lastChunk <- readIORef lcr
    --putStrLn (show lastSize)

   
    --putStrLn (show bskip)
    when (lastSize == l && lastChunk == chunk) (case o of
                           Cost -> condOutMandel cost  
                           Time -> condOutMandel time 
                           Traces -> condOutMandel traces
                           Tracing -> condOutMandel tracing )

    printOutputMandel chunk lcr o i l xs
printOutputMandel  _ _ _ _  _ [] = return ()

condOut s lastSize l e skip = when (s /= [] && lastSize == l) ( putStrLn (s !! 0) >> case e of
                                               None -> return ()
                                               _ -> writeIORef skip True)

condOutMandel s = when  (s /= []) ( putStrLn (s !! 0))

main :: IO ()
main = do
  (Option size output euler mandel file)  <- execParser (info parser fullDesc)
  putStrLn (show size)
  putStrLn (show euler)
  contents <- B.readFile file
  i <- newIORef 0
  skip <- newIORef False
  cr <- newIORef mandel
  case euler of
   Chunk -> return ()
   Stride -> writeIORef skip True >> putStrLn "FOO"
   None -> return ()
  putStrLn $ "Starting..." ++ (show euler)
  if (mandel < 0)
    then printOutput skip euler output i size (C.lines contents)
    else printOutputMandel mandel cr output i size (C.lines contents)
  return ()
