module Main where

import Options.Applicative
import Options.Applicative.Builder

import Control.Monad.Reader
data Option = Option
             { size :: Int
             , file :: String}

parser :: Parser Option
parser = Option
         <$> option auto
         ( long "size"
           <> metavar "SIZE"
           <> help "Matrix/euler size" )
         <*> argument str (metavar "FILE")

main :: IO ()
main = do
  (Option size file)  <- execParser (info parser fullDesc)
  putStrLn (show size)
  putStrLn file
  
