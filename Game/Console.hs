import Kernel
import Data.Matrix
import Data.List

import System.Random
import System.Environment
import System.IO

import Parser
import SimpleParsers
import ParseNumbers

import Control.Monad(when)

game quest matr = do
    putStr quest
    putStrLn $ show matr
    answer <- getLine
    let pair_ = fst $ head $ apply twoPair answer
    let newMatr = makeMove pair_ matr
    when (not $ isAllFounded newMatr) $ do
        game quest newMatr
        
main :: IO ()
main = do
    [size] <- getArgs 
    gen <- newStdGen
    let matr = generateFieldMatrix gen (read size)
    game "Enter two positions\n" matr