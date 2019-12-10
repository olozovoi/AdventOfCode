{-# LANGUAGE OverloadedLists #-}
module Day2 where

import Prelude hiding (read)
import Debug.Trace

import Data.Vector as V
import Data.Vector.Mutable as VM

input :: Vector Int
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0]

pre :: Int -> Int -> Vector Int -> Vector Int
pre noun verb = V.modify $ \v -> do
    write v 1 noun
    write v 2 verb

runOp op i = \v -> do
    ia <- read v (i+1)
    a  <- read v ia
    ib <- read v (i+2)
    b  <- read v ib
    ir <- read v (i+3)
    write v ir (op a b)

-- alg :: Int -> Vector Int -> Vector Int
alg i = \v -> do
    op <- read v i
    case op of
        1 -> runOp (+) i v >> alg (i+4) v
        2 -> runOp (*) i v >> alg (i+4) v
        99 -> pure ()

solution :: Int -> Int -> Int
solution noun verb = V.head $ V.modify (alg 0) $ pre noun verb input

solution1 :: Int
solution1 = solution 12 2

solution2 :: [Int]
solution2 =  [ 100*n+v | n <- [0..99], v <- [0..99], solution n v == 19690720]