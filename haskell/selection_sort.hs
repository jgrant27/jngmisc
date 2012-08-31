
-- Copyright (c) 2012, Justin Grant <justin at imagine27 dot com>
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:

-- Redistributions of source code must retain the above copyright notice, this list
-- of conditions and the following disclaimer.
-- Redistributions in binary form must reproduce the above copyright notice, this
-- list of conditions and the following disclaimer in the documentation and/or
-- other materials provided with the distribution.
-- Neither the name of the <ORGANIZATION> nor the names of its contributors may be
-- used to endorse or promote products derived from this software without specific
-- prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


module Main( main ) where

import Data.List
import System.Environment( getArgs )
import System.Random


-- Selection Sort
selectionSort :: [Int] -> [Int] -> [Int]
selectionSort sorted [] = reverse sorted
selectionSort sorted unsorted = selectionSort (min:sorted) (delete min unsorted)
                     where min = minimum unsorted


-- Main entry point for testing
main :: IO ()
main = do
    args <- getArgs
    seed  <- newStdGen
    let cnt = (read (head args) :: Int)
        unsorted = take cnt (randomRs (1,cnt) seed)
        sorted = selectionSort [] unsorted
    putStrLn ("unsorted : " ++ show unsorted)
    putStrLn ("sorted : "   ++ show sorted)



-- jgrant@aristotle:~/jngmisc/haskell$ ghc selection_sort.hs ; time ./selection_sort 100 
-- Linking selection_sort ...
-- unsorted : [69,17,74,16,76,84,75,79,33,61,19,43,76,11,74,86,93,35,41,88,8,40,29,60,41,15,20,67,32,90,15,51,46,27,52,62,64,53,93,84,37,87,54,22,16,7,14,86,57,2,42,2,46,4,80,40,7,1,18,37,77,40,43,33,70,4,15,70,15,52,90,62,82,28,22,44,52,15,18,3,25,88,26,36,98,27,54,27,8,15,10,85,72,47,1,71,44,17,88,28]
-- sorted : [1,1,2,2,3,4,4,7,7,8,8,10,11,14,15,15,15,15,15,15,16,16,17,17,18,18,19,20,22,22,25,26,27,27,27,28,28,29,32,33,33,35,36,37,37,40,40,40,41,41,42,43,43,44,44,46,46,47,51,52,52,52,53,54,54,57,60,61,62,62,64,67,69,70,70,71,72,74,74,75,76,76,77,79,80,82,84,84,85,86,86,87,88,88,88,90,90,93,93,98]

-- real	0m0.003s
-- user	0m0.000s
-- sys	0m0.000s
