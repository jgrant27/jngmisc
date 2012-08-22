
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

import System.Environment( getArgs )
import Data.Vector



-- Disjoint set data type (weighted and using path compression).
-- O((M+N)lg*N) worst-case union time
-- For M union operations on a set of N elements.
-- O((M+N)lg*N) worst-case find time
-- For M connected(find) operations on a set of N elements.
data DisjointSet = DisjointSet
     { count :: Int, ids :: (Vector Int), sizes :: (Vector Int) }
     deriving (Read,  Show)

-- Return id of root object
findRoot :: DisjointSet -> Int -> Int
findRoot set p = let parent = (ids set) ! (p - 1)
                     in if (p == parent)
                        then p else (findRoot set parent)

-- Are objects P and Q connected ?
connected :: DisjointSet -> Int -> Int -> Bool
connected set p q = (findRoot set p) == (findRoot set q)

-- Replace sets containing P and Q with their union
quickUnion :: DisjointSet -> Int -> Int -> DisjointSet
quickUnion set p q =
           let i = findRoot set p
               j = findRoot set q
               size = ((sizes set) ! (i-1)) + ((sizes set) ! (j-1))
               in if (i == j)
                  then set
                  -- Always make smaller root point to the larger one
                  else if (((sizes set) ! (i-1)) < ((sizes set) ! (j-1)))
                       then (DisjointSet
                            ((count set)-1) ((ids set) // [(i-1, j)])
                            ((sizes set) // [(j-1, size)]))
                       else (DisjointSet
                            ((count set)-1) ((ids set) // [(j-1, i)])
                            ((sizes set) // [(i-1, size)]))

createUnions :: DisjointSet -> [(Int, Int)] -> DisjointSet
createUnions set [] = set
createUnions set ((p,q):xs) = createUnions (quickUnion set p q) xs


-- Main entry point for testing
main :: IO ()
main = do
    args <- getArgs
    let cnt1 = (read (Prelude.head args) :: Int)
        cnt  = if (cnt1 < 10) then 10 else cnt1
        in do
           let set = (DisjointSet cnt (fromList [1, 2..cnt]) (Data.Vector.replicate cnt 1))
               in do
                  putStr ("\ncreating union find with " Prelude.++ (show cnt) Prelude.++ " objects ...")
                  putStrLn ("DONE\n" Prelude.++ (show set))
                  putStrLn ("All objects are disconnected.")
                  putStrLn ("1 and 9 connected ? " Prelude.++ (show (connected set 1 9)))
                  putStrLn ("4 and 6 connected ? " Prelude.++ (show (connected set 4 6)))
                  putStrLn ("3 and 1 connected ? " Prelude.++ (show (connected set 3 1)))
                  putStrLn ("7 and 8 connected ? " Prelude.++ (show (connected set 7 8)))
                  putStr ("\ncreating unions ...")
                  let nset = (createUnions set [(4,1), (8,2), (7,3), (8,5), (3,4), (5,9), (5,1), (10,4), (6,1)])
                      in do
                         putStrLn ("DONE\n" Prelude.++ (show nset))
                         putStrLn ("All objects are connected (only 1 group).")
                         putStrLn ("1 and 9 connected ? " Prelude.++ (show (connected nset 1 9)))
                         putStrLn ("4 and 6 connected ? " Prelude.++ (show (connected nset 4 6)))
                         putStrLn ("3 and 1 connected ? " Prelude.++ (show (connected nset 3 1)))
                         putStrLn ("7 and 8 connected ? " Prelude.++ (show (connected nset 7 8)))



-- jgrant@aristotle:~/jngmisc/haskell$ ghc quick_union.hs ; time ./quick_union 10
-- [1 of 1] Compiling Main             ( quick_union.hs, quick_union.o )
-- Linking quick_union ...

-- creating union find with 10 objects ...DONE
-- DisjointSet {count = 10, ids = fromList [1,2,3,4,5,6,7,8,9,10], sizes = fromList [1,1,1,1,1,1,1,1,1,1]}
-- All objects are disconnected.
-- 1 and 9 connected ? False
-- 4 and 6 connected ? False
-- 3 and 1 connected ? False
-- 7 and 8 connected ? False

-- creating unions ...DONE
-- DisjointSet {count = 1, ids = fromList [4,8,7,7,8,8,8,8,8,8], sizes = fromList [1,1,1,2,1,1,4,10,1,1]}
-- All objects are connected (only 1 group).
-- 1 and 9 connected ? True
-- 4 and 6 connected ? True
-- 3 and 1 connected ? True
-- 7 and 8 connected ? True

-- real	0m0.003s
-- user	0m0.000s
-- sys	0m0.000s
