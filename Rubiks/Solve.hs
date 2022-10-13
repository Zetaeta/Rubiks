
module Rubiks.Solve where

import Rubiks
import System.Random

moves :: [Cube -> Cube]
moves = [_U, _U', _U2,
         _D, _D', _D2,
         _F, _F', _F2,
         _B, _B', _B2,
         _R, _R', _R2,
         _L, _L', _L2,

         _E, _E', _E2,
         _M, _M', _M2,
         _S, _S', _S2,

         u, u', u2,
         d, d', d2,
         f, f', f2,
         b, b', b2,
         r, r', r2,
         l, l', l2,

         x, x', x2,
         y, y', y2,
         z, z', z2]

randomList :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randomList 0 g = ([], g)
randomList i g = (this : rest, g')
    where (rest, g'') = randomList (i-1) g'
          (this, g') = random g

randomRList :: (RandomGen g, Random a) => (a, a) -> Int -> g -> ([a], g)
randomRList _ 0 g = ([], g)
randomRList range i g = (this : rest, g')
    where (rest, g'') = randomRList range (i-1) g'
          (this, g') = randomR range g

shuffle :: (RandomGen g) => g -> Cube -> Cube
shuffle g cube = (foldl1 (.) . map (moves !!) $ rands) $ cube
    where (len, g') = randomR (10, 30) g
          (rands, g'') = randomRList (0, length moves - 1) len g'
    
    
shuffledCube :: IO Cube
shuffledCube = newStdGen >>= \g -> return $ shuffle g solvedCube


--solveCube :: Cube -> Cube
