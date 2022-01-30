module Lib where

import Data.Complex
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Generic as G
import Codec.Picture

w :: Int
w = 500
h :: Int
h = 500
sz :: Float
sz = fromIntegral st / 2.5
st :: Int
st = 30

colors :: V.Vector PixelRGB8
colors = V.fromList [
    PixelRGB8 22 160 133,
    PixelRGB8 192 57 43,
    PixelRGB8 44 62 80,
    PixelRGB8 142 68 173 ]

roots :: [Complex Float]
roots = [
    10 :+ 6,
    (-2) :+ 4,
    (-3) :+ (-9),
    6 :+ (-3) ]

rootsV :: V.Vector (Complex Float)
rootsV = V.fromList roots

coefs :: [Complex Float]
coefs = coefsFromRoots rootsV
coefs' :: [Complex Float]
coefs' = deriveCoefs coefs


podd :: (Integral a, Num p) => a -> p
podd n
    | odd n = -1
    | otherwise = 1

binom :: Int -> Int -> [S.Vector Int]
binom n k
  | k < 0 = []
  | k > n = []
  | n <= 0 = [G.empty]
  | k == n = [G.enumFromN 1 n]
  | otherwise = binom (n-1) k ++ ((`G.snoc` n) <$> binom (n-1) (k-1))

coefsFromRoots :: (Num a, S.Storable a, G.Vector v a) => v a -> [a]
coefsFromRoots roots =
    let n = G.length roots
     in [ podd (n-k) * sum [ G.product (G.map (\i -> roots G.! (i-1)) p) | p <- binom n (n-k) ] | k <- [0..n] ]

deriveCoefs :: Num b => [b] -> [b]
deriveCoefs coefs = (\(i, c) -> fromIntegral i * c) <$> zip [1..] (tail coefs)

evalPoly :: Num a => [a] -> a -> a
evalPoly [] _ = 0
evalPoly (c0:cs) z = c0 + z * evalPoly cs z

newton :: (RealFloat a, Num t, Eq t) => t -> [Complex a] -> [Complex a] -> Complex a -> Complex a
newton steps coefs coefs' x0 =
    if steps == 0 || magnitude fx < 0.1 then x0
    else newton (steps - 1) coefs coefs' (x0 - fx/fpx)
    where
        fx = evalPoly coefs x0
        fpx = evalPoly coefs' x0

d :: RealFloat a => Complex a -> Complex a -> a
d c1 c2 = magnitude (c2 - c1)

pix :: Int -> Float -> Int -> Int -> PixelRGB8
pix st sz i j =
    let z = realToFrac i / sz :+ realToFrac j / sz
        zr = newton st coefs coefs' z
        (dist, idx) = minimum (zip [d zr r | r <- roots] [0..])
    in colors V.! idx
