module Main where

import Lib
import Codec.Picture
import Control.Monad

main :: IO ()
main =
    let fractals = (\sz -> ImageRGB8 $ generateImage (pix 1000 (10*sz^2)) w h) <$> drop 1 [0,1..50]
    in forM_ (zip [0..] fractals) (\(i, f) -> saveBmpImage ("ah" ++ show i ++ ".bmp") f)
