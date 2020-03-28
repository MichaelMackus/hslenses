module Main where

import Lib
import Lenses
import Traversals

main :: IO ()
main = do
        -- IO + lens
        sequence_ [ print . show =<< ioPlus   (1,1)
                  , print . show =<< ioAll    [1,2,3]
                  , print . show =<< modTuple (1,1) ]
    where
        ioPlus = (_1 (\n -> (+n) <$> readLn))
        ioAll  = _all 0 (const readLn) . concat . map (:[0])
        modTuple t = do
            input <- readLn :: IO Int
            choosing _1 _2 (\n -> return $ n + abs input) $ if input < 0 then Left t else Right t

