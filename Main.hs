{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import System.Environment
import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Attoparsec
import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

main :: IO ()
main = do
    filePath <- unwords <$> getArgs
    numbers <- runResourceT $ sourceFile filePath $$ sinkParser parser
    let concatenated = B.pack $ concatMap show $ uncurry multiplicativeInverse <$> numbers
        flag = hash concatenated :: Digest SHA1
    print flag

egcd' :: (Integral a) => (a, a, a, a, a, a) -> (a, a, a, a, a, a)
egcd' (a, r, s, b, t, u)
  | a >= b     = (a - b, r - t, s - u, b, t, u)
  | otherwise = (b, t, u, a, r, s)

egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a b = (a', r', s')
  where (a', r', s', _, _, _) = until (\(_, _, _, b', _, _) -> b' == 0) egcd' (a, 1, 0, b, 0, 1)

multiplicativeInverse :: Integral a => a -> a -> a
multiplicativeInverse a b = until (> 0) (+ b) r
  where (_, r, _) = egcd a b

igcd :: (Integral a) => a -> a -> a
igcd a b
  | b == 0    = a
  | otherwise = gcd b $ a `mod` b

parser :: A.Parser [(Integer, Integer)]
parser = ((,) <$> A.decimal <* A.string " : " <*> A.decimal) `A.sepBy` A.char '\n'
