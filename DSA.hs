module DSA where

import System.Random
import Test.QuickCheck
import Control.Monad
import Data.Maybe

-- 1:

type PrivateKey = Integer
type PublicKey  = Integer
type KeyPair    = (PrivateKey, PublicKey)
type ParamTuple = (Integer, Integer, Integer) -- (p, q, g)


type Digest    = Integer
type Signature = (Integer, Integer) -- (r, s)

-- 2:

sign :: ParamTuple -> KeyPair -> Digest -> Signature
sign = undefined

-- 3:

verify :: ParamTuple -> PublicKey -> Digest -> Signature -> Bool
verify = undefined

-- 4:

check :: ParamTuple -> Bool
check = undefined

-- 5:

hex :: String -> Integer
hex s = read ("0x" ++ s)

main :: IO ()
main = do
  g <- getStdGen
  putStrLn "undefined,   NOT!"

-- Prime checker:

definetlyComposite :: Integer -- ^ The Prime?
                   -> Integer -- ^ Guessed Counterexample
                   -> Bool
definetlyComposite n a =
    let x0 = a^m `mod` n
    in fromMaybe True
        (msum $ map snd $ take (s+1) $ iterate aux (x0, myReturn (x0 == 1) False))
  where
    (s, m) = head [(s, m) | s <- [0..], let m = (n-1) `div` 2^s, odd m]
    aux (x, _) = let x' = x^2 `mod` n
                 in (x', myReturn (x == n-1) False `mplus`
                         myReturn (x' == 1)  True)
    myReturn :: Bool -> a -> Maybe a
    myReturn b a = guard b >> Just a

probablyPrime :: Integer -> Bool
probablyPrime n = not . or $ map f list
  where f    = definetlyComposite n
        list = undefined

-- Random generation:

constantStdGen = mkStdGen 0

randomBigInteger :: RandomGen g => Int -- ^ Number of bytes
                                -> g   -- ^ Generator
                                -> (Integer, g)
randomBigInteger n g = undefined

randomBigPrime :: RandomGen g => Int -- ^ Number of bytes
                                -> g   -- ^ Generator
                                -> (Integer, g)
randomBigPrime n g = undefined

genBigInteger :: Int -> Gen Integer
genBigInteger n = undefined

-- Bonus properties:



prop_checkTestP = check (7, 5, 4) == False
prop_checkTestQ = check (7, 5, 4) == False
prop_checkTestG = check (7, 5, 4) == False


