module DSA where

import System.Random
import System.Exit
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List

-- 1:

mod2 a b = mod ((mod a b)+b) b

type PrivateKey = Integer
type PublicKey  = Integer
type KeyPair    = (PrivateKey, PublicKey) -- (x, y)
type ParamTuple = (Integer, Integer, Integer) -- (p, q, g)


type Digest    = Integer
type Signature = (Integer, Integer) -- (r, s)

-- 2:

sign :: ParamTuple -> KeyPair -> Digest -> Signature
sign (p, q, g) (x, y) z = (r, s)
  where
    (k, _) = randomR (1, q-1) constantStdGen
    r = moduloPower p g k `mod2` q
    s = (z + x*r) // k
    (//) = moduloDiv q

-- 3:

verify :: ParamTuple -> PublicKey -> Digest -> Signature -> Bool
verify (p, q, g) y z (r, s) = v == r
  where
    w  = moduloDiv q 1 s
    u1 = (z*w) `mod2` q
    u2 = (r*w) `mod2` q
    v  = let g' = moduloPower p g u1
             y' = moduloPower p y u2
         in (g'*y') `mod2` q

-- 4:

check :: ParamTuple -> Bool
check (p, q, g) = and [
   p < 2^1024
 , q < 2^160
 , probablyPrime p
 , probablyPrime q
 , ((p-1) `mod2` q) == 0
 , g > 1
 , moduloPower p g q == 1
 ]

-- 5:

hex :: String -> Integer
hex s = read ("0x" ++ s)

readVar :: Read a => IO a
readVar = fmap (read . drop 2) getLine

main :: IO ()
main = do
  gen <- getStdGen
  pqg@(p, q, g) <- liftM3 (,,) readVar readVar readVar
  unless (check pqg) $ putStrLn "invalid_group" >> exitSuccess
  putStrLn "valid_group"
  workType <- getLine
  case workType of
    "genkey" -> do
       n <- readVar
       let xs = take n $ randomRs (2, p-2) gen
       let ys = map (moduloPower p g) xs
       forM_ (xs `zip` ys) $ \(x, y) -> do
         putStrLn $ "x=" ++ show x
         putStrLn $ "y=" ++ show y

-- Modulo Arithmetic:

gcdE :: Integer -> Integer -> (Integer, Integer, Integer)
gcdE a 0 = (a, 1, 0)
gcdE a b = (d, t, s - q*t)
  where
    r         = a `mod2` b
    q         = a `div` b
    (d, s, t) = gcdE b r

-- This division assumes that everything
-- will go ok, and it will if m is prime
-- and den /= 0
moduloDiv :: Integer -- n
          -> Integer -- num
          -> Integer -- den
          -> Integer -- (num/den) mod n
moduloDiv n b a = b*t `mod2` n
  where (_d, _s, t) = gcdE n (a `mod2` n)

moduloPower :: Integer -- m
            -> Integer -- b
            -> Integer -- e
            -> Integer -- (b^e) mod m
moduloPower m b 0          = 1
moduloPower m b e | (e<0)  = error "negative exponent"
                  | even e = (v*v) `mod2` m
                  | odd e  = (v*v*b) `mod2` m
  where
    v = moduloPower m b (e `div` 2)

-- Prime checker:

definetlyComposite :: Integer -- ^ The Prime?
                   -> Integer -- ^ Guessed Counterexample
                   -> Bool
definetlyComposite n a =
    let x0 = a^^m
    in fromMaybe True
        (msum $ map snd $ genericTake (s+1) $ iterate aux (x0, myReturn (x0 == 1) False))
  where
    (s, m) = head [(s, m) | s <- [0..], let m = (n-1) `div` 2^s, odd m]
    aux (x, _) = let x' = x^^2
                 in (x', myReturn (x == n-1) False `mplus`
                         myReturn (x' == 1)  True)
    myReturn :: Bool -> a -> Maybe a
    myReturn b a = guard b >> Just a
    (^^) = moduloPower n

probablyPrime :: Integer -> Bool
probablyPrime n = not . or $ map f list
  where f    = definetlyComposite n
        list = take 40 $ randomRs (2, n-1) constantStdGen

-- Random generation:

constantStdGen = mkStdGen 1234567 -- a random number

randomBigInteger :: RandomGen g => Int -- ^ Number of bytes
                                -> g   -- ^ Generator
                                -> (Integer, g)
randomBigInteger n g = randomR (0, 2^n-1) g

randomBigPrime :: RandomGen g => Int -- ^ Number of bytes
                              -> g   -- ^ Generator
                              -> (Integer, g)
randomBigPrime n g = let (bignum, g') = randomBigInteger n g
                     in (firstPrimeFrom bignum, g')

firstPrimeFrom n = fromJust $ find probablyPrime [n..]

genBigInteger :: Int -> Gen Integer
genBigInteger n = undefined

-- Bonus properties:

prop_division p' num den = p' > 2 && (den `mod` p) /= 0 ==> ((num - den*quotient) `mod` p) == 0
  where p        = firstPrimeFrom p'
        quotient = moduloDiv p num den

prop_exponentiation n b e = n > 0 && e >= 0 ==> ((f e)*b - f (e+1)) `mod` n == 0
  where f = moduloPower n b

prop_checkTestP = check (7, 5, 4) == False
prop_checkTestQ = check (7, 5, 4) == False
prop_checkTestG = check (7, 5, 4) == False


