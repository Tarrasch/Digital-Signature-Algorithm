module DSA where

import System.Random
import System.IO
import System.Exit
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import Test.QuickCheck.Property

-- 0:

-- This version always give positive numbers
mod2 a b = mod ((mod a b)+b) b

-- 1:

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
verify (p, q, g) y z (r, s) = (v - r) `mod` p == 0
  where
    w  = moduloDiv q 1 s
    u1 = (z*w) `mod2` q
    u2 = (r*w) `mod2` q
    v  = let g' = moduloPower p g u1
             y' = moduloPower p y u2
         in ((g'*y') `mod2` p) `mod2` q

-- 4:

check :: ParamTuple -> Bool
check (p, q, g) = and [
   p < 2^1024
 , 2^1023 < p
 , q < 2^160
 , 2^159 < q
 , probablyPrime p
 , probablyPrime q
 , ((p-1) `mod` q) == 0
 , g > 1
 , moduloPower p g q == 1
 ]

-- 5:

hex :: String -> Integer
hex s = read ("0x" ++ s)

readVar :: Read a => IO a
readVar = fmap (read . drop 2) getLine

readHex = fmap (hex . drop 2) getLine

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  gen <- getStdGen
  pqg@(p, q, g) <- liftM3 (,,) readVar readVar readVar
  unless (check pqg) $ putStrLn "invalid_group" >> exitSuccess
  putStrLn "valid_group"
  workType <- getLine
  case workType of
    "genkey" -> do
       n <- readVar
       let xs = take n $ randomRs (1, q-1) gen
       let ys = map (moduloPower p g) xs
       forM_ (xs `zip` ys) $ \(x, y) -> do
         putStrLn $ "x=" ++ show x
         putStrLn $ "y=" ++ show y
    "sign" -> do
       keyPair <- liftM2 (,) readVar readVar
       let loop = do
              d <- readHex
              let (r, s) = sign pqg keyPair d
              putStrLn $ "r=" ++ show r
              putStrLn $ "s=" ++ show s
              isEOF >>= (flip unless loop)
       loop
    "verify" -> do
       y <- readVar
       let loop = do
              (digest, signature) <- (liftM3 (\d r s -> (d,(r,s)))) readHex readVar readVar
              putStrLn $ "signature_" ++ (if verify pqg y digest signature then "" else "in") ++ "valid"
              isEOF >>= (flip unless loop)
       loop

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

-- Bonus properties:

prop_division p' num den = p' > 2 && (den `mod` p) /= 0 ==> ((num - den*quotient) `mod` p) == 0
  where p        = firstPrimeFrom p'
        quotient = moduloDiv p num den

prop_division2 p' den = p' > 2 && (den `mod` p) /= 0 ==>
    ((moduloPower p den (p-2) - inverse) `mod` p) == 0
  where p       = firstPrimeFrom p'
        inverse = moduloDiv p 1 den

prop_exponentiation n b e = n > 0 && e >= 0 ==> ((f e)*b - f (e+1)) `mod` n == 0
  where f = moduloPower n b

-- This test *sometimes* works!
prop_signverify q' x z = q' > 2 && x > 0 ==> verify (p, q, g) y z sig
  where q        = firstPrimeFrom (q'+100)
        g        = head [ g | g <- [2..], moduloPower p g q == 1 ]
        y        = moduloPower p g x
        p        = head [ p | i <- [1..], let p = q*i+1, probablyPrime p]
        sig      = sign (p, q, g) (x, y) z

prop_testcase1 = verify (p, q, g) y z sig && (r, s) == sig
  where
    s = 2400393222564555908391347207445523231451472962791755231183768202200889239149312173931785496320125091509036296492264578483620741286808772954056
    r = 483492193095924188572321574179634603476096843404
    p = 102865584259843077175583195011997798900482038016705824136288380475734860009055428071534495956844807748416572686838253895244634687898659646424515259679129905513743899853971066468883670407530107234961085482225328667572772611162756643027105617873895021996158552984843708233824989792811721408577351617080369547993
    q = 734415599462729831694143846331445277609193755927
    g = 63615006880335642768473038477258757436464860136916565207798584167060621564899979263408565137993978149206751054438974059615983337126379668370747907507911540381031959187353048278562320341063050939775344313271013777131358834376209974551749493023310606751625276738876397935042130121966817767949476523717161640453
    x = 26113103522523020176414873679932136740129942582
    y = 101952397139177835361091861837631220146700449496896620149665197558694791053831413351345428198152979773356960102186871728746279840083490428839319519387598558491748752838120567713828245452116639056468933861000487483478440050473297219009905336213325893010208431445187193695880138304957565638232229852380831184780
    z = hex "767FACDD7987C122B6F531EF4ABEB2522EFF3664"
    sig = sign (p, q, g) (x, y) z
