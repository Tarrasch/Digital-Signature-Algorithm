module DSA where

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

probablyPrime :: Integer -> Bool
probablyPrime = undefined

check :: ParamTuple -> Bool
check = undefined

-- 5:

hex :: String -> Integer
hex s = read ("0x" ++ s)

main :: IO ()
main = putStrLn "undefined,   NOT!"
