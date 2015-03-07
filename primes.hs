import Data.Bits ((.&.), shiftR, Bits)
import Data.List (union)
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

main = print . take 1000000 $  primeBits

primeBits :: [Int]
primeBits = concatMap (reverse . bitsOf) thePrimes

bitsOf :: (Bits a, Num a) => a -> [a]
bitsOf = map (.&. 1) . takeWhile (/= 0) . iterate (`shiftR` 1)

vonNeumannDebias :: [Int] -> [Int]
vonNeumannDebias (a:b:xs) | a == b = vonNeumannDebias xs
                          | a == 1 = 1 : vonNeumannDebias xs
                          | a == 0 = 0 : vonNeumannDebias xs

-- Fast prime generation
sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve
 
primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

thePrimes :: [Int]
thePrimes = primesToUA 100000000