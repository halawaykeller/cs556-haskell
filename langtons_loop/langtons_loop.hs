import Data.Char
import Data.Array
import System.IO
import System.Console.ANSI
import TransitionTable
import Data.Digits

putStrFatLn :: [Char] -> IO ()
putStrFatLn cs = do
  sequence_ [putChar c >> putChar ' ' | c <- cs]
  putChar '\n'

printArrayFat :: Array (Int, Int) Char -> IO ()
printArrayFat a = mapM_ putStrFatLn (arrayToLulz a)

emptyArray :: (Ix p1, Ix p2, Num p1, Num p2) => e -> p1 -> p2 -> Array (p1, p2) e
emptyArray init n m = array b [(i,init) | i <- range b]
    where b = ((0,0),(n-1,m-1))

lulzToArray :: [[Char]] -> Array (Int,Int) Char
lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(n,m))
          n = length xs - 1
          m = length x - 1

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]

printArray :: Array (Int,Int) Char -> IO ()
printArray a = mapM_ putStrLn (arrayToLulz a)

rowIndices :: Enum a => Array (a, b) e -> [a]
rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a

colIndices :: Enum a1 => Array (a2, a1) e -> [a1]
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a

row :: (Enum t, Ix a1, Ix t) => Array (a1, t) a2 -> a1 -> [a2]
row a j = [a ! (j,k) | k <- colIndices a]

inset :: Ix i => Array i e -> Array i e -> Array i e
inset a a' = a' // assocs a

digitToIntArray :: Functor f => f Char -> f Int
digitToIntArray = fmap digitToInt

intToDigitArray :: Functor f => f Int -> f Char
intToDigitArray = fmap intToDigit

zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int,Int) b -> Array (Int,Int) c
zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) (b ! (j,k)) ) | i@(j,k) <- indices a]


(%) :: Array (Int,Int) a -> (Int,Int) -> a
a % (j,k) = a ! (j `mod` (m+1),k `mod` (n+1)) where (m,n) = snd $ bounds a

neighbors :: Array (Int, Int) Int -> (Int, Int) -> [Int]
neighbors a (j,k) = [a % (j,k), a % (j - 1, k), a % (j, k + 1), a % (j + 1, k), a % (j, k - 1)]

neighborsArray :: Array (Int, Int) Int -> Array (Int, Int) [Int]
neighborsArray a = array (bounds a) [ (i, neighbors a (j,k)) | i@(j,k) <- indices a]

showLoop :: Int -> Char
showLoop n = case n of
    0 -> ' ' 
    _ -> intToDigit n

startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]


fromDigits :: (Functor f, Foldable t, Show a) => f (t a) -> f [Char]
fromDigits x = (fmap . concatMap) show x

update :: Array (Int, Int) Int -> Array (Int, Int) Int
update a = zipWithArray transition a (fromDigits $ neighborsArray a)

loop :: Array (Int, Int) Int -> IO ()
loop a = do
    setCursorPosition 0 0
    printArrayFat $ showLoop <$> a
    loop $ update a

main :: IO ()
main = do
    hideCursor
    clearScreen
    loop $ inset (digitToIntArray $ lulzToArray startLoop) (emptyArray 0 100 100)




