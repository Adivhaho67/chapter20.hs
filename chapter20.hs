{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import System.Random
import Data.Functor.Identity
import Data.Maybe
import Control.Exception
import System.IO

---------------------------------------------------------
-- HC20T1: safeDivide with Maybe Monad
---------------------------------------------------------
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

---------------------------------------------------------
-- HC20T2: sequenceMaybe
---------------------------------------------------------
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

---------------------------------------------------------
-- HC20T3: Writer Monad Logging Calculator
---------------------------------------------------------
addWithLog :: Int -> Int -> Writer [String] Int
addWithLog x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

---------------------------------------------------------
-- HC20T4: countChars with State Monad
---------------------------------------------------------
countChars :: Char -> String -> State Int ()
countChars c str = put $ length $ filter (==c) str

---------------------------------------------------------
-- HC20T5: Reader Monad for Configurable Greeting
---------------------------------------------------------
type Config = String
greet :: Reader Config String
greet = do
    name <- ask
    return $ "Hello, " ++ name ++ "!"

---------------------------------------------------------
-- HC20T6: doubleMonad Combining Maybe and List
---------------------------------------------------------
doubleMonad :: Maybe Int -> [Int] -> [Int]
doubleMonad mx xs = do
    x <- maybeToList mx
    y <- xs
    return (x + y)

---------------------------------------------------------
-- HC20T7: findFirst with Either Monad
---------------------------------------------------------
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst pred xs = maybe (Left "No element found") Right (find pred xs)

---------------------------------------------------------
-- HC20T8: Parser Monad for Simple Expressions
---------------------------------------------------------
newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

item :: Parser Char
item = Parser $ \s -> case s of
    [] -> Nothing
    (c:cs) -> Just (c, cs)

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (a, rest) <- p s
        return (f a, rest)

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser pf) <*> (Parser pa) = Parser $ \s -> do
        (f, s1) <- pf s
        (a, s2) <- pa s1
        return (f a, s2)

---------------------------------------------------------
-- HC20T9: replicateMonad with Identity Monad
---------------------------------------------------------
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = Identity $ replicate n x

---------------------------------------------------------
-- HC20T10: Nested StateT and MaybeT Transformer
---------------------------------------------------------
type NestedMonad s a = MaybeT (StateT s IO) a

---------------------------------------------------------
-- HC20T11: randomWalk with State Monad
---------------------------------------------------------
randomWalk :: State StdGen (Int, Int)
randomWalk = do
    gen <- get
    let (dx, g1) = randomR (-1,1) gen
        (dy, g2) = randomR (-1,1) g1
    put g2
    return (dx, dy)

---------------------------------------------------------
-- HC20T12: File Reading with IO Monad
---------------------------------------------------------
readFileLines :: FilePath -> IO ()
readFileLines file = do
    contents <- readFile file
    mapM_ putStrLn (lines contents)

---------------------------------------------------------
-- HC20T13: fibonacciMemo with State Monad
---------------------------------------------------------
fibonacciMemo :: Int -> State ([(Int,Int)]) Int
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
    memo <- get
    case lookup n memo of
        Just val -> return val
        Nothing -> do
            a <- fibonacciMemo (n-1)
            b <- fibonacciMemo (n-2)
            let val = a + b
            modify ((n,val):)
            return val

---------------------------------------------------------
-- HC20T14: mapMFilter Monadic Map-Filter
---------------------------------------------------------
mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter p = foldM (\acc x -> do
                        res <- p x
                        return $ if res then acc ++ [x] else acc) []

---------------------------------------------------------
-- HC20T15: treeSum with Custom Monad
---------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

treeSum :: Num a => Tree a -> a
treeSum (Leaf x) = x
treeSum (Node l r) = treeSum l + treeSum r

---------------------------------------------------------
-- HC20T16: retryIO with IO Monad
---------------------------------------------------------
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = catch (Just <$> action) (\(_ :: SomeException) -> retryIO (n-1) action)

---------------------------------------------------------
-- HC20T17: validatePassword with Either Monad
---------------------------------------------------------
validatePassword :: String -> Either [String] String
validatePassword pw =
    let errors = concat [if length pw < 6 then ["Too short"] else []
                        ,if not (any (`elem` ['0'..'9']) pw) then ["No digit"] else []
                        ,if not (any (`elem` ['A'..'Z']) pw) then ["No uppercase"] else []]
    in if null errors then Right pw else Left errors

---------------------------------------------------------
-- HC20T18: MaybeT Monad Transformer for User Input
---------------------------------------------------------
maybeReadInt :: MaybeT IO Int
maybeReadInt = do
    liftIO $ putStrLn "Enter a number:"
    input <- liftIO getLine
    MaybeT $ return (readMaybe input)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x,"")] -> Just x
    _ -> Nothing

---------------------------------------------------------
-- HC20T19: Writer Monad-based Logging System
---------------------------------------------------------
logAdd :: Int -> Int -> Writer [String] Int
logAdd x y = writer (x+y, ["Added " ++ show x ++ " and " ++ show y])

---------------------------------------------------------
-- HC20T20: batchProcessing with Monadic Bind
---------------------------------------------------------
batchProcessing :: Monad m => m a -> m b -> m (a,b)
batchProcessing action1 action2 = do
    a <- action1
    b <- action2
    return (a,b)

---------------------------------------------------------
-- MAIN FUNCTION: Demonstrates key HC20 tasks
---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--- HC20T1: safeDivide ---"
    print $ safeDivide 10 2
    print $ safeDivide 5 0

    putStrLn "\n--- HC20T2: sequenceMaybe ---"
    print $ sequenceMaybe [Just 1, Just 2, Just 3]
    print $ sequenceMaybe [Just 1, Nothing, Just 3]

    putStrLn "\n--- HC20T3: Writer Monad Logging Calculator ---"
    let (res, log) = runWriter $ addWithLog 5 10
    print res
    print log

    putStrLn "\n--- HC20T4: countChars with State Monad ---"
    print $ execState (countChars 'a' "banana") 0

    putStrLn "\n--- HC20T5: Reader Monad Greeting ---"
    print $ runReader greet "Alice"

    putStrLn "\n--- HC20T6: doubleMonad ---"
    print $ doubleMonad (Just 3) [1,2,3]

    putStrLn "\n--- HC20T7: findFirst ---"
    print $ findFirst even [1,3,5,6,7]
    print $ findFirst even [1,3,5]

    putStrLn "\n--- HC20T9: replicateMonad ---"
    print $ runIdentity $ replicateMonad 4 "Hi"

    putStrLn "\n--- HC20T15: treeSum ---"
    print $ treeSum (Node (Leaf 5) (Node (Leaf 10) (Leaf 3)))

    putStrLn "\n--- HC20T17: validatePassword ---"
    print $ validatePassword "Ab1"
    print $ validatePassword "Abcdef1"

    putStrLn "\n--- HC20T20: batchProcessing ---"
    print =<< batchProcessing (return 5) (return 10)
