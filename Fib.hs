module Main where

main :: IO ()
main = getLine >>= writeFibNumbers . read 

writeFibNumbers :: Int -> IO ()
writeFibNumbers n = doFib idIO (n+1) 0 1 2 >> putChar '\n'

doFib :: IO () -> Int -> Integer -> Integer -> Int -> IO ()
doFib act n ant acc i
    | i > n     = act
    | otherwise = act >> doFib (writeNum acc) n acc (ant+acc) (i+1)

writeNum :: Integer -> IO ()
writeNum n = putStr $ show n ++ " "

idIO :: IO ()
idIO = return ()