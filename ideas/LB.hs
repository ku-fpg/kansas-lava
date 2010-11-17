{- 
 - Example of interacting with a lambda bridge.
 -
 - Author: Andy Gill (andygill@ku.edu)
 -}


module Main where

import Foreign.LambdaBridge
import System.IO

main = do
	putStrLn "Connecting to 'FSL' lava lambda bridge"
	([send],[recv]) <- board_connect (1,1) ["./FSL"]

	hPutStrLn send "Hello, World!"
	hFlush send

	loop $ do
		c <- hGetChar recv
		print c
		

	putStrLn "Exiting lambda bridge"

loop m = do { m ; loop m }