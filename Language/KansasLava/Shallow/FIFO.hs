{-# LANGUAGE ScopedTypeVariables #-}
module Language.KansasLava.Shallow.FIFO 
	( ShallowFIFO		-- abstract
	, newShallowFIFO
	, writeToFIFO
	, readFromFIFO
	, readFileToFIFO
	, writeFileFromFIFO
	, hGetToFIFO
	, hPutFromFIFO
	, getFIFOContents
	, putFIFOContents
	) where

import Language.KansasLava.Stream
import Language.KansasLava.Wire
import Language.KansasLava.Types
import Language.KansasLava.Signal
import Language.KansasLava.Seq
import Language.KansasLava.Comb
import Language.KansasLava.Protocols
import Language.KansasLava.StdLogicVector
import Language.KansasLava.Utils -- for fromSLV

import Data.Sized.Ix
import Data.Sized.Unsigned
import Debug.Trace
import Data.Maybe as Maybe
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.Char as Char 
import System.IO
import Control.Concurrent


-- We include maybe, so we can simulate the concept
-- of there being no data available to pass on
-- at a specific point from a FIFO.

data ShallowFIFO a = ShallowFIFO (MVar (Maybe a))

newShallowFIFO :: IO (ShallowFIFO a)
newShallowFIFO = do
	var <- newEmptyMVar 
	return $ ShallowFIFO var

-- | blocks if the FIFO is not cycled on.
--   Nothing means step a cycle;
--   Just means cycle until value is read.
writeToFIFO :: (Show a) => ShallowFIFO a -> Maybe a -> IO ()
writeToFIFO (ShallowFIFO var) a = do
--	print "writing to FIFO"
	putMVar var a
--	print $ "written " ++ show a

-- | block if the FIFO has no values in it yet.
-- Nothing means no value issued in a cycle;
-- Just means value accepted from circuit.
readFromFIFO :: (Show a) => ShallowFIFO a -> IO (Maybe a)
readFromFIFO (ShallowFIFO var) = do
--	print "reading from FIFO"
	v <- takeMVar var
--	print $ "read " ++ show v
	return v

-- | readFileToFIFO returns after the file has been consumed
-- by the FIFO.

readFileToFIFO :: String -> ShallowFIFO Byte -> IO ()
readFileToFIFO file fifo = do
	h <- openFile file ReadMode
	hGetToFIFO h fifo


writeFileFromFIFO :: String -> ShallowFIFO Byte -> IO ()
writeFileFromFIFO file fifo = do
	h <- openFile file WriteMode
	hSetBuffering h NoBuffering	
	hPutFromFIFO h fifo

-- | read a file into a fifo as bytes.
hGetToFIFO :: Handle -> ShallowFIFO Byte -> IO ()
hGetToFIFO h fifo = do 
	str <- hGetContents h
	putFIFOContents fifo (map (Just . toByte) str)
	putFIFOContents fifo (repeat $ Nothing)

hPutFromFIFO :: Handle -> ShallowFIFO Byte -> IO ()
hPutFromFIFO h fifo = do
   forkIO $ do
	xs <- getFIFOContents fifo
	sequence_ 
		[ do hPutChar h $ fromByte x
		     hFlush h
	        | Just x <- xs
	        ]
	-- never finishes
   return ()

---------------------------------------------------------
-- Candidates for another package

-- Very general, might be useful elsewhere
getFIFOContents :: (Show a) => ShallowFIFO a -> IO [Maybe a]
getFIFOContents fifo = unsafeInterleaveIO $ do
	x <- readFromFIFO fifo
	xs <- getFIFOContents fifo
	return (x:xs)

putFIFOContents :: ShallowFIFO a -> [Maybe a] -> IO ()
putFIFOContents (ShallowFIFO var) xs = sequence_ (map (putMVar var) xs)

-- Runs a program from stdin to stdout;
-- also an example of coding
-- interact :: (Src a -> Sink b) -> IO ()

