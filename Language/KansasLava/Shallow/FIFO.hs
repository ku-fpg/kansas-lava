{-# LANGUAGE ScopedTypeVariables #-}
module Language.KansasLava.Shallow.FIFO where

import System.IO.Unsafe
import Control.Concurrent.MVar

-- We include maybe, so we can simulate the concept
-- of there being no data available to pass on
-- at a specific point from a FIFO.

data ShallowFIFO a = ShallowFIFO (MVar (Maybe a))
{-
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

readFileToFIFO :: String -> ShallowFIFO Word8 -> IO ()
readFileToFIFO file fifo = do
	h <- openFile file ReadMode
	hGetToFIFO h fifo


writeFileFromFIFO :: String -> ShallowFIFO Word8 -> IO ()
writeFileFromFIFO file fifo = do
	h <- openFile file WriteMode
	hSetBuffering h NoBuffering
	hPutFromFIFO h fifo

-- | read a file into a fifo as bytes. Does not terminate.
hGetToFIFO :: Handle -> ShallowFIFO Word8 -> IO ()
hGetToFIFO h (ShallowFIFO fifo) = do
    b <- hIsEOF h
    return ()
{-
    if b then do
        hClose h
--        forever $ putMVar

      else do
	str <- BS.hGetNonBlocking h 128 -- 128 is for no specific reason, just a number
					-- because it is non-blocking, only what is there
					-- is received.
	putFIFOContents fifo (map (Just . fromIntegral) (BS.unpack str))
	putFIFOContents fifo [Nothing]
	hGetToFIFO h fifo
-}

hPutFromFIFO :: Handle -> ShallowFIFO Word8 -> IO ()
hPutFromFIFO h fifo = do
   _ <- forkIO $ do
	xs <- getFIFOContents fifo
	sequence_
		[ do hPutChar h $ Char.chr (fromIntegral x)
		     hFlush h
	        | Just x <- xs
	        ]
	-- never finishes
   return ()

---------------------------------------------------------
-- Candidates for another package

-- Very general, might be useful elsewhere
-}
getFIFOContents :: (Show a) => MVar a -> IO [Maybe a]
getFIFOContents fifo = unsafeInterleaveIO $ do
	x <- tryTakeMVar fifo
	xs <- getFIFOContents fifo
	return (x:xs)

putFIFOContents :: MVar a -> [a] -> IO ()
putFIFOContents fifo xs = sequence_ (map (putMVar fifo) xs)

