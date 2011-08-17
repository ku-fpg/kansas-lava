{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Protocols where

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned


tests :: TestSeq -> IO ()
tests test = do
        -- testing Streams

        let fifoTest :: forall w . (Rep w,Eq w, Show w, Size (W w)) 
		      => String
		      -> Patch (Seq (Enabled w)) (Seq (Enabled w)) (Seq Ack) (Seq Ack) -> StreamTest w w
            fifoTest n f = StreamTest
                        { theStream = f
                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
                                case () of
                                  () | outs /= take (length outs) ins -> return "in/out differences"
                                  () | length outs < fromIntegral count 
     								      -> return ("to few transfers: " ++ show (length outs))
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 10000
                        , theStreamName = n
                        }
	   	where
			count = 100

{-
	let bridge' :: forall w . (Rep w,Eq w, Show w, Size (W w)) 
		      	=> (Seq (Enabled w), Seq Full) -> (Seq Ack, Seq (Enabled w))
	    bridge' =  bridge `connect` shallowFIFO `connect` bridge
-}

	testStream test "U5"   (fifoTest "fifo1" fifo1) (arbitrary :: Gen (Maybe U5))
	testStream test "Bool" (fifoTest "fifo1" fifo1) (arbitrary :: Gen (Maybe Bool))
	testStream test "U5"   (fifoTest "fifo2" fifo2) (arbitrary :: Gen (Maybe U5))
	testStream test "Bool" (fifoTest "fifo2" fifo2) (arbitrary :: Gen (Maybe Bool))

	return ()
