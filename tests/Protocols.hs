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
		      -> ((Seq (Enabled w), Seq Full) -> (Seq Ack, Seq (Enabled w))) -> StreamTest w w
            fifoTest n f = StreamTest
                        { theStream = f :: (Seq (Enabled w), Seq Full) -> (Seq Ack, Seq (Enabled w))
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
	let handShakeMailBox' :: forall w . (Rep w,Eq w, Show w, Size (W w)) 
		      	=> (Seq (Enabled w), Seq Full) -> (Seq Ack, Seq (Enabled w))
	    handShakeMailBox' =  handShakeMailBox `connect` shallowFIFO `connect` handShakeMailBox
-}

	testStream test "U5"   (fifoTest "handShakeMailBox" handShakeMailBox) (arbitrary :: Gen (Maybe U5))
	testStream test "Bool" (fifoTest "handShakeMailBox" handShakeMailBox) (arbitrary :: Gen (Maybe Bool))
--	testStream test "U5"   (fifoTest "shallow/handShakeMailBox2" handShakeMailBox') (arbitrary :: Gen (Maybe U5))
--	testStream test "Bool" (fifoTest  "shallow/handShakeMailBox2" handShakeMailBox') (arbitrary :: Gen (Maybe Bool))

	return ()
