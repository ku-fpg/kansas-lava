{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

module Protocols where

import Language.KansasLava

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix ((!), matrix, Matrix)
--import qualified Data.Sized.Matrix as M
--import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
        -- testing Streams

        let fifoTest :: forall w . (Rep w, Eq w, Show w, Size (W w)) 
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

	testStream test "U5"   (fifoTest "idPatch" idPatch :: StreamTest U5 U5)
	testStream test "Bool" (fifoTest "idPatch" idPatch :: StreamTest Bool Bool)
	testStream test "U5"   (fifoTest "fifo1" fifo1 :: StreamTest U5 U5)
	testStream test "Bool" (fifoTest "fifo1" fifo1 :: StreamTest Bool Bool)
	testStream test "U5"   (fifoTest "fifo2" fifo2 :: StreamTest U5 U5)
	testStream test "Bool" (fifoTest "fifo2" fifo2 :: StreamTest Bool Bool)


	-- This tests dupPatch and zipPatch
        let patchTest1 :: forall w . (Rep w,Eq w, Show w, Size (W w), Num w) 
		      => StreamTest w (w,w)
            patchTest1 = StreamTest
                        { theStream = dupPatch $$ fstPatch (forwardPatch $ mapEnabled (+1)) $$ zipPatch
                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
--				trace (show (ins,outs)) $ 
                                case () of
				  () | length outs /= length ins -> return "in/out differences"
				     | any (\ (x,y) -> x - 1 /= y) outs -> return "bad result value"
				     | ins /= map snd outs -> return "result not as expected"
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 10000
                        , theStreamName = "dupPatch-zipPatch"
                        }
	   	where
			count = 100

	testStream test "U5" (patchTest1 :: StreamTest U5 (U5,U5))


	-- This tests matrixDupPatch and matrixZipPatch
        let patchTest2 :: forall w . (Rep w,Eq w, Show w, Size (W w), Num w) 
		      => StreamTest w (Matrix X3 w)
            patchTest2 = StreamTest
                        { theStream = matrixDupPatch $$ matrixStack (matrix [ 
								forwardPatch $ mapEnabled (+0),
								forwardPatch $ mapEnabled (+1),
								forwardPatch $ mapEnabled (+2)]								
								) $$ matrixZipPatch
                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
--				trace (show (ins,outs)) $ 
                                case () of
				  () | length outs /= length ins -> return "in/out differences"
				     | any (\ m -> m ! 0 /= (m ! 1) - 1) outs -> return "bad result value 0,1"
				     | any (\ m -> m ! 0 /= (m ! 2) - 2) outs -> return $ "bad result value 0,2"
				     | ins /= map (! 0) outs -> return "result not as expected"
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 10000
                        , theStreamName = "matrixDupPatch-matrixZipPatch"
                        }
	   	where
			count = 100

	testStream test "U5" (patchTest2 :: StreamTest U5 (Matrix X3 U5))

	-- This tests muxPatch (and matrixMuxPatch)
        let patchTest3 :: forall w . (Rep w,Eq w, Show w, Size (W w), Num w, w ~ U5)
		      => StreamTest w w
            patchTest3 = StreamTest
                        { theStream = 
				fifo1 $$
				dupPatch $$ 
				stack (forwardPatch (mapEnabled (*2)) $$ fifo1)
				      (forwardPatch (mapEnabled (*3)) $$ fifo1) $$
				openPatch $$
				fstPatch (cyclePatch (matrix [True,False] :: Matrix X2 Bool) $$ fifo1) $$
				muxPatch


                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
--				trace (show (ins,outs)) $ 
                                case () of
				  () | length outs /= length ins * 2 -> return "in/out size issues"
			             | outs /= concat [ [n * 2,n * 3] | n <- ins ]
								     -> return "value out distored"
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 10000
                        , theStreamName = "muxPatch"
                        }
	   	where
			count = 100

	testStream test "U5" (patchTest3 :: StreamTest U5 U5)

	-- This tests deMuxPatch (and matrixDeMuxPatch), and zipPatch
        let patchTest4 :: forall w . (Rep w,Eq w, Show w, Size (W w), Num w, w ~ U5)
		      => StreamTest w (w,w)
            patchTest4 = StreamTest
                        { theStream = 
				openPatch $$
				fstPatch (cyclePatch (matrix [True,False] :: Matrix X2 Bool) $$ fifo1) $$
				deMuxPatch $$
				stack (fifo1) (fifo1) $$
				zipPatch 
                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
--				trace (show (ins,outs)) $ 
                                case () of
				  () | length outs /= length ins `div` 2 -> return "in/out size issues"
			             | concat [ [a,b] | (a,b) <- outs ] /= ins
								     -> return "value out distored"
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 10000
                        , theStreamName = "deMuxPatch-zipPatch"
                        }
	   	where
			count = 100

	testStream test "U5" (patchTest4 :: StreamTest U5 (U5,U5))
	return ()
