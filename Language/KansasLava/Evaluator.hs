{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables, FlexibleInstances #-}
module Language.KansasLava.Evaluator where

import Data.Map as M

import Language.KansasLava.Entity
import Language.KansasLava.Signal
import Language.KansasLava.Reify
import Language.KansasLava.Seq as Seq

import Data.Reify
--import Control.Concurrent.Chan
import Control.Concurrent
import Control.Applicative
import Control.Exception as E
import Debug.Trace
import Data.Monoid
import Data.Traversable

import Prelude hiding (abs)
import qualified Prelude

-- This will move into the Data.Reify world, later. 
-- It only depends on Entity being a Functor.
evaluateSignal
	:: Signal a	                        -- the circuit 
	-> (Entity v -> v)                      -- evaluate each entity
	-> IO v  	                        -- the output, via IO
evaluateSignal signal eval = do
	Graph circuit result <- reifyCircuit signal
	print circuit
	let fn u =
	           case M.lookup u $ M.fromList
                                [ (uq,eval $ fmap fn nd)
                                | (uq,nd) <- circuit
		                ] of
		     Nothing -> error $ "can not find : " ++ show u     
		     Just val -> val
	return (fn result)

-- This assumes a list of values, over time.

liftToRep :: (Value a) => Eval a -> Eval (Rep a)
liftToRep (Eval fn) = Eval $ \ entity ->
   case fn (demote entity) of
      Nothing -> Nothing
      Just fn' -> Just $ \ vs -> rep $ fn' (Prelude.map abs vs)
 where
    demote :: (Value a) => Entity (Rep a) -> Entity a
    demote = fmap abs

{-
evalFromInteger :: (Num a, OpType a, Rep Integer ~ Rep a) => Eval (Rep a)
evalFromInteger = Eval $ \ entity ->
    let modName = findEntityTyModName (fmap abs entity) in
    case entity of
        (Entity (Name nm' "fromInteger") _) 
                | nm' == modName -> return $ \ [v1] -> rep (fromInteger (abs v1))
        _ -> Nothing
-}

class Value v where
  type Rep v
  rep :: v -> Rep v
  abs :: Rep v -> v
{-
                :: (Eval (Seq v))                       -- eval helper function
                -> (Signal v -> (Signal v -> Signal v))               -- fun to be evaled
                -> IO (v -> (v -> IO v))

                :: (Eval (Seq v))
                -> (Signal v -> rest v)
                -> IO (v -> rest' v)
                
                -> (Signal v)
                -> IO (IO v)
                
F (Signal v -> r) = v -> F r
-}
{-
class X v where
  type F v

instance X (Signal v -> r) where
  type F (Signal v -> r) = v -> F r

instance X (Signal v) where
  type F (Signal v) = IO v

class Input a where
-}


{-

evaluateInnerCircuit
                :: (Eval (Seq v))                       -- eval helper function
                -> (Signal v -> Signal v)               -- fun to be evaled
                -> IO (v -> IO v)
evaluateCircuit circuit (Eval eval)  = do
        let pad = Signal $ Wire $ Pad $ Name "" "input"
        inChan <- newChan
        outChan <- newChan
        as <- getChanContents inChan 
        let eval' (Pad (Name "" "input")) = foldr (:~) (error "END") $ Prelude.map rep as
            eval' e = case eval e of
                        Nothing -> error $ ("problem with evaluator: " ++ 
                                                case e of
                                                  Entity n _ -> show n
                                                  Lit i -> "LIT: " ++ show i
                                                  Port _ _ -> "PORT"
                                                  Pad n  -> "PAD: " ++ show n
                                                  _ -> "???")
                        Just fn -> case e of
                                     (Entity _ vs) -> fn vs
                                     (Lit i)       -> fn []
                                     _ -> error $ "problem with evaluator"
                                     
        signal <- evaluateSignal (circuit pad) eval'
        let seqToList (a :~ as) = a : seqToList as
        let signal' = Seq.toList signal
        forkIO $ E.catch
                (writeList2Chan outChan (Prelude.map abs signal'))
                (\ (e :: E.SomeException) -> print e)
        return $ \ a -> do
                writeChan inChan a
                readChan outChan
-}

-- Not sure about this; perhaps should be v -> v

evaluateCircuit :: (Value a, Rep a ~ v,
                    Value b, Rep b ~ v,
                    Show a) 
                => (Signal a -> Signal b)               -- fun to be evaled
                -> (Eval (Seq v))                       -- eval helper function
                -> IO (a -> IO b)
evaluateCircuit circuit (Eval eval)  = do
        let pad = Signal $ Wire $ Pad $ Name "" "input"
        inChan <- newChan
        outChan <- newChan
        as <- getChanContents inChan 
        let eval' (Pad (Name "" "input")) = foldr (:~) (error "END") $ Prelude.map rep as
            eval' e = case eval e of
                        Nothing -> error $ ("problem with evaluator: " ++ 
                                                case e of
                                                  Entity n _ -> show n
                                                  Lit i -> "LIT: " ++ show i
                                                  Port _ _ -> "PORT"
                                                  Pad n  -> "PAD: " ++ show n
                                                  _ -> "???")
                        Just fn -> case e of
                                     (Entity _ vs) -> fn vs
                                     (Lit i)       -> fn []
                                     _ -> error $ "problem with evaluator"
                                     
        signal <- evaluateSignal (circuit pad) eval'
        let seqToList (a :~ as) = a : seqToList as
        let signal' = Seq.toList signal
        forkIO $ E.catch
                (writeList2Chan outChan (Prelude.map abs signal'))
                (\ (e :: E.SomeException) -> print e)
        return $ \ a -> do
                writeChan inChan a
                readChan outChan

newtype Eval a = Eval (Entity a -> Maybe ([a] -> a))
       
-- This is *really* gunky, but works.
liftEntityEval :: Eval a -> Eval (Seq a)
liftEntityEval (Eval fn) = Eval $ \ entity ->
   case fn (demote entity) of
      Nothing -> Nothing
      Just fn' -> Just $ \ vs -> case vs of
         [] -> pure (fn' [])                    -- once, for all time
         _  -> fmap fn' (sequenceA vs)          -- over time
 where
    demote :: Entity (Seq a) -> Entity a
    demote (Entity nm _) = Entity nm []
    demote (Port nm v)   = Port nm (error "port problem") 
    demote (Pad pd)      = Pad pd
    demote (Lit i)       = Lit i

instance Monoid (Eval a) where
    mempty = Eval $ \ _ -> Nothing
    mappend (Eval f1) (Eval f2) = Eval $ \ e ->
        case f1 e of
          Nothing -> f2 e
          Just fn -> Just fn

evaluateNumClass :: (Num a, Show a) => String -> Maybe ([a] -> a)
-- evaluateNumClass op | trace (show op) False = undefined
evaluateNumClass "+"      = return $ \ [v1,v2] -> v1 + v2
evaluateNumClass "-"      = return $ \ [v1,v2] -> v1 - v2
evaluateNumClass "*"      = return $ \ [v1,v2] -> v1 * v2
evaluateNumClass "negate" = return $ \ [v1] -> negate v1 
evaluateNumClass "abs"    = return $ \ [v1] -> Prelude.abs v1
evaluateNumClass "signum" = return $ \ [v1] -> signum v1 
evaluateNumClass "fromInteger" = Nothing -- handled elsewhere
evaluateNumClass _   = fail "not in evaluateNum"

-- We need to include everything inside Signal

evalNumClass :: (Num a, OpType a) => Eval a
evalNumClass = Eval $ \ entity ->
    let modName = findEntityTyModName entity in
    case entity of
      (Entity (Name nm' op') _) | nm' == modName -> evaluateNumClass op'
      _ -> Nothing

evaluateFractionalClass :: (Fractional a, Show a) => String -> Maybe ([a] -> a)
-- evaluateFractionalClass op | trace (show op) False = undefined
evaluateFractionalClass "/"            = return $ \ [v1,v2] -> v1 / v2
evaluateFractionalClass "recip"        = return $ \ [v1]    -> recip v1
evaluateFractionalClass "fromRational" = Nothing -- handled elsewhere
evaluateFractionalClass _   = fail "not in evaluateFractional"

-- We need to include everything inside Signal

evalFractionalClass :: (Fractional a, OpType a) => Eval a
evalFractionalClass = Eval $ \ entity ->
    let modName = findEntityTyModName entity in
    case entity of
      (Entity (Name nm' op') _) | nm' == modName -> evaluateFractionalClass op'
      _ -> Nothing

evaluateFloatingClass :: (Floating a, Show a) => String -> Maybe ([a] -> a)
-- evaluateFloatingClass op | trace (show op) False = undefined
evaluateFloatingClass "sqrt"   = return $ \ [v1]    -> sqrt v1
evaluateFloatingClass _   = fail "not in evaluateFloating"

-- We need to include everything inside Signal

evalFloatingClass :: (Floating a, OpType a) => Eval a
evalFloatingClass = Eval $ \ entity ->
    let modName = findEntityTyModName entity in
    case entity of
      (Entity (Name nm' op') _) | nm' == modName -> evaluateFloatingClass op'
      _ -> Nothing



delayEval :: Eval (Seq a)
delayEval = Eval $ \ e ->
  case e of
   Entity (Name "" "delay") _ -> return $ \ [v1 :~ _, v2] -> v1 :~ v2
   _ -> Nothing
