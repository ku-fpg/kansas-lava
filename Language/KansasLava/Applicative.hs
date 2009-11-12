module Language.KansasLava.Applicative where

import Language.KansasLava.Signal
import Language.KansasLava.Entity
import Control.Applicative



instance Functor Signal where
   fmap f (Signal s d) = o0 $ ESignal (fmap f s) $ E $  e
     where e =  (Entity (Name "Applicative" "fmap") (error "Can't have fmap in ent") (error "can't have fmap in ent"))

instance Applicative Signal where
   pure a = o0 $ ESignal (pure a) $ E $  e
     where e =  (Entity (Name "Applicative" "pure") (error "Can't have pure in ent") (error "can't have pure in ent"))

   (Signal f1 _) <*> (Signal f2 _) = o0 $ ESignal (f1 <*> f2) $ E $  e
     where e =  (Entity (Name "Applicative" "<*>") (error "Can't have pure in <*>") (error "can't have <*>  in ent"))

