{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}
module Fmap where
import Data.Generics
import Unsafe.Coerce
import Data.Coerce
 
{- | C tags the type that is actually parameterized, so to avoid touching the
Int when a ~ Int:
 
> data T a = T Int a
 
by changing the type (not representation) to:
 
> x :: T Int (C Int)
-}
newtype C a = C a deriving (Data,Typeable)
 
fmapData :: forall t a b. (Typeable a, Data (t (C a)), Data (t a)) =>
    (a -> b) -> t a -> t b
fmapData f input = uc . everywhere (mkT $ \(x::C a) -> uc (f (uc x)))
                    $ (uc input :: t (C a))
    where uc = unsafeCoerce


