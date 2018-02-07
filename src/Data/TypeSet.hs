{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.TypeSet where

import           GHC.Exts (Constraint)

data OneOf set where
  Next :: OneOf set -> OneOf (e:set)
  Sel  :: e -> OneOf (e:set)

data Nat = Z | S Nat

class Contains set e where
  inj :: e -> OneOf set
  proj :: OneOf set -> Maybe e

instance {-# OVERLAPPABLE #-} Contains (e:set) e where
  inj = Sel
  proj (Sel x)  = Just x
  proj (Next _) = Nothing

instance {-# OVERLAPS #-} (Contains set e) => Contains (any:set) e where
  inj = Next . inj
  proj (Sel _)  = Nothing
  proj (Next x) = proj x

type family Remove (set :: [*]) (e :: *) :: [*] where
  Remove '[] e = '[]
  Remove (e:set) e = set
  Remove (any:set) e = any:Remove set e

class Shrink' set e set' where
  shrink' :: OneOf set -> Either e (OneOf set')

instance Shrink' '[] e '[] where
  shrink' = error "should not be called, as OneOf '[] is inhabited"

instance {-# INCOHERENT #-} Shrink' (e:set) e set where
  shrink' (Sel x)  = Left x
  shrink' (Next x) = Right x

instance (Shrink' set e set') => Shrink' (any:set) e (any:set') where
  shrink' (Sel x) = Right (Sel x)
  shrink' (Next x) = case shrink' x of
    Left x  -> Left x
    Right x -> Right (Next x)

newtype Handler set a = Handler (OneOf set -> a)

(+>) :: (e -> a) -> Handler set a -> Handler (e:set) a
f +> (Handler fset) = Handler $ \case
  Sel x -> f x
  Next x -> fset x

closeFunction :: Handler '[] a
closeFunction = Handler $ \_ -> error "should not be possible, as OneOf '[] is inhabited"

infixr +>

class Shrink e set where
  shrink :: OneOf set -> Either e (OneOf (Remove set e))

instance (Remove set e ~ set', Shrink' set e set') => Shrink e set where
  shrink = shrink'

data AllOf set where
  Nop :: AllOf '[]
  Push :: e -> AllOf set -> AllOf (e:set)

type family Join set set' :: [*] where
  Join (a:rst) set = a:Join rst set
  Join '[] set = set

class Split set set' where
  split :: OneOf (Join set set') -> Either (OneOf set) (OneOf set')

instance Split '[] set where
  split = Right

instance Split set set' => Split (e:set) set' where
  split (Sel x) = Left (Sel x)
  split (Next x) = case split @set @set' x of
    Right x -> Right x
    Left x  -> Left $ Next x
