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

module Data.TypeSet where

import           GHC.Exts (Constraint)

data OneOf set where
  Next :: OneOf set -> OneOf (e:set)
  Sel  :: e -> OneOf (e:set)

type family Remove set e :: [*] where
  Remove (e:set) e = set
  Remove (any:set) e = any:Remove set e

class Contains set e where
  inj :: e -> OneOf set
  proj :: OneOf set -> Either e (OneOf (Remove set e))

instance {-# OVERLAPPING #-} Contains (e:set) e where
  inj = Sel
  proj (Sel x)  = Left x
  proj (Next r) = Right r

instance {-# OVERLAPPABLE #-} (Contains set e, Remove (any:set) e ~ (any:Remove set e)) => Contains (any:set) e where
  inj = Next . inj
  proj (Sel x)  = Right $ Sel @any @(Remove set e) x
  proj (Next x) = case proj x of
    Right x -> Right $ Next @(Remove set e) @any x
    Left e  -> Left e

newtype Handler set a = Handler (OneOf set -> a)

(+>) :: (e -> a) -> Handler set a -> Handler (e:set) a
f +> (Handler fset) = Handler $ \case
  Sel x -> f x
  Next x -> fset x

-- | Stands for “end of handler”.
eoh :: Handler '[] a
eoh = Handler $ pure (error "should not be possible, as OneOf '[] is inhabited")

infixr +>

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
