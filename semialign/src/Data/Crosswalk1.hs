{-# LANGUAGE Trustworthy #-}
module Data.Crosswalk1 (
    -- * Crosswalk
    Crosswalk1 (..),
    -- * Bicrosswalk
    Bicrosswalk1 (..),
    ) where

import Control.Applicative   (pure, (<$>))
import Data.Bifoldable1      (Bifoldable1 (..))
import Data.Bifunctor        (Bifunctor (..))
import Data.Foldable1        (Foldable1 (..))
import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity (..))
import Prelude               (Either (..), Functor (fmap), id, (.))

import qualified Data.List.NonEmpty  as NE

import Data.Align
import Data.These

-- --------------------------------------------------------------------------
-- | Foldable functors supporting traversal through an alignable
--   functor.
--
--   Minimal definition: @crosswalk1@ or @sequenceL1@.
--
--   Laws:
--
-- @
-- crosswalk1 (const nil) = const nil
-- crosswalk1 f = sequenceL1 . fmap f
-- @
class (Functor t, Foldable1 t) => Crosswalk1 t where
    crosswalk1 :: Semialign f => (a -> f b) -> t a -> f (t b)
    crosswalk1 f = sequenceL1 . fmap f

    sequenceL1 :: Semialign f => t (f a) -> f (t a)
    sequenceL1 = crosswalk1 id

    {-# MINIMAL crosswalk1 | sequenceL1 #-}

instance Crosswalk1 Identity where
    crosswalk1 f (Identity a) = fmap Identity (f a)

instance Crosswalk1 NE.NonEmpty where
    crosswalk1 f = foldrMap1 (fmap NE.singleton . f) (alignWith cons . f)
      where
        cons = these NE.singleton id NE.cons

instance Crosswalk1 ((,) a) where
    crosswalk1 fun (a, x) = fmap ((,) a) (fun x)

instance (Crosswalk1 f, Crosswalk1 g) => Crosswalk1 (Compose f g) where
    crosswalk1 f
        = fmap Compose
        . crosswalk1 (crosswalk1 f)
        . getCompose

-- --------------------------------------------------------------------------
-- | Bifoldable bifunctors supporting traversal through an semi-alignable
--   functor.
--
--   Minimal definition: @bicrosswalk1@ or @bisequenceL1@.
--
--   Laws:
--
-- @
-- bicrosswalk1 (const empty) (const empty) = const empty
-- bicrosswalk1 f g = bisequenceL1 . bimap f g
-- @
class (Bifunctor t, Bifoldable1 t) => Bicrosswalk1 t where
    bicrosswalk1 :: (Semialign f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
    bicrosswalk1 f g = bisequenceL1 . bimap f g

    bisequenceL1 :: (Semialign f) => t (f a) (f b) -> f (t a b)
    bisequenceL1 = bicrosswalk1 id id

    {-# MINIMAL bicrosswalk1 | bisequenceL1 #-}

instance Bicrosswalk1 Either where
    bicrosswalk1 f _ (Left x)  = Left  <$> f x
    bicrosswalk1 _ g (Right x) = Right <$> g x

instance Bicrosswalk1 These where
    bicrosswalk1 f _ (This x) = This <$> f x
    bicrosswalk1 _ g (That x) = That <$> g x
    bicrosswalk1 f g (These x y) = align (f x) (g y)
