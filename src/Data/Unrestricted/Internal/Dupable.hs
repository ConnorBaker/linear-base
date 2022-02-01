{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Internal.Dupable
  ( -- * Dupable
    Dupable (..),
    dup,
    dup3,
  )
where

import Data.Type.Equality
import Data.Unrestricted.Internal.Consumable
import Data.V.Linear.Internal.V (V)
import qualified Data.V.Linear.Internal.V as V
import GHC.TypeLits
import Data.Unrestricted.Internal.Ur
import GHC.Types (TYPE)
import Data.Unrestricted.Internal.Replicator
import Prelude.Linear.Internal ((&), id, ($))
import qualified Prelude

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- Where the @(≃)@ sign represents equality up to type isomorphism.
--
-- When implementing 'Dupable' instances for composite types, using 'dupR'
-- should be more convenient since 'Replicator' has an 'Applicative' instance.
class Consumable a => Dupable a where
  {-# MINIMAL dupR | dup2 #-}

  dupR :: a %1 -> Replicator a
  dupR a = CollectFrom $ RepStream id dup2 consume a

  dupV :: forall n. KnownNat n => a %1 -> V n a
  dupV a =
    case V.caseNat @n of
      Prelude.Left Refl -> a `lseq` V.make @0 @a
      Prelude.Right Refl -> V.iterate dup2 a

  dup2 :: a %1 -> (a, a)
  dup2 a = dupR a & \case
    Moved a -> (a, a)
    CollectFrom (RepStream givea dupsa _ sa) -> dupsa sa & \case
      (sa1, sa2) -> (givea sa1, givea sa2)

dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a %1 -> (a, a)
dup = dup2
