module Control.Monad.VexceptT 
  ( VexceptT(..), runVexceptT, withVexceptT, mapVexceptT, vexcept
  , module Control.Monad.Trans.Class
  , module Control.Monad.Error.Class
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont (class MonadCont, callCC)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, listen, pass, tell)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Data.Veither (Veither(..), _veither, veither)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Unsafe.Coerce (unsafeCoerce)

-- | Same as `ExceptT`, but uses `Veither` rather than `Either`.
-- |
-- | This type has instances for all the type classes that `ExceptT` has
-- | except for the following type classes:
-- | - Alt
-- | - Plus
-- | - Alternative
-- | - MonadPlus
-- |
-- | Note: throwing and catching errors will need to throw and catch `Variants`.
newtype VexceptT :: Row Type -> (Type -> Type) -> Type -> Type
newtype VexceptT errorRows m a = VexceptT (m (Veither errorRows a))

-- | Removes the `VexceptT` newtype wrapper.
runVexceptT ∷ forall errorRows m a. VexceptT errorRows m a → m (Veither errorRows a)
runVexceptT (VexceptT m) = m

-- | Transform any exceptions thrown by an `VexceptT` computation using the given function.
withVexceptT :: forall e e' m a. Functor m => (Variant e -> Variant e') -> VexceptT e m a -> VexceptT e' m a
withVexceptT f (VexceptT t) = VexceptT $ map (mapErrors f) t
  where
  mapErrors :: (Variant e -> Variant e') -> Veither e a -> Veither e' a
  mapErrors f' v = veither (Veither <<< coerceR <<< f') pure v

  coerceR :: Variant e' -> Variant ("_" :: a | e')
  coerceR = unsafeCoerce

-- | Transform the unwrapped computation using the given function.
mapVexceptT :: forall e e' m n a b. (m (Veither e a) -> n (Veither e' b)) -> VexceptT e m a -> VexceptT e' n b
mapVexceptT f (VexceptT m) = VexceptT (f m)

-- | Construct a computation in the `VexceptT` transformer from an `Veither` value.
vexcept :: forall e m a. Applicative m => Veither e a -> VexceptT e m a
vexcept = VexceptT <<< pure

derive instance newtypeVexceptT ∷ Newtype (VexceptT errorRows m a) _

instance functorVexceptT ∷ Functor m => Functor (VexceptT errorRows m) where
  map ∷ forall a b. (a → b) → VexceptT errorRows m a → VexceptT errorRows m b
  map f = mapVexceptT (map (map f))

instance applyVexceptT ∷ Apply m => Apply (VexceptT errorRows m) where
  apply ∷ forall a b. VexceptT errorRows m (a → b) → VexceptT errorRows m a → VexceptT errorRows m b
  apply (VexceptT mf) (VexceptT ma) = VexceptT ado
    f <- mf
    a <- ma
    in apply f a

instance applicativeVexceptT ∷ Applicative m => Applicative (VexceptT errorRows m) where
  pure ∷ forall a. a → VexceptT errorRows m a
  pure = VexceptT <<< pure <<< pure

instance bindVexceptT ∷ Monad m => Bind (VexceptT errorRows m) where
  bind ∷ forall a b. VexceptT errorRows m a → (a → VexceptT errorRows m b) → VexceptT errorRows m b
  bind (VexceptT mv) f = VexceptT do
    v <- mv
    veither (\_ -> pure (coerceR v)) (\a → case f a of VexceptT m → m) v
    where
    coerceR ∷ forall a b. Veither errorRows a → Veither errorRows b
    coerceR = unsafeCoerce

instance monadVexceptT ∷ Monad m => Monad (VexceptT errorRows m)

instance monadRecVexceptT :: MonadRec m => MonadRec (VexceptT e m) where
  tailRecM :: forall a b. (a -> VexceptT e m (Step a b)) -> a -> VexceptT e m b
  tailRecM f = VexceptT <<< tailRecM \a ->
    case f a of VexceptT mv ->
      mv >>= \v -> do
        let
          handleLeft :: forall a b. Variant e -> Step a (Veither e b)
          handleLeft ve = Done (Veither (coerceR ve))

          coerceR :: forall b. Variant e -> Variant ("_" :: b | e)
          coerceR = unsafeCoerce

          handleRight :: forall a b. Step a b -> Step a (Veither e b)
          handleRight = case _ of
            Loop a1 -> Loop a1
            Done b -> Done (Veither (inj _veither b))

        pure (veither handleLeft handleRight v)

-- Alt instance - issue: no Semigroup instance for `errorRows`
  
-- Plus instance - needs Alt instance first

-- Alternative instance - issue: no Monoid instance for `errorRows`

-- MonadPlus instance - needs Alternative instance first

instance monadTransVexceptT :: MonadTrans (VexceptT e) where
  lift m = VexceptT do
    a <- m
    pure $ pure a

instance monadEffectVexceptT :: MonadEffect m => MonadEffect (VexceptT e m) where
  liftEffect = lift <<< liftEffect

instance monadAffVexceptT :: MonadAff m => MonadAff (VexceptT e m) where
  liftAff = lift <<< liftAff

instance monadContVexceptT :: MonadCont m => MonadCont (VexceptT e m) where
  callCC f = VexceptT $ callCC \c ->
    case f (\a -> VexceptT $ c (pure a)) of VexceptT b -> b

instance monadThrowVexceptT :: Monad m => MonadThrow (Variant e) (VexceptT e m) where
  throwError :: forall a. (Variant e) -> VexceptT e m a
  throwError = VexceptT <<< pure <<< Veither <<< coerceV
    where
      coerceV :: forall a. Variant e -> Variant ("_" :: a | e)
      coerceV = unsafeCoerce

instance monadErrorVexceptT :: Monad m => MonadError (Variant e) (VexceptT e m) where
  catchError (VexceptT m) k =
    VexceptT (m >>= veither (\a -> case k a of VexceptT b -> b) (pure <<< pure))

instance monadAskExceptT :: MonadAsk r m => MonadAsk r (VexceptT e m) where
  ask = lift ask

instance monadReaderExceptT :: MonadReader r m => MonadReader r (VexceptT e m) where
  local f = mapVexceptT (local f)

instance monadStateExceptT :: MonadState s m => MonadState s (VexceptT e m) where
  state f = lift (state f)

instance monadTellExceptT :: MonadTell w m => MonadTell w (VexceptT e m) where
  tell = lift <<< tell

instance monadWriterExceptT :: MonadWriter w m => MonadWriter w (VexceptT e m) where
  listen = mapVexceptT \m -> do
    Tuple a w <- listen m
    pure $ (\r -> Tuple r w) <$> a

  pass :: forall a. VexceptT e m (Tuple a (w -> w)) -> VexceptT e m a
  pass = mapVexceptT \mv -> pass do
    v <- mv
    let
      handleLeft :: forall a. Variant e -> Tuple (Veither e a) (w -> w)
      handleLeft ve = Tuple (Veither (coerceV ve)) identity

      coerceV :: forall a. Variant e -> Variant ("_" :: a | e)
      coerceV = unsafeCoerce

      handleRight :: forall a. Tuple a (w -> w) -> Tuple (Veither e a) (w -> w)
      handleRight (Tuple r f) = Tuple (pure r) f

    pure (veither handleLeft handleRight v)

instance semigroupVexceptT :: (Monad m, Semigroup a) => Semigroup (VexceptT e m a) where
  append = lift2 (<>)

instance monoidVexceptT :: (Monad m, Monoid a) => Monoid (VexceptT e m a) where
  mempty = pure mempty
