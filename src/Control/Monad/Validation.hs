{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Validation where

import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Aeson
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Monoid
import Data.Text as T
import Data.Vector as V
import Test.QuickCheck

-- | Collects all throwed "warnings" throwed through StateT and "errors" throwed
-- through ExceptT to single value using Monoid
--  FIXME: give more instances like HReaderT and MonadBaseControl/MonadMask
newtype ValidationT e m a = ValidationT
  { unValidationT :: ExceptT e (StateT e m) a
  } deriving ( Functor, Applicative, Monad, MonadThrow, MonadCatch
             , MonadBase b )

instance MonadTrans (ValidationT e) where
  lift = ValidationT . lift . lift

-- | Map with 'Monoid' instance which 'mappend' its values
newtype MonoidMap k v = MonoidMap (Map k v)
  deriving (Eq, Ord, Show, Arbitrary)

makePrisms ''MonoidMap

type instance IxValue (MonoidMap k v) = v
type instance Index (MonoidMap k v) = k
instance (Ord k) => Ixed (MonoidMap k v) where
  ix key = _MonoidMap . ix key
instance (Ord k) => At (MonoidMap k v) where
  at key = _MonoidMap . at key

#if MIN_VERSION_base(4,11,0)
instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  (<>) = mmAppend
#endif

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap M.empty
  mappend = mmAppend

instance (ToJSON k, ToJSON v) => ToJSON (MonoidMap k v) where
  toJSON (MonoidMap m) = toJSON $ L.map toObj $ M.toList m
    where
      toObj (k, v) = object
        [ "id" .= k
        , "value" .= v ]

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (MonoidMap k v) where
  parseJSON v = withArray "MonoidMap" go v
    where
      go arr = do
        keyvals <- traverse fromObj arr
        return $ MonoidMap $ M.fromList $ V.toList keyvals
      fromObj objV = flip (withObject "element of MonoidMap") objV $ \obj -> do
        key <- obj .: "id"
        val <- obj .: "value"
        return (key, val)

#if MIN_VERSION_base(4,11,0)
mmAppend :: (Ord k, Semigroup v) => MonoidMap k v -> MonoidMap k v -> MonoidMap k v
#else
mmAppend :: (Ord k, Monoid v) => MonoidMap k v -> MonoidMap k v -> MonoidMap k v
#endif
mmAppend (MonoidMap a) (MonoidMap b) = MonoidMap $ M.unionWith (<>) a b

-- | Convenient for 'vZoom' as first artument. Will prevent generation
-- of map with 'mempty' values
mmSingleton :: (Eq v, Monoid v, Ord k) => k -> v -> MonoidMap k v
mmSingleton k = memptyWrap mempty $ MonoidMap . M.singleton k

-- | Set given value to 'mempty'
setMempty :: (Monoid s) => ASetter' s a -> a -> s
setMempty setter a = set setter a mempty

memptyWrap :: (Eq a, Monoid a) => b -> (a -> b) -> a -> b
memptyWrap b f a
  | a == mempty = b
  | otherwise = f a

-- | If given container is not 'mempty', then use given function to
-- append all its elements and return 'Just' result
neConcat
  :: (Foldable f, Eq (f a), Monoid a, Monoid (f a))
  => (a -> a -> a)
  -> f a
  -> Maybe a
neConcat f = memptyWrap Nothing (Just . F.foldl' f mempty)

textErrors :: [Text] -> Maybe Text
textErrors = neConcat (\a b -> a <> ", " <> b)

-- | Returns `mempty` instead of error if no warnings was occured. So, your
-- error should have `Eq` instance to detect that any error was occured. Returns
-- Nothing for second element of tuple if compuration was interruped by 'vError'
runValidationT :: (Monoid e, Monad m) => ValidationT e m a -> m (e, Maybe a)
runValidationT (ValidationT m) = do
  (res, warnings) <- runStateT (runExceptT m) mempty
  return $ case res of
    Left err -> (err <> warnings, Nothing)
    Right a  -> (warnings, Just a)

runValidationTEither
  :: (Monoid e, Eq e, Monad m)
  => ValidationT e m a
  -> m (Either e a)
runValidationTEither action = do
  (err, res) <- runValidationT action
  return $ case res of
    Just a | err == mempty -> Right a
    _                      -> Left err

handleValidationT
  :: (Monoid e, Monad m, Eq e)
  => (e -> m a)
  -> ValidationT e m a
  -> m a
handleValidationT handler action = do
  runValidationTEither action >>= either handler return

-- | Stops further execution of validation
vError :: (Monad m) => e -> ValidationT e m a
vError e = ValidationT $ throwError e

-- | Does not stop further execution, append warning to
vWarning :: (Monad m, Monoid e) => e -> ValidationT e m ()
vWarning e = ValidationT $ modify' (<> e)

vErrorL :: (Monad m, Monoid e) => ASetter' e a -> a -> ValidationT e m x
vErrorL l a = vError $ setMempty l a

vWarningL :: (Monad m, Monoid e) => ASetter' e a -> a -> ValidationT e m ()
vWarningL l a = vWarning $ setMempty l a

vZoom
  :: (Monad m, Monoid a, Monoid b)
  => (a -> b)
  -> ValidationT a m x
  -> ValidationT b m x
vZoom up action = do
  (err, res) <- lift $ runValidationT action
  case res of
    Nothing -> vError $ up err
    Just a  -> vWarning (up err) *> return a

vZoomL
  :: (Monad m, Monoid a, Monoid b)
  => ASetter' b a
  -> ValidationT a m x
  -> ValidationT b m x
vZoomL l action = vZoom (setMempty l) action
