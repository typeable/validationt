{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Validation
  ( ValidationT(..)
  , runValidationT
  , runValidationTEither
  , handleValidationT
  , vError
  , vWarning
  , vErrorL
  , vWarningL
  , vZoom
  , vZoomL
  , mmSingleton
  , setMempty
  , neConcat
  , textErrors
  , _MonoidMap
  , MonoidMap(..)
  ) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Foldable as F
import Data.Functor
import Data.List as L
import Data.Map.Strict as M
import Data.Monoid
import Data.Text as T
import Data.Vector as V
import Test.QuickCheck


-- | Collects all thrown warnings in 'StateT' and errors
-- in 'ExceptT' into a single value using 'Monoid'.
newtype ValidationT e m a = ValidationT
  { unValidationT :: ExceptT e (StateT e m) a
  } deriving ( Functor, Applicative, Monad, MonadThrow, MonadCatch
             , MonadBase b, Alternative, MonadFix, MonadFail, Contravariant
             , MonadIO, MonadPlus, MonadBaseControl b, MonadMask )

instance MonadTrans (ValidationT e) where
  lift = ValidationT . lift . lift

-- | Map with 'Monoid' instance which 'mappend' its values
--
-- This can be used as the `e` in `ValidationT e m a` to provide different
-- sets of errors and warnings for different keys.
--
-- >>> :{
--   mconcat
--   [ MonoidMap $ M.fromList [(1, "foo"), (2, "hello, "), (3, "oh no")]
--   , MonoidMap $ M.fromList [(1, "bar"), (2, "world")]
--   ]
-- :}
-- MonoidMap (fromList [(1,"foobar"),(2,"hello, world"),(3,"oh no")])
newtype MonoidMap k v = MonoidMap (Map k v)
  deriving (Eq, Ord, Show, Arbitrary, Foldable)

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
  parseJSON = withArray "MonoidMap" go
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

-- | Convenient for 'vZoom' as first argument. Will prevent generation
-- of map with 'mempty' values.
mmSingleton :: (Eq v, Monoid v, Ord k) => k -> v -> MonoidMap k v
mmSingleton k v
  | v == mempty = mempty
  | otherwise   = MonoidMap . M.singleton k $ v

-- | Sets given value to 'mempty'.
setMempty :: (Monoid s) => ASetter' s a -> a -> s
setMempty setter a = set setter a mempty

-- | If the given container is not 'mempty', then use the given function to
-- append all its elements and return 'Just' result.
--
-- >>> neConcat (<>) ["a", "b", "c"]
-- Just "abc"
--
-- >>> neConcat (\a b -> a <> " " <> b) ["a", "b"]
-- Just "a b"
--
-- >>> neConcat (<>) []
-- Nothing
neConcat :: Foldable f => (a -> a -> a) -> f a -> Maybe a
neConcat f a
  | F.null a  = Nothing
  | otherwise = Just $ F.foldl1 f a

-- | Returns the strings, concatanated with @", "@ if the list is not empty.
--
-- Returns Nothing if the list is empty
--
-- >>> textErrors ["foo", "bar"]
-- Just "foo, bar"
--
-- >>> textErrors ["foo"]
-- Just "foo"
--
-- >>> textErrors []
-- Nothing
textErrors :: [Text] -> Maybe Text
textErrors = neConcat (\a b -> a <> ", " <> b)

-- | Returns 'mempty' instead of error if no warnings have occured.
-- Returns 'Nothing' as the second element of tuple if computation was
-- interrupted by 'vError'.
--
-- Returns all concatenated errors and warnings and the result if no
-- errors have occured (warnings could have occured).
--
-- >>> :{
--  runValidationT $ do
--    vWarning ["warning1"]
--    vError ["error"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- (["error","warning1"],Nothing)
--
-- >>> :{
--  runValidationT $ do
--    vWarning ["warning1"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- (["warning1","warning2"],Just 8)
runValidationT :: (Monoid e, Monad m) => ValidationT e m a -> m (e, Maybe a)
runValidationT (ValidationT m) = do
  (res, warnings) <- runStateT (runExceptT m) mempty
  return $ case res of
    Left err -> (err <> warnings, Nothing)
    Right a  -> (warnings, Just a)

-- | Like 'runValidationT' but doesn't return the result
-- if any warning has occured.
--
-- >>> :{
--  runValidationTEither $ do
--    vWarning ["warning1"]
--    vError ["error"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- Left ["error","warning1"]
--
-- >>> :{
--  runValidationTEither $ do
--    vWarning ["warning1"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- Left ["warning1","warning2"]
runValidationTEither
  :: (Monoid e, Eq e, Monad m)
  => ValidationT e m a
  -> m (Either e a)
runValidationTEither action = do
  (err, res) <- runValidationT action
  return $ case res of
    Just a | err == mempty -> Right a
    _                      -> Left err

-- | Like 'runValidationTEither', but takes an error handler instead of
-- returning errors and warnings.
--
-- >>> :{
--  handleValidationT (\_ -> return 11) $ do
--    vWarning ["warning1"]
--    vError ["error"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- 11
--
-- >>> :{
--  handleValidationT (\_ -> return 11) $ do
--    vWarning ["warning1"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- 11
handleValidationT
  :: (Monoid e, Monad m, Eq e)
  => (e -> m a)
  -> ValidationT e m a
  -> m a
handleValidationT handler action =
  runValidationTEither action >>= either handler return

-- | Stops further execution and appends the given error.
vError :: (Monad m) => e -> ValidationT e m a
vError e = ValidationT $ throwError e

-- | Does not stop further execution and appends the given warning.
vWarning :: (Monad m, Monoid e) => e -> ValidationT e m ()
vWarning e = ValidationT $ modify' (<> e)

-- | Like 'vError' but allows you to use a setter to insert an error somewhere
-- deeper into an empty ('mempty') "e" from "ValidationT e m x", which is then
-- combined with all gathered warnings.
vErrorL :: (Monad m, Monoid e) => ASetter' e a -> a -> ValidationT e m x
vErrorL l a = vError $ setMempty l a

-- | Like 'vWarning' but allows you to use a setter to insert an error somewhere
-- deeper into an empty ('mempty') "e" from "ValidationT e m x", which is then
-- combined with all gathered warnings.
vWarningL :: (Monad m, Monoid e) => ASetter' e a -> a -> ValidationT e m ()
vWarningL l a = vWarning $ setMempty l a

-- | Allows you apply a transformation to the "e" in "ValidationT e m x".
--
-- >>> :{
--runValidationT . vZoom (Data.Map.singleton "password errors") $ do
--  vWarning ["warning1"]
--  vError ["error"]
--  vWarning ["warning2"]
--  return 8
-- :}
-- (fromList [("password errors",["error","warning1"])],Nothing)
--
-- >>> :{
--  runValidationT . vZoom (Data.Map.singleton "password errors") $ do
--    vWarning ["warning1"]
--    vWarning ["warning2"]
--    return 8
-- :}
-- (fromList [("password errors",["warning1","warning2"])],Just 8)
vZoom
  :: (Monad m, Monoid a, Monoid b)
  => (a -> b)
  -> ValidationT a m x
  -> ValidationT b m x
vZoom up action = do
  (err, res) <- lift $ runValidationT action
  case res of
    Nothing -> vError $ up err
    Just a  -> vWarning (up err) $> a

-- | Like 'vZoom' but takes a setter instead of a function.
vZoomL
  :: (Monad m, Monoid a, Monoid b)
  => ASetter' b a
  -> ValidationT a m x
  -> ValidationT b m x
vZoomL l = vZoom (setMempty l)
