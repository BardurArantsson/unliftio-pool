module Unlift.Pool
  ( Pool
  , LocalPool
  , createPool
  , withResource
  , takeResource
  , tryWithResource
  , tryTakeResource
  , destroyResource
  , putResource
  , destroyAllResources
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import qualified Data.Pool as P
import           Data.Time.Clock (NominalDiffTime)

type Pool = P.Pool

type LocalPool = P.LocalPool

createPool :: MonadUnliftIO m => m a -> (a -> m ()) -> Int -> NominalDiffTime -> Int -> m (Pool a)
createPool create destroy stripes keepAlive maxPerStripe =
  withRunInIO $ \io ->
    liftIO $ P.createPool (io create) (io . destroy) stripes keepAlive maxPerStripe

withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool action =
  withRunInIO $ \io ->
    liftIO $ P.withResource pool $ \a ->
      io $ action a

takeResource :: MonadUnliftIO m => Pool a -> m (a, LocalPool a)
takeResource pool = liftIO $ P.takeResource pool

tryWithResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m (Maybe b)
tryWithResource pool action =
  withRunInIO $ \io ->
    liftIO $ P.tryWithResource pool $ \a ->
      io $ action a

tryTakeResource :: MonadUnliftIO m => Pool a -> m (Maybe (a, LocalPool a))
tryTakeResource pool = liftIO $ P.tryTakeResource pool

destroyResource :: MonadUnliftIO m => Pool a -> LocalPool a -> a -> m ()
destroyResource pool localPool a = liftIO $ P.destroyResource pool localPool a

putResource :: MonadUnliftIO m => LocalPool a -> a -> m ()
putResource localPool a = liftIO $ P.putResource localPool a

destroyAllResources :: MonadUnliftIO m => Pool a -> m ()
destroyAllResources pool = liftIO $ P.destroyAllResources pool
