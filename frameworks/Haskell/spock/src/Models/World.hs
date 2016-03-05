{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.World
    ( World(..)
    , fetchWorldById
    , getRandomWorld
    , updateWorldRandom
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Functor.Contravariant
import           Data.Int
import           Data.Monoid                ((<>))
import           GHC.Generics
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as HP
import qualified Hasql.Query                as HQ
import qualified Hasql.Session              as H
import           System.Random


data World = World
    { _idW           :: !Int32
    , _randomNumberW :: !Int32
    } deriving (Show, Generic)

-- | JSON serialization
instance ToJSON World where
    toEncoding w =
        pairs (  "id"            .= _idW w
              <> "randomNumber"  .= _randomNumberW w
              )
    {-# INLINE toEncoding #-}

-- | Transforming a database row into a World datatype.
worldRow :: HD.Row World
worldRow = World <$> HD.value HD.int4 <*> HD.value HD.int4
{-# INLINE worldRow #-}

-- | Get a World by Id, this will return a Just World, or Nothing
-- if the id is not in the database.
fetchWorldById :: Int32 -> H.Session (Maybe World)
fetchWorldById !i =
    sess where
        sess    = H.query i $ HQ.statement sql encoder decoder True
        sql     = "SELECT id, randomNumber FROM World WHERE id = $1"
        encoder = contramap id (HE.value HE.int4)
        decoder = HD.maybeRow worldRow
{-# INLINE fetchWorldById #-}

-- | Get a random World from the database. For the tests
-- the id must be bound between 1-10000
getRandomWorld :: HP.Pool -> IO (Maybe World)
getRandomWorld !pool = do
    i <- liftIO (randomRIO (1, 10000))
    result <- HP.use pool (fetchWorldById i)
    return $ case result of
      Left _ -> Nothing
      Right xs -> xs
{-# INLINE getRandomWorld #-}


updateWorld :: Int32 -> World -> H.Session ()
updateWorld !i !(World _id _) =
    sess where
        sess    = H.query i $ HQ.statement sql encoder decoder True
        sql     = "UPDATE World SET randomNumber = $1 WHERE id = $2"
        encoder = contramap id (HE.value HE.int4) <>
                  contramap id (HE.value HE.int4)
        decoder = HD.unit
{-# INLINE updateWorld #-}

-- | Update a World with a random number
updateWorldRandom :: HP.Pool -> World -> IO World
updateWorldRandom !pool !w@(World _id _) = do
    i <- liftIO $ randomRIO (1, 10000)
    result <- HP.use pool (updateWorld i w)
    case result of
      Left e -> error (show e)
      Right _ -> return $ World _id i
{-# INLINE updateWorldRandom #-}
