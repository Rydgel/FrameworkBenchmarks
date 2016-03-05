{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Fortune
    ( Fortune(..)
    , fetchFortunes
    ) where

import           Data.Aeson
import           Data.Int
import           Data.Monoid    ((<>))
import           Data.Ord
import qualified Data.Text      as T
import           GHC.Generics
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Pool     as HP
import qualified Hasql.Query    as HQ
import qualified Hasql.Session  as H


data Fortune = Fortune
    { _idF      :: !Int32
    , _messageF :: !T.Text
    } deriving (Show, Generic)

-- | JSON serialization
instance ToJSON Fortune where
    toEncoding f =
        pairs (  "id"      .= _idF f
              <> "message" .= _messageF f
              )
    {-# INLINE toEncoding #-}

-- | Transforming a database row into a Fortune datatype.
fortuneRow :: HD.Row Fortune
fortuneRow = Fortune <$> HD.value HD.int4 <*> HD.value HD.text
{-# INLINE fortuneRow #-}

-- | For sorting purposes
instance Eq Fortune where
    (==) fa fb =
        _idF fa      == _idF fb
     && _messageF fa == _messageF fb
    {-# INLINE (==) #-}

instance Ord Fortune where
    compare = comparing _messageF
    {-# INLINE compare #-}


fetchFortunes :: HP.Pool -> IO [Fortune]
fetchFortunes pool = do
    result <- HP.use pool sess
    return $ case result of
      Left _ -> []
      Right xs -> xs
    where
      sess    = H.query () $ HQ.statement sql encoder decoder True
      sql     = "SELECT id, message FROM Fortune"
      encoder = HE.unit
      decoder = HD.rowsList fortuneRow
{-# INLINE fetchFortunes #-}
