module Network.WebSockets.Sync.Request where

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception

import Data.JSON
import Data.Maybe

import Prelude

import qualified Network.WebSockets.Sync.Socket as S

-- can't use Type.Proxy because of orphan instances
data Proxy t = Proxy

instance proxyToJson :: (ToJSON t) => ToJSON (Proxy t) where
  toJSON (Proxy ) = object $
    [ "tag" .= "Proxy"
    , "contents" .= ([] :: Array String)
    ]

instance proxyFromJson :: (FromJSON t) => FromJSON (Proxy t) where
    parseJSON (JObject o) = return Proxy
    parseJSON _ = fail "Can't parse Proxy"

data Tuple3 a b c = Tuple3 a b c

fst3 :: forall a b c. Tuple3 a b c -> a
fst3 (Tuple3 a _ _) = a

snd3 :: forall a b c. Tuple3 a b c -> b
snd3 (Tuple3 _ b _) = b

trd3 :: forall a b c. Tuple3 a b c -> c
trd3 (Tuple3 _ _ c) = c

instance tuple3Eq :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  eq (Tuple3 a b c) (Tuple3 a' b' c') = a == a' && b == b' && c == c'

instance tuple3Ord :: (Ord a, Ord b, Ord c) => Ord (Tuple3 a b c) where
  compare (Tuple3 a1 b1 c1) (Tuple3 a2 b2 c2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2

newtype Key a = Key {
  unKey :: Tuple3 Int Int Int
}

instance keyEq :: Eq (Key a) where
  eq (Key k1) (Key k2) = k1.unKey == k2.unKey

instance keyOrd :: Ord (Key a) where
  compare (Key k1) (Key k2) = compare k1.unKey k2.unKey

mkKey :: forall a. Int -> Int -> Int -> Key a
mkKey a b c = Key { unKey: Tuple3 a b c }

coerceKey :: forall a b. Key a -> Key b
coerceKey (Key t) = Key t

data Session

type SessionId = Key Session

instance tuple3ToJson :: (ToJSON a, ToJSON b, ToJSON c) => ToJSON (Tuple3 a b c) where
  toJSON (Tuple3 a b c) = JArray [ toJSON a, toJSON b, toJSON c ]

instance tuple3FromJson :: (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Tuple3 a b c) where
  parseJSON (JArray [a, b, c]) = Tuple3 <$> parseJSON a <*> parseJSON b <*> parseJSON c
  parseJSON _ = fail "Can not parse tuple3"

instance keyToJson :: ToJSON (Key a) where
  toJSON (Key k) = toJSON k.unKey

instance keyFromJson :: FromJSON (Key a) where
  parseJSON v = do
    tuple <- parseJSON v
    return $ Key { unKey : tuple }

type Text = String

send :: forall eff a. (ToJSON a) => S.Socket -> a -> Aff (websocket :: S.WebSocket | eff) Unit
send socket req = makeAff f
  where
    f err cb = do
      S.send socket (encode req)
      cb unit

sendSync' :: forall eff a. (FromJSON a) => S.Socket -> String -> Aff (websocket :: S.WebSocket | eff) a
sendSync' socket req = makeAff f
  where
    f err cb = do
      S.sendSync socket req $ \msg -> case decode msg of
        Just msg' -> cb msg'
        Nothing   -> err $ error $ "Could not parse message: " ++ msg

sendSync :: forall eff a b. (ToJSON a, FromJSON b) => S.Socket -> (Proxy b -> a) -> Aff (websocket :: S.WebSocket | eff) b
sendSync socket req = makeAff f
  where
    f err cb = do
      S.sendSync socket (encode (req Proxy)) $ \msg -> case decode msg of
        Just msg' -> cb msg'
        Nothing   -> err $ error $ "Could not parse message: " ++ msg
