module Network.WebSockets.Sync.Socket where

import Control.Monad.Eff

import Data.Function

import Prelude

foreign import data WebSocket :: !

foreign import data Socket :: *

type WebSocketEff eff r = Eff (websocket :: WebSocket | eff) r

type WebSocketHandlers eff =
  { connected :: Socket -> WebSocketEff eff Unit
  , disconnected :: WebSocketEff eff Unit
  , message :: String -> WebSocketEff eff Unit
  , debug :: Boolean
  }

type KeepAlive = Boolean

foreign import connectImpl :: forall eff. Fn3 String KeepAlive (WebSocketHandlers eff) (WebSocketEff eff Socket)

foreign import sendImpl :: forall eff. Fn2 Socket String (WebSocketEff eff Unit)

foreign import sendSyncImpl :: forall eff. Fn3 Socket String (String -> WebSocketEff eff Unit) (WebSocketEff eff Unit)

foreign import setHandlersImpl :: forall eff. Fn2 Socket (WebSocketHandlers eff) (WebSocketEff eff Unit)

connect :: forall eff. String -> KeepAlive -> WebSocketHandlers eff -> WebSocketEff eff Socket
connect url keepAlive handlers = runFn3 connectImpl url keepAlive handlers

send :: forall eff. Socket -> String -> WebSocketEff eff Unit
send socket msg = runFn2 sendImpl socket msg

sendSync :: forall eff. Socket -> String -> (String -> WebSocketEff eff Unit) -> WebSocketEff eff Unit
sendSync socket msg cb = runFn3 sendSyncImpl socket msg cb

setHandlers :: forall eff. Socket -> WebSocketHandlers eff -> WebSocketEff eff Unit
setHandlers socket handlers = runFn2 setHandlersImpl socket handlers
