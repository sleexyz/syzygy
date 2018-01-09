{-# LANGUAGE PatternSynonyms #-}

module Syzygy.Base where
import Data.HashMap.Strict (HashMap)
import qualified Data.ByteString as BS

import Syzygy.Core


type ParamMap = HashMap BS.ByteString Value

data Value = VS BS.ByteString | VF Double | VI Int
  deriving (Show)

mapVS :: (BS.ByteString -> BS.ByteString) -> Value -> Value
mapVS f (VS x)  =  VS (f x)
mapVS _ x  =  x

mapVF :: (Double -> Double) -> Value -> Value
mapVF f (VF x)  =  VF (f x)
mapVF _ x  =  x

mapVI :: (Int -> Int) -> Value -> Value
mapVI f (VI x)  =  VI (f x)
mapVI _ x  =  x

type Dispatcher = EventDispatcher ParamMap
type CoreConfig = CoreConfig_ ParamMap
type Env = Env_ ParamMap

runDispatcher :: Dispatcher -> CoreConfig -> IO ()
runDispatcher = runEventDispatcher
