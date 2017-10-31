module Syzygy.ParamMap where
import Data.Map.Strict (Map)


type ParamMap = Map String Value

data Value = VS String | VF Double | VI Int

mapVS :: (String -> String) -> Value -> Value
mapVS f (VS x)  =  VS (f x)
mapVS _ x  =  x

mapVF :: (Double -> Double) -> Value -> Value
mapVF f (VF x)  =  VF (f x)
mapVF _ x  =  x

mapVI :: (Int -> Int) -> Value -> Value
mapVI f (VI x)  =  VI (f x)
mapVI _ x  =  x

