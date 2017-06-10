module Actions where

import Data.Function.Uncurried (Fn2, runFn2)


foreign import data ApiAiClient ∷ Type


type Option = {
  request ∷ String,
  response ∷ String
}


foreign import tellImpl ∷ Fn2
                          ApiAiClient
                          String
                          String


tell ∷ ApiAiClient
       → String
       → String
tell client text = runFn2 tellImpl client text
