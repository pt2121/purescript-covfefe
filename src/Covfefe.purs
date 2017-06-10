module Covfefe (tellTweets) where

import Twitter.Twitter
import Actions (ApiAiClient, tell)
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExcept, throwError)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, URL, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (class Show, Unit, bind, const, discard, pure, show, unit, ($), (<>), (>>>))
import Text.Base64 (Base64, encode64)


consumer ∷ Consumer
consumer = { key: ""
           , secret: ""
           }

authStr ∷ Base64 → Base64 → Base64
authStr k s = encode64 $ (k <> ":" <> s)

authHeader ∷ String → RequestHeader
authHeader s = RequestHeader "Authorization" ("Basic " <> s)

authReq ∷ AffjaxRequest String
authReq = defaultRequest { url = "https://api.twitter.com/oauth2/token"
                         , method = Left POST
                         , headers = [authHeader (authStr consumer.key consumer.secret), RequestHeader "Content-Type" "application/x-www-form-urlencoded;charset=UTF-8"]
                         , content = Just "grant_type=client_credentials"
                         }

newtype Token = Token
                { token_type ∷ String,
                  access_token ∷ String
                }

derive instance genericToken ∷ Generic Token _

instance showToken ∷ Show Token where
  show = genericShow

instance decodeToken ∷ Decode Token where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeToken ∷ Encode Token where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


type BearerToken = String


get' ∷ ∀ e a
       . Respondable a
       ⇒ BearerToken
       → URL
       → Affjax e a
get' t u = affjax $ defaultRequest { url = u, headers = [RequestHeader "Authorization" t] }


eitherThrow ∷ ∀ a b c d
              . MonadThrow Error b
              ⇒ Show d
              ⇒ (a → b c)
              → Either d a
              → b c
eitherThrow = either (show >>> error >>> throwError)


getBearerToken ∷ ∀ m
                 . MonadThrow Error m
                 ⇒ String
                 → m String
getBearerToken = decodeTokenJSON >>> eitherThrow (\(Token t) → pure (t.token_type <> " " <> t.access_token)) where
            decodeTokenJSON ∷ String → Either (NonEmptyList ForeignError) Token
            decodeTokenJSON = decodeJSON >>> runExcept


getTweets ∷ ∀ e a
            . Respondable a
            ⇒ BearerToken
            → String
            → Affjax e a
getTweets t q = get' t $ "https://api.twitter.com/1.1/search/tweets.json?q=" <> q


getSearchResponse ∷ ∀ m
                    . MonadThrow Error m
                    ⇒ String
                    → m SearchResult
getSearchResponse = decodeTweetsJSON >>> eitherThrow pure


decodeTweetsJSON ∷ String → Either (NonEmptyList ForeignError) SearchResult
decodeTweetsJSON = decodeJSON >>> runExcept


queryTweets ∷ ∀ e
              . String
              → Aff
                ( ajax ∷ AJAX
                | e
                )
                SearchResult
queryTweets q =
  do
    res ← affjax authReq
    t ← getBearerToken res.response
    r ← getTweets t q
    s ← getSearchResponse r.response
    pure s


log' ∷ ∀ s e
         . Show s
         ⇒ s
         → Eff
           ( console ∷ CONSOLE
           | e
           )
           Unit
log' = show >>> log


tellTweets ∷ ∀ eff
             . ApiAiClient
             → Eff
               ( console ∷ CONSOLE
               , ajax ∷ AJAX
               | eff
               )
               (Canceler
                 ( console ∷ CONSOLE
                 , ajax ∷ AJAX
                 | eff
                 )
               )
tellTweets app = runAff log' (const $ pure unit) do
  SearchResult result ← queryTweets "from:realDonaldTrump"
  let tweetOrEmpty = maybe "" (\(Tweet tw) → tw.text)
  let t = tweetOrEmpty $ head result.statuses
  let res = tell app t
  liftEff $ log res
  liftEff $ log' t
