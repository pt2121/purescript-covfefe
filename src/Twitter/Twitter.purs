module Twitter.Twitter where

import Prelude (class Show, ($))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


newtype Tweet = Tweet
                { created_at ∷ String,
                  id ∷ Number,
                  id_str ∷ String,
                  text ∷ String,
                  truncated ∷ Boolean,
                  -- entities ∷ NullOrUndefined String,
                  -- extended_entities ∷ NullOrUndefined String,
                  -- metadata ∷ NullOrUndefined String,
                  source ∷ String,
                  in_reply_to_status_id ∷ NullOrUndefined Number,
                  in_reply_to_status_id_str ∷ NullOrUndefined String,
                  in_reply_to_user_id ∷ NullOrUndefined Number,
                  in_reply_to_user_id_str ∷ NullOrUndefined String,
                  in_reply_to_screen_name ∷ NullOrUndefined String,
                  user ∷ User,
                  geo ∷ NullOrUndefined String,
                  coordinates ∷ NullOrUndefined String,
                  -- place ∷ NullOrUndefined String,
                  contributors ∷ NullOrUndefined String,
                  is_quote_status ∷ Boolean,
                  retweet_count ∷ Number,
                  favorite_count ∷ Number,
                  favorited ∷ Boolean,
                  retweeted ∷ Boolean,
                  possibly_sensitive ∷ NullOrUndefined Boolean,
                  lang ∷ String
                }

derive instance genericTweet ∷ Generic Tweet _

instance showTweet ∷ Show Tweet where
  show = genericShow

instance decodeTweet ∷ Decode Tweet where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeTweet ∷ Encode Tweet where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


newtype User = User
               { id ∷ Number,
                 id_str ∷ String,
                 name ∷ String,
                 screen_name ∷ String,
                 location ∷ NullOrUndefined String,
                 description ∷ NullOrUndefined String,
                 url ∷ NullOrUndefined String,
                 -- entities ∷ NullOrUndefined String,
                 protected ∷ Boolean,
                 followers_count ∷ Number,
                 friends_count ∷ Number,
                 listed_count ∷ Number,
                 created_at ∷ String,
                 favourites_count ∷ Number,
                 utc_offset ∷ Number,
                 time_zone ∷ String,
                 geo_enabled ∷ Boolean,
                 verified ∷ Boolean,
                 statuses_count ∷ Number,
                 lang ∷ String,
                 contributors_enabled ∷ Boolean,
                 is_translator ∷ Boolean,
                 is_translation_enabled ∷ Boolean,
                 profile_background_color ∷ String,
                 profile_background_image_url ∷ NullOrUndefined String,
                 profile_background_image_url_https ∷ NullOrUndefined String,
                 profile_background_tile ∷ Boolean,
                 profile_image_url ∷ NullOrUndefined String,
                 profile_image_url_https ∷ NullOrUndefined String,
                 profile_banner_url ∷ NullOrUndefined String,
                 profile_link_color ∷ String,
                 profile_sidebar_border_color ∷ String,
                 profile_sidebar_fill_color ∷ String,
                 profile_text_color ∷ String,
                 profile_use_background_image ∷ Boolean,
                 has_extended_profile ∷ Boolean,
                 default_profile ∷ Boolean,
                 default_profile_image ∷ Boolean,
                 following ∷ NullOrUndefined String,
                 follow_request_sent ∷ NullOrUndefined String,
                 notifications ∷ NullOrUndefined String,
                 translator_type ∷ String
               }

derive instance genericUser ∷ Generic User _

instance showUser ∷ Show User where
  show = genericShow

instance decodeUser ∷ Decode User where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeUser ∷ Encode User where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


newtype SearchResult = SearchResult {
  statuses ∷ Array Tweet
  -- search_metadata ∷ NullOrUndefined SearchMetadata
}

derive instance genericSearchResult ∷ Generic SearchResult _

instance showSearchResult ∷ Show SearchResult where
  show = genericShow

instance decodeSearchResult ∷ Decode SearchResult where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeSearchResult ∷ Encode SearchResult where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


type Consumer = {
  key ∷ String,
  secret ∷ String
}
