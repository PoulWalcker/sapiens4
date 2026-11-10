module Sapiens.Data where

import Prelude
import Data.Argonaut.Core (Json, jsonNull, fromString, fromBoolean, fromNumber, fromArray, fromObject, toString, toBoolean, toNumber, toArray, toObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.String.CodeUnits as CU
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (RegexFlags, global, noFlags)
import Data.Tuple (Tuple)
import Foreign.Object as O

get :: String -> Json -> Json
get key value = fromMaybe jsonNull (toObject value >>= O.lookup key)

at :: Array String -> Json -> Json
at keys value = foldl (flip get) value keys

str :: Json -> String
str = fromMaybe "" <<< toString

s :: String -> Json -> String
s key = str <<< get key

num :: Json -> Number
num = fromMaybe 0.0 <<< toNumber

n :: String -> Json -> Number
n key = num <<< get key

int :: Json -> Int
int = I.floor <<< num

flag :: Json -> Boolean
flag = fromMaybe false <<< toBoolean

b :: String -> Json -> Boolean
b key = flag <<< get key

arr :: Json -> Array Json
arr = fromMaybe [] <<< toArray

rows :: String -> Json -> Array Json
rows key = arr <<< get key

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< O.fromFoldable

json :: forall a. EncodeJson a => a -> Json
json = encodeJson

put :: String -> Json -> Json -> Json
put key value = fromObject <<< O.insert key value <<< fromMaybe O.empty <<< toObject

merge :: Json -> Json -> Json
merge old fresh = fromObject (O.union (fromMaybe O.empty (toObject fresh)) (fromMaybe O.empty (toObject old)))

remove :: String -> Json -> Json
remove key = fromObject <<< O.delete key <<< fromMaybe O.empty <<< toObject

parse :: String -> Json
parse raw = case jsonParser raw of
  Right value -> value
  Left _ -> jsonNull

text :: String -> Json
text = fromString

bool :: Boolean -> Json
bool = fromBoolean

number :: Number -> Json
number = fromNumber

integer :: Int -> Json
integer = number <<< I.toNumber

array :: Array Json -> Json
array = fromArray

empty :: Json
empty = obj []

null :: Json
null = jsonNull

valid :: Json -> Boolean
valid value = value /= jsonNull

find :: String -> Array Json -> Json
find id = fromMaybe jsonNull <<< A.find (\a -> s "id" a == id)

first :: Array Json -> Json
first = fromMaybe jsonNull <<< A.head

last :: Array Json -> Json
last = fromMaybe jsonNull <<< A.last

strings :: Json -> Array String
strings = map str <<< arr

has :: String -> Json -> Boolean
has id = A.elem id <<< strings

fallback :: String -> String -> String
fallback backup value = if value == "" then backup else value

matches :: String -> String -> Boolean
matches pattern value = case R.regex pattern noFlags of
  Left _ -> false
  Right re -> R.test re value

replace :: String -> String -> String -> String
replace pattern replacement value = case R.regex pattern global of
  Left _ -> value
  Right re -> R.replace re replacement value

cut :: Int -> String -> String
cut = S.take

encode :: Json -> String
encode = stringify

objectStrings :: Json -> O.Object String
objectStrings = map str <<< fromMaybe O.empty <<< toObject

type Match = { before :: String, full :: String, groups :: Array String, after :: String }

match :: String -> String -> Maybe Match
match = matchWith noFlags

matchWith :: RegexFlags -> String -> String -> Maybe Match
matchWith flags pattern value = case R.regex pattern flags of
  Left _ -> Nothing
  Right regex -> do
    index <- R.search regex value
    groups <- R.match regex value
    let values = map (fromMaybe "") (NEA.toArray groups)
        full = fromMaybe "" $ A.head values
    if full == "" then Nothing else Just {before:CU.take index value,full,groups:values,after:CU.drop (index+CU.length full) value}
