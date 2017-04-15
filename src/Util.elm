module Util exposing(updateList, makeRange, belongsToRange)

import List exposing (map)

type alias Range = {
  from: Int,
  to: Int
}

makeRange : Int -> Int -> Range
makeRange from to =
  Range (min from to) (max from to)

belongsToRange : Int -> Range -> Bool
belongsToRange number range =
  number >= range.from && number <= range.to

updateList : List a -> (a -> Bool) -> a -> List a
updateList list shouldUpdateItem newItem =
  (map (\item -> 
    if shouldUpdateItem item then
      newItem
    else
      item
  ) list)