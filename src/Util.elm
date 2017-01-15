module Util exposing(updateList)

import List exposing (map)

updateList : List a -> (a -> Bool) -> a -> List a
updateList list shouldUpdateItem newItem =
  (map (\item -> 
    if shouldUpdateItem item then
      newItem
    else
      item
  ) list)