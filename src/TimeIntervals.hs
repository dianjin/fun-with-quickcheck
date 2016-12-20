module TimeIntervals where

  import Data.List
  import Data.Ord

  type TimeInterval = (Integer, Integer)

  overlapping :: TimeInterval -> TimeInterval -> Bool
  overlapping (_, f1) (s2, _) = f1 >= s2

  groupAdjacentsRecur :: (TimeInterval -> TimeInterval -> Bool) -> [TimeInterval] -> [[TimeInterval]] -> TimeInterval -> [[TimeInterval]]
  groupAdjacentsRecur _ [] acc _ =
    reverse $ map reverse acc
  groupAdjacentsRecur fn remainingList acc latestEnd =
    let
      (x:xs) = remainingList
      (firstChunk:restChunks) = acc
    in
      if fn latestEnd x then
        -- put current elem in firstChunk
        let
          newLatestEnd = if snd latestEnd >= snd x then
            latestEnd
          else
            x
        in
          groupAdjacentsRecur fn xs ((x:firstChunk):restChunks) newLatestEnd
      else
        -- create new firstChunk
        groupAdjacentsRecur fn xs ([x]:acc) x

  groupAdjacents :: (TimeInterval -> TimeInterval -> Bool) -> [TimeInterval] -> [[TimeInterval]]
  groupAdjacents _ [] =
    []
  groupAdjacents fn (x:xs) =
    groupAdjacentsRecur fn xs [[x]] x

  {- Correct implementation -}

  simplifyTimes :: [TimeInterval] -> [TimeInterval]
  simplifyTimes inputTimes = combined
    where
      sorted = sortBy (comparing fst) inputTimes
      grouped = groupAdjacents overlapping sorted
      maxEndTime times = maximum (map snd times)
      combine rs = (fst (head rs), maxEndTime rs)
      combined = map combine grouped

  {- Incorrect implementations -}

  incorrectFn1 :: [TimeInterval] -> [TimeInterval]
  incorrectFn1 inputTimes = combined
    where
      sorted = sortBy (comparing fst) inputTimes
      grouped = groupBy overlapping sorted
      maxEndTime times = maximum (map snd times)
      combine rs = (fst (head rs), maxEndTime rs)
      combined = map combine grouped

  incorrectFn2 :: [TimeInterval] -> [TimeInterval]
  incorrectFn2 [] =
    []
  incorrectFn2 times =
    let
      minStart = minimum (map fst times)
      maxEnd = maximum (map snd times)
    in
      [(minStart, maxEnd)]
