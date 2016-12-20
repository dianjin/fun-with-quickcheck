module Test.TimeIntervals where

  import Test.QuickCheck
  import TimeIntervals
  import Data.List
  import Data.Ord

  type TestFunctionPredicate =
    ([TimeInterval] -> [TimeInterval]) -> [TimeInterval] -> Bool

  arbitraryTimeIntervals :: (Integral a, Arbitrary a) => Gen [(a, a)]
  arbitraryTimeIntervals =
    let
      arbitraryTimeInterval = do
        Positive t <- arbitrary
        Positive n <- arbitrary
        return (t, t+n)
    in
      sized (\n -> vectorOf n arbitraryTimeInterval)

  covers :: TimeInterval -> TimeInterval -> Bool
  covers (s1, f1) (s2, f2) =
    (s2 <= s1) && (f2 >= f1)

  overlapsWith :: TimeInterval -> TimeInterval -> Bool
  overlapsWith t1 t2 =
    let
      [a, b] = sortBy (comparing fst) [t1, t2]
    in
      overlapping a b

  subtractTimeInterval :: TimeInterval -> TimeInterval -> [TimeInterval]
  subtractTimeInterval toRemove removeFrom =
    let
      (startRemove, endRemove) = toRemove
      (startFrom, endFrom) = removeFrom
    in
      if startRemove > startFrom then
        (startFrom, startRemove):subtractTimeInterval toRemove (startRemove, endFrom)
      else
        if startRemove < startFrom then
          subtractTimeInterval (startFrom, endRemove) removeFrom
        else
          if endRemove >= endFrom then
            []
          else
            [(endRemove, endFrom)]

  subtractTimeIntervalRecur :: [TimeInterval] -> [TimeInterval] -> [TimeInterval]
  subtractTimeIntervalRecur [] reduced =
    reduced
  subtractTimeIntervalRecur remainingToRemove reduced =
    let
      (x:xs) = remainingToRemove
      (covered, notCovered) = partition (overlapsWith x) reduced
      newCoveredList = concatMap (subtractTimeInterval x) covered
    in
      subtractTimeIntervalRecur
        xs
        (newCoveredList ++ notCovered)

  prop_all_inputs_covered :: TestFunctionPredicate
  prop_all_inputs_covered fn inputTimes =
    let
      outputTimes = fn inputTimes
      coveredByOutput inputTime =
        length (filter (covers inputTime) outputTimes) == 1
    in
      all coveredByOutput inputTimes

  prop_no_extraneous_output :: TestFunctionPredicate
  prop_no_extraneous_output fn inputTimes =
    let
      outputTimes = fn inputTimes
    in
      null $ subtractTimeIntervalRecur inputTimes outputTimes

  prop_idempotent :: TestFunctionPredicate
  prop_idempotent fn inputTimes =
    fn (fn inputTimes) == fn inputTimes

  doList [] =
    return ()
  doList (x:xs) =
    do
      x
      doList xs

  testFunction fn =
    let
      properties =
        [ prop_no_extraneous_output
        , prop_idempotent
        , prop_all_inputs_covered
        ]
      checkProp prop =
          quickCheck $ forAll arbitraryTimeIntervals $ prop fn
    in
      doList (map checkProp properties)

  runTests = do
    putStrLn "Running tests for module TimeIntervals"
    putStrLn "Correct implementation:"
    testFunction simplifyTimes
    putStrLn "Incorrect implementations:"
    testFunction incorrectFn1
    testFunction incorrectFn2
