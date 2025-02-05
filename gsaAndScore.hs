-- {Still to be implemented openly!}
import qualified Data.Map as M
import Data.List(sortOn)   

-- Some tuple operations

-- Swap the elements of a tuple
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- Extract the third element of a 3-tuple
thd :: (a,b,c) -> c
thd (_, _, x) = x

-- Some Decimal Operations

-- Round a Float to `n` decimal places
roundTo :: Int -> Float -> Float
roundTo n x = fromIntegral (truncate $ x * 10^n) / 10^n

-- Sample data

boyNames :: [String]
boyNames = ["Raju", "Kaju", "Daju"]
boysAnswers :: [[Int]]
boysAnswers = [[1,2,3,4,5], [2,3,1,4,5], [5,5,5,5,5]]
girlNames :: [String]
girlNames = ["Rina", "Ina", "Mina"]
girlsAnswers :: [[Int]]
girlsAnswers = [[5,4,3,2,1], [1,1,1,1,1], [2,5,1,3,4]]

-- Make profiles using the answers
boysProfiles :: M.Map String [Int]
boysProfiles = M.fromList (zip boyNames boysAnswers)

girlsProfiles :: M.Map String [Int]
girlsProfiles = M.fromList (zip girlNames girlsAnswers)

-- Calculate the distance between two lists of integers
powerOfExponation :: Int
powerOfExponation = 2

dist :: [Int] -> [Int] -> Int
dist [] [] = 0
dist [] _ = error "Both lists must have same length"
dist _ [] = error "Both lists must have same length"
dist (x:xs) (y:ys) = (x-y)^powerOfExponation + dist xs ys

-- Given a girl, generate a list of boys with their corresponding scores
-- The score is the distance between the girl's answers and the boy's answers. The lower the better.
boyPreferenceOrder :: String -> [(String, Int)]
boyPreferenceOrder girl = map go boyNames where
    go x = (x, score x)
    score x = dist (girlsProfiles M.! girl) (boysProfiles M.! x)

-- Given a boy, generate a list of girls with their corresponding scores
girlPreferenceOrder :: String -> [(String, Int)]
girlPreferenceOrder boy = map go girlNames where
    go x = (x, score x)
    score x = dist (boysProfiles M.! boy) (girlsProfiles M.! x)

-- List of all girls with their preference order of boys, including scores
allGirlsPreferenceOrderWithScore :: [(String, [(String, Int)])]
allGirlsPreferenceOrderWithScore = map (\x -> (x, boyPreferenceOrder x)) girlNames

-- List of all boys with their preference order of girls, including scores
allBoysPreferenceOrderWithScore :: [(String, [(String, Int)])]
allBoysPreferenceOrderWithScore = map (\x -> (x, girlPreferenceOrder x)) boyNames

-- List of all boys with their preference order of girls(sorted). without scores
allBoysPreferenceOrder :: [(String, [String])]
allBoysPreferenceOrder = map (\(x,y) -> (x, map fst $ sortOn snd y)) allBoysPreferenceOrderWithScore

-- List of all girls with their preference order of boys(sorted). without scores
allGirlsPreferenceOrder :: [(String, [String])]
allGirlsPreferenceOrder = map (\(x,y) -> (x, map fst $ sortOn snd y)) allGirlsPreferenceOrderWithScore

-- The Gale-Shapley algorithm implementation
-- `proposers` are the ones proposing (e.g., boys), and `choosers` are the ones choosing (e.g., girls)
gsa :: [(String, [String])] -> [(String, [String])] -> [(String, String)]
gsa proposers choosers = go (M.fromList proposers) (M.fromList choosers) M.empty (map fst proposers) where
    go :: M.Map String [String] -> M.Map String [String] -> M.Map String String -> [String] -> [(String, String)]
    go _ _ ans [] = M.toList ans  -- Base case: no more proposers, return the answer
    go boysPref girlsPref ans (b:bs) = case M.findWithDefault [] b boysPref of
        [] -> go (M.delete b boysPref) girlsPref ans bs  -- If no more preferences, move to next proposer
        (p:ps) ->
            let g = p
                prefGirl = M.findWithDefault [] g girlsPref
            in if M.notMember g ans then 
                -- If the girl is not already matched, match her with the current proposer
                go (M.insert b ps (M.delete b boysPref)) girlsPref (M.insert g b ans) bs 
            else
                -- If the girl is already matched, check if the current proposer is better
                let b' = M.findWithDefault "none" g ans
                    pred = elem b (takeWhile (/= b') prefGirl)  -- True if `b` is better than `b'`
                in if pred then 
                    -- If `b` is better, rematch the girl with `b` and put `b'` back in the queue
                    go (M.insert b ps (M.delete b boysPref)) girlsPref (M.insert g b (M.delete g ans)) (b' : bs) 
                else 
                    -- If `b` is not better, move to the next preference
                    go (M.insert b ps (M.delete b boysPref)) girlsPref ans (b:bs)

-- Determine the marriages based on the gsa
-- If there are more boys than girls, girls propose to boys; otherwise, boys propose to girls
marraiges :: [(String, String)]  -- By default, the order is (boy, girl)
marraiges = if length allBoysPreferenceOrder > length allGirlsPreferenceOrder
            then gsa allGirlsPreferenceOrder allBoysPreferenceOrder
            else map swap (gsa allBoysPreferenceOrder allGirlsPreferenceOrder)

-- Find unstable pairs in the current matching
unstablePairs :: [(String, String)] -> [((String, String), (String, String))]
unstablePairs pairs =
    [ ((m1, w2), (m2, w1))
    | (m1, w1) <- pairs
    , (m2, w2) <- pairs
    , m1 /= m2
    , let prefw1 = getPreference w1 allGirlsPreferenceOrder
          prefw2 = getPreference w2 allGirlsPreferenceOrder
          prefm1 = getPreference m1 allBoysPreferenceOrder
          prefm2 = getPreference m2 allBoysPreferenceOrder
    , m2 `elem` takeWhile (/= m1) prefw1  -- `m2` is preferred over `m1` by `w1`
    , m1 `elem` takeWhile (/= m2) prefw2  -- `m1` is preferred over `m2` by `w2`
    , w2 `elem` takeWhile (/= w1) prefm1  -- `w2` is preferred over `w1` by `m1`
    , w1 `elem` takeWhile (/= w2) prefm2  -- `w1` is preferred over `w2` by `m2`
    ]
  where
    -- Helper function to get the preference list of a person
    getPreference :: String -> [(String, [String])] -> [String]
    getPreference person preferences =
        case lookup person preferences of
            Just prefs -> prefs
            Nothing    -> error $ "Person not found in preferences: " ++ person

-- Generate a list of all possible couples with their scores
allCouplesScores :: [(String, String, Int)]
allCouplesScores = sortOn thd $ [(x,y,z) | x <- boyNames, (y,z) <- girlPreferenceOrder x ]

-- Calculate the percentile score of a couple
percentileScore :: (String, String) -> Float
percentileScore (a,b) =100 - 100 * fromIntegral(length ( takeWhile (\(_, _, z) -> z < score) allCouplesScores)) / fromIntegral (length allCouplesScores) where score = thd $ head $ filter (\(x,y,z) -> x == a && b == y) allCouplesScores

-- List of all couples with their percentile scores, rounded to some decimal places
roundingPlace :: Int
roundingPlace = 2
allCouplesPercentile :: [(String, String, Float)]
allCouplesPercentile = [ (x,y, roundTo roundingPlace (percentileScore (x,y)) ) | (x,y,z) <- allCouplesScores ]

-- List of all paured couples with their percentile scores, rounded to `a` decimal places
allPairedPercentile :: [(String, String, Float)]
allPairedPercentile = [(x,y, roundTo roundingPlace (percentileScore (x,y))) | (x,y) <- marraiges]