module Irving where

import qualified Data.Map as M
import Data.List (nubBy)

-------------------------------------------------------
-- Test Case
-------------------------------------------------------

-- | Example test case from the original paper, used for verifying correctness.
testCase0 :: [(String, [String])]
testCase0 = [
  ("Ralph", ["Penny", "Boris", "Oliver", "Tammy", "Ginny"]),
  ("Penny", ["Oliver", "Ginny", "Ralph", "Boris", "Tammy"]),
  ("Boris", ["Oliver", "Tammy", "Penny", "Ralph", "Ginny"]),
  ("Ginny", ["Ralph", "Boris", "Tammy", "Penny", "Oliver"]),
  ("Oliver", ["Ralph", "Penny", "Ginny", "Tammy", "Boris"]),
  ("Tammy", ["Penny", "Ralph", "Ginny", "Boris", "Oliver"])
  ]

-------------------------------------------------------
-- Phase 1: Stable Matching (Proposal Phase)
-------------------------------------------------------

-- | Performs the first phase of the Irving algorithm.
--   Each participant proposes to their most preferred option until they either:
--   1. Get accepted by an unmatched participant, or
--   2. Compete with another match and get rejected based on preference.
--
--   Returns:
--   - A list of accepted pairs
--   - A list of remaining preferences after rejections
irv1 :: [(String, [String])] -> ([(String, String)], [(String, [String])])
irv1 people = go (M.fromList people) M.empty (map fst people)
  where
    -- Recursive function processing each participant
    go pref ans [] = (M.toList ans, M.toList pref)  -- Return results when everyone is processed
    go pref ans (b:bs) = case M.findWithDefault [] b pref of
        [] -> go (M.delete b pref) ans bs  -- If b has no preferences left, remove them
        (p:ps) ->
            let prefP = M.findWithDefault [] p pref
            in if M.notMember p ans then go pref (M.insert p b ans) bs  -- p is unmatched, accept proposal
               else
                let b' = M.findWithDefault "none" p ans
                    pred = elem b (takeWhile (/= b') prefP)  -- True if b is preferred over b'
                    newB' = filter (/= p) (M.findWithDefault [] b' pref)  -- Remove p from b's list
                    newB = filter (/= p) (M.findWithDefault [] b pref)  -- Remove p from b's list
                    newP' = filter (/= b) (M.findWithDefault [] p pref)  -- Remove b from p's list
                    newP = filter (/= b') (M.findWithDefault [] p pref)  -- Remove b' from p's list
                    newPref = M.insert b' newB' $ M.insert p newP pref  -- Update preferences after rejection
                    newPref' = M.insert b newB $ M.insert p newP' pref  -- Update preferences if b' is preferred
                in if pred then go newPref (M.insert p b ans) (b':bs) else go newPref' ans (b:bs)

-------------------------------------------------------
-- Phase 2: Rejection Phase
-------------------------------------------------------

-- | Helper function that splits a preference list when a rejection occurs.
--   Returns a tuple of (accepted, rejected).
rejection :: Eq a => [a] -> a -> ([a], [a])
rejection [] _ = ([], [])
rejection (x:xs) y = if x == y then ([x], xs) else (x:l, r) where (l, r) = rejection xs y

-- | Phase 2 of the Irving algorithm: Eliminate unfeasible matches.
irv2 :: ([(String, String)], [(String, [String])]) -> [(String, [String])]
irv2 (pairs, pref) = go (M.fromList pref) pairs
  where
    go ans [] = M.toList ans
    go ans ((x, y):xs) =
        let xPref = M.findWithDefault [] x ans
            (new, rejects) = rejection xPref y
        in go (removeRejects (M.insert x new ans) x rejects) xs

    -- | Removes rejected elements from the preference lists.
    removeRejects :: M.Map String [String] -> String -> [String] -> M.Map String [String]
    removeRejects ans _ [] = ans
    removeRejects ans x (y:ys) = removeRejects newAns x ys
      where
        prefY = M.findWithDefault [] y ans
        newAns = M.insert y (filter (/= x) prefY) ans

-------------------------------------------------------
-- Phase 3: Cycle Detection and Final Matching
-------------------------------------------------------

-- | Checks if the table is stable (i.e., no empty preference lists).
tableStable :: [(String, [String])] -> Bool
tableStable l = [] `notElem` map snd l 

-- | Checks if the matching process is complete.
--   A match is complete when every person is matched exactly once.
areWeDone :: [(String, [String])] -> Bool
areWeDone = all ((\x -> length x == 1) . snd)

-- | Detects cycles in the preference lists and returns them as a list of pairs.
detectCycle :: [(String, [String])] -> [(String, String)]
detectCycle pref = go (M.fromList pref) [x] [] x
  where
    x = fst $ head pref
    go pMap ll@(l:ls) rl name =
      let lPref = M.findWithDefault [] l pMap
          newRmember = if length lPref > 1 then lPref !! 1 else ""
          rPref = M.findWithDefault [] newRmember pMap
          newLmember = if null rPref then "" else last rPref
      in if newLmember == name
         then zip (newLmember:ll) (newRmember:rl)
         else go pMap (newLmember:ll) (newRmember:rl) name

-- | Final phase of the Irving algorithm: resolving cycles.
irv3 :: [(String, [String])] -> [(String, String)]
irv3 pref
    | areWeDone pref = map (\(x, y) -> (x, head y)) pref  -- If done, return final pairs
    | not (tableStable pref) = error "No stable pairing found!"  -- Error if instability detected
    | otherwise = irv3 (go (M.fromList pref) (detectCycle pref))  -- Recursively resolve cycles
  where
    -- | Recursively removes cycle-related elements from preference lists.
    go ans [] = M.toList ans
    go ans ((x, y):ys) =
        let xPref = M.findWithDefault [] x ans
            yPref = M.findWithDefault [] y ans
            xPref' = filter (/= y) xPref
            yPref' = filter (/= x) yPref
            newAns = M.insert x xPref' $ M.insert y yPref' ans
        in go newAns ys

-------------------------------------------------------
-- Entry Point: Running the Full Irving Algorithm
-------------------------------------------------------

-- | Runs all three phases of the Irving algorithm to compute stable roommate matches.
roomies :: [(String, [String])] -> [(String, String)]
roomies list = nubBy (\(a, b) (c, d) -> ((a, b) == (c, d)) || ((a, b) == (d, c))) $ irv3 . irv2 . irv1 $ list
