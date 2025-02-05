module Irving where
import qualified Data.Map as M
import Data.List(nubBy)

-- From the paper, used while making the code
testCase0 :: [(String, [String])]
testCase0 = [
  ("Ralph", ["Penny", "Boris", "Oliver", "Tammy", "Ginny"]),
  ("Penny", ["Oliver", "Ginny", "Ralph", "Boris", "Tammy"]),
  ("Boris", ["Oliver", "Tammy", "Penny", "Ralph", "Ginny"]),
  ("Ginny", ["Ralph", "Boris", "Tammy", "Penny", "Oliver"]),
  ("Oliver", ["Ralph", "Penny", "Ginny", "Tammy", "Boris"]),
  ("Tammy", ["Penny", "Ralph", "Ginny", "Boris", "Oliver"])
  ]


irv1 :: [(String, [String])] -> ([(String,String)], [(String, [String])])
irv1 people = go (M.fromList people) M.empty (map fst people) where
    go pref ans [] = (M.toList ans, M.toList pref)
    go pref ans (b:bs) = case M.findWithDefault [] b pref of
        [] -> go (M.delete b pref) ans bs
        (p:ps) ->
            let prefP = M.findWithDefault [] p pref
            in if M.notMember p ans then go pref (M.insert p b ans) bs else
                let b' = M.findWithDefault "none" p ans
                    pred = elem b (takeWhile (/= b') prefP) -- This is true if b is better than b' and false otherwise
                    newB' = filter (/= p) (M.findWithDefault [] b' pref) -- if p rejects b'
                    newB = filter (/= p) (M.findWithDefault [] b pref) -- if p rejects b
                    newP' = filter (/= b) (M.findWithDefault [] p pref)  -- if p rejects b
                    newP = filter (/= b') (M.findWithDefault [] p pref) -- if p rejects b'
                    newPref = M.insert b' newB' $ M.insert p newP pref
                    newPref' = M.insert b newB $ M.insert p newP' pref
                in if pred then go newPref (M.insert p b ans) (b':bs) else go newPref' ans (b:bs)
                -- If pred == True; update preference of p and b' to remove each other; this is reflected in newPref
                -- If pred == False; update preference of p and b; this is reflected in newPref'


rejection :: Eq a => [a] -> a -> ([a],[a])
rejection [] _ = ([],[])
rejection (x:xs) y = if x==y then ([x], xs) else (x:l, r) where (l,r) =rejection xs y

irv2 :: ([(String,String)], [(String, [String])]) -> [(String, [String])]
irv2 (pairs, pref) = go (M.fromList pref) pairs where
    go ans [] = M.toList ans
    go ans ((x,y):xs) =
        let xPref = M.findWithDefault [] x ans
            (new, rejects) = rejection xPref y
        in go (removeRejects (M.insert x new ans) x rejects) xs

    removeRejects :: M.Map String [String] -> String -> [String] -> M.Map String [String]
    removeRejects ans _ [] = ans
    removeRejects ans x (y:ys) = removeRejects newAns x ys where
        prefY = M.findWithDefault [] y ans
        newAns = M.insert y (filter (/= x) prefY) ans

tableStable :: [(String, [String])] -> Bool
tableStable l = [] `notElem` map snd l 
-- && flc l where
--     flc list@((x,y):ys) =
--         case lookup (head y) list of
--             Nothing -> False
--             Just prefY -> (last prefY == x) && flc ys
-- Note the first-last condition is not checked for.
-- I will hopefully implement it later, although it not being here will cause irv3 to error out anyways!

areWeDone :: [(String, [String])] -> Bool
areWeDone = all ((\x -> length x == 1) . snd)

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

irv3 :: [(String, [String])] -> [(String, String)]
irv3 pref
    | areWeDone pref = map (\(x,y) -> (x, head y)) pref
    | not (tableStable pref) = error "No stable pairing found!"
    | otherwise = irv3 (go (M.fromList pref) (detectCycle pref))
  where
    go ans [] = M.toList ans
    go ans ((x,y):ys) =
        let xPref = M.findWithDefault [] x ans
            yPref = M.findWithDefault [] y ans
            xPref' = filter (/= y) xPref
            yPref' = filter (/= x) yPref
            newAns = M.insert x xPref' $ M.insert y yPref' ans
        in go newAns ys

roomies list = nubBy (\(a,b) (c,d) -> ((a,b) == (c,d)) || ((a,b) == (d,c))) $ irv3.irv2.irv1 $ list
