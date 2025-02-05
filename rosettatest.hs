import qualified Data.Map as M

allBoysPreferenceOrder :: [(String, [String])]
allBoysPreferenceOrder = [
    ("abe", ["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"]), 
    ("bob", ["cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"]),
    ("col", ["hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"]),
    ("dan", ["ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"]),
    ("ed", ["jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"]),
    ("fred", ["bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"]),
    ("gav", ["gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"]),
    ("hal", ["abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"]),
    ("ian", ["hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"]),
    ("jon", ["abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"])
    ]

allGirlsPreferenceOrder :: [(String, [String])]
allGirlsPreferenceOrder = [
    ("abi", ["bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"]),
    ("bea", ["bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"]),
    ("cath", ["fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"]),
    ("dee", ["fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"]),
    ("eve", ["jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"]),
    ("fay", ["bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"]),
    ("gay", ["jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"]),
    ("hope", ["gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"]),
    ("ivy", ["ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"]),
    ("jan", ["ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"])
    ]

gsa :: [(String, [String])] -> [(String, [String])] -> [(String, String)]
gsa proposers choosers = go (M.fromList proposers) (M.fromList choosers) M.empty (map fst proposers) where
    go :: M.Map String [String] -> M.Map String [String] -> M.Map String String -> [String] -> [(String, String)]
    go _ _ ans [] = M.toList ans
    go boysPref girlsPref ans (b:bs) = case M.findWithDefault [] b boysPref of
        [] -> go (M.delete b boysPref) girlsPref ans bs
        (p:ps) ->
            let g = p
                prefGirl = M.findWithDefault [] g girlsPref
            in if M.notMember g ans then go (M.insert b ps (M.delete b boysPref)) girlsPref (M.insert g b ans) bs else
                let b' = M.findWithDefault "none" g ans
                    pred = elem b (takeWhile (/= b') prefGirl) -- This is true if b is better than b' and false otherwise
                in if pred then go (M.insert b ps (M.delete b boysPref)) girlsPref (M.insert g b (M.delete g ans)) (b' : bs) else go (M.insert b ps (M.delete b boysPref)) girlsPref ans (b:bs)


marraiges :: [(String, String)]
marraiges = if length allBoysPreferenceOrder > length allGirlsPreferenceOrder 
            then gsa allGirlsPreferenceOrder allBoysPreferenceOrder 
            else map swap (gsa allBoysPreferenceOrder allGirlsPreferenceOrder)

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
    , elem m2 (takeWhile (/= m1) prefw1) 
    , elem m1 (takeWhile (/= m2) prefw2) 
    , elem w2 (takeWhile (/= w1) prefm1) 
    , elem w1 (takeWhile (/= w2) prefm2)
    ]
  where
    -- Helper function to get the preference list for a given person
    getPreference :: String -> [(String, [String])] -> [String]
    getPreference person preferences = 
        case lookup person preferences of
            Just prefs -> prefs
            Nothing    -> error $ "Person not found in preferences: " ++ person

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

