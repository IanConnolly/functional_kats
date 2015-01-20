module Test (everyNth, takeNthFromStart) where


takeNthFromStart :: Int -> [a] -> [a]
takeNthFromStart n (x:xs) = x : everyNth n xs

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n - 1) xs of
                    (y:ys) -> y : everyNth n ys
                    [] -> []