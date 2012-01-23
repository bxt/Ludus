
-- based on http://notes-on-haskell.blogspot.com/2008/03/finger-exercises.html

yn :: [a] -> Int -> [[a]]
yn = (!!) . flip iterate [[]] . (=<<) . flip (map . flip (:))

main = print $ yn "YNiIKpd" 9

