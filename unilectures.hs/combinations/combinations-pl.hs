
-- based on http://notes-on-haskell.blogspot.com/2008/03/finger-exercises.html

yn = (sequence .) . flip replicate

main = print $ yn "YNiIKpd" 9

