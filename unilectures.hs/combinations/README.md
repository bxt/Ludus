Here's a little piece of why Haskell takes you so damn
much time to write but is really effing pretty. (*cum grano salis*)

Consider your task is to list all combinations of Y and N 
with length 5. Well you might google and find [this][1]:

    [[a,b,c,d,e] | let l <- "YN", 
                  a <- l, b <- l, c <- l, d <- l, e <- l]

It's pretty straightforward, just a simple list comprehension. 
But, you think, if I'm coding anyway, lets do this so you 
can not only do this for 5 but also for an arbitrary number, 
because this a a pretty foreseeable requirement. As you scroll
down a little you find this little bastard:

    yn i = (iterate f [""]) !! i
      where f l = map ('Y':) l ++ map ('N':) l

Wow, you think, my work is already done. Let's skim over it and
paste this into our codebase. While skimming, you stumble
upon the `('Y':)` constructs and figure what that code does
and eventually you fully grasp it. 

But, as you copy the code and try to move on, the problem
keeps coming back to you as it bugs you that you can
still only combine the same old two 'Y' and 'N' chars so you
decide  to revisit that piece of code. Since it would
be to simple to generalize this to just any two elements
you have to prove yourself your hacker skills by 
generalizing to any list of elements. 

You start fiddling around, un-shelf your old monad book,
read a little, and after some `:t` checking in `ghci` you 
end up with these lines: 

    yn xs i = (iterate f [""]) !! i
      where f l = l >>= ssfm
            ssfm s = map ($s) ssf
            ssf = map (\x -> (x:)) xs

You're glad it finally works, but if your look at your
line count (4!!!) and the code (who needs `\` anyway?) 
you still feel like an undergrad cs student. (And maybe you actually are!)
 The first thing you see is how to simplify
line 4 to a mere `ssf = map (:) xs`. Pretty cool and you can already
make up some function composition basics and put line 4 into 3 like this:
`ssfm s = map (($s).(:)) ssf`. But still your code looks like Haskell abused. 
You unsodomize it and eliminate some brackets: `ssfm s = map (:s) ssf`. 
Now that looks a lot better but it's still like 3 lines. 
Who cares about readability, only hackers can read Haskell anyway, 
so lets scramble this even more, plus
we're adding a type signature and a main method for testing:

    yn :: [a] -> Int -> [[a]]
    yn xs n = (iterate (>>=f) [""]) !! n
      where f s = map (:s) xs
    
    main = print $ yn "YN" 5

Well that looks like a nice piece of code, but it'd better be pointfree so 
that only real nerds know what happens:

    yn = (!!) . flip iterate [[]] . (=<<) . flip (map . flip (:))

And after all you see that its at least a little faster than:

    yn = (sequence .) . flip replicate

Looking at your neat pointless, uhm pointfree, function you might wonder
what you have actually done. Since a list of .-ed functions is a litte
like a inversed list of |-ed commands in bash, we'll look at it from the
back: The first function is aready composed of a flipped cons (:) and 
a map. So while cons would add something to a list, the flipped cons
would take a list first and add something to it then. Next we map this 
flipped cons over a list, and flipping this again it is 
composing us a function that takes two lists
and returns a list of lists, so for the second list build
a function that conses anything to that list and map this over the
first list. This process looks like this: 

    > (flip $ map . flip (:)) [1,2] [3,4]
    [[1,3,4],[2,3,4]]

So this is the usage of the fist composed function. Now we compose
it with the >>= operator. It looks like this:

    (=<<) :: (Monad m) => (a -> m b) -> m a -> m b

And for lists it takes a function the transforms every
list item in a list and a list and appeds these result lists together. 
Compare to map or <$>:

    >  (replicate  4) `map` "ab"
    ["aaaa","bbbb"]
    >  (replicate  4) =<< "ab"
    "aaaabbbb"

Now this operator gets our list appending function piped. So our pipe stack
could now take a list, build its appending function and return a function that
would append these elements to every list in a list. This resulting function
is now piped to flipped iterate that is already applied to [[]]. The flipped
itarate takes [[]] as a starting value and repeatedly applies our append to
every list in list funtion forever, yielding us the results after each iteration
in a list. The last step pipes this function to the supscript operator !!, so that
our final result is, when given a list, a function that, given an argument n, applies
our append function n times. If you thik about this, its just what we wanted to do
in the first place. 

Conclusion: Haskell is fun! :)

  [1]:http://notes-on-haskell.blogspot.com/2008/03/finger-exercises.html
