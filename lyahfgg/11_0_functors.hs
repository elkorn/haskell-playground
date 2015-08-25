import Data.Char
import Data.List

main = do
    line <-
        fmap (++ "!") $
        fmap reverse getLine
    print line
    print $
        fmap reverse $
        Just "blah"
    -- if you find yourself binding an IO result to a name, consider fmapping it over a function composition.
    line2 <-
        fmap
            (intersperse '-' .
             reverse . map toUpper)
            getLine
    print line2
    -- The chosen implementation of `fmap` to be used depends on the functor that is passed to it as the last argument.
    print $
        fmap (* 3) (+ 100) 1
    print $
        fmap (replicate 3) Just 123
    print $
        fmap (replicate 3) "yeah"
    -- Functors have to have `id` as their neutral element.
    print $
        identityLaw $
        Just 3
    print $
        identityLaw
            [1 .. 100]
    print $
        compositionLaw
            (+ 3)
            (* 12)
            (Just 18)

identityLaw :: (Functor f, Eq (f a))
            => f a -> Bool
identityLaw f = (fmap id f) ==
    f
compositionLaw :: (Functor f, Eq (f c))
               => (b -> c) -> (a -> b) -> f a -> Bool
compositionLaw f g fnctr = fmap (f . g) fnctr ==
    fmap f (fmap g fnctr)
