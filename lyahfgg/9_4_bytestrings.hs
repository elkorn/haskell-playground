{-
    Strings are `[Char]`. This has performance consequences.
   `Char`s don't have a fixed size, because you might need more than one byte to represent some characters.
   Furthermore, `List`s are really lazy. A list of `[1,2,3]`, being equivalent to `1:2:3:[]` is a series of promises for succeeding elements. Each element will be evaluated only when it is actually accessed. Processing a series of promises is slower than having a static, eager data structure.
   This overhead is problematic when dealing with large files.
   Thunk is the technical term for a Promise.

   Bytestrings come in two variants, strict ones in `Data.ByteString` and lazy ones in `Data.BteString.Lazy`.
   The strict variant works just as expected - there is no laziness whatsoever.
   It's faster, but fills your memory up much quicker.
   The lazy variant is different in its laziness from other data structures i.e. it is evaluated in 64k chunks.
   When you read the first character of a bytestring, the first 64k chunk is loaded into memory. After that, there is a thunk.
-}

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified System.Environment as E

main = do
    print $
        Lazy.pack [97 .. 120]
    -- Useful for joining strict bytestrings without having to join them into a bytestring in memory.
    print $
        Lazy.fromChunks
            [ Strict.pack
                  [97, 98, 99]
            , Strict.pack
                  [100, 101, 102]
            , Strict.pack
                  [103, 104, 105]]
    print $
        Lazy.toChunks $
        Lazy.pack [97, 98, 99]
    print $
        Lazy.unpack $
        Lazy.pack [97, 98, 99]
    -- cons is lazy 'in the standard way' and will create a new chunk everytime, even if the data fits in the old chunk.
    print $
        Lazy.cons 85 $
        Lazy.pack [80 .. 84]
-- it's better to use `cons'` when inserting a lot of bytes at the beginning of a bytestring
    print $
        Lazy.cons' 85 $
        Lazy.pack [80 .. 84]
    print $
        Lazy.pack [256, 336, 80]
    print $ foldr Lazy.cons Lazy.empty [50..60]
    print $ foldr Lazy.cons' Lazy.empty [50..60]
    lazyContents <- Lazy.readFile "todos.txt"
    print $ lazyContents
    strictContents <- Strict.readFile "todos.txt"
    print $ strictContents
    -- (fileName1:fileName2:_) <- E.getArgs
    -- copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  contents <- Lazy.readFile src
  Lazy.writeFile dst contents


    
    
