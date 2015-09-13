module HBugSpots where

import qualified Data.List as List
import qualified HBugSpots.Internal.Commits as Commits

import qualified Github.Repos.Commits as Github

data Hotspot =
  Hotspot {file :: String
          ,score :: Float}
  deriving (Show)

formatAuthor :: Github.GitUser -> String
formatAuthor author =
  (Github.gitUserName author) ++
  " <" ++
  (Github.gitUserEmail author) ++
  ">"

formatCommit :: Github.Commit -> String
formatCommit commit =
  let gitCommit = Github.commitGitCommit commit
  in "commit " ++
     (Github.commitSha commit) ++
     "\nAuthor: " ++
     (formatAuthor (Github.gitCommitAuthor gitCommit)) ++
     "\nDate: " ++
     (Github.gitCommitMessage gitCommit)

showPossibleCommits :: Either Github.Error [Github.Commit] -> IO ()
showPossibleCommits possibleCommits =
  case possibleCommits of
    (Left err) ->
      putStrLn $
      "Error: " ++
      (show err)
    (Right commits) ->
      putStrLn $
      List.intercalate "\n\n" $
      map formatCommit commits

printNewest :: Either Github.Error [Github.Commit] -> IO ()
printNewest (Left err) = print err
printNewest (Right commits) = print $ head commits

find :: String -> String -> IO ()
find owner repo =
  Commits.get owner repo >>=
  printNewest
