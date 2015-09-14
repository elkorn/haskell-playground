module HBugSpots where

import qualified Data.List as List
import qualified HBugSpots.Internal.Git as Git

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

printNewest :: Git.GitResult [Github.Commit] -> IO ()
printNewest (Left err) = print err
printNewest (Right commits) = print $ head commits

showFiles :: Git.GitResult [Github.Commit] -> IO ()
showFiles (Left err) = print err
showFiles (Right commits) =
  print $
  map (Github.commitFiles) commits

newestDiff :: Git.RepoDescription -> Git.GitResult [Github.Commit] -> IO ()
newestDiff _ (Left err) = print err
newestDiff repo (Right (x:y:_)) =
  Git.getDiff
    Git.CommitDescription {Git.commitDescriptionRepo = repo
                          ,Git.commitDescriptionSha = Github.commitSha x}
    Git.CommitDescription {Git.commitDescriptionRepo = repo
                          ,Git.commitDescriptionSha = Github.commitSha y} >>=
  print
newestDiff _ _ = print "not enough commits."

listCommits :: String -> String -> IO ()
listCommits owner repo =
  Git.listCommits
    Git.RepoDescription {Git.repoDescriptionOwner = owner
                        ,Git.repoDescriptionName = repo} >>=
  showFiles

getNewestDiff :: String -> String -> IO ()
getNewestDiff owner repo =
  let repoDescription =
        Git.RepoDescription {Git.repoDescriptionOwner = owner
                            ,Git.repoDescriptionName = repo}
  in Git.listCommits repoDescription >>= newestDiff repoDescription
