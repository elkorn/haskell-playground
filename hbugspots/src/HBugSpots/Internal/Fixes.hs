module HBugSpots.Internal.Fixes (Fix(..), findAll) where

import qualified Github.Repos.Commits as Github
import Text.Regex.Posix

data Fix =
  Fix {fixMessage :: String
      ,fixDate :: String
      ,fixFiles :: [String]}
  deriving (Show,Eq)

defaultPattern :: String
defaultPattern = "(fix(es|ed)?|close(s|d)? #[0-9]+)"

isFixCommit :: String -> Github.Commit -> Bool
isFixCommit fixPattern commit =
  (Github.gitCommitMessage . Github.commitGitCommit $ commit) =~
  fixPattern

fix :: Github.Commit -> Fix
fix commit =
  Fix {fixMessage = Github.gitCommitMessage . Github.commitGitCommit $ commit
      ,fixFiles = map Github.fileFilename $ Github.commitFiles commit
      ,fixDate = show $ Github.fromGithubDate . Github.gitUserDate .
                                                Github.gitCommitAuthor .
                                                Github.commitGitCommit $ commit}

findAll :: [Github.Commit] -> Maybe String -> [Fix]
findAll commits Nothing =
  findAll commits $
  Just defaultPattern
findAll commits (Just fixPattern) =
  map fix .
  filter (isFixCommit fixPattern) $
  commits
