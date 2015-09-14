module HBugSpots.Internal.Git
       (GitResult, CommitDescription(..), RepoDescription(..),
        listCommits, getDiff)
       where

import qualified Github.Repos.Commits as Github

type GitResult a = Either Github.Error a

data RepoDescription =
  RepoDescription {repoDescriptionOwner :: String
                  ,repoDescriptionName :: String}
  deriving (Show,Eq)

data CommitDescription =
  CommitDescription {commitDescriptionRepo :: RepoDescription
                    ,commitDescriptionSha :: String}
  deriving (Show,Eq)

listCommits :: RepoDescription -> IO (GitResult [Github.Commit])
listCommits repoDescription =
  Github.commitsFor (repoDescriptionOwner repoDescription)
                    (repoDescriptionName repoDescription)

getDiff :: CommitDescription -> CommitDescription -> IO (GitResult Github.Diff)
getDiff commit1Description commit2Description =
  Github.diff (repoDescriptionOwner . commitDescriptionRepo $ commit1Description)
              (repoDescriptionName . commitDescriptionRepo $ commit1Description)
              (commitDescriptionSha commit1Description)
              (commitDescriptionSha commit2Description)
