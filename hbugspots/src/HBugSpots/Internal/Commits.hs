module HBugSpots.Internal.Commits (get) where

import qualified Github.Repos.Commits as Github

get :: String -> String -> IO (Either Github.Error [Github.Commit])
get = Github.commitsFor
