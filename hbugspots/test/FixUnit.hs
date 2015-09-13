module FixUnit (tests) where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified HBugSpots.Internal.Fixes as Fixes
import Github.Repos.Commits
import FakeCommit

tests =
  testGroup "Fix unit tests"
            [testCase "recognizes a non-fix commit" $
             (Fixes.findAll [normalCommit]
                          Nothing) @?=
             ([] :: [Fixes.Fix])
            ,testCase "recognizes a fix commit" $
             (Fixes.findAll [fixCommit]
                          Nothing) @?=
             ([Fixes.Fix {Fixes.fixMessage =
                          (gitCommitMessage . commitGitCommit) fixCommit
                       ,Fixes.fixFiles = map fileFilename $ commitFiles fixCommit
                       ,Fixes.fixDate =
                          show $ (fromGithubDate . gitUserDate . gitCommitAuthor . commitGitCommit) fixCommit}])]
