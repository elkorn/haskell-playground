module FakeCommit (normalCommit, fixCommit) where

import Github.Repos.Commits
import Data.Time.Format
 
normalCommit :: Commit
normalCommit = Commit {commitSha = "dcb8fd3fd8968e8607131b3f4efe99f782661c7b", commitParents = [Tree {treeSha = "1f3e4bcf7b165caabf95ce7276d4b78008e7019c", treeUrl = "https://api.github.com/repos/elkorn/wiki/commits/1f3e4bcf7b165caabf95ce7276d4b78008e7019c", treeGitTrees = []}], commitUrl = "https://api.github.com/repos/elkorn/wiki/commits/dcb8fd3fd8968e8607131b3f4efe99f782661c7b", commitGitCommit = GitCommit {gitCommitMessage = "Begin migrating to orgmode.", gitCommitUrl = "https://api.github.com/repos/elkorn/wiki/git/commits/dcb8fd3fd8968e8607131b3f4efe99f782661c7b", gitCommitCommitter = GitUser {gitUserName = "Korneliusz Caputa", gitUserEmail = "helluinster@gmail.com", gitUserDate = GithubDate {fromGithubDate = read "2015-08-08 13:13:41 UTC"}}, gitCommitAuthor = GitUser {gitUserName = "Korneliusz Caputa", gitUserEmail = "helluinster@gmail.com", gitUserDate = GithubDate {fromGithubDate = read "2015-08-08 13:13:41 UTC"}}, gitCommitTree = Tree {treeSha = "1e502c34a00845600dd9580e57cae3fd1eee5d1f", treeUrl = "https://api.github.com/repos/elkorn/wiki/git/trees/1e502c34a00845600dd9580e57cae3fd1eee5d1f", treeGitTrees = []}, gitCommitSha = Nothing, gitCommitParents = []}, commitCommitter = Just (GithubUser {githubOwnerAvatarUrl = "https://avatars.githubusercontent.com/u/1392368?v=3", githubOwnerLogin = "elkorn", githubOwnerUrl = "https://api.github.com/users/elkorn", githubOwnerId = 1392368, githubOwnerGravatarId = Just ""}), commitAuthor = Just (GithubUser {githubOwnerAvatarUrl = "https://avatars.githubusercontent.com/u/1392368?v=3", githubOwnerLogin = "elkorn", githubOwnerUrl = "https://api.github.com/users/elkorn", githubOwnerId = 1392368, githubOwnerGravatarId = Just ""}), commitFiles = [], commitStats = Nothing}

fixCommit :: Commit
fixCommit = Commit {commitSha = "dcb8fd3fd8968e8607131b3f4efe99f782661c7b", commitParents = [Tree {treeSha = "1f3e4bcf7b165caabf95ce7276d4b78008e7019c", treeUrl = "https://api.github.com/repos/elkorn/wiki/commits/1f3e4bcf7b165caabf95ce7276d4b78008e7019c", treeGitTrees = []}], commitUrl = "https://api.github.com/repos/elkorn/wiki/commits/dcb8fd3fd8968e8607131b3f4efe99f782661c7b", commitGitCommit = GitCommit {gitCommitMessage = "Begin migrating to orgmode. (fixes #123)", gitCommitUrl = "https://api.github.com/repos/elkorn/wiki/git/commits/dcb8fd3fd8968e8607131b3f4efe99f782661c7b", gitCommitCommitter = GitUser {gitUserName = "Korneliusz Caputa", gitUserEmail = "helluinster@gmail.com", gitUserDate = GithubDate {fromGithubDate = read "2015-08-08 13:13:41 UTC"}}, gitCommitAuthor = GitUser {gitUserName = "Korneliusz Caputa", gitUserEmail = "helluinster@gmail.com", gitUserDate = GithubDate {fromGithubDate = read "2015-08-08 13:13:41 UTC"}}, gitCommitTree = Tree {treeSha = "1e502c34a00845600dd9580e57cae3fd1eee5d1f", treeUrl = "https://api.github.com/repos/elkorn/wiki/git/trees/1e502c34a00845600dd9580e57cae3fd1eee5d1f", treeGitTrees = []}, gitCommitSha = Nothing, gitCommitParents = []}, commitCommitter = Just (GithubUser {githubOwnerAvatarUrl = "https://avatars.githubusercontent.com/u/1392368?v=3", githubOwnerLogin = "elkorn", githubOwnerUrl = "https://api.github.com/users/elkorn", githubOwnerId = 1392368, githubOwnerGravatarId = Just ""}), commitAuthor = Just (GithubUser {githubOwnerAvatarUrl = "https://avatars.githubusercontent.com/u/1392368?v=3", githubOwnerLogin = "elkorn", githubOwnerUrl = "https://api.github.com/users/elkorn", githubOwnerId = 1392368, githubOwnerGravatarId = Just ""}), commitFiles = [], commitStats = Nothing}
