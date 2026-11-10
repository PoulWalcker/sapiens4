module Source (requireMainAncestor) where

import Control.Monad
import Data.Char (isSpace)
import Options (releaseRepository)
import System.Exit
import System.Process

-- A dedicated ref binds production to upstream even in a contributor fork.
requireMainAncestor :: FilePath -> String -> IO ()
requireMainAncestor root commit = do
 unless (length commit == 40 && all (`elem` ['0'..'9'] ++ ['a'..'f']) commit) $
  fail "Production source must be a full commit SHA"
 (localStatus, local, _) <- git ["rev-parse", "--verify", reference ++ "^{commit}"]
 unless (localStatus == ExitSuccess) $ fail ("Missing production main reference. " ++ refresh)
 (remoteStatus, remote, _) <- git ["ls-remote", "--exit-code", repository, "refs/heads/main"]
 unless (remoteStatus == ExitSuccess) $ fail ("Cannot verify current production main; check access to " ++ releaseRepository ++ " and refresh the release ref")
 let mainCommit = takeWhile (not . isSpace) local
 unless (mainCommit == takeWhile (not . isSpace) remote) $
  fail ("Stale production main reference. " ++ refresh)
 (status, _, _) <- git ["merge-base", "--is-ancestor", commit, mainCommit]
 unless (status == ExitSuccess) $
  fail ("Production source " ++ commit ++ " is not in " ++ releaseRepository ++ " main history (" ++ mainCommit ++ ")")
 where
 repository = "https://github.com/" ++ releaseRepository ++ ".git"
 reference = "refs/remotes/release/main"
 refresh = "Run: git fetch " ++ repository ++ " +refs/heads/main:" ++ reference
 git arguments = readProcessWithExitCode "git" (["-C", root] ++ arguments) ""
