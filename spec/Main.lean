import jobs.user.Desktop
import jobs.user.Execution
import jobs.user.Conversation
import jobs.user.Planning
import jobs.user.Release
import jobs.system.Access
import jobs.system.PrivateData
import jobs.system.Recovery
import jobs.system.RemoteCrypto
import jobs.system.Browser
import jobs.founder.Governance
import jobs.founder.DeviceConsent
import jobs.founder.Accountability

open Lean Spec

abbrev Job := String × IO Unit

def headlessJobs : List Job := [
  ("Persist valid model settings", User.retainModelSettingsAcrossRestart),
  ("MCP work, pause and resume", User.completeWorkThroughMcp),
  ("Restore private notes and bookmarks", User.retainPrivateNotesAndBookmarks),
  ("Scheduled review timing", User.finishOneScheduledReview),
  ("Delegated review ownership and results", User.delegateOneAccountableReview),
  ("Recover interrupted work without replay", SystemJobs.preserveInterruptedWorkAcrossCrash),
  ("Exclusive storage ownership", SystemJobs.rejectConcurrentStorageOwner),
  ("Release cancelled computer access", SystemJobs.releaseExclusiveComputerAccessOnCancellation),
  ("Review failures before retry", Founder.reviewFailedWorkBeforeRetry),
  ("Restore replies and drafts", User.resumeConversationAfterRestart),
  ("Owner control of paused schedules", User.keepScheduledWorkPausedUntilItsOwnerResumes),
  ("Reject untrusted roster access", SystemJobs.rejectUntrustedRequestsWithoutDisclosingLocalState),
  ("Retain attachment and file ownership", SystemJobs.keepFilePreviewsAndAttachmentsWithTheirOwner),
  ("Scope and lifecycle authority across retries", Founder.retainControlOverScopeAndSuspendedWork),
  ("Corpora and draft isolation across restart", SystemJobs.keepLocalCorporaSeparateAcrossRestart),
  ("Cancelled audio cleanup and stale retries", SystemJobs.eraseCancelledVoiceDraftsAcrossRestart)]
def browserJobs : List Job := [
  ("Browser bindings, crypto and onboarding", SystemJobs.preserveBrowserApplicationBehavior),
  ("Remote crypto requires an open browser", SystemJobs.requireAnOpenBrowserForRemoteCrypto),
  ("Resume remote work after reopening", Founder.resumeRemoteWorkWhenBrowserReopens),
  ("Deduplicate remote work across restart", Founder.processRedeliveredRemoteWorkOnce),
  ("Consent withdrawal before pairing approval", Founder.withdrawConsentBeforeTheOtherDeviceApproves),
  ("Remote device approval, reconnection and revocation", Founder.approveOneDeviceThenRevokeItsMailbox)]

def desktopJobs : List Job := [
  ("Desktop messaging and reply persistence", User.sendAndReadOneDesktopReply),
  ("Desktop pause and resume across relaunch", User.pauseAndResumeFromTheDesktop)]

def main (args : List String) : IO UInt32 := do
  let root ← repoRoot
  let built ← IO.Process.output {cmd := "runghc", args := #["spec/BrowserBuild.hs"], cwd := root}
  unless built.exitCode == 0 do throw <| IO.userError built.stderr
  let jobs ← match args with
    | [] => pure (headlessJobs ++ browserJobs ++ desktopJobs)
    | ["headless"] => pure headlessJobs
    | ["browser"] => pure browserJobs
    | ["desktop"] => pure desktopJobs
    | _ => throw <| IO.userError "Usage: lake -d spec test [-- headless|browser|desktop]"
  let mut failures := 0
  let mut examples : Array Json := #[]
  for (name, exercise) in jobs do
    try
      exercise
      IO.println s!"PASS {name}"
      examples := examples.push (json% {description: $(name), status: "passed"})
    catch error =>
      failures := failures + 1
      IO.eprintln s!"FAIL {name}: {error}"
      examples := examples.push (json% {description: $(name), status: "failed", message: $(toString error)})
  if let some path ← IO.getEnv "SAPIENS_TEST_RESULTS" then
    IO.FS.writeFile path (json% {examples: $(examples), summary: {example_count: $(jobs.length), failure_count: $(failures), pending_count: 0}}).compress
  IO.println s!"{jobs.length - failures}/{jobs.length} jobs passed"
  return if failures == 0 then 0 else 1
