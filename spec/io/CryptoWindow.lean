import io.Desktop

open Lean System
namespace Spec

abbrev CryptoWindow := Json → IO Json

def Desktop.fixtureField (desktop : Desktop) (label : String) : IO Json :=
  untilSome label do
    IO.ofExcept <| enabledControl (← desktop.findRole (if Platform.isWindows then "AXTextField" else "AXTextArea")) label

def withBrowserFixture (host : Host) (use : Desktop → CryptoWindow → IO α) : IO α :=
  withDesktopConfig fun profile => do
    let configured ← IO.getEnv "SAPIENS_TEST_BROWSER"
    let programFiles ← IO.getEnv "PROGRAMFILES"
    let programFilesX86 ← IO.getEnv "PROGRAMFILES(X86)"
    let candidates : List String := if Platform.isOSX then
      ["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"]
      else if Platform.isWindows then [
        programFiles.getD "C:/Program Files" ++ "/Google/Chrome/Application/chrome.exe",
        programFilesX86.getD "C:/Program Files (x86)" ++ "/Microsoft/Edge/Application/msedge.exe"]
      else ["/usr/bin/chromium", "/usr/bin/chromium-browser", "/usr/bin/google-chrome"]
    let some browser ← (configured.toList ++ candidates).findM? (fun path => (FilePath.mk path).pathExists)
      | throw <| IO.userError "Install Chrome/Chromium/Edge or set SAPIENS_TEST_BROWSER"
    withWindow browser #["--user-data-dir=" ++ profile.toString, "--no-first-run",
      "--no-default-browser-check", "--disable-background-networking", "--disable-extensions",
      "--force-renderer-accessibility", "--app=" ++ host.url ++ "/workspace/#spec"] #[] fun desktop => do
      discard <| desktop.fixtureField "Fixture request"
      let serial ← IO.mkRef (0 : Nat)
      let call := fun (input : Json) => do
        serial.modify (· + 1)
        let id ← serial.get
        let request := (json% {id: $(id), input: $(input)}).compress
        let field ← desktop.fixtureField "Fixture request"
        discard <| desktop.invoke "paste" #["--target-path", ← IO.ofExcept <| stringField field "path", "--text", request]
        untilSome "fixture result" do
          let raw ← IO.ofExcept <| stringField (← desktop.fixtureField "Fixture result") "value"
          match Json.parse raw with
          | .ok response =>
            if response.getObjValD "id" != toJson id then return none
            if response.getObjValD "error" != Json.null then throw <| IO.userError response.compress
            return some (response.getObjValD "value")
          | .error _ => return none
      use desktop call

def withCryptoSession (host : Host) (use : CryptoWindow → IO α) : IO α :=
  withBrowserFixture host (fun _ call => use call)

def withCryptoWindow (host : Host) (use : IO α) : IO α :=
  withCryptoSession host (fun _ => use)

def withCryptoHost (directory : FilePath) (use : Host → IO α) : IO α :=
  withHost directory fun host => withCryptoWindow host (use host)

end Spec
