; Installer-only: no updater, migration, auth or process termination.
[Setup]
AppId={#PackageId}
AppName={#PackageName}
AppVersion={#AppVersion}
VersionInfoVersion={#FileVersion}
DefaultDirName={localappdata}\Programs\{#PackageName}
DefaultGroupName={#PackageName}
PrivilegesRequired=lowest
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
MinVersion=10.0
DisableDirPage=yes
DisableProgramGroupPage=yes
UsePreviousAppDir=yes
OutputDir={#OutputDir}
OutputBaseFilename=Sapiens-Setup
Compression=lzma2/fast
SolidCompression=yes
CloseApplications=no
RestartApplications=no
RestartIfNeededByRun=no
UninstallDisplayIcon={app}\Sapiens4.exe
#ifdef PublisherSign
SignTool=sapiens
SignedUninstaller=yes
#else
SignedUninstaller=no
#endif

[Files]
Source: "{#SourceDir}\*"; DestDir: "{app}"; Excludes: "sapiens-installation.id"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SourceDir}\sapiens-installation.id"; DestDir: "{app}"; Flags: ignoreversion uninsneveruninstall

[Icons]
Name: "{userprograms}\{#PackageName}"; Filename: "{app}\Sapiens4.exe"

[Run]
Filename: "{app}\Sapiens4.exe"; Description: "Launch {#PackageName}"; Flags: nowait postinstall skipifsilent unchecked

[Code]
const
  Identity = '{#PackageId}';

function OwnedDirectory: Boolean;
var Marker: AnsiString;
begin
  Result := LoadStringFromFile(ExpandConstant('{app}\sapiens-installation.id'), Marker);
  Result := Result and (Trim(String(Marker)) = Identity);
end;

function FileAttributes(FileName: String): Cardinal;
  external 'GetFileAttributesW@kernel32.dll stdcall';

function SafeDestination: Boolean;
var Target, StateRoot, Parent, NextParent: String; Item: TFindRec; Attributes: Cardinal;
begin
  Target := AddBackslash(ExpandFileName(ExpandConstant('{app}')));
  Result := True;
  { Reject junctions/symlinks before ownership checks: lexical guards alone
    cannot protect a state directory reached through a redirected parent. }
  Parent := RemoveBackslash(Target);
  while Parent <> '' do begin
    Attributes := FileAttributes(Parent);
    if (Attributes <> $FFFFFFFF) and ((Attributes and $400) <> 0) then begin Result := False; Exit; end;
    NextParent := ExtractFileDir(Parent);
    if CompareText(Parent, NextParent) = 0 then Break;
    Parent := NextParent;
  end;
  StateRoot := AddBackslash(ExpandConstant('{localappdata}\Sapiens4'));
  if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  StateRoot := AddBackslash(ExpandConstant('{localappdata}\Sapiens4 Development'));
  if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  if GetEnv('SAPIENS_DATA_DIR') <> '' then begin
    StateRoot := AddBackslash(ExpandFileName(GetEnv('SAPIENS_DATA_DIR')));
    if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  end;
  if GetEnv('SAPIENS_CONFIG_DIR') <> '' then begin
    StateRoot := AddBackslash(ExpandFileName(GetEnv('SAPIENS_CONFIG_DIR')));
    if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  end;
  if OwnedDirectory then Exit;
  if FindFirst(Target + '*', Item) then begin
    try
      repeat
        if (Item.Name <> '.') and (Item.Name <> '..') then begin
          Result := False;
          Exit;
        end;
      until not FindNext(Item);
    finally
      FindClose(Item);
    end;
  end;
end;

function InstallerProcessId: Cardinal;
  external 'GetCurrentProcessId@kernel32.dll stdcall';

function ProcessesStopped(ForUninstall: Boolean): Boolean;
var Locator, Service, Processes, Process, SelfProcess: Variant;
    I: Integer; ParentId: Cardinal; Prefix, Executable, Uninstaller: String;
begin
  Result := False;
  try
    Prefix := ExpandConstant('{app}');
    if DirExists(Prefix) then Prefix := GetShortName(Prefix);
    Prefix := AddBackslash(Prefix);
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('', 'root\CIMV2');
    ParentId := 0; Uninstaller := '';
    if ForUninstall then begin
      SelfProcess := Service.Get('Win32_Process.Handle="' + IntToStr(InstallerProcessId) + '"');
      ParentId := SelfProcess.ParentProcessId;
      Uninstaller := GetShortName(ExpandConstant('{uninstallexe}'));
    end;
    Processes := Service.ExecQuery('SELECT ExecutablePath, ProcessId FROM Win32_Process WHERE ExecutablePath IS NOT NULL');
    for I := 0 to Processes.Count - 1 do begin
      Process := Processes.ItemIndex(I);
      Executable := Process.ExecutablePath;
      if FileExists(Executable) then Executable := GetShortName(Executable);
      { Inno keeps its original installed first phase alive while the temporary
        second phase runs. Exempt only this exact path and current/parent PID. }
      if ForUninstall and (CompareText(Executable, Uninstaller) = 0) and
         ((Process.ProcessId = InstallerProcessId) or (Process.ProcessId = ParentId)) then Continue;
      if CompareText(Copy(Executable, 1, Length(Prefix)), Prefix) = 0 then begin
        Log('Install/uninstall refused: an executable from the installation is running.');
        Exit;
      end;
    end;
    Result := True;
  except
    Log('Cannot establish stopped installation: ' + GetExceptionMessage);
  end;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  Result := '';
  if not SafeDestination then
    Result := 'Choose a new installation directory. Existing data or unowned files cannot be replaced.'
  else if not ProcessesStopped(False) then
    Result := 'Quit {#PackageName} and its backend, then retry. No running process will be stopped by Setup.';
end;

function InitializeUninstall: Boolean;
begin
  Result := OwnedDirectory and ProcessesStopped(True);
  if not Result then begin
    Log('Uninstall refused: missing installation identity, running process, or process-check failure.');
    if not UninstallSilent then
      MsgBox('Quit {#PackageName} and its backend, then retry uninstall. No running process will be stopped.', mbError, MB_OK);
  end;
end;
