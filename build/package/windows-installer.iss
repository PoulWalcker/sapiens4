[Setup]
AppId={#PackageId}
AppName={#PackageName}
AppVersion={#AppVersion}
VersionInfoVersion={#FileVersion}
#ifdef UpdatePlatform
VersionInfoProductName={#PackageId}
VersionInfoDescription=Sapiens4 Setup {#UpdatePlatform}
VersionInfoProductTextVersion={#AppVersion}
#endif
DefaultDirName={localappdata}\Programs\{#PackageName}
DefaultGroupName={#PackageName}
PrivilegesRequired=lowest
ArchitecturesAllowed={#PackageArchitecture}
ArchitecturesInstallIn64BitMode={#PackageArchitecture}
MinVersion=10.0
DisableDirPage=yes
DisableProgramGroupPage=yes
UsePreviousAppDir=yes
OutputDir={#OutputDir}
OutputBaseFilename={#ArtifactBaseName}
Compression=lzma2/fast
SolidCompression=yes
CloseApplications=no
RestartApplications=no
RestartIfNeededByRun=no
SetupMutex=Global\Sapiens4Setup-{username}
UninstallDisplayIcon={app}\Sapiens4.exe
#ifdef PublisherSign
SignTool=sapiens
SignedUninstaller=yes
#else
SignedUninstaller=no
#endif

[Files]
Source: "{#SourceDir}\*"; DestDir: "{app}"; Excludes: "sapiens-installation.id"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SourceDir}\sapiens-installation.id"; DestDir: "{app}"; Flags: ignoreversion

; Runtime is wholly owned by the package; Data Roots and configuration are
; required to live outside the installation. Remove obsolete B runtime files
; when reinstalling A as well as obsolete A files when installing B.
[InstallDelete]
Type: filesandordirs; Name: "{app}\runtime"

[Icons]
Name: "{userprograms}\{#PackageName}"; Filename: "{app}\Sapiens4.exe"

[Run]
Filename: "{app}\Sapiens4.exe"; Description: "Launch {#PackageName}"; Flags: nowait postinstall skipifsilent unchecked

[Code]
const
  Identity = '{#PackageId}';

function OwnedDirectory: Boolean;
var Marker: AnsiString; Installed: String;
begin
  Result := LoadStringFromFile(ExpandConstant('{app}\sapiens-installation.id'), Marker);
  Result := Result and (Trim(String(Marker)) = Identity);
  Result := Result and RegQueryStringValue(HKCU,
    'Software\Microsoft\Windows\CurrentVersion\Uninstall\' + Identity + '_is1', 'InstallLocation', Installed);
  Result := Result and (CompareText(AddBackslash(Installed), AddBackslash(ExpandConstant('{app}'))) = 0);
end;

function FileAttributes(FileName: String): Cardinal;
  external 'GetFileAttributesW@kernel32.dll stdcall';

function SafeDestination: Boolean;
var Target, StateRoot, Parent, NextParent: String; Item: TFindRec; Attributes: Cardinal;
begin
  Target := AddBackslash(ExpandFileName(ExpandConstant('{app}')));
  Result := True;
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

function OpenInstalledImage(FileName: String; Access, Sharing: Cardinal;
  Security: THandle; Creation, Attributes: Cardinal; Template: THandle): THandle;
  external 'CreateFileW@kernel32.dll stdcall';
function CloseInstalledImage(Handle: THandle): BOOL;
  external 'CloseHandle@kernel32.dll stdcall';
function FlushInstallerFile(Handle: THandle): BOOL;
  external 'FlushFileBuffers@kernel32.dll stdcall';
function PublishInstallerFile(Source, Destination: String; Flags: Cardinal): BOOL;
  external 'MoveFileExW@kernel32.dll stdcall';

procedure FlushCachedFile(Path: String);
var Handle: THandle;
begin
  Handle := OpenInstalledImage(Path, $40000000, 7, 0, 3, 0, 0);
  if Handle = THandle(-1) then RaiseException('Cannot open the recovery installer for flushing.');
  try
    if not FlushInstallerFile(Handle) then RaiseException('Cannot flush the recovery installer.');
  finally
    CloseInstalledImage(Handle);
  end;
end;

procedure CurStepChanged(Step: TSetupStep);
var App, State, Source, Cached, Digest, Next: String;
begin
  if Step <> ssPostInstall then Exit;
  App := RemoveBackslash(ExpandConstant('{app}'));
  State := AddBackslash(ExtractFileDir(App)) + '.' + ExtractFileName(App) + '.update';
  if not ForceDirectories(State + '\installers') then RaiseException('Cannot create the recovery installer directory.');
  Source := ExpandConstant('{srcexe}');
  Digest := GetSHA256OfFile(Source);
  Cached := State + '\installers\' + Digest + '.exe';
  if CompareText(Source, Cached) <> 0 then begin
    if not FileCopy(Source, Cached, False) then RaiseException('Cannot preserve the recovery installer.');
    FlushCachedFile(Cached);
  end;
  if GetSHA256OfFile(Cached) <> Digest then RaiseException('The recovery installer copy is incomplete.');
  Next := State + '\current-installer.new';
  if not SaveStringToFile(Next, AnsiString(Digest), False) then RaiseException('Cannot record the recovery installer.');
  FlushCachedFile(Next);
  if not PublishInstallerFile(Next, State + '\current-installer', 9) then
    RaiseException('Cannot publish the recovery installer.');
end;

type
  TImageFindData = array[0..147] of Cardinal;
function FindImageFile(Pattern: String; var Data: TImageFindData): THandle;
  external 'FindFirstFileW@kernel32.dll stdcall';
function CloseImageSearch(Handle: THandle): BOOL;
  external 'FindClose@kernel32.dll stdcall';

function InstallationImagesWritable(Directory, Exempt: String): Boolean;
var Item: TFindRec; Path, Extension: String; Handle: THandle; Attributes: Cardinal;
    FindData: TImageFindData;
begin
  Attributes := FileAttributes(Directory);
  if Attributes = $FFFFFFFF then begin
    Result := (DLLGetLastError = 2) or (DLLGetLastError = 3);
    Exit;
  end;
  Result := False;
  if (Attributes and $400) <> 0 then Exit;
  if not FindFirst(AddBackslash(Directory) + '*', Item) then begin
    Handle := FindImageFile(AddBackslash(Directory) + '*', FindData);
    if Handle = THandle(-1) then Result := DLLGetLastError = 2
    else CloseImageSearch(Handle);
    Exit;
  end;
  Result := True;
  try
    repeat
      if (Item.Name = '.') or (Item.Name = '..') then Continue;
      Path := AddBackslash(Directory) + Item.Name;
      if (Item.Attributes and $400) <> 0 then begin Result := False; Exit; end;
      if (Item.Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        Result := InstallationImagesWritable(Path, Exempt)
      else begin
        Extension := ExtractFileExt(Path);
        if ((CompareText(Extension, '.exe') = 0) or (CompareText(Extension, '.dll') = 0)) and
           (CompareText(Path, Exempt) <> 0) then begin
          Handle := OpenInstalledImage(Path, $40000000, 7, 0, 3, 0, 0);
          Result := Handle <> THandle(-1);
          if Result then Result := CloseInstalledImage(Handle);
        end;
      end;
      if not Result then begin Log('Installation image unavailable: ' + Path); Exit; end;
    until not FindNext(Item);
  finally
    FindClose(Item);
  end;
end;

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
      if ForUninstall and (CompareText(Executable, Uninstaller) = 0) and
         ((Process.ProcessId = InstallerProcessId) or (Process.ProcessId = ParentId)) then Continue;
      if CompareText(Copy(Executable, 1, Length(Prefix)), Prefix) = 0 then begin
        Log('Install/uninstall refused: an executable from the installation is running.');
        Exit;
      end;
    end;
    Executable := '';
    if ForUninstall then Executable := ExpandConstant('{uninstallexe}');
    Result := InstallationImagesWritable(ExpandConstant('{app}'), Executable);
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
