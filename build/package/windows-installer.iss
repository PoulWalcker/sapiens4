; Installer-only: no updater, migration, auth or process termination.
[Setup]
AppId=com.poulwalcker.sapiens4.development
AppName=Sapiens4 Development
AppVersion={#AppVersion}
VersionInfoVersion={#FileVersion}
DefaultDirName={localappdata}\Programs\Sapiens4 Development
DefaultGroupName=Sapiens4 Development
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
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{userprograms}\Sapiens4 Development"; Filename: "{app}\Sapiens4.exe"

[Run]
Filename: "{app}\Sapiens4.exe"; Description: "Launch Sapiens4 Development"; Flags: nowait postinstall skipifsilent unchecked

[Code]
const
  Identity = 'com.poulwalcker.sapiens4.development';

function OwnedDirectory: Boolean;
var Marker: AnsiString;
begin
  Result := LoadStringFromFile(ExpandConstant('{app}\sapiens-installation.id'), Marker);
  Result := Result and (Trim(String(Marker)) = Identity);
end;

function SafeDestination: Boolean;
var Target, StateRoot: String; Item: TFindRec;
begin
  Target := AddBackslash(ExpandConstant('{app}'));
  StateRoot := AddBackslash(ExpandConstant('{localappdata}\Sapiens4 Development'));
  Result := CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) <> 0;
  if not Result then Exit;
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

function ProcessesStopped: Boolean;
var Locator, Service, Processes, Process: Variant; I: Integer; Prefix, Executable: String;
begin
  Result := False;
  try
    Prefix := AddBackslash(ExpandConstant('{app}'));
    Locator := CreateOleObject('WbemScripting.SWbemLocator');
    Service := Locator.ConnectServer('', 'root\CIMV2');
    Processes := Service.ExecQuery('SELECT ExecutablePath FROM Win32_Process WHERE ExecutablePath IS NOT NULL');
    for I := 0 to Processes.Count - 1 do begin
      Process := Processes.ItemIndex(I);
      Executable := Process.ExecutablePath;
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
  else if not ProcessesStopped then
    Result := 'Quit Sapiens4 Development and its backend, then retry. No running process will be stopped by Setup.';
end;

function InitializeUninstall: Boolean;
begin
  Result := OwnedDirectory and ProcessesStopped;
  if not Result then begin
    Log('Uninstall refused: missing installation identity, running process, or process-check failure.');
    if not UninstallSilent then
      MsgBox('Quit Sapiens4 Development and its backend, then retry uninstall. No running process will be stopped.', mbError, MB_OK);
  end;
end;
