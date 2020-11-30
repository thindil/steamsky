; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Steam Sky"
#define MyAppVersion "5.6"
#define MyAppPublisher "Bartek thindil Jasicki"
#define MyAppURL "https://thindil.itch.io/steam-sky"
#define MyAppExeName "bin\steamsky.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{812DBD30-4EEE-4554-BBCD-6CC66F2AB480}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={userpf}\{#MyAppName}
DisableProgramGroupPage=yes
OutputBaseFilename=steamsky-{#MyAppVersion}-64-bit
Compression=lzma
SolidCompression=yes
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "Z:\home\thindil\steamsky\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Z:\home\thindil\steamsky\share\fonts\Amarante-Regular.ttf"; DestDir: "{fonts}"; FontInstall: "Amarante"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "Z:\home\thindil\steamsky\share\fonts\Font Awesome 5 Free-Solid-900.otf"; DestDir: "{fonts}"; FontInstall: "Font Awesome 5 Free Solid"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "Z:\home\thindil\steamsky\share\fonts\Hack Bold Nerd Font Complete Mono Windows Compatible.ttf"; DestDir: "{fonts}"; FontInstall: "Hack NF"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "Z:\home\thindil\steamsky\share\fonts\Roboto-Regular.ttf"; DestDir: "{fonts}"; FontInstall: "Roboto"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "Z:\home\thindil\steamsky\share\fonts\Rye-Regular.ttf"; DestDir: "{fonts}"; FontInstall: "Rye"; Flags: onlyifdoesntexist uninsneveruninstall
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
; NOTE2: If you build installer by self, change Source directory to proper value

[Icons]
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

