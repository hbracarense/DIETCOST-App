[Setup]
AppName=DIETCOST
AppVersion=1.0.0
DefaultDirName={localappdata}\DIETCOST
DefaultGroupName=DIETCOST
OutputBaseFilename=DIETCOST_Setup
Compression=lzma
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64

[Files]
Source: "C:\Users\hbrac\OneDrive\Documentos\Nutrição\Produtos\app\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs

[Icons]
Name: "{group}\DIETCOST"; Filename: "{app}\DIETCOST.exe"; WorkingDir: "{app}"
Name: "{commondesktop}\DIETCOST"; Filename: "{app}\DIETCOST.exe"; WorkingDir: "{app}"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Criar atalho na área de trabalho"; GroupDescription: "Atalhos:"; Flags: unchecked

[Run]
Filename: "{app}\DIETCOST.exe"; WorkingDir: "{app}"; Description: "Abrir DIETCOST"; Flags: nowait postinstall skipifsilent