unit recent;

{----------------------------------------------}
{ TRecent Delphi/Lazarus component             }
{ (c)2005, 2008, 2022, 2024 MoNsTeR/GDC, Jakub Noniewicz }
{ monster@noniewicz.com                        }
{----------------------------------------------}
{ History:                                     }
{ Version 1.00, update: 2005.06.15             }
{ Version 1.0a, update: 2008.08.03             }
{ Version 1.01, update: 2008.08.05             }
{ Version 1.02, update: 2008.08.07             }
{ Version 1.03, update: 2022.07.24 GitHub      }
{ Version 1.04, update: 2024.04.08             }
{----------------------------------------------}

{todo:
- auto reg/ini save/load
- separate by ext
}

{
history:
v1.01:
- FIXED: there is some crash bug! [when menu assigned in design time]
v1.02:
- more fixes...
v1.03:
- ?
v1.04:
- code cleanup, crosscompilable
}

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  //Windows,
  Classes, Forms, Controls, Menus,
  SysUtils, StdCtrls, ExtCtrls, ComCtrls,
  iniFiles;

type
  TRecent = class(TComponent)
  private
    FFiles: TStringList;
    FMenu: TMenuItem;
    FMaxFiles: integer;
    FMaxLen: integer;
    FNoneCaption: string;
    FOnClick: TNotifyEvent;
    procedure CWriteFMenu(PMenu: TMenuItem);
    procedure CWriteFMaxFiles(PMaxFiles: integer);
    procedure CWriteFNoneCaption(PNoneCaption: string);
    function CReadFFilesText: string;
    procedure CWriteFFilesText(PFilesText: string);
    procedure CWriteFMaxLen(PMaxLen: integer);
    function FormatTitle(title: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertLastFile(filename: string);
    procedure Clear;
    procedure Refresh;
    function GetFileNameByMenuItemTag(i: integer): string;
  published
    property Menu: TMenuItem read FMenu write CWriteFMenu;
    property MaxFiles: integer read FMaxFiles write CWriteFMaxFiles;
    property MaxLen: integer read FMaxLen write CWriteFMaxLen;
    property NoneCaption: string read FNoneCaption write CWriteFNoneCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property FilesAsText: string read CReadFFilesText write CWriteFFilesText;  
  end;


procedure Register;


implementation                       


procedure Register;
begin
  RegisterComponents('Monster', [TRecent]);
end;



constructor TRecent.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  FFiles.Sorted := false;
  FMenu := nil;
  FOnClick := nil;
  FMaxFiles := 10;
  FMaxLen := 40;
  FNoneCaption := '{none}';
end; { Create }

destructor TRecent.Destroy;
begin
  FFiles.Free;
  inherited;
end; { Destroy }

procedure TRecent.Clear;
begin
  FFiles.Clear;
  self.Refresh;
end;

//---

procedure TRecent.CWriteFMenu(PMenu: TMenuItem);
begin
  FMenu := PMenu;
  self.Refresh;  
end;

procedure TRecent.CWriteFMaxFiles(PMaxFiles: integer);
begin
  if PMaxFiles > 0 then   
    FMaxFiles := PMaxFiles
  else
    FMaxFiles := 1;
  self.Refresh;
end;

procedure TRecent.CWriteFMaxLen(PMaxLen: integer);
begin
  if PMaxLen > 7 then //like at least 'c:\' + '.txt'
    FMaxLen := PMaxLen
  else
    FMaxLen := 7;
  self.Refresh;
end;

procedure TRecent.CWriteFNoneCaption(PNoneCaption: string);
begin
  FNoneCaption := PNoneCaption;
  self.Refresh;
end;

function TRecent.CReadFFilesText: string;
begin
  Result := FFiles.Text;
end;

procedure TRecent.CWriteFFilesText(PFilesText: string);
begin
  FFiles.Text := PFilesText;
  self.Refresh;
end;

//---

function TRecent.FormatTitle(title: string): string;
var sd, sf, sp, spok: string;
    i: integer;
begin
  if (length(title) <= FMaxLen) then
  begin
    result := title
  end
  else
  begin
    sd := ExtractFileDrive(title);
    sp := ExtractFilePath(title);
    sf := ExtractFileName(title);
    delete(sp, 1, length(sd));
    spok := Copy(sp, 1, FMaxLen-length(sd)-length(sf));
    while (length(spok) > 0) and (spok[length(spok)] <> '\') do
      delete(spok, length(spok), 1);

    result := sd + spok + '...\' + sf;
  end;
end; { FormatTitle }

procedure TRecent.Refresh;
var i: integer;
    item: TMenuItem;
    nonone: boolean;
begin
  if not assigned(FMenu) then exit;

  if csLoading in self.ComponentState then exit;
  if csDestroying in self.ComponentState then exit;
  if csReading in self.ComponentState then exit;

  nonone := true;

  //remove all
  for i := FMenu.Count-1 downto 0 do
  try
    if pos(self.Name+'Item', FMenu.Items[i].Name)>0 then
      FMenu.Items[i].Free;
  except
  end;

  //create menu items
  for i := 0 to FFiles.Count-1 do
    if trim(FFiles[i]) <> '' then
    begin
      item := TMenuItem.Create(FMenu);
      item.Name := self.Name+'Item'+ intToStr(i+1);
      item.Enabled := true;
      item.Visible := true;
      item.Tag := i;
      item.Caption := '&' + chr(i+65) + ' ' + FormatTitle(FFiles[i]);
      item.OnClick := FOnClick;
      FMenu.Add(item);
      nonone := false;
    end;

  //only empty item
  if nonone then {none}
  begin
    item := TMenuItem.Create(FMenu);
    item.Name := self.Name+'ItemNone';
    item.Caption := FNoneCaption;
    item.Enabled := false;
    item.Visible := true;
    item.OnClick := nil;
    item.Tag := 0;
    FMenu.Add(item);
  end;
end; { Refresh }

procedure TRecent.InsertLastFile(filename: string);
var newFile: boolean;
    i, p: integer;
begin
  newFile := true;
  p := 0;
  for i := 0 to FFiles.Count-1 do
    if UpperCase(FFiles[i]) = UpperCase(filename) then
    begin
      newFile := false;
      p := i;
    end;

  if not newFile then FFiles.Delete(p);     ///hmm only one?

  FFiles.Insert(0, filename);

  while FFiles.Count > FMaxFiles do
    FFiles.Delete(FFiles.Count-1);

  self.Refresh;
end; { InsertLastFile }

function TRecent.GetFileNameByMenuItemTag(i: integer): string;
begin
  if (i < 0) or (i > FFiles.Count-1) then
    Result := ''
  else
    Result := FFiles[i];
end;


end.

