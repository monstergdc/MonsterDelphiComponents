unit wwwlabel;

{----------------------------------------------------}
{ TWWWLabel component for Delphi 3-7 or higher       }
{ Version 1.42                                       }
{ Created: 2003.08.23                                }
{ E-mail:  jnoniewicz@gmail.com                      }
{ WWW:     https://www.Noniewicz.com                 }
{ Legal:   (c)2003-2017, 2022, 2024 MoNsTeR/GDC, Jakub Noniewicz }
{ Licence: BSD 2-Clause License                      }
{----------------------------------------------------}
{ Description:                                       }
{ The TWWWLabel component is simple clickable label  }
{ that opens default web browser with given URL      }
{----------------------------------------------------}
{ History:                                           }
{ Version 1.00, update: 2003.08.23                   }
{ Version 1.10, update: 2004.01.30                   }
{ Version 1.20, update: 2006.05.17                   }
{ Version 1.30, update: 2006.05.18                   }
{ Version 1.40, update: 2017.10.24 GitHub            }
{ Version 1.41, update: 2022.07.24 Lazarus           }
{ Version 1.42, update: 2024.04.08                   }
{----------------------------------------------------}

{CHANGELOG:
# v1.00
- base stuff
# v1.10
- added "" and escape function
# v1.20
- fixed registry access rights (now readonly)
# v1.30
- fixed registry readonly to old code for D3
# v1.40 / v1.41
- ?
# v1.42
- make compilable crossplatform (but will work only on Windows)
}

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
     {$ifdef windows}
     Windows, Registry,
     {$endif}
     Classes, Graphics, Forms, Controls, StdCtrls, SysUtils;
     //dialogs

type
  TWWWLabel = class(TCustomLabel)
  private
    FURL: String;
    procedure ClickGoToWWW(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property URL: String read FURL write FURL;

    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Height;
    property Hint;
    property Layout;
    property Left;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Tag;
    property Top;
    property Transparent;
    property Visible;
    property Width;
    property WordWrap;

    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;


function MyExec(s: string): boolean;
procedure GoToWWW(URL: String);
function escape(s: string): string;


procedure Register;


implementation

{$ifdef FPC}
  uses LResources;
{$endif}



procedure Register;
begin
  RegisterComponents('Monster', [TWWWLabel]);
end;


//exec app
function MyExec(s: string): boolean;
var ret: boolean;
begin
  {$ifdef windows}
  if s <> '' then
  begin
    if WinExec(PChar(s), SW_SHOWNORMAL) < 32 then
      ret := false
    else
      ret := true;
  end
  else
    ret := false;
  result := ret;
  {$else}
  result := false;
  {$endif}
end; { MyExec }


function escape(s: string): string;
const hex='0123456789ABCDEF';
var i, h: integer;
begin
  Result := '';
  for i := 1 to length(s) do
  case s[i] of
    ' ': Result := Result + '+';
    '0'..'9',
    'A'..'Z',
    'a'..'z': Result := Result + s[i];
    else
    begin
      h := ord(s[i]);
      Result := Result + '%'+hex[1+(h shr 4)]+hex[1+(h and 15)];
    end;
  end;
end;

procedure GoToWWW(URL: String);
var www: String;
    key: TRegistry;
    stat: boolean;
begin
  key := TRegistry.create;
  www := '';
  try
    key.RootKey := HKEY_CLASSES_ROOT;

    {$IFNDEF VER100} { Borland Delphi 3.0 }
    key.Access := KEY_READ;
    {$ENDIF}

    {$IFNDEF VER100} { Borland Delphi 3.0 }
    stat := key.OpenKeyReadOnly('\http\shell\open\command');
    {$ELSE}
    stat := key.OpenKey('\http\shell\open\command', false);
    {$ENDIF}

    if not stat then
    begin
      key.CloseKey;
      key.RootKey := HKEY_LOCAL_MACHINE;
      {$IFNDEF VER100} { Borland Delphi 3.0 }
      stat := key.OpenKeyReadOnly('\SOFTWARE\Classes\HTTP\shell\open\command');
      {$ELSE}
      stat := key.OpenKey('\SOFTWARE\Classes\HTTP\shell\open\command', false);
      {$ENDIF}
    end;

    if stat and key.ValueExists('') then www := key.ReadString('');
  finally
    key.CloseKey;
  end;
  key.Free;

  if strpos(PChar(UpperCase(www)+#0),'"%1"') <> nil then
    Delete(www, pos('"%1"',www), length(www)-pos('"%1"',www)+1);

  if not(stat) or (www = '') or (URL = '') then {ignore}
  else
    MyExec(www+' "'+URL+'"');
end; { GoToWWW }


procedure TWWWLabel.ClickGoToWWW(Sender: TObject);
begin
  GoToWWW(FURL);
end; { ClickGoToWWW }

constructor TWWWLabel.Create(AOwner: TComponent);
begin
  inherited;
  FURL := '';
  Font.Style := Font.Style + [fsUnderline];
  Font.Color := clBlue;
  OnClick := ClickGoToWWW;
  cursor := crHandPoint;
end; { Create }

destructor TWWWLabel.Destroy;
begin
  inherited;
end; { Destroy }

initialization

{$ifdef FPC}
  {$I monsterpackagelazarus_icons.lrs}
{$endif}

end.

