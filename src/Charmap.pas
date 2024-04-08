unit CharMap;

{---------------------------------------------}
{ TCharMap Delphi/Lazarus component           }
{ (c)2002, 2003, 2024 Noniewicz.com           }
{ Jakub Noniewicz aka MoNsTeR/GDC             }
{---------------------------------------------}
{ History:                                    }
{ Version 1.00, update: 2003.08.27            }
{ Version 1.01, update: 2024.04.07, 08        }
{---------------------------------------------}

{TODO:
- ?
}

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses Classes, Controls,
     {$ifdef FPC}
     LMessages,
     {$else}
     Messages,
     Windows,
     {$endif}
     Forms, Graphics, StdCtrls,
     Grids, SysUtils;

{255-32=223}
{224/8 = 28}

const CharRows = 8;
      CharCols = 28;

type
  TCharMap = class(TCustomGrid)
  private
    FOnChange: TNotifyEvent;
    FSelectedChar: char;
    FSelectedCharCode: Byte;
    MyCells: array[0..CharCols-1, 0..CharRows-1] of char;
    function GetCellChar(ACol, ARow: Integer): char;
    procedure FillChars;
  protected
    procedure Change; dynamic;
    procedure Click; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    {$ifdef FPC}
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    {$else}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedChar: Char read FSelectedChar;
    property SelectedCharCode: Byte read FSelectedCharCode;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property GridLineWidth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

procedure Register;

implementation



procedure Register;
begin
  RegisterComponents('Monster', [TCharMap]);
end;

procedure TCharMap.FillChars;
var x, y: byte;
begin
  for x := 0 to CharCols-1 do
    for y := 0 to CharRows-1 do
      MyCells[x, y] := chr(32+x+y*CharCols);
end;

constructor TCharMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //defaults
  FixedCols := 0;
  FixedRows := 0;
  ColCount := CharCols;
  RowCount := CharRows;
  ScrollBars := ssNone;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected, goVertLine, goHorzLine];

  FSelectedChar := ' ';
  FSelectedCharCode := ord(FSelectedChar);
  FillChars;
  SelectCell(0, 0);
  Invalidate;
end;

procedure TCharMap.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCharMap.Click;
var TheCellChar: char;
begin
  inherited Click;
  TheCellChar := GetCellChar(Col, Row);
  if TheCellChar <> '' then
  begin
    FSelectedChar := TheCellChar;
    FSelectedCharCode := ord(TheCellChar);
  end;
  Change;
end;

procedure TCharMap.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var txt: string;
begin
  inherited; //2024 laz fix
  txt := GetCellChar(ACol, ARow);
  with ARect, Canvas do
    TextRect(ARect, Left + (Right - Left - TextWidth(txt)) div 2, Top + (Bottom - Top - TextHeight(txt)) div 2, txt)
end;

function TCharMap.GetCellChar(ACol, ARow: Integer): char;
begin
  Result := MyCells[ACol, ARow];
end;

function TCharMap.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  invalidate;
end;

{$ifdef FPC}
procedure TCharMap.WMSize(var Message: TLMSize);
{$else}
procedure TCharMap.WMSize(var Message: TWMSize);
{$endif}
var GridLines: Integer;
begin
  GridLines := 6 * GridLineWidth;
  DefaultColWidth := (Message.Width - GridLines) div CharCols;
  DefaultRowHeight := (Message.Height - GridLines) div CharRows;
end;

end.

