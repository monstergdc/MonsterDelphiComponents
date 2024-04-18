unit charMap;

{---------------------------------------------}
{ TCharMap Delphi/Lazarus component           }
{ (c)2002, 2003, 2024 Noniewicz.com           }
{ Jakub Noniewicz aka MoNsTeR/GDC             }
{---------------------------------------------}
{ History:                                    }
{ Version 1.00, update: 2003.08.27            }
{ Version 1.01, update: 2024.04.07, 08,       }
{ Version 1.02, update: 2024.04.18            }
{---------------------------------------------}

{TODO:
- ?
}

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses Classes, SysUtils, Controls,
     {$ifdef FPC}
     LConvEncoding,
     LMessages,
     {$else}
     Messages,
     Windows,
     {$endif}
     Forms, Graphics, StdCtrls, Grids;


const
  //255-32=223 -> 224
  //CharCols = 28; //8*28 = 224
  //CharRows = 8;

  CharCols = 16;
  CharRows = (256-32) div CharCols; //224/16 = 14


type
  TCharMap = class(TCustomGrid)
  private
    FOnChange: TNotifyEvent;
    FUseHeader: boolean;
    FSelectedChar: string;
    FSelectedCharCode: Byte;
    MyCells: array[0..CharCols-1, 0..CharRows-1] of ansichar;
    function GetCellChar(ACol, ARow: Integer): ansichar;
    function GetCellCharStr(ACol, ARow: Integer): string;
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
    procedure FWriteUseHeader(PUseHeader: boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UseHeader: boolean read FUseHeader write FWriteUseHeader;
    property SelectedChar: string read FSelectedChar;
    property SelectedCharCode: byte read FSelectedCharCode;

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
  FWriteUseHeader(true);
  ScrollBars := ssNone;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected, goVertLine, goHorzLine];

  FillChars;
  FSelectedChar := ' ';
  FSelectedCharCode := ord(FSelectedChar[1]);
  SelectCell(0, 0);
  Invalidate;
end;

procedure TCharMap.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCharMap.Click;
var TheCellChar: ansichar;
begin
  inherited Click;
  TheCellChar := GetCellChar(Col, Row);
  if TheCellChar <> #0 then
  begin
    FSelectedChar := GetCellCharStr(col, Row);
    FSelectedCharCode := ord(TheCellChar);
  end;
  Change;
end;

procedure TCharMap.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var txt: string;
    chset: integer;
begin
  inherited; //2024 laz fix

  chset := self.Font.CharSet;

  if FUseHeader and ((ACol = 0) or (ARow = 0)) then  //2024 head opt
  begin
    if (ACol = 0) and (ARow = 0) then exit;
    if ACol = 0 then txt := inttohex((ARow-1)*16+32, 2);
    if ARow = 0 then txt := inttohex(ACol-1, 2);
  end
  else
  begin
    txt := GetCellCharStr(ACol, ARow);
  end;

  if FUseHeader and ((ACol = 0) or (ARow = 0)) then
  begin
    self.Font.CharSet := 0;
  end;

  with ARect, Canvas do
  begin
    TextRect(ARect, Left + (Right - Left - TextWidth(txt)) div 2, Top + (Bottom - Top - TextHeight(txt)) div 2, txt)
  end;

  self.Font.CharSet := chset;
end;

function TCharMap.GetCellChar(ACol, ARow: Integer): ansiChar;
begin
  //todo: chk range
  if FUseHeader then
    result := MyCells[ACol-1, ARow-1]
  else
    result := MyCells[ACol, ARow];
end;

function TCharMap.GetCellCharStr(ACol, ARow: Integer): string;
var Encoded: boolean;
begin
  //2024 laz utf8 issue fix
  {$ifdef FPC}
  result := ConvertEncodingToUTF8(GetCellChar(ACol, ARow), GetDefaultTextEncoding, Encoded);
  {$else}
  result := GetCellChar(ACol, ARow);
  {$endif}
end;

function TCharMap.SelectCell(ACol, ARow: Longint): Boolean;
begin
  result := inherited SelectCell(ACol, ARow);
  invalidate;
end;

{$ifdef FPC}
procedure TCharMap.WMSize(var Message: TLMSize);
{$else}
procedure TCharMap.WMSize(var Message: TWMSize);
{$endif}

  function b2i: integer;
  begin
    if FUseHeader then result := 1 else result := 0;
  end;

var GridLines: Integer;
begin
  GridLines := 6 * GridLineWidth;
  DefaultColWidth := (Message.Width - GridLines) div (CharCols + b2i);
  DefaultRowHeight := (Message.Height - GridLines) div (CharRows + b2i);
end;

procedure TCharMap.FWriteUseHeader(PUseHeader: boolean);
begin
  if FUseHeader <> PUseHeader then
  begin
    FUseHeader := PUseHeader;

    if FUseHeader then
    begin
      FixedCols := 1;
      FixedRows := 1;
      ColCount := CharCols+1;
      RowCount := CharRows+1;
    end
    else
    begin
      FixedCols := 0;
      FixedRows := 0;
      ColCount := CharCols;
      RowCount := CharRows;
    end;

    invalidate;
  end;
end;

end.

