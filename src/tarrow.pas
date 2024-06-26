unit tarrow;

{---------------------------------------------}
{ TArrowImg Delphi/Lazarus component          }
{ (D3|D7|BDS2k6)                              }
{ Version 1.03                                }
{ Created: 2003.11.11                         }
{ E-mail:  jnoniewicz@gmail.com               }
{ WWW:     https://www.Noniewicz.com          }
{ Legal:   (c)2003-2017, 2024 Noniewicz.com,  }
{ Jakub Noniewicz aka MoNsTeR/GDC             }
{ Licence: BSD 2-Clause License               }
{---------------------------------------------}
{ Description:                                }
{ The TArrowImg component is simple visual    }
{ component that represents an arrow.         }
{ Parameters include:                         }
{ length, angle, color, line width            }
{ also len and angle of the 'pointy' part.    }
{---------------------------------------------}
{ History:                                    }
{ Version 1.00, update: 2003.11.11            }
{ Version 1.01, update: 2005.07.24            }
{ Version 1.02, update: 2017.10.29 GitHub     }
{ updated: 2020.07.24 TArrow -> TArrowImg     }
{ Version 1.03, update: 2024.04.08            }
{---------------------------------------------}

{todo:
- be-on-top prblm (?)
- init prblm (?)
}

{CHANGELOG:
v1.00-1.01:
- all base stuff
v1.02:
- minor code cleanup
v1.03:
- Lazarus/crossplatform
}

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
    {$ifndef FPC}
    Windows,
    {$else}
    LCLIntf,
    {$endif}
    Classes, Graphics, Controls, SysUtils;

type
  TArrowImg = class(TGraphicControl)
  private
    FStartPoint: TPoint;
    FEndPoint: TPoint;
    FAngle: single;
    FArrAngle: single;
    FLength: Integer;
    FArrLength: Integer;
    FLineWidth: Integer;
    FArrowColor: TColor;
    FTransparent: Boolean;
    procedure CWriteFAngle(PAngle: single);
    procedure CWriteFArrowColor(PArrowColor: TColor);
    procedure CWriteFLength(PLength: Integer);
    procedure CWriteFStartPointX(PStartPointX: Integer);
    procedure CWriteFStartPointY(PStartPointY: Integer);
    procedure CWriteFEndPointX(PEndPointX: Integer);
    procedure CWriteFEndPointY(PEndPointY: Integer);
    procedure CWriteFTransparent(PTransparent: boolean);
    procedure CWriteFArrLength(PArrLength: Integer);
    procedure CWriteFLineWidth(PLineWidth: Integer);
    procedure CWriteFArrAngle(PArrAngle: single);
    procedure Paint; Override;
    procedure A2C;
    procedure C2A;
  public
    procedure PaintToCanvas(Canvas: TCanvas);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Angle: single read FAngle write CWriteFAngle;
    property ArrowColor: TColor read FArrowColor write CWriteFArrowColor;
    property Length: integer read FLength write CWriteFLength;
    property StartX: integer read FStartPoint.x write CWriteFStartPointX;
    property StartY: integer read FStartPoint.y write CWriteFStartPointY;
    property EndX: integer read FEndPoint.x write CWriteFEndPointX;
    property EndY: integer read FEndPoint.y write CWriteFEndPointY;
    property Transparent: Boolean read FTransparent write CWriteFTransparent;
    property ArrLength: Integer read FArrLength write CWriteFArrLength;
    property LineWidth: Integer read FLineWidth write CWriteFLineWidth;
    property ArrAngle: single read FArrAngle write CWriteFArrAngle;

    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height;
    property Hint;
    property Left;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Tag;
    property Top;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnMouseMove; 
    property OnMouseDown;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;


procedure Register;


implementation

{.$R *.dcr}


procedure Register;
begin
  RegisterComponents('Monster', [TArrowImg]);
end;

//------------------------------

procedure TArrowImg.A2C;
begin
  FEndPoint.x := FStartPoint.x + round(Flength*cos(FAngle/180*pi));
  FEndPoint.y := FStartPoint.y + round(Flength*sin(FAngle/180*pi));
end; { A2C }

procedure TArrowImg.C2A;
var dx, dy: Integer;
begin
  dx := FEndPoint.x-FStartPoint.x;
  dy := FEndPoint.y-FStartPoint.y;
  if dy <> 0 then
    FAngle := ArcTan(dx/dy)/pi*180
  else
    FAngle := 0;
  FLength := Round(sqrt( sqr(dy) + sqr(dx) ));
end; { C2A }

procedure TArrowImg.Paint;
begin
  PaintToCanvas(self.Canvas);
end; { Paint }

procedure TArrowImg.PaintToCanvas(Canvas: TCanvas);
var x, y: Integer;
begin
  Canvas.Brush.Color := color;
  Canvas.Pen.Color := FArrowColor;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := FLineWidth;
  if not FTransparent then
    Canvas.FillRect(Rect(0, 0, width, height));
  Canvas.MoveTo(FStartPoint.x, FStartPoint.y);
  Canvas.LineTo(FEndPoint.x, FEndPoint.y);
  x := FEndPoint.x - round(FArrLength*cos((FAngle+FArrAngle)/180*pi));
  y := FEndPoint.y - round(FArrLength*sin((FAngle+FArrAngle)/180*pi));
  Canvas.LineTo(x, y);
  Canvas.MoveTo(FEndPoint.x, FEndPoint.y);
  x := FEndPoint.x - round(FArrLength*cos((FAngle-FArrAngle)/180*pi));
  y := FEndPoint.y - round(FArrLength*sin((FAngle-FArrAngle)/180*pi));
  Canvas.LineTo(x, y);
end; { PaintToCanvas }

constructor TArrowImg.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 100;
  color := clWhite;

  FLineWidth := 1;
  FTransparent := true; 
  FArrLength := 10;
  FArrAngle := 45.0;

  FArrowColor := clRed;
  FStartPoint := Point(0, 0);
  FEndPoint := Point(Width div 2, Height div 2);
  C2A;
end; { Create }

destructor TArrowImg.Destroy;
begin
  inherited;
end; { Destroy }

procedure TArrowImg.CWriteFAngle(PAngle: single);
begin
  if FAngle <> PAngle then
  begin
    FAngle := PAngle;
    A2C;
    Refresh;
  end;
end; { CWriteFAngle }

procedure TArrowImg.CWriteFArrowColor(PArrowColor: TColor);
begin
  if FArrowColor <> PArrowColor then
  begin
    FArrowColor := PArrowColor;
    Refresh;
  end;
end; { CWriteFArrowColor }

procedure TArrowImg.CWriteFLength(PLength: Integer);
begin
  if FLength <> PLength then
  begin
    FLength := PLength;
    A2C;
    Refresh;
  end;
end; { CWriteFLength }

procedure TArrowImg.CWriteFStartPointX(PStartPointX: Integer);
begin
  if FStartPoint.x <> PStartPointX then
  begin
    FStartPoint.x := PStartPointX;
    C2A;
    Refresh;
  end;
end; { CWriteFStartPointX }

procedure TArrowImg.CWriteFStartPointY(PStartPointY: Integer);
begin
  if FStartPoint.y <> PStartPointY then
  begin
    FStartPoint.y := PStartPointY;
    C2A;
    Refresh;
  end;
end; { CWriteFStartPointY }

procedure TArrowImg.CWriteFEndPointX(PEndPointX: Integer);
begin
  if FEndPoint.x <> PEndPointX then
  begin
    FEndPoint.x := PEndPointX;
    C2A;
    Refresh;
  end;
end; { CWriteFEndPointX }

procedure TArrowImg.CWriteFEndPointY(PEndPointY: Integer);
begin
  if FEndPoint.y <> PEndPointY then
  begin
    FEndPoint.y := PEndPointY;
    C2A;
    Refresh;
  end;
end; { CWriteFEndPointY }

procedure TArrowImg.CWriteFTransparent(PTransparent: boolean);
begin
  if FTransparent <> PTransparent then
  begin
    FTransparent := PTransparent;
    Refresh;
  end;
end; { CWriteFTransparent }

procedure TArrowImg.CWriteFArrLength(PArrLength: Integer);
begin
  if FArrLength <> PArrLength then
  begin
    FArrLength := PArrLength;
    Refresh;
  end;
end; { CWriteFArrLength }

procedure TArrowImg.CWriteFLineWidth(PLineWidth: Integer);
begin
  if FLineWidth <> PLineWidth then
  begin
    FLineWidth := PLineWidth;
    Refresh;
  end;
end; { CWriteFLineWidth }

procedure TArrowImg.CWriteFArrAngle(PArrAngle: single);
begin
  if FArrAngle <> PArrAngle then
  begin
    FArrAngle := PArrAngle;
    Refresh;
  end;
end; { CWriteFArrAngle }

end.

