unit tarrow;

{$mode Delphi}

{---------------------------------------------}
{ TArrow Delphi component (D3|D7|BDS2k6)      }
{ Version 1.02                                }
{ Created: 2003.11.11                         }
{ E-mail:  monster@Noniewicz.com              }
{ WWW:     http://www.Noniewicz.com           }
{ Legal:   (c)2003-2017 Noniewicz.com,        }
{ Jakub Noniewicz aka MoNsTeR/GDC             }
{ Licence: BSD 2-Clause License               }
{---------------------------------------------}
{ Description:                                }
{ The TArrow component is simple visual       }
{ component that represents an arrow.         }
{ Parameters include:                         }
{ length, angle, color, line width            }
{ also len and angle of the 'pointy' part.    }
{---------------------------------------------}
{ History:                                    }
{ Version 1.00, update: 2003.11.11            }
{ Version 1.01, update: 2005.07.24            }
{ Version 1.02, update: 2017.10.29 GitHub     }
{ updated: 2020724                            }
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
}

interface

uses Windows, Classes, Graphics, Controls, SysUtils;

type
  TArrow = class(TGraphicControl)
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

{$R *.DCR}


procedure Register;
begin
  RegisterComponents('Monster', [TArrow]);
end;

//------------------------------

procedure TArrow.A2C;
begin
  FEndPoint.x := FStartPoint.x + round(Flength*cos(FAngle/180*pi));
  FEndPoint.y := FStartPoint.y + round(Flength*sin(FAngle/180*pi));
end; { A2C }

procedure TArrow.C2A;
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

procedure TArrow.Paint;
begin
  PaintToCanvas(self.Canvas);
end; { Paint }

procedure TArrow.PaintToCanvas(Canvas: TCanvas);
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

constructor TArrow.Create(AOwner: TComponent);
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

destructor TArrow.Destroy;
begin
  inherited;
end; { Destroy }

procedure TArrow.CWriteFAngle(PAngle: single);
begin
  if FAngle <> PAngle then
  begin
    FAngle := PAngle;
    A2C;
    Refresh;
  end;
end; { CWriteFAngle }

procedure TArrow.CWriteFArrowColor(PArrowColor: TColor);
begin
  if FArrowColor <> PArrowColor then
  begin
    FArrowColor := PArrowColor;
    Refresh;
  end;
end; { CWriteFArrowColor }

procedure TArrow.CWriteFLength(PLength: Integer);
begin
  if FLength <> PLength then
  begin
    FLength := PLength;
    A2C;
    Refresh;
  end;
end; { CWriteFLength }

procedure TArrow.CWriteFStartPointX(PStartPointX: Integer);
begin
  if FStartPoint.x <> PStartPointX then
  begin
    FStartPoint.x := PStartPointX;
    C2A;
    Refresh;
  end;
end; { CWriteFStartPointX }

procedure TArrow.CWriteFStartPointY(PStartPointY: Integer);
begin
  if FStartPoint.y <> PStartPointY then
  begin
    FStartPoint.y := PStartPointY;
    C2A;
    Refresh;
  end;
end; { CWriteFStartPointY }

procedure TArrow.CWriteFEndPointX(PEndPointX: Integer);
begin
  if FEndPoint.x <> PEndPointX then
  begin
    FEndPoint.x := PEndPointX;
    C2A;
    Refresh;
  end;
end; { CWriteFEndPointX }

procedure TArrow.CWriteFEndPointY(PEndPointY: Integer);
begin
  if FEndPoint.y <> PEndPointY then
  begin
    FEndPoint.y := PEndPointY;
    C2A;
    Refresh;
  end;
end; { CWriteFEndPointY }

procedure TArrow.CWriteFTransparent(PTransparent: boolean);
begin
  if FTransparent <> PTransparent then
  begin
    FTransparent := PTransparent;
    Refresh;
  end;
end; { CWriteFTransparent }

procedure TArrow.CWriteFArrLength(PArrLength: Integer);
begin
  if FArrLength <> PArrLength then
  begin
    FArrLength := PArrLength;
    Refresh;
  end;
end; { CWriteFArrLength }

procedure TArrow.CWriteFLineWidth(PLineWidth: Integer);
begin
  if FLineWidth <> PLineWidth then
  begin
    FLineWidth := PLineWidth;
    Refresh;
  end;
end; { CWriteFLineWidth }

procedure TArrow.CWriteFArrAngle(PArrAngle: single);
begin
  if FArrAngle <> PArrAngle then
  begin
    FArrAngle := PArrAngle;
    Refresh;
  end;
end; { CWriteFArrAngle }

end.

