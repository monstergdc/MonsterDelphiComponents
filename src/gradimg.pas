unit gradimg;

{-------------------------------------------------}
{ TGradientImage Delphi component (D3|D7|BDS2k6)  }
{ Version 1.08                                    }
{ Created: 2003.12.10                             }
{ E-mail:  monster@Noniewicz.com                  }
{ WWW:     http://www.Noniewicz.com               }
{ Legal:   (c)2003-2017 Noniewicz.com,            }
{ Jakub Noniewicz aka MoNsTeR/GDC                 }
{ Licence: BSD 2-Clause License                   }
{-------------------------------------------------}
{ Description:                                    }
{ The TGradientImage component is simple visual   }
{ component that represents various types of      }
{ gradient. It can also draw color map            }
{ and paint [RBG|HSL]=f(x, y) functions,          }
{ Rendering to external canvas is possible.       }
{-------------------------------------------------}
{ History:                                        }
{ Version 1.00, update: 2003.12.10                }
{ Version 1.01, update: 2005.06.15                }
{ Version 1.02, update: 2005.11.01-03             }
{ Version 1.03, update: 2005.11.11                }
{ Version 1.04, update: 2005.11.15                }
{ Version 1.04, update: 2006.04.09                }
{ Version 1.05, update: 2007.03.09                }
{ Version 1.06, update: 2008.07.07                }
{ Version 1.07, update: 2008.08.06-08             }
{ Version 1.08, update: 2012.02.17-18, 25         }
{ Version 1.08, update: 2014.07.02                }
{ Version 1.09, update: 2017.10.24 GitHub         }
{-------------------------------------------------}

{todo:
- repeat gradient
- any angle - finish - fill full canvas
- in round gradient: 3-color, ellipse, center
- any cnt of intermediate colors + any step/speed between
}

{CHANGELOG:
v1.01:
- dlft size fix added
- horizontal property added
- twoonly property added
v1.02:
- (lame) AssignToBitmap added
v1.03:
- added events: OnMouseDown, OnMouseMove, OnMouseUp
- colormap mode
- GetColor function
v1.04:
- works also on external canvas
- PaintRGBFunction added
v1.05:
- can rotate gradient (not fully working)
v1.06:
- round gradient
v1.07:
- PaintGradient-> PaintGradientRect
- +small fixes
- OnDblClick
- Assign method
v1.08:
- extra paint callback
- GetColorVirtual
- minor fix
- PaintRGBFunction2
- PaintHSLFunction2
v1.09:
- minor fixes, code sync and cleanup
}

interface

uses Windows, Classes, Graphics, Forms, Controls, extctrls,
     messages, dialogs;

type
  TRGBFunction = function(x, y: integer): Tcolor;
  TGRadientExtraPaintCallback = procedure(const canvas: TCanvas; const Width, Height: integer) of object;

  TGradientImage = class(TGraphicControl)
  private
    FColorStart: TColor;
    FColorMid: TColor;
    FColorEnd: TColor;
    FTwoOnly: boolean;
    FHorizontal: boolean;
    FColorMap: boolean;
    FAngle: double;
    FRound: boolean;
    sinus, cosin: double;
    md: TPoint;
    procedure Paint; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CWriteFColorStart(PColorStart: TColor);
    procedure CWriteFColorMid(PColorMid: TColor);
    procedure CWriteFColorEnd(PColorEnd: TColor);
    procedure CWriteFTwoOnly(PTwoOnly: boolean);
    procedure CWriteFHorizontal(PHorizontal: boolean);
    procedure CWriteFColorMap(PColorMap: boolean);
    procedure CWriteFAngle(PAngle: double);
    procedure CWriteFRound(PRound: boolean);
    procedure SubFill(ExtCanvas: TCanvas; R: TRect);
  public
    RGBfunction: TRGBFunction;
    ExtraPainter: TGRadientExtraPaintCallback;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignToBitmap(var bmp: TBitmap);
    function GetColor(x, y: integer): TColor;
    function GetColorVirtual(w, x: integer): TColor;
    procedure GenColorVirtual(w: integer; var arr: array of TColor);
    procedure PaintRGBFunction(ExtCanvas: TCanvas; w, h, x0, y0: integer);
    procedure PaintHSLFunction(ExtCanvas: TCanvas; w, h, x0, y0: integer);
    procedure PaintRGBFunction2(ExtCanvas: TCanvas; w, h, x0, y0, gw, gh: integer);
    procedure PaintHSLFunction2(ExtCanvas: TCanvas; w, h, x0, y0, gw, gh: integer);
    procedure PaintColorMap(ExtCanvas: TCanvas; w, h: integer);
    procedure PaintGradient(ExtCanvas: TCanvas; w, h: integer);
    procedure PaintGradientRect(ExtCanvas: TCanvas; x0, y0, x1, y1: integer);    
  published
    property ColorStart: TColor read FColorStart write CWriteFColorStart;
    property ColorMid: TColor read FColorMid write CWriteFColorMid;
    property ColorEnd: TColor read FColorEnd write CWriteFColorEnd;
    property TwoOnly: boolean read FTwoOnly write CWriteFTwoOnly;
    property Horizontal: boolean read FHorizontal write CWriteFHorizontal;
    property AsColorMap: boolean read FColorMap write CWriteFColorMap;
    property Angle: double read FAngle write CWriteFAngle;
    property IsRound: boolean read FRound write CWriteFRound;

    property ShowHint;
    property Hint;
    property Align;
    property Name;
    property Visible;
    property Tag;
    property Enabled;
    property Height;
    property Left;
    property Top; 
    property Width;
    property Cursor;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
  end;


procedure Register;


implementation



procedure Register;
begin
  RegisterComponents('Monster', [TGradientImage]);
end;


//--- RGB <-> HSL

procedure HSL2RGB(Hue1, Sat1, Light1: byte; var Red, Green, Blue: byte);
var H, S, L, R, G, B, m1, m2: double;

  function Value(n1, n2, h: double): double;
  var hue, v: double;
  begin
    hue := h;
    v := 9999;  //?
    If hue > 360 then hue := hue - 360;
    If hue < 0   then hue := hue + 360;
    If hue < 60 then v := n1 + (n2-n1) * hue/60;
    If (hue >= 60) and (hue < 180) then v := n2;
    If (hue >= 180) and (hue < 240) then v := n1 + (n2-n1) * (240-hue) / 60;
    If (hue >= 240) then v := n1;
    result := v;
  end;
begin
  H := Hue1*360/255; S := Sat1/255; L := Light1/255;
  if S = 0 then
  begin
    Red := Round(255*L);
    Green := Round(255*L);
    Blue := Round(255*L);
    exit;
  end;
  if L <= 0.5 then m2 := L*(1+S) else m2 := L+S-L*S;
  m1 := 2*L-m2;
  R := Value(m1,m2,H+128);
  G := Value(m1,m2,H);
  B := Value(m1,m2,H-128);

  Red := Round(255*R);
  Green := Round(255*G);
  Blue := Round(255*B);
end; { HSL2RGB }

//---

constructor TGradientImage.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 21;
  FColorStart := clRed;
  FColorMid := clYellow;
  FColorEnd := clLime;
  FTwoOnly := false;
  FHorizontal := false;
  FColorMap := false;
  RGBfunction := nil;
  FAngle := 0;
  FRound := false;
  ExtraPainter := nil;
end; { Create }

destructor TGradientImage.Destroy;
begin
  inherited;
end; { Destroy }

procedure TGradientImage.Assign(Source: TPersistent);
var src: TGradientImage;
begin
  if assigned(Source) then
  if Source is TGradientImage then
  begin
    src := Source as TGradientImage;

    ColorStart := src.ColorStart;
    ColorMid := src.ColorMid;
    ColorEnd := src.ColorEnd;
    TwoOnly := src.TwoOnly;
    Horizontal := src.Horizontal;
    AsColorMap := src.AsColorMap;
    Angle := src.Angle;
    IsRound := src.IsRound;

    ShowHint := src.ShowHint;
    Hint := src.Hint;
    Align := src.Align;
    Name := src.Name;
    Visible := src.Visible;
    Tag := src.Tag;
    Enabled := src.Enabled;
    Height := src.Height;
    Left := src.Left;
    Top := src.Top;
    Width := src.Width;
    Cursor := src.Cursor;

    OnMouseDown := src.OnMouseDown;
    OnMouseMove := src.OnMouseMove;
    OnMouseUp := src.OnMouseUp;
    OnDblClick := src.OnDblClick;
  end;
end; { Assign }

function TGradientImage.GetColor(x, y: integer): TColor;
begin
  Result := self.Canvas.Pixels[x, y];
end; { GetColor }

function TGradientImage.GetColorVirtual(w, x: integer): TColor;
var bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Width := w;
  bmp.Height := 1;
  bmp.PixelFormat := pf24bit;
  PaintGradient(bmp.Canvas, w, 1);
  result := bmp.Canvas.Pixels[x, 0];
  bmp.Free;
end; { GetColorVirtual }

procedure TGradientImage.GenColorVirtual(w: integer; var arr: array of TColor);
var bmp: TBitmap;
    i: integer;
begin
  bmp := TBitmap.Create;
  bmp.Width := w;
  bmp.Height := 1;
  bmp.PixelFormat := pf24bit;
  PaintGradient(bmp.Canvas, w, 1);
  for i := 0 to w - 1 do arr[i] := bmp.Canvas.Pixels[i, 0];
  bmp.Free;
end; { GenColorVirtual }

procedure TGradientImage.PaintHSLFunction(ExtCanvas: TCanvas; w, h, x0, y0: integer);
var c: Tcolor;
    x, y: integer;
    r, g, b: byte;
begin
  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;

  for x := 0 to w do
    for y := 0 to h do
    begin
      try
        if assigned(RGBfunction) then
          c := RGBfunction(x0+x, y0+y)
        else
          c := 0;
      except
        c := 0;
      end;
      HSL2RGB((c shr 16) and 255, (c shr 8) and 255, c and 255, R, G, B);
      ExtCanvas.Pixels[x, y] := RGB(R, G, B);
    end;

end; { PaintHSLFunction }

procedure TGradientImage.PaintRGBFunction(ExtCanvas: TCanvas; w, h, x0, y0: integer);
var c: Tcolor;
    x, y: integer;
begin
  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;

  for x := 0 to w do
    for y := 0 to h do
    begin
      try
        if assigned(RGBfunction) then
          c := RGBfunction(x0+x, y0+y)
        else
          c := 0;
      except
        c := 0;
      end;
      ExtCanvas.Pixels[x, y] := c;
    end;

end; { PaintRGBFunction }


procedure TGradientImage.PaintHSLFunction2(ExtCanvas: TCanvas; w, h, x0, y0, gw, gh: integer);
var c: Tcolor;
    x, y: integer;
    w_, h_: integer;
    r, g, b: byte;
begin
  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;
  if (gh <= 0) or (gw <= 0) then exit;
  
  w_ := w div gw;
  h_ := h div gh;

  if assigned(RGBfunction) then
  for x := 0 to w_ do
    for y := 0 to h_ do
    begin
      try
        c := RGBfunction(x0+x*gw, y0+y*gh);
      except
        c := 0;
      end;
      HSL2RGB((c shr 16) and 255, (c shr 8) and 255, c and 255, R, G, B);
      ExtCanvas.Brush.Color := RGB(R, G, B);
      ExtCanvas.FillRect(RECT(gw*x, gh*y, gw*x+gw, gh*y+gh));
    end;
end; { PaintHSLFunction2 }

procedure TGradientImage.PaintRGBFunction2(ExtCanvas: TCanvas; w, h, x0, y0, gw, gh: integer);
var c: Tcolor;
    x, y: integer;
    w_, h_: integer;
begin
  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;
  if (gh <= 0) or (gw <= 0) then exit;

  w_ := w div gw;
  h_ := h div gh;

  if assigned(RGBfunction) then
  for x := 0 to w_ do
    for y := 0 to h_ do
    begin
      try
        c := RGBfunction(x0+x*gw, y0+y*gh);
      except
        c := 0;
      end;
      ExtCanvas.Brush.Color := c;
      ExtCanvas.FillRect(RECT(gw*x, gh*y, gw*x+gw, gh*y+gh));
    end;
end; { PaintRGBFunction2 }

procedure TGradientImage.PaintColorMap(ExtCanvas: TCanvas; w, h: integer);
var hue, lum, sat: integer;
    r1, g1, b1: byte;
    xv, yv, x, y: integer;
    xs, ys: single;
begin
  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;

  xs := 255 / w;
  ys := 255 / h;

  for x := 0 to w do
  begin
    xv := round(xs * x);
    for y := 0 to h do
    begin
      yv := round(ys * y);
      if FHorizontal then
      begin
        hue := xv;
        if FTwoOnly then
          begin lum := 128; sat := yv; end
        else
          begin lum := yv; sat := 128; end;
      end
      else
      begin
        hue := yv;
        if FTwoOnly then
          begin lum := 128; sat := xv; end
        else
          begin lum := xv; sat := 128; end;
      end;
      HSL2RGB(hue, sat, lum, R1, G1, B1);
      ExtCanvas.Pixels[x, y] := RGB(r1, g1, b1);
    end;
  end;
end; { PaintColorMap }

procedure TGradientImage.SubFill(ExtCanvas: TCanvas; R: TRect);
var px: array[1..4] of TPoint;
    n: integer;
    xtr, ytr: double;
begin
  if FAngle = 0 then
    ExtCanvas.FillRect(R)
  else
  begin
    px[1].x := R.Left;   px[1].y := R.Top;
    px[2].x := R.Right;  px[2].y := R.Top;
    px[3].x := R.Right;  px[3].y := R.Bottom;
    px[4].x := R.Left;   px[4].y := R.Bottom;
    for n := 1 to 4 do
    begin
      xtr := px[n].x - md.x;
      ytr := px[n].y - md.y;
      px[n].x := md.x+round(xtr*cosin - ytr*sinus);
      px[n].y := md.y+round(xtr*sinus + ytr*cosin);
    end;
    ExtCanvas.Polygon(px);
  end;
end; { SubFill }

procedure TGradientImage.PaintGradient(ExtCanvas: TCanvas; w, h: integer);
begin
  PaintGradientRect(ExtCanvas, 0, 0, w, h);
  if assigned(ExtraPainter) then
    ExtraPainter(ExtCanvas, w, h);
end; { PaintGradient }

procedure TGradientImage.PaintGradientRect(ExtCanvas: TCanvas; x0, y0, x1, y1: integer);
var i, ii: integer;
    w, h: integer;
    R: TRect;
    r1,b1,g1: integer;
    r2, downc, upc: single;
begin
  w := x1 - x0;
  h := y1 - y0;

  if not assigned(ExtCanvas) or (h <= 0) or (w <= 0) then exit;

  ExtCanvas.Brush.Style := bsSolid;
  ExtCanvas.Pen.Style := psClear;

  md.x := x0 + w div 2;
  md.y := y0 + h div 2;

  sinus := sin(FAngle*pi/180);
  cosin := cos(FAngle*pi/180);

  if FRound then
  begin
    r2 := sqrt((w div 2)*(w div 2) + (h div 2)*(h div 2));
    ii := 2 + round(r2);

//    if FTwoOnly then
    begin
      for i := ii downto 0 do
      begin
        downc := (ii-i)/ii;
        upc := i/ii;
        r1 := round( (FColorStart and 255)*downc + (FColorEnd and 255)*upc );
        g1 := round( ((FColorStart shr 8) and 255)*downc + ((FColorEnd shr 8) and 255)*upc );
        b1 := round( ((FColorStart shr 16) and 255)*downc + ((FColorEnd shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
        ExtCanvas.Ellipse(md.x - i,
                          md.y - i,
                          md.x + i,
                          md.y + i
                         );
      end
//    else
//    begin
//    end;
    end;

  end
  else


  if FHorizontal then
  begin
    R.Top := y0+0;     //+y0
    R.Bottom := y0+h;  //+y0
    if FTwoOnly then      //horizontal 2 colors
    begin
      for i := 0 to w do
      begin
        downc := (w-i)/w;
        upc := i/w;
        R.Left := x0 + i;       //+x0
        R.Right := x0 + i+1;    //+x0
        r1 := round( (FColorStart and 255)*downc + (FColorEnd and 255)*upc );
        g1 := round( ((FColorStart shr 8) and 255)*downc + ((FColorEnd shr 8) and 255)*upc );
        b1 := round( ((FColorStart shr 16) and 255)*downc + ((FColorEnd shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
        SubFill(ExtCanvas, R);
      end;
    end
    else                  //horizontal 3 colors
    begin
      for i := 0 to w div 2 do
      begin
        downc := ((w/2)-i)/(w/2);
        upc := i/(w/2);
        R.Left := x0 + i;       //+x0
        R.Right := x0 + i+1;    //+x0
        r1 := round( (FColorStart and 255)*downc + (FColorMid and 255)*upc );
        g1 := round( ((FColorStart shr 8) and 255)*downc + ((FColorMid shr 8) and 255)*upc );
        b1 := round( ((FColorStart shr 16) and 255)*downc + ((FColorMid shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
//        ExtCanvas.FillRect(R);
        SubFill(ExtCanvas, R);
        R.Left := x0 + i + w div 2;     //+x0
        R.Right := x0 + i+1 + w div 2;  //+x0
        r1 := round( (FColorMid and 255)*downc + (FColorEnd and 255)*upc );
        g1 := round( ((FColorMid shr 8) and 255)*downc + ((FColorEnd shr 8) and 255)*upc );
        b1 := round( ((FColorMid shr 16) and 255)*downc + ((FColorEnd shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
//        ExtCanvas.FillRect(R);
        SubFill(ExtCanvas, R);
      end;
    end;
  end
  else
  begin
    R.Left := x0 + 0;    //+x0
    R.Right := x0 + w;   //+x0
    if FTwoOnly then      //vertical 2 colors
    begin
      for i := 0 to h do
      begin
        downc := (h-i)/h;
        upc := i/h;
        R.Top := y0 + i;        //+y0
        R.Bottom := y0 + i+1;   //+y0
        r1 := round( (FColorStart and 255)*downc + (FColorEnd and 255)*upc );
        g1 := round( ((FColorStart shr 8) and 255)*downc + ((FColorEnd shr 8) and 255)*upc );
        b1 := round( ((FColorStart shr 16) and 255)*downc + ((FColorEnd shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
//        ExtCanvas.FillRect(R);
        SubFill(ExtCanvas, R);
      end;
    end
    else                  //vertical 3 colors
    begin
      for i := 0 to h div 2 do
      begin
        downc := ((h/2)-i)/(h/2);
        upc := i/(h/2);
        R.Top := y0 + i;        //+y0
        R.Bottom := y0 + i+1;   //+y0
        r1 := round( (FColorStart and 255)*downc + (FColorMid and 255)*upc );
        g1 := round( ((FColorStart shr 8) and 255)*downc + ((FColorMid shr 8) and 255)*upc );
        b1 := round( ((FColorStart shr 16) and 255)*downc + ((FColorMid shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
//        ExtCanvas.FillRect(R);
        SubFill(ExtCanvas, R);
        R.Top := y0 + i + h div 2;       //+y0
        R.Bottom := y0 + i+1 + h div 2;  //+y0
        r1 := round( (FColorMid and 255)*downc + (FColorEnd and 255)*upc );
        g1 := round( ((FColorMid shr 8) and 255)*downc + ((FColorEnd shr 8) and 255)*upc );
        b1 := round( ((FColorMid shr 16) and 255)*downc + ((FColorEnd shr 16) and 255)*upc );
        ExtCanvas.Brush.Color := RGB(r1, g1, b1);
//        ExtCanvas.FillRect(R);
        SubFill(ExtCanvas, R);
      end;
    end;
  end;
end; { PaintGradient }

procedure TGradientImage.Paint;
begin
  if FColorMap then
    PaintColorMap(self.Canvas, self.Width, self.Height)
  else
    PaintGradient(self.Canvas, self.Width, self.Height);
end; { Paint }

procedure TGradientImage.WMSize(var Message: TWMSize);
begin
  self.Width := Message.Width;
  self.Height := Message.Height;
  Refresh;
end; { WMSize }

procedure TGradientImage.CWriteFColorStart(PColorStart: TColor);
begin
  if FColorStart <> PColorStart then
  begin
    FColorStart := PColorStart;
    Refresh;
  end;
end; { CWriteFColorStart }

procedure TGradientImage.CWriteFColorMid(PColorMid: TColor);
begin
  if FColorMid <> PColorMid then
  begin
    FColorMid := PColorMid;
    Refresh;
  end;
end; { CWriteFColorMid }

procedure TGradientImage.CWriteFColorEnd(PColorEnd: TColor);
begin
  if FColorEnd <> PColorEnd then
  begin
    FColorEnd := PColorEnd;
    Refresh;
  end;
end; { CWriteFColorEnd }

procedure TGradientImage.CWriteFTwoOnly(PTwoOnly: boolean);
begin
  if FTwoOnly <> PTwoOnly then
  begin
    FTwoOnly := PTwoOnly;
    Refresh;
  end;
end; { CWriteFTwoOnly }

procedure TGradientImage.CWriteFHorizontal(PHorizontal: boolean);
begin
  if FHorizontal <> PHorizontal then
  begin
    FHorizontal := PHorizontal;
    Refresh;
  end;
end; { CWriteFHorizontal }

procedure TGradientImage.CWriteFColorMap(PColorMap: boolean);
begin
  if FColorMap <> PColorMap then
  begin
    FColorMap := PColorMap;
    Refresh;
  end;
end; { CWriteFColorMap }

procedure TGradientImage.CWriteFAngle(PAngle: double);
begin
  if FAngle <> PAngle then
  begin
    FAngle := PAngle;
    Refresh;
  end;
end; { CWriteFAngle }

procedure TGradientImage.CWriteFRound(PRound: boolean);
begin
  if FRound <> PRound then
  begin
    FRound := PRound;
    Refresh;
  end;
end; { CWriteFRound }

procedure TGradientImage.AssignToBitmap(var bmp: TBitmap);
begin
  if assigned(bmp) then
  begin
    bmp.Width := width;
    bmp.Height := height;
    bmp.Canvas.CopyRect(self.ClientRect, self.Canvas, self.ClientRect);
  end;
end; { AssignToBitmap }

end.

