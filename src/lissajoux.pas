unit lissajoux;

{-------------------------------------------------}
{ TLissajoux Delphi component (D3|D7|BDS2k6)      }
{ Version: 1.31                                   }
{ Created: 2004.11.20                             }
{ E-mail:  monster@Noniewicz.com                  }
{ WWW:     http://www.Noniewicz.com               }
{ Legal:   (c)1992-2012, 2017, 2024 Noniewicz.com,}
{ Jakub Noniewicz aka MoNsTeR/GDC                 }
{ Licence: BSD 2-Clause License                   }
{-------------------------------------------------}
{ Description:                                    }
{ The TLissajoux component is simple visual       }
{ component that rendering Lissajoux curves.      }
{-------------------------------------------------}
{ History:                                        }
{ Version 1.00, update: 2004.11.20                }
{ Version 1.10, update: 2005.11.20                }
{ Version 1.20, update: 2012.09.15                }
{ Version 1.30, update: 2017.10.29 GitHub         }
{ Version 1.31, update: 2024.04.08                }
{-------------------------------------------------}

{todo:
- var fi+freq?
}

{CHANGELOG:
v1.10:
- paint to external canvas
- dotmode/linemode
- line width
v1.20:
- minor changes
v1.30:
- code cleanup
v1.31:
- crossplatform
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
     Classes, Graphics, Controls, Extctrls, Messages, SysUtils;

type
  TLissajoux = class(TCustomControl)
  private
    FF1, FF2, FFi, FdT: double;
    FLineColor: TColor;
    FBGColor: TColor;
    FSteps: integer;
    FDotmode: boolean;
    FWidth: integer;
    procedure Paint; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CWriteFLineColor(PLineColor: TColor);
    procedure CWriteFBGColor(PBGColor: TColor);
    procedure CWriteFF1(PF1: double);
    procedure CWriteFF2(PF2: double);
    procedure CWriteFFi(PFi: double);
    procedure CWriteFdT(PdT: double);
    procedure CWriteFSteps(PSteps: integer);
    procedure CWriteFDotmode(PDotmode: boolean);
    procedure CWriteFWidth(PWidth: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintExt(ExtCanvas: TCanvas; w, h: integer);
    function SaveToFile(FileName: string): integer;
    procedure AssignToBitmap(var bmp: TBitmap);
  published
    property LineColor: TColor read FLineColor write CWriteFLineColor;
    property BGColor: TColor read FBGColor write CWriteFBGColor;
    property Freq1: double read FF1 write CWriteFF1;
    property Freq2: double read FF2 write CWriteFF2;
    property Phase: double read FFi write CWriteFFi;
    property dT: double read FdT write CWriteFdT;
    property Steps: integer read FSteps write CWriteFSteps;
    property Dotmode: boolean read FDotmode write CWriteFDotmode;
    property LineWidth: integer read FWidth write CWriteFWidth;

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
    property PopupMenu;
  end;


procedure Register;


implementation


const rd = pi/180;


procedure Register;
begin
  RegisterComponents('Monster', [TLissajoux]);
end;

//---

constructor TLissajoux.Create(AOwner: TComponent);
begin
  inherited;

  FF1 := 19.0;
  FF2 := 31.0;
  FFi := 0;
  Fdt := 1;
  FSteps := 10000;
  FLineColor := clBlack;
  FBGColor := clWhite;
  FDotmode := false;
  FWidth := 1; 
  self.Width := 320;
  self.Height := 256;
end; { Create }

destructor TLissajoux.Destroy;
begin
  inherited;
end; { Destroy }

procedure TLissajoux.Paint;
begin
  inherited;
  PaintExt(self.Canvas, self.Width, self.Height);
end; { Paint }

procedure TLissajoux.PaintExt(ExtCanvas: TCanvas; w, h: integer);
var i, x, y, ampli, sx, sy: integer;
    time, Freq, Fi: double;
begin
  Time := 0;
  if FF2 > 0 then
    Freq := FF1/FF2
  else
    Freq := 0; 
  Fi := FFi*pi;
  sx := w div 2;
  sy := h div 2;
  if sx > sy then ampli := sy-4 else ampli := sx-4;

  ExtCanvas.Brush.Color := FBGColor;
  ExtCanvas.Brush.Style := bsSolid;
  ExtCanvas.Pen.Color := FLineColor;
  ExtCanvas.Pen.Width := FWidth;
  ExtCanvas.FillRect(RECT(0, 0, w, h));

   for i := 1 to FSteps do
   begin
     x := round(sx+ampli*sin(rd*Time));
     y := round(sy+ampli*sin(rd*(Time*Freq+Fi)));
     if FDotmode then
       ExtCanvas.Pixels[x, y] := FLineColor
     else
     begin
       if i = 1 then
         ExtCanvas.MoveTo(x, y)
       else
         ExtCanvas.LineTo(x, y);
     end;
     Time := Time+dT;
   end;

end; { PaintExt }

procedure TLissajoux.WMSize(var Message: TWMSize);
begin
  self.Width := Message.Width;
  self.Height := Message.Height;
  Refresh;
end; { WMSize }

//---

procedure TLissajoux.CWriteFLineColor(PLineColor: TColor);
begin
  if FLineColor <> PLineColor then
  begin
    FLineColor := PLineColor;
    Refresh;
  end;
end; { CWriteFLineColor }

procedure TLissajoux.CWriteFBGColor(PBGColor: TColor);
begin
  if FBGColor <> PBGColor then
  begin
    FBGColor := PBGColor;
    Refresh;
  end;
end; { CWriteFBGColor } 

procedure TLissajoux.CWriteFF1(PF1: double);
begin
  if FF1 <> PF1 then
  begin
    FF1 := PF1;
    Refresh;
  end;
end; { CWriteFF1 }

procedure TLissajoux.CWriteFF2(PF2: double);
begin
  if FF2 <> PF2 then
  begin
    FF2 := PF2;
    Refresh;
  end;
end; { CWriteFF2 }

procedure TLissajoux.CWriteFFi(PFi: double);
begin
  if FFi <> PFi then
  begin
    FFi := PFi;
    Refresh;
  end;
end; { CWriteFFi }

procedure TLissajoux.CWriteFdT(PdT: double);
begin
  if FdT <> PdT then
  begin
    FdT := PdT;
    Refresh;
  end;
end; { CWriteFdT }

procedure TLissajoux.CWriteFSteps(PSteps: integer);
begin
  if FSteps <> PSteps then
  begin
    FSteps := PSteps;
    Refresh;
  end;
end; { CWriteFSteps }

procedure TLissajoux.CWriteFDotmode(PDotmode: boolean);
begin
  if FDotmode <> PDotmode then
  begin
    FDotmode := PDotmode;
    Refresh;
  end;
end; { CWriteFDotmode }

procedure TLissajoux.CWriteFWidth(PWidth: integer);
begin
  if FWidth <> PWidth then
  begin
    FWidth := PWidth;
    Refresh;
  end;
end; { CWriteFWidth }

function TLissajoux.SaveToFile(FileName: string): integer;
var img: TBitmap;
    R: TRect;
begin
  result := -1;
  if FileName <> '' then
  begin
    img := TBitmap.Create;
    img.Width := width;
    img.Height := height;
    R.Left := 0;
    R.Top := 0;
    R.Right := self.Width;
    R.Bottom := self.Height;
    img.Canvas.CopyRect(r, self.Canvas, r);
    try
      img.SaveToFile(FileName);
      result := 0;
    except
    end;
    img.Free;
  end;
end; { SaveToFile }

procedure TLissajoux.AssignToBitmap(var bmp: TBitmap);
begin
  if assigned(bmp) then
  begin
    bmp.Width := width;
    bmp.Height := height;
    self.PaintTo(bmp.Canvas.Handle, 0, 0);
  end;
end; { AssignToBitmap }


end.

