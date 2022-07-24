unit clbitbtn;

{---------------------------------------------------}
{ TColorBitBtn component                            }
{ Version 1.00                                      }
{ Legal:   (c)2003-2005, 2022 Noniewicz.com,        }
{ Jakub Noniewicz aka MoNsTeR/GDC                   }
{ E-mail:  monster@Noniewicz.com                    }
{ WWW:     http://www.Noniewicz.com                 }
{ Version 1.0, update: 2005.11.12                   }
{---------------------------------------------------}


interface

uses Windows, Classes, Graphics, Buttons, Controls, SysUtils;

type
  TColorBitBtn = class(TBitBtn)
  private
    FBMP: TBitmap;
    FColor: TColor;
    FX, FY: integer;
    procedure PaintBMP;
    procedure CWriteFColor(PColor: TColor);
    procedure CWriteFX(PX: integer);
    procedure CWriteFY(PY: integer);
    procedure Repaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GlyphColor: TColor read FColor write CWriteFColor;
    property GlyphWidth: integer read FX write CWriteFX;
    property GlyphHeight: integer read FY write CWriteFY;

    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
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
    property ShowHint;
    property Tag;
    property Top;
    property Visible;
    property Width;
  end;



procedure Register;


implementation



procedure Register;
begin
  RegisterComponents('Monster', [TColorBitBtn]);
end;


procedure TColorBitBtn.PaintBMP;
begin
  FBMP.Width := FX;
  FBMP.Height := FY;

  FBMP.Canvas.Brush.Style := bsSolid;
  FBMP.Canvas.Brush.Color := self.Color;
  FBMP.Canvas.FillRect(Rect(0, 0, FX, FY));

  FBMP.Canvas.Brush.Color := FColor;
  FBMP.Canvas.Pen.Color := FColor xor $ffffff;
  FBMP.Canvas.Pen.Style := psSolid;
  FBMP.Canvas.Rectangle(0, 0, FX-1, FY-1);

  self.Glyph.Assign(FBMP);
end;

procedure TColorBitBtn.Repaint;
begin
  PaintBMP;
  inherited;
end;

procedure TColorBitBtn.CWriteFColor(PColor: TColor);
begin
  FColor := PColor;
  self.Repaint;
end;

procedure TColorBitBtn.CWriteFX(PX: integer);
begin
  FX := PX;
  self.Repaint;
end;

procedure TColorBitBtn.CWriteFY(PY: integer);
begin
  FY := PY;
  self.Repaint;
end;

constructor TColorBitBtn.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clRed;
  FX := 16;
  FY := 16;
  FBMP := TBitmap.Create;
  self.Repaint;
end;

destructor TColorBitBtn.Destroy;
begin
  self.Glyph := nil;
  FBMP.Free;
  inherited;
end;

end.
