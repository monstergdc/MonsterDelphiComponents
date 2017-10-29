unit DemoMainUnit;

//MoNsTeR'd Delphi components - Delphi 7 demo application
//(c)2017 Noniewicz.com
//created: 20171024
//updated: 20171029

//note that all components are created from code here,
//in D7 one can put them directly on form,
//but in free BDS 2006 this cna't be done with 3rd party components    

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  wwwlabel, loglib, gradimg, arrow, lissajoux;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    EditLog: TEdit;
    bnLog: TButton;
    GroupBox3: TGroupBox;
    Image1: TImage;
    MemoShowLog: TMemo;
    bnLoadLog: TButton;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bnLogClick(Sender: TObject);
    procedure bnLoadLogClick(Sender: TObject);
  private
    log: TLogger;
    www: TWWWLabel;
    gi1: TGradientImage;
    gi2: TGradientImage;
    gi3: TGradientImage;
    gi4: TGradientImage;
    gi5: TGradientImage;
    gi6: TGradientImage;
    arr1: TArrow;
    arr2: TArrow;
    lis: TLissajoux;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

  function B1(x, y: integer): TColor;
  var r, g, b: byte;
  begin
    r := 0;
    g := y xor x;
    b := 0;
    Result := RGB(r, g, b);
  end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  log := TLogger.Create(self);
  log.FileName := ExtractFilePath(Application.ExeName)+'sample.log';
  log.UseDate := true;
  log.UseTime := true;
  log.UseLevel := true;
  log.LogPid := true;
  log.DateFormat := 'yyyy-mm-dd';
  log.TimeFormat := 'hh:nn:ss';
  log.OpenLog;

  www := TWWWLabel.Create(GroupBox2);
  www.Parent := GroupBox2;
  www.Left := 10;
  www.Top := 24;
  www.Caption := 'http://noniewicz.com/';
  www.URL := 'http://noniewicz.com/';
  www.Hint := www.URL;
  www.ShowHint := true;

  gi1 := TGradientImage.Create(GroupBox3);
  gi1.Parent := GroupBox3;
  gi1.Left := 10;
  gi1.Top := 24;
  gi1.Width := 256;
  gi1.ShowHint := true;
  gi1.Hint := 'Vertical gradient';

  gi2 := TGradientImage.Create(GroupBox3);
  gi2.Parent := GroupBox3;
  gi2.Left := 10;
  gi2.Top := 24+32;
  gi2.Horizontal := true;
  gi2.Width := 256;
  gi2.ShowHint := true;
  gi2.Hint := 'Horizontal gradient';

  gi3 := TGradientImage.Create(GroupBox3);
  gi3.Parent := GroupBox3;
  gi3.Left := 10;
  gi3.Top := 24+32+32;
  gi3.Horizontal := true;
  gi3.Width := 256;
  gi3.TwoOnly := true;
  gi3.ColorStart := RGB(0, 0, 0);
  gi3.ColorEnd := RGB(255, 255, 255);
  gi3.ShowHint := true;
  gi3.Hint := 'Two color gradient';

  gi4 := TGradientImage.Create(GroupBox3);
  gi4.Parent := GroupBox3;
  gi4.Left := 10;
  gi4.Top := 24+32+32+32;
  gi4.Width := 192;
  gi4.Height := 192;
  gi4.Angle := 45;
  gi4.ShowHint := true;
  gi4.ColorStart := RGB(255, 0, 0);
  gi4.ColorMid := RGB(0, 255, 0);
  gi4.ColorEnd := RGB(0, 0, 255);
  gi4.Hint := 'Rotated gradient';

  gi5 := TGradientImage.Create(GroupBox3);
  gi5.Parent := GroupBox3;
  gi5.Left := 10 + 192 + 10;
  gi5.Top := 24+32+32+32;
  gi5.Width := 192;
  gi5.Height := 192;
  gi5.IsRound := true;
  gi5.ColorStart := RGB(255-32, 255, 0);
  gi5.ColorEnd := RGB(255, 0, 0);
  gi5.ShowHint := true;
  gi5.Hint := 'Radial gradient!';

  gi6 := TGradientImage.Create(GroupBox3);
  gi6.Parent := GroupBox3;
  gi6.Left := 10 + 192 + 10 + 192 + 10;
  gi6.Top := 24+32+32+32;
  gi6.Width := 192;
  gi6.Height := 192;
  gi6.AsColorMap := true;
  gi6.ShowHint := true;
  gi6.Hint := 'As color map!';
  gi6.RGBFunction := B1;

  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
  gi6.PaintRGBFunction(Image1.Picture.Bitmap.Canvas, Image1.Width, Image1.Height, 0, 0);

  arr1 := TArrow.Create(GroupBox4);
  arr1.Parent := GroupBox4;
  arr1.Left := 10;
  arr1.Top := 24;
  arr1.LineWidth := 2;

  arr2 := TArrow.Create(GroupBox4);
  arr2.Parent := GroupBox4;
  arr2.Left := 10 + 50;
  arr2.Top := 24;
  arr2.ArrowColor := clBlue;
  arr2.Angle := 15;
  arr2.ArrAngle := 20;
  arr2.ArrLength := 30;
  arr2.LineWidth := 4;

  lis := TLissajoux.Create(GroupBox5);
  lis.Parent := GroupBox5;
  lis.Top := 20;
  lis.Left := 10;
  lis.Width := 128;
  lis.Height := 128;
  lis.LineColor := clWhite;
  lis.BGColor := clBlack;
  lis.Freq1 := 3;
  lis.Freq2 := 4;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  lis.Free;
  arr2.Free;
  arr1.Free;
  gi6.Free;
  gi5.Free;
  gi4.Free;
  gi3.Free;
  gi2.Free;
  gi1.Free;
  www.Free;
  log.CloseLog;
  log.Free;
end;

procedure TForm1.bnLogClick(Sender: TObject);
begin
  log.WriteLog(EditLog.Text);
end;

procedure TForm1.bnLoadLogClick(Sender: TObject);
begin
  log.CloseLog;
  MemoShowLog.Lines.LoadFromFile(log.FileName);
  log.OpenLog;
end;

end.

