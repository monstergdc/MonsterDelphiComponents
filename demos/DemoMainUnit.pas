unit DemoMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  wwwlabel, loglib;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    EditLog: TEdit;
    bnLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bnLogClick(Sender: TObject);
  private
    log: TLogger;
    www: TWWWLabel;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
  www.Top := 32;
  www.Caption := 'http://noniewicz.com/';
  www.URL := 'http://noniewicz.com/';

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  www.Free;

  log.CloseLog;
  log.Free;
end;

procedure TForm1.bnLogClick(Sender: TObject);
begin
  log.WriteLog(EditLog.Text);
end;

end.
