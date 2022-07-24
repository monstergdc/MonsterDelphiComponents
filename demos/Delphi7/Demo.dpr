program Demo;

//MoNsTeR's Delphi components - demo
//(c)2017-2022 Noniewicz.com

uses
  Forms,
  DemoMainUnit in 'DemoMainUnit.pas' {Form1},
  loglib in '..\..\src\loglib.pas',
  wwwlabel in '..\..\src\wwwlabel.pas',
  gradimg in '..\..\src\gradimg.pas',
  lissajoux in '..\..\src\lissajoux.pas',
  tarrow in '..\..\src\tarrow.pas',
  clbitbtn in '..\..\src\clbitbtn.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
