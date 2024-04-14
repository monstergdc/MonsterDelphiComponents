{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MonsterPackageLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  gradimg, wwwlabel, loglib, lissajoux, recent, tarrow, clbitbtn, lazkeymap, 
  charmap, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('gradimg', @gradimg.Register);
  RegisterUnit('wwwlabel', @wwwlabel.Register);
  RegisterUnit('loglib', @loglib.Register);
  RegisterUnit('lissajoux', @lissajoux.Register);
  RegisterUnit('recent', @recent.Register);
  RegisterUnit('tarrow', @tarrow.Register);
  RegisterUnit('clbitbtn', @clbitbtn.Register);
  RegisterUnit('lazkeymap', @lazkeymap.Register);
  RegisterUnit('charmap', @charmap.Register);
end;

initialization
  RegisterPackage('MonsterPackageLazarus', @Register);
end.
