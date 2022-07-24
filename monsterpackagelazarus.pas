{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MonsterPackageLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  gradimg, wwwlabel, loglib, arrow, lissajoux, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('gradimg', @gradimg.Register);
  RegisterUnit('wwwlabel', @wwwlabel.Register);
  RegisterUnit('loglib', @loglib.Register);
end;

initialization
  RegisterPackage('MonsterPackageLazarus', @Register);
end.
