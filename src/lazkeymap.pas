unit lazkeymap;

// user app key mapping control (non visual) v1.0-laz
// (c)2005, 2024 MoNsTeR/GDC, Jakub Noniewicz, monster@Noniewicz.com
// update: 20050623 (D3)
// update: 20240404 (Lazarus)

{todo:
- set from props 
}

interface

uses
  Windows, 
  Classes, Controls, Forms, Dialogs, ExtCtrls, 
  IniFiles, ComCtrls, Buttons, StdCtrls, Menus,
  lclproc;


type TShortCutEdit = class(TComponent)
     private
       FCol1Cap, FCol2Cap, FMsgIllegal, FMsgCant, FMsgReassing: string;
       FCheckBoxSHIFT: TCheckBox;
       FCheckBoxCTRL: TCheckBox;
       FCheckBoxALT: TCheckBox;
       FSHIFTState: boolean;
       FCTRLState: boolean;
       FALTState: boolean;
       FEditKey: TEdit;
       FListViewKeys: TListView;
       FKeyText: string;
       FHint: string;
       FHintLabel: TLabel;
       FMenu: TMainMenu;
       keyExceptions: TStringList;
       procedure ListViewKeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
       procedure EditKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       procedure Init;
       procedure BnSetClick;
       procedure BnClrClick;
       procedure GetShortcuts;
       function FindKeyShortFree(shortCut: TShortCut): boolean;
       function KeyLoad(FileName: string): boolean;
       function KeySave(FileName: string): boolean;
     published
       property Menu: TMainMenu read FMenu write FMenu; //!
       property ListView: TListView read FListViewKeys write FListViewKeys;
       property ItemHint: string read FHint;
       property KeyText: string read FKeyText write FKeyText;
       property CheckBoxSHIFT: TCheckBox read FCheckBoxSHIFT write FCheckBoxSHIFT;
       property CheckBoxCTRL: TCheckBox read FCheckBoxCTRL write FCheckBoxCTRL;
       property CheckBoxALT: TCheckBox read FCheckBoxALT write FCheckBoxALT;
       property EditKey: TEdit read FEditKey write FEditKey;
       property HintLabel: TLabel read FHintLabel write FHintLabel;
       property Col1Caption: string read FCol1Cap write FCol1Cap; 
       property Col2Caption: string read FCol2Cap write FCol2Cap;
       property SHIFTState: boolean read FSHIFTState write FSHIFTState;
       property CTRLState: boolean read FCTRLState write FCTRLState;
       property ALTState: boolean read FALTState write FALTState;
       property MsgIllegal: string read FMsgIllegal write FMsgIllegal;
       property MsgCant: string read FMsgCant write FMsgCant;
       property MsgReassing: string read FMsgReassing write FMsgReassing;
  end;


procedure Register;


implementation

const
      S_REASSIGN = 'This key combination is already in use. Reassign anyway?';
      S_ILLEGAL = 'Illegal key assigment!';
      S_CANT = 'This shortcut cannot be reassigned! Shorcuts assigned to dynamically building menus cannot be used by you.'; 
      S_SHORTCUT = 'Shortcut';
      S_CAP = 'Menu command';

procedure Register;
begin
  RegisterComponents('Monster', [TShortCutEdit]);
end;



constructor TShortCutEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  keyExceptions := TStringList.Create;
  FMenu := nil;
  FListViewKeys := nil;
  FCheckBoxSHIFT := nil;
  FCheckBoxCTRL := nil;
  FCheckBoxALT := nil;
  FEditKey := nil;
  FHintLabel := nil;
  FKeyText := '';
  FHint := '';
  FCol1Cap := S_CAP;
  FCol2Cap := S_SHORTCUT;
  FMsgIllegal := S_ILLEGAL;
  FMsgCant := S_CANT;
  FMsgReassing := S_REASSIGN; 
  FSHIFTState := false;
  FCTRLState := false;
  FALTState := false;
end;

destructor TShortCutEdit.Destroy;
begin
  keyExceptions.Free;
  inherited;
end;

procedure TShortCutEdit.Init;
var c: TListColumn;
begin
  if assigned(FEditKey) then
  begin
    FEditKey.MaxLength := 4;
    FEditKey.OnKeyDown := @EditKeyKeyDown;
  end;
  if assigned(FListViewKeys) then
  begin
    FListViewKeys.ReadOnly := true;
    FListViewKeys.ShowColumnHeaders := true;
    FListViewKeys.RowSelect := true;
    FListViewKeys.MultiSelect := false;
    FListViewKeys.HideSelection := false;
    FListViewKeys.ViewStyle := vsReport;
    FListViewKeys.GridLines := true;
    FListViewKeys.SortType := stNone;
    FListViewKeys.Columns.Clear;
    c := FListViewKeys.Columns.Add;
    c.Caption := FCol1Cap;
    c.Width := 215;
    c := FListViewKeys.Columns.Add;
    c.Caption := FCol2Cap;
    c.Width := 85;
    FListViewKeys.OnChange := @ListViewKeysChange;
  end;

  GetShortcuts;
end;

procedure TShortCutEdit.GetShortcuts;
var i: integer;
    LVItem: TListItem;

  function killamp(s: string): string;
  var s1: string;
  begin
    s1 := s;
    while (pos('&', s1)>0) do delete(s1, pos('&', s1), 1);
    result := s1;
  end;

  procedure getItems(item: TMenuItem);
  var i: integer;
      s1, s2: string;
  begin
    if (item.Tag = 1) and (ShortCutToText(item.ShortCut) <> '') then
      keyExceptions.Add(ShortCutToText(item.ShortCut)+'=1');
    if (item.Caption <> '-') and (item.Tag <> 1) then
    begin
      s1 := killamp(item.Caption);
      s2 := ShortCutToText(item.ShortCut);
      LVItem := FListViewKeys.Items.Add;
      LVItem.Caption := s1;
      LVItem.SubItems.Add(s2);
      LVItem.Data := item;
    end;
    for i := 0 to item.Count-1 do
    begin
      if (item.Items[i].Tag = 1) and (ShortCutToText(item.Items[i].ShortCut) <> '') then
        keyExceptions.Add(ShortCutToText(item.Items[i].ShortCut)+'=1');
      if (item.Items[i].Caption <> '-') and (item.Items[i].Tag <> 1) then
      begin
        s1 := killamp(item.Items[i].Caption);
        s2 := ShortCutToText(item.Items[i].ShortCut);
        LVItem := FListViewKeys.Items.Add;
        LVItem.Caption := s1;
        LVItem.SubItems.Add(s2);
        LVItem.Data := item.Items[i];
      end;
      if item.Items[i].Count > 0 then getItems(item.Items[i]);
    end;
  end;

begin
  keyExceptions.Clear;
  if not assigned(FListViewKeys) then exit;
  FListViewKeys.Items.Clear;
  if assigned(FMenu) then
    for i := 0 to FMenu.Items.Count-1 do getItems(FMenu.items[i]);
end;

function TShortCutEdit.FindKeyShortFree(shortCut: TShortCut): boolean;
var i: integer;
    sh1: TShortCut;
    res: boolean;
    LVItem: TListItem;
    menuitem: TMenuItem;
begin
  if not assigned(FListViewKeys) then begin Result := false; exit; end;

  res := true;
  LVItem := nil;
  for i := 0 to FListViewKeys.Items.Count-1 do
  begin
    sh1 := TextToShortCut(FListViewKeys.Items[i].Subitems[0]);
    if sh1 = shortCut then
    begin
      res := false;
      LVItem := FListViewKeys.Items[i]; //item to clear possibly
    end;
  end;
  if not res then
    if MessageDlg(FMsgReassing, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      menuitem := TMenuItem(LVItem.Data);
      menuitem.ShortCut := TextToShortCut('');
      res := true;
    end;
  Result := res;
end;

procedure TShortCutEdit.EditKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if assigned(FCheckBoxSHIFT) then
    if ssShift in Shift then CheckBoxSHIFT.Checked := true; //no unckeck
  if assigned(FCheckBoxCTRL) then
    if ssCtrl in Shift then CheckBoxCTRL.Checked := true;
  if assigned(FCheckBoxALT) then
    if ssAlt in Shift then CheckBoxALT.Checked := true;
  if assigned(FEditKey) then
  begin
    FEditKey.Text := '';
    case key of
      45: FEditKey.Text := 'ins';
      46: FEditKey.Text := 'del';
      36: FEditKey.Text := 'home';
      35: FEditKey.Text := 'end';
      33: FEditKey.Text := 'pgup';
      34: FEditKey.Text := 'pgdn';
      112..120: FEditKey.Text := 'f'+chr(key-112+1+48);
      121..123: FEditKey.Text := 'f1'+chr(key-121+48);
    end;     //else dflt edit behaviour
    if FEditKey.Text <> '' then Key := 0;
  end;
end;


procedure TShortCutEdit.ListViewKeysChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var menuitem: TMenuItem;
    keys: string;
begin
  if (Change = ctState) and (FListViewKeys.Selected <> nil) and (item <> nil) then
  begin
    menuitem := TMenuItem(FListViewKeys.Selected.Data);
    if menuitem = nil then exit;
    {-}
    FHint := menuitem.Hint;
    if assigned(FHintLabel) then FHintLabel.Caption := menuitem.Hint;
    {-}
    keys := item.SubItems[0];
    if assigned(FCheckBoxSHIFT) then
      FCheckBoxSHIFT.Checked := pos('Shift', keys) > 0;
    if assigned(FCheckBoxCTRL) then
      FCheckBoxCTRL.Checked := pos('Ctrl', keys) > 0;
    if assigned(FCheckBoxALT) then
      FCheckBoxALT.Checked := pos('Alt', keys) > 0;
    while pos('+', keys) > 0 do delete(keys, 1, 1);
    FKeyText := keys;
    FSHIFTState := pos('Shift', keys) > 0;
    FCTRLState := pos('Ctrl', keys) > 0;
    FALTState := pos('Alt', keys) > 0;
    if assigned(FEditKey) then FEditKey.Text := keys;
  end
  else
  begin
    if assigned(FHintLabel) then FHintLabel.Caption := '?';
    FHint := '?';
    FKeyText := '';
    if assigned(FEditKey) then FEditKey.Text := '';
  end;
end;

procedure TShortCutEdit.BnSetClick;
var keys: string;
    shortCut: TShortCut;
    menuitem: TMenuItem;
    LVI: integer;
begin
  if not assigned(FListViewKeys) then exit;

  if assigned(FCheckBoxSHIFT) then
    if FCheckBoxSHIFT.Checked then keys := 'Shift+';
  if assigned(FCheckBoxCTRL) then
    if FCheckBoxCTRL.Checked then keys := keys + 'Ctrl+';
  if assigned(FCheckBoxALT) then
    if FCheckBoxALT.Checked then keys := keys + 'Alt+';
  if assigned(FEditKey) then
    keys := keys + FEditKey.Text;
  shortCut := TextToShortCut(keys);
  if (FListViewKeys.Selected <> nil) then
  begin
    LVI := FListViewKeys.Selected.Index;
    if ShortCutToText(shortCut) = '' then
      MessageDlg(FMsgIllegal, mtError, [mbOk], 0)
    else
      if keyExceptions.Values[ShortCutToText(shortCut)] = '1' then
        MessageDlg(FMsgCant, mtError, [mbOk], 0)
      else
        if findKeyShortFree(shortCut) then
        begin
          menuitem := TMenuItem(FListViewKeys.Selected.Data);
          menuitem.ShortCut := shortCut;
          GetShortcuts;
          FListViewKeys.Selected := FListViewKeys.Items[LVI];
          FListViewKeys.Selected.MakeVisible(false);
        end;
  end;
end;

procedure TShortCutEdit.BnClrClick;
var menuitem: TMenuItem;
    LVI: integer;
begin
  if not assigned(FListViewKeys) then exit;

  if (FListViewKeys.Selected <> nil) then
  begin
    LVI := FListViewKeys.Selected.Index;
    menuitem := TMenuItem(FListViewKeys.Selected.Data);
    menuitem.ShortCut := TextToShortCut('');
    GetShortcuts;
    FListViewKeys.Selected := FListViewKeys.Items[LVI];
    FListViewKeys.Selected.MakeVisible(false);
  end;
end;

function TShortCutEdit.KeyLoad(FileName: string): boolean;
var ini: TIniFile;
    i: integer;
    menuitem: TMenuItem;
begin
  if not assigned(FListViewKeys) then begin Result := false; exit; end;
  
  Result := true;
  try
    ini := TIniFile.Create(FileName);
    for i := 0 to FListViewKeys.Items.Count-1 do
    begin
      menuitem := TMenuItem(FListViewKeys.Items[i].Data);
      menuitem.ShortCut := TextToShortCut(ini.ReadString('keymapping', menuitem.Name, ''));
    end;
    ini.Free;
  except
    Result := false;
  end;
  GetShortcuts;
end;

function TShortCutEdit.KeySave(FileName: string): boolean;
var ini: TIniFile;
    i: integer;
    menuitem: TMenuItem;
begin
  if not assigned(FListViewKeys) then begin Result := false; exit; end;

  Result := true;
  try
    ini := TIniFile.Create(FileName);
    for i := 0 to FListViewKeys.Items.Count-1 do
    begin
      menuitem := TMenuItem(FListViewKeys.Items[i].Data);
      ini.WriteString('keymapping', menuitem.Name, ShortCutToText(menuitem.ShortCut));
    end;
    ini.Free;
  except
    Result := false;
  end;
end;

end.

