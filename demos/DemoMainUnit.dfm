object Form1: TForm1
  Left = 188
  Top = 107
  Width = 562
  Height = 291
  Caption = 'Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 105
    Caption = 'GroupBox1'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Event:'
    end
    object EditLog: TEdit
      Left = 48
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'sample text'
    end
    object bnLog: TButton
      Left = 94
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Log event'
      TabOrder = 1
      OnClick = bnLogClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 200
    Top = 8
    Width = 185
    Height = 105
    Caption = 'TWWWLabel'
    TabOrder = 1
  end
end
