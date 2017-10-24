object Form1: TForm1
  Left = 188
  Top = 107
  Width = 665
  Height = 462
  Caption = 'Demo of MoNsTeR'#39's Delphi components'
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
    Width = 473
    Height = 89
    Caption = 'TLogger'
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
      Width = 137
      Height = 21
      TabOrder = 0
      Text = 'sample text'
    end
    object bnLog: TButton
      Left = 46
      Top = 56
      Width = 67
      Height = 25
      Caption = 'Log event'
      TabOrder = 1
      OnClick = bnLogClick
    end
    object MemoShowLog: TMemo
      Left = 200
      Top = 9
      Width = 265
      Height = 73
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object bnLoadLog: TButton
      Left = 120
      Top = 56
      Width = 65
      Height = 25
      Caption = 'Load log'
      TabOrder = 3
      OnClick = bnLoadLogClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 488
    Top = 8
    Width = 161
    Height = 89
    Caption = 'TWWWLabel'
    TabOrder = 1
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 104
    Width = 641
    Height = 329
    Caption = 'TGradientImage'
    TabOrder = 2
    object Image1: TImage
      Left = 352
      Top = 16
      Width = 256
      Height = 80
      Hint = 'Can use TGradientImage to paint function on external canvas'
      ParentShowHint = False
      ShowHint = True
    end
  end
end
