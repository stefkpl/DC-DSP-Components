object frmDSPSound3D: TfrmDSPSound3D
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Sound 3D'
  ClientHeight = 82
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'Volume'
  end
  object Label13: TLabel
    Left = 322
    Top = 13
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = '10 %'
  end
  object tb3DSound: TTrackBar
    Left = 72
    Top = 8
    Width = 225
    Height = 33
    Max = 10000
    Frequency = 200
    Position = 1000
    TabOrder = 0
    OnChange = tb3DSoundChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 56
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkEnabledClick
  end
end
