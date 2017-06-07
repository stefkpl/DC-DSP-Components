object frmDSPPitchShift: TfrmDSPPitchShift
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Pitch Shift'
  ClientHeight = 80
  ClientWidth = 353
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
    Width = 24
    Height = 13
    Caption = 'Pitch'
  end
  object Label13: TLabel
    Left = 319
    Top = 13
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = '1.00x'
  end
  object tbPitchShift: TTrackBar
    Left = 72
    Top = 8
    Width = 225
    Height = 33
    Max = 3000
    Min = 10
    Frequency = 200
    Position = 1000
    TabOrder = 0
    OnChange = tbPitchShiftChange
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
