object frmDSPTempo: TfrmDSPTempo
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Tempo'
  ClientHeight = 79
  ClientWidth = 355
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
    Width = 33
    Height = 13
    Caption = 'Tempo'
  end
  object Label13: TLabel
    Left = 319
    Top = 13
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = '1.00x'
  end
  object tbTempo: TTrackBar
    Left = 72
    Top = 8
    Width = 225
    Height = 33
    Max = 8000
    Min = 50
    Frequency = 200
    Position = 1000
    TabOrder = 0
    OnChange = tbTempoChange
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
