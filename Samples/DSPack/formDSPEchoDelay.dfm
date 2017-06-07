object frmDSPEchoDelay: TfrmDSPEchoDelay
  Left = 192
  Top = 156
  BorderStyle = bsToolWindow
  Caption = ' Echo/Delay'
  ClientHeight = 118
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 336
    Top = 17
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 ms'
  end
  object Label9: TLabel
    Left = 262
    Top = 89
    Width = 82
    Height = 13
    Caption = 'Number of Echos'
  end
  object Label7: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Gain'
  end
  object Label5: TLabel
    Left = 8
    Top = 17
    Width = 27
    Height = 13
    Caption = 'Delay'
  end
  object Label8: TLabel
    Left = 336
    Top = 56
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object tbDelay: TTrackBar
    Left = 40
    Top = 5
    Width = 297
    Height = 37
    Max = 30000
    ParentShowHint = False
    Frequency = 1000
    ShowHint = False
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = tbDelayChange
  end
  object tbDelayGain: TTrackBar
    Left = 40
    Top = 43
    Width = 297
    Height = 37
    Max = 10000
    ParentShowHint = False
    Frequency = 250
    ShowHint = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = tbDelayGainChange
  end
  object spNumEchos: TSpinEdit
    Left = 349
    Top = 86
    Width = 39
    Height = 22
    EditorEnabled = False
    MaxValue = 10
    MinValue = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    Value = 1
    OnChange = spNumEchosChange
  end
  object chkEchoHighPass: TCheckBox
    Left = 179
    Top = 88
    Width = 70
    Height = 17
    Hint = 'Delays only Frequencys above 500Hz'
    Caption = 'Highpass'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = chkEchoHighPassClick
  end
  object chkKillMain: TCheckBox
    Left = 80
    Top = 88
    Width = 95
    Height = 17
    Hint = 'Only the first Delay is hearable.'
    Caption = 'Kill Main Signal'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = chkKillMainClick
  end
  object chkEchoEnabled: TCheckBox
    Left = 8
    Top = 88
    Width = 63
    Height = 17
    Caption = 'Enabled'
    TabOrder = 5
    OnClick = chkEchoEnabledClick
  end
end
