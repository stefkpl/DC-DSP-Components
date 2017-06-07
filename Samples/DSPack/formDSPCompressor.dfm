object frmDSPCompressor: TfrmDSPCompressor
  Left = 192
  Top = 156
  BorderStyle = bsToolWindow
  Caption = ' Compressor'
  ClientHeight = 230
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 336
    Top = 17
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label7: TLabel
    Left = 8
    Top = 56
    Width = 57
    Height = 13
    Caption = 'Decay Time'
  end
  object Label1: TLabel
    Left = 8
    Top = 17
    Width = 57
    Height = 13
    Caption = 'Attack Time'
  end
  object Label8: TLabel
    Left = 336
    Top = 56
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 47
    Height = 13
    Caption = 'Threshold'
  end
  object Label3: TLabel
    Left = 336
    Top = 96
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label9: TLabel
    Left = 8
    Top = 136
    Width = 25
    Height = 13
    Caption = 'Ratio'
  end
  object Label10: TLabel
    Left = 336
    Top = 136
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label11: TLabel
    Left = 8
    Top = 176
    Width = 22
    Height = 13
    Caption = 'Gain'
  end
  object Label12: TLabel
    Left = 336
    Top = 176
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object label13: TLabel
    Left = 204
    Top = 208
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
  end
  object label14: TLabel
    Left = 264
    Top = 208
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
  end
  object label15: TLabel
    Left = 324
    Top = 208
    Width = 60
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
  end
  object tbAttack: TTrackBar
    Left = 72
    Top = 5
    Width = 265
    Height = 37
    Max = 1000
    Min = 1
    ParentShowHint = False
    Frequency = 100
    Position = 100
    ShowHint = False
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = tbAttackChange
  end
  object tbDecay: TTrackBar
    Left = 72
    Top = 43
    Width = 265
    Height = 37
    Max = 3000
    Min = 10
    ParentShowHint = False
    Frequency = 100
    Position = 1000
    ShowHint = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = tbDecayChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 208
    Width = 63
    Height = 17
    Caption = 'Enabled'
    TabOrder = 2
    OnClick = chkEnabledClick
  end
  object tbThreshold: TTrackBar
    Left = 72
    Top = 83
    Width = 265
    Height = 37
    Max = 0
    Min = -2000
    ParentShowHint = False
    Frequency = 300
    ShowHint = False
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = tbThresholdChange
  end
  object tbRatio: TTrackBar
    Left = 72
    Top = 123
    Width = 265
    Height = 37
    Max = 1000
    Min = 100
    ParentShowHint = False
    Frequency = 60
    Position = 100
    ShowHint = False
    TabOrder = 4
    TickMarks = tmBoth
    OnChange = tbRatioChange
  end
  object tbGain: TTrackBar
    Left = 72
    Top = 163
    Width = 265
    Height = 37
    Max = 100
    ParentShowHint = False
    Frequency = 35
    ShowHint = False
    TabOrder = 5
    TickMarks = tmBoth
    OnChange = tbGainChange
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 104
    Top = 114
  end
end
