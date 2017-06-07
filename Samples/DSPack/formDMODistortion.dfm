object frmDMODistortion: TfrmDMODistortion
  Left = 229
  Top = 198
  BorderStyle = bsToolWindow
  Caption = ' DMO Distortion'
  ClientHeight = 240
  ClientWidth = 370
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
  object SpeedButton1: TSpeedButton
    Left = 240
    Top = 208
    Width = 121
    Height = 22
    Caption = 'Show Property Page'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 8
    Top = 13
    Width = 22
    Height = 13
    Caption = 'Gain'
  end
  object Label3: TLabel
    Left = 320
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 8
    Top = 53
    Width = 25
    Height = 13
    Caption = 'Edge'
  end
  object Label5: TLabel
    Left = 320
    Top = 53
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label6: TLabel
    Left = 8
    Top = 77
    Width = 57
    Height = 44
    AutoSize = False
    Caption = 'PostEQ Center Frequency'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 320
    Top = 93
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label8: TLabel
    Left = 8
    Top = 125
    Width = 65
    Height = 36
    AutoSize = False
    Caption = 'PostEQ Bandwidth'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 320
    Top = 133
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label10: TLabel
    Left = 8
    Top = 165
    Width = 73
    Height = 36
    AutoSize = False
    Caption = 'PreLowpass Cutoff'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 320
    Top = 173
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object tbGain: TTrackBar
    Left = 80
    Top = 8
    Width = 225
    Height = 33
    Max = 0
    Min = -600
    Frequency = 25
    Position = -76
    TabOrder = 0
    OnChange = tbGainChange
  end
  object tbEdge: TTrackBar
    Left = 80
    Top = 48
    Width = 225
    Height = 33
    Max = 1000
    Frequency = 500
    Position = 1
    TabOrder = 1
    OnChange = tbEdgeChange
  end
  object tbFrequency: TTrackBar
    Left = 80
    Top = 88
    Width = 225
    Height = 33
    Max = 80000
    Min = 1000
    Frequency = 40
    Position = 1000
    TabOrder = 2
    OnChange = tbFrequencyChange
  end
  object tbBandwidth: TTrackBar
    Left = 80
    Top = 128
    Width = 225
    Height = 33
    Max = 80000
    Min = 1000
    Frequency = 13
    Position = 1000
    TabOrder = 3
    OnChange = tbBandwidthChange
  end
  object tbCutoff: TTrackBar
    Left = 80
    Top = 168
    Width = 225
    Height = 33
    Max = 80000
    Min = 1000
    Frequency = 20
    Position = 1000
    TabOrder = 4
    OnChange = tbCutoffChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 216
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 5
    OnClick = chkEnabledClick
  end
end
