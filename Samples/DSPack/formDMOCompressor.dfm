object frmDMOCompressor: TfrmDMOCompressor
  Left = 196
  Top = 203
  BorderStyle = bsToolWindow
  Caption = ' DMO Compressor'
  ClientHeight = 288
  ClientWidth = 369
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
    Top = 256
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
    Width = 31
    Height = 13
    Caption = 'Attack'
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
    Top = 93
    Width = 39
    Height = 13
    Caption = 'Release'
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
    Top = 133
    Width = 47
    Height = 13
    Caption = 'Threshold'
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
    Top = 173
    Width = 25
    Height = 13
    Caption = 'Ratio'
  end
  object Label11: TLabel
    Left = 320
    Top = 173
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label12: TLabel
    Left = 8
    Top = 213
    Width = 46
    Height = 13
    Caption = 'Pre Delay'
  end
  object Label13: TLabel
    Left = 320
    Top = 213
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object tbGain: TTrackBar
    Left = 80
    Top = 8
    Width = 225
    Height = 33
    Max = 600
    Min = -600
    Frequency = 25
    Position = 1
    TabOrder = 0
    OnChange = tbGainChange
  end
  object tbAttack: TTrackBar
    Left = 80
    Top = 48
    Width = 225
    Height = 33
    Max = 50000
    Min = 1
    Frequency = 500
    Position = 1
    TabOrder = 1
    OnChange = tbAttackChange
  end
  object tbRelease: TTrackBar
    Left = 80
    Top = 88
    Width = 225
    Height = 33
    Max = 30000
    Min = 500
    Frequency = 40
    Position = 500
    TabOrder = 2
    OnChange = tbReleaseChange
  end
  object tbThreshold: TTrackBar
    Left = 80
    Top = 128
    Width = 225
    Height = 33
    Max = 0
    Min = -600
    Frequency = 13
    TabOrder = 3
    OnChange = tbThresholdChange
  end
  object tbRatio: TTrackBar
    Left = 80
    Top = 168
    Width = 225
    Height = 33
    Max = 1000
    Min = 10
    Frequency = 20
    Position = 10
    TabOrder = 4
    OnChange = tbRatioChange
  end
  object tbPreDelay: TTrackBar
    Left = 80
    Top = 208
    Width = 225
    Height = 33
    Max = 40
    TabOrder = 5
    OnChange = tbPreDelayChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 264
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 6
    OnClick = chkEnabledClick
  end
end
