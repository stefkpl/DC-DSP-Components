object frmDMOWavesReverb: TfrmDMOWavesReverb
  Left = 189
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' DMO Waves Reverb'
  ClientHeight = 202
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
    Top = 168
    Width = 121
    Height = 22
    Caption = 'Show Property Page'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 8
    Top = 13
    Width = 31
    Height = 13
    Caption = 'InGain'
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
    Width = 54
    Height = 13
    Caption = 'Reverb Mix'
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
    Width = 61
    Height = 13
    Caption = 'Reverb Time'
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
    Width = 42
    Height = 13
    Caption = 'HF Ratio'
  end
  object Label9: TLabel
    Left = 320
    Top = 133
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object tbInGain: TTrackBar
    Left = 80
    Top = 8
    Width = 225
    Height = 33
    Max = 0
    Min = -960
    Frequency = 20
    Position = -66
    TabOrder = 0
    OnChange = tbInGainChange
  end
  object tbReverbMix: TTrackBar
    Left = 80
    Top = 48
    Width = 225
    Height = 33
    Max = 0
    Min = -960
    Frequency = 20
    Position = -66
    TabOrder = 1
    OnChange = tbReverbMixChange
  end
  object tbReverbTime: TTrackBar
    Left = 80
    Top = 88
    Width = 225
    Height = 33
    Max = 3000000
    Min = 1
    Frequency = 50
    Position = 786
    TabOrder = 2
    OnChange = tbReverbTimeChange
  end
  object tbHFRatio: TTrackBar
    Left = 80
    Top = 128
    Width = 225
    Height = 33
    Max = 999
    Min = 1
    Frequency = 20
    Position = 587
    TabOrder = 3
    OnChange = tbHFRatioChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 176
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 4
    OnClick = chkEnabledClick
  end
end
