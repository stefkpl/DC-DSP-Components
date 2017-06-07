object frmDMOChorus: TfrmDMOChorus
  Left = 197
  Top = 228
  BorderStyle = bsToolWindow
  Caption = ' DMO Chorus'
  ClientHeight = 320
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
    Top = 288
    Width = 121
    Height = 22
    Caption = 'Show Property Page'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Waveform : '
  end
  object Label2: TLabel
    Left = 8
    Top = 45
    Width = 55
    Height = 13
    Caption = 'WetDry Mix'
  end
  object Label3: TLabel
    Left = 320
    Top = 45
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 8
    Top = 85
    Width = 29
    Height = 13
    Caption = 'Depth'
  end
  object Label5: TLabel
    Left = 320
    Top = 85
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label6: TLabel
    Left = 8
    Top = 125
    Width = 48
    Height = 13
    Caption = 'Feedback'
  end
  object Label7: TLabel
    Left = 320
    Top = 125
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label8: TLabel
    Left = 8
    Top = 165
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object Label9: TLabel
    Left = 320
    Top = 165
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label10: TLabel
    Left = 8
    Top = 205
    Width = 27
    Height = 13
    Caption = 'Delay'
  end
  object Label11: TLabel
    Left = 320
    Top = 205
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label12: TLabel
    Left = 8
    Top = 245
    Width = 53
    Height = 13
    Caption = 'LFO Phase'
  end
  object Label13: TLabel
    Left = 320
    Top = 245
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object rbSine: TRadioButton
    Left = 128
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Sine'
    TabOrder = 0
    OnClick = rbSineClick
  end
  object rbTriangle: TRadioButton
    Left = 200
    Top = 8
    Width = 89
    Height = 17
    Caption = 'Triangle'
    TabOrder = 1
    OnClick = rbTriangleClick
  end
  object tbWetDryMix: TTrackBar
    Left = 80
    Top = 40
    Width = 225
    Height = 33
    Max = 1000
    Frequency = 20
    TabOrder = 2
    OnChange = tbWetDryMixChange
  end
  object tbDepth: TTrackBar
    Left = 80
    Top = 80
    Width = 225
    Height = 33
    Max = 1000
    Frequency = 20
    TabOrder = 3
    OnChange = tbDepthChange
  end
  object tbFeedback: TTrackBar
    Left = 80
    Top = 120
    Width = 225
    Height = 33
    Max = 990
    Min = -990
    Frequency = 40
    TabOrder = 4
    OnChange = tbFeedbackChange
  end
  object tbFrequency: TTrackBar
    Left = 80
    Top = 160
    Width = 225
    Height = 33
    Max = 100
    Frequency = 2
    TabOrder = 5
    OnChange = tbFrequencyChange
  end
  object tbDelay: TTrackBar
    Left = 80
    Top = 200
    Width = 225
    Height = 33
    Max = 200
    Frequency = 4
    TabOrder = 6
    OnChange = tbDelayChange
  end
  object tbLFOPhase: TTrackBar
    Left = 80
    Top = 240
    Width = 225
    Height = 33
    Max = 4
    TabOrder = 7
    OnChange = tbLFOPhaseChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 296
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 8
    OnClick = chkEnabledClick
  end
end
