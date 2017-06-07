object frmDMOEcho: TfrmDMOEcho
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' DMO Echo'
  ClientHeight = 241
  ClientWidth = 367
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Delay'
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
    Width = 48
    Height = 13
    Caption = 'Feedback'
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
    Caption = 'Left Delay'
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
    Width = 55
    Height = 13
    Caption = 'Right Delay'
  end
  object Label9: TLabel
    Left = 320
    Top = 165
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 216
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 0
    OnClick = chkEnabledClick
  end
  object rbNormalPan: TRadioButton
    Left = 112
    Top = 8
    Width = 81
    Height = 17
    Caption = 'Normal Pan'
    TabOrder = 1
    OnClick = rbNormalPanClick
  end
  object rbSwapChannels: TRadioButton
    Left = 200
    Top = 8
    Width = 105
    Height = 17
    Caption = 'Swap Channels'
    TabOrder = 2
    OnClick = rbSwapChannelsClick
  end
  object tbWetDryMix: TTrackBar
    Left = 80
    Top = 40
    Width = 225
    Height = 33
    Max = 1000
    Frequency = 20
    TabOrder = 3
    OnChange = tbWetDryMixChange
  end
  object tbFeedback: TTrackBar
    Left = 80
    Top = 80
    Width = 225
    Height = 33
    Max = 1000
    Frequency = 20
    TabOrder = 4
    OnChange = tbFeedbackChange
  end
  object tbLeftDelay: TTrackBar
    Left = 80
    Top = 120
    Width = 225
    Height = 33
    Max = 20000
    Min = 10
    Frequency = 40
    Position = 10
    TabOrder = 5
    OnChange = tbLeftDelayChange
  end
  object tbRightDelay: TTrackBar
    Left = 80
    Top = 160
    Width = 225
    Height = 33
    Max = 20000
    Min = 10
    Frequency = 2
    Position = 10
    TabOrder = 6
    OnChange = tbRightDelayChange
  end
end
