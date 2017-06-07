object frmDMOParamEQ: TfrmDMOParamEQ
  Left = 189
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' DMO Param EQ'
  ClientHeight = 161
  ClientWidth = 375
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
    Top = 128
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
    Caption = 'Center'
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
    Width = 50
    Height = 13
    Caption = 'Bandwidth'
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
    Width = 22
    Height = 13
    Caption = 'Gain'
  end
  object Label7: TLabel
    Left = 320
    Top = 93
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object tbCenter: TTrackBar
    Left = 80
    Top = 8
    Width = 225
    Height = 33
    Max = 160000
    Min = 800
    Frequency = 20
    Position = 800
    TabOrder = 0
    OnChange = tbCenterChange
  end
  object tbBandwidth: TTrackBar
    Left = 80
    Top = 48
    Width = 225
    Height = 33
    Max = 360
    Min = 10
    Frequency = 8
    Position = 10
    TabOrder = 1
    OnChange = tbBandwidthChange
  end
  object tbGain: TTrackBar
    Left = 80
    Top = 88
    Width = 225
    Height = 33
    Max = 150
    Min = -150
    Frequency = 8
    Position = 5
    TabOrder = 2
    OnChange = tbGainChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 136
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 3
    OnClick = chkEnabledClick
  end
end
