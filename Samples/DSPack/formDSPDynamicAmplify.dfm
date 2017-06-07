object frmDSPDynamicAmplify: TfrmDSPDynamicAmplify
  Left = 258
  Top = 237
  BorderStyle = bsToolWindow
  Caption = ' Dynamic Amplify'
  ClientHeight = 144
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 124
    Width = 99
    Height = 13
    Caption = 'Current Amplification:'
  end
  object Label2: TLabel
    Left = 5
    Top = 12
    Width = 88
    Height = 13
    Caption = 'Max Amplification: '
  end
  object Label7: TLabel
    Left = 360
    Top = 12
    Width = 24
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '10.0x'
  end
  object Label3: TLabel
    Left = 360
    Top = 124
    Width = 24
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '10.0x'
  end
  object Label4: TLabel
    Left = 5
    Top = 52
    Width = 31
    Height = 13
    Caption = 'Attack'
  end
  object Label5: TLabel
    Left = 352
    Top = 52
    Width = 32
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '1.0db'
  end
  object Label6: TLabel
    Left = 5
    Top = 92
    Width = 58
    Height = 13
    Caption = 'Releasetime'
  end
  object Label8: TLabel
    Left = 360
    Top = 92
    Width = 24
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '3.0s'
  end
  object tbDynAmp: TTrackBar
    Left = 104
    Top = 5
    Width = 249
    Height = 37
    Hint = '0 %'
    Max = 10000
    Min = 1000
    ParentShowHint = False
    Frequency = 250
    Position = 10000
    ShowHint = False
    TabOrder = 0
    OnChange = tbDynAmpChange
  end
  object chkDynAmp: TCheckBox
    Left = 8
    Top = 122
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkDynAmpClick
  end
  object tbAttack: TTrackBar
    Left = 104
    Top = 45
    Width = 249
    Height = 37
    Hint = '0 %'
    Max = 10000
    Min = 10
    ParentShowHint = False
    Frequency = 250
    Position = 1000
    ShowHint = False
    TabOrder = 2
    OnChange = tbAttackChange
  end
  object tbrelease: TTrackBar
    Left = 104
    Top = 85
    Width = 249
    Height = 37
    Hint = '0 %'
    Max = 10000
    Min = 10
    ParentShowHint = False
    Frequency = 250
    Position = 3000
    ShowHint = False
    TabOrder = 3
    OnChange = tbreleaseChange
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 104
    Top = 114
  end
end
