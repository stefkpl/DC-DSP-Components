object frmDSPDynamicAmplify: TfrmDSPDynamicAmplify
  Left = 287
  Top = 174
  BorderStyle = bsNone
  Caption = 'frmDSPDynamicAmplify'
  ClientHeight = 426
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl2: TTabControl
    Left = -10
    Top = -137
    Width = 587
    Height = 423
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 15
      Top = 138
      Width = 383
      Height = 267
      Caption = ' Dynamic Amplify '
      TabOrder = 0
      object label7: TLabel
        Left = 331
        Top = 15
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label10: TLabel
        Left = 10
        Top = 15
        Width = 107
        Height = 13
        Caption = 'Maximum Amplification'
      end
      object label5: TLabel
        Left = 331
        Top = 71
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label11: TLabel
        Left = 10
        Top = 71
        Width = 31
        Height = 13
        Caption = 'Attack'
      end
      object label8: TLabel
        Left = 331
        Top = 127
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label14: TLabel
        Left = 10
        Top = 127
        Width = 58
        Height = 13
        Caption = 'Releasetime'
      end
      object Label1: TLabel
        Left = 231
        Top = 188
        Width = 104
        Height = 13
        Caption = 'Current Amplification:'
      end
      object Label3: TLabel
        Left = 332
        Top = 188
        Width = 33
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '10.0x'
      end
      object tbDynAmp: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 2000
        Min = 100
        ParentShowHint = False
        Frequency = 50
        Position = 1000
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDynAmpChange
      end
      object tbAttack: TTrackBar
        Left = 3
        Top = 85
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        Min = 10
        ParentShowHint = False
        Frequency = 250
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbAttackChange
      end
      object tbRelease: TTrackBar
        Left = 3
        Top = 141
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        Min = 10
        ParentShowHint = False
        Frequency = 250
        Position = 10
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbReleaseChange
      end
    end
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 104
    Top = 114
  end
end
