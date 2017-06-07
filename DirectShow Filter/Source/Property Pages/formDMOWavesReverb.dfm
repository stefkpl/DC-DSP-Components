object frmDMOWavesReverb: TfrmDMOWavesReverb
  Left = 313
  Top = 240
  BorderStyle = bsNone
  Caption = 'frmDMOWavesReverb'
  ClientHeight = 483
  ClientWidth = 759
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
      Caption = ' DMO Waves Reverb '
      TabOrder = 0
      object Label3: TLabel
        Left = 248
        Top = 15
        Width = 120
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label10: TLabel
        Left = 10
        Top = 15
        Width = 34
        Height = 13
        Caption = 'In Gain'
      end
      object Label5: TLabel
        Left = 288
        Top = 71
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label12: TLabel
        Left = 10
        Top = 71
        Width = 54
        Height = 13
        Caption = 'Reverb Mix'
      end
      object Label7: TLabel
        Left = 288
        Top = 127
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label14: TLabel
        Left = 10
        Top = 127
        Width = 60
        Height = 13
        Caption = 'Reverb Time'
      end
      object Label9: TLabel
        Left = 288
        Top = 183
        Width = 80
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label16: TLabel
        Left = 10
        Top = 183
        Width = 41
        Height = 13
        Caption = 'HF Ratio'
      end
      object tbInGain: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 0
        Min = -960
        ParentShowHint = False
        Frequency = 20
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbInGainChange
      end
      object tbReverbMix: TTrackBar
        Left = 3
        Top = 85
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 0
        Min = -960
        ParentShowHint = False
        Frequency = 20
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbReverbMixChange
      end
      object tbReverbTime: TTrackBar
        Left = 3
        Top = 141
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 3000000
        Min = 1
        ParentShowHint = False
        Frequency = 200000
        Position = 10
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbReverbTimeChange
      end
      object tbHFRatio: TTrackBar
        Left = 3
        Top = 197
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 999
        Min = 1
        ParentShowHint = False
        Frequency = 22
        Position = 10
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbHFRatioChange
      end
    end
  end
end
