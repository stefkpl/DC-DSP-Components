object frmDSPPhaser: TfrmDSPPhaser
  Left = 229
  Top = 151
  BorderStyle = bsNone
  Caption = 'frmDSPPhaser'
  ClientHeight = 468
  ClientWidth = 768
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
      Caption = ' Phaser '
      TabOrder = 0
      object Label15: TLabel
        Left = 277
        Top = 241
        Width = 42
        Height = 13
        Caption = 'Channel '
      end
      object Label6: TLabel
        Left = 314
        Top = 23
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label5: TLabel
        Left = 10
        Top = 23
        Width = 60
        Height = 13
        Caption = 'Dry/Wet Mix'
      end
      object Label8: TLabel
        Left = 314
        Top = 63
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label17: TLabel
        Left = 10
        Top = 63
        Width = 29
        Height = 13
        Caption = 'Depth'
      end
      object Label3: TLabel
        Left = 314
        Top = 103
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label19: TLabel
        Left = 10
        Top = 103
        Width = 33
        Height = 13
        Caption = 'Stages'
      end
      object Label10: TLabel
        Left = 314
        Top = 143
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label21: TLabel
        Left = 10
        Top = 143
        Width = 46
        Height = 13
        Caption = 'Feedback'
      end
      object Label23: TLabel
        Left = 10
        Top = 231
        Width = 56
        Height = 13
        Caption = 'Start Phase'
      end
      object Label14: TLabel
        Left = 312
        Top = 183
        Width = 54
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label25: TLabel
        Left = 10
        Top = 183
        Width = 51
        Height = 13
        Caption = 'Frequency'
      end
      object cmbPhaserChannels: TComboBox
        Left = 324
        Top = 238
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'All'
        OnChange = cmbPhaserChannelsChange
        Items.Strings = (
          'All'
          'CH0'
          'CH1'
          'CH2'
          'CH3'
          'CH4'
          'CH5'
          'CH6'
          'CH7'
          'CH8'
          'CH9')
      end
      object tbDryWetMix: TTrackBar
        Left = 80
        Top = 8
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 255
        ParentShowHint = False
        Frequency = 8
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDryWetMixChange
      end
      object tbDepth: TTrackBar
        Left = 80
        Top = 48
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 255
        ParentShowHint = False
        Frequency = 8
        Position = 10
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDryWetMixChange
      end
      object tbStages: TTrackBar
        Left = 80
        Top = 88
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 255
        ParentShowHint = False
        Frequency = 8
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDryWetMixChange
      end
      object tbFeedback: TTrackBar
        Left = 80
        Top = 128
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 100
        Min = 1
        ParentShowHint = False
        Frequency = 4
        Position = 1
        ShowHint = False
        TabOrder = 4
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDryWetMixChange
      end
      object tbFrequency: TTrackBar
        Left = 80
        Top = 168
        Width = 241
        Height = 41
        Hint = '0 %'
        Max = 1000
        Min = 1
        ParentShowHint = False
        Frequency = 30
        Position = 1
        ShowHint = False
        TabOrder = 5
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDryWetMixChange
      end
      object SpinEdit1: TSpinEdit
        Left = 88
        Top = 229
        Width = 81
        Height = 22
        MaxValue = 360
        MinValue = -360
        TabOrder = 6
        Value = 0
        OnChange = SpinEdit1Change
      end
    end
  end
end
