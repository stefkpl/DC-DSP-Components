object frmDMOEcho: TfrmDMOEcho
  Left = 223
  Top = 131
  BorderStyle = bsNone
  Caption = 'frmDMOEcho'
  ClientHeight = 462
  ClientWidth = 767
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
      Caption = ' DMO Echo '
      TabOrder = 0
      object Label3: TLabel
        Left = 314
        Top = 38
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label15: TLabel
        Left = 10
        Top = 38
        Width = 60
        Height = 13
        Caption = 'Wet/Dry Mix'
      end
      object Label5: TLabel
        Left = 314
        Top = 94
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label17: TLabel
        Left = 10
        Top = 94
        Width = 46
        Height = 13
        Caption = 'Feedback'
      end
      object Label7: TLabel
        Left = 314
        Top = 150
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label19: TLabel
        Left = 10
        Top = 150
        Width = 49
        Height = 13
        Caption = 'Left Delay'
      end
      object Label9: TLabel
        Left = 314
        Top = 206
        Width = 52
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label21: TLabel
        Left = 10
        Top = 206
        Width = 55
        Height = 13
        Caption = 'Right Delay'
      end
      object Label10: TLabel
        Left = 8
        Top = 17
        Width = 34
        Height = 13
        Caption = 'Delay :'
      end
      object tbWetDryMix: TTrackBar
        Left = 3
        Top = 52
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 1000
        ParentShowHint = False
        Frequency = 20
        Position = 10
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbWetDryMixChange
      end
      object tbFeedback: TTrackBar
        Left = 3
        Top = 108
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 1000
        ParentShowHint = False
        Frequency = 20
        Position = 10
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbFeedbackChange
      end
      object tbLeftDelay: TTrackBar
        Left = 3
        Top = 164
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 20000
        Min = 10
        ParentShowHint = False
        Frequency = 40
        Position = 10
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbLeftDelayChange
      end
      object tbRightDelay: TTrackBar
        Left = 3
        Top = 220
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 20000
        Min = 10
        ParentShowHint = False
        Frequency = 2
        Position = 10
        ShowHint = False
        TabOrder = 3
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbRightDelayChange
      end
      object rbNormalPan: TRadioButton
        Left = 112
        Top = 16
        Width = 81
        Height = 17
        Caption = 'Normal Pan'
        TabOrder = 4
        OnClick = rbNormalPanClick
      end
      object rbSwapChannels: TRadioButton
        Left = 200
        Top = 16
        Width = 105
        Height = 17
        Caption = 'Swap Channels'
        TabOrder = 5
        OnClick = rbSwapChannelsClick
      end
    end
  end
end
