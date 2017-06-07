object frmDSPEchoDelay: TfrmDSPEchoDelay
  Left = 334
  Top = 192
  BorderStyle = bsNone
  Caption = 'frmDSPEchoDelay'
  ClientHeight = 439
  ClientWidth = 736
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
      Caption = ' Echo/Delay '
      TabOrder = 0
      object label6: TLabel
        Left = 280
        Top = 15
        Width = 92
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label2: TLabel
        Left = 10
        Top = 15
        Width = 27
        Height = 13
        Caption = 'Delay'
      end
      object label8: TLabel
        Left = 288
        Top = 71
        Width = 84
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label3: TLabel
        Left = 10
        Top = 71
        Width = 21
        Height = 13
        Caption = 'Gain'
      end
      object Label9: TLabel
        Left = 219
        Top = 132
        Width = 81
        Height = 13
        Caption = 'Number of Echos'
      end
      object tbDelay: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 30000
        ParentShowHint = False
        Frequency = 20
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDelayChange
      end
      object tbDelayGain: TTrackBar
        Left = 3
        Top = 85
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        ParentShowHint = False
        Frequency = 250
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDelayGainChange
      end
      object spNumEchos: TSpinEdit
        Left = 308
        Top = 129
        Width = 40
        Height = 22
        EditorEnabled = False
        MaxValue = 10
        MinValue = 1
        ParentShowHint = False
        ShowHint = False
        TabOrder = 2
        Value = 1
        OnChange = spNumEchosChange
      end
      object chkEchoHighPass: TCheckBox
        Left = 107
        Top = 131
        Width = 70
        Height = 17
        Hint = 'Delays only Frequencys above 500Hz'
        Caption = 'Highpass'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = chkEchoHighPassClick
      end
      object chkKillMain: TCheckBox
        Left = 8
        Top = 131
        Width = 95
        Height = 17
        Hint = 'Only the first Delay is hearable.'
        Caption = 'Kill Main Signal'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = chkKillMainClick
      end
    end
  end
end
