object frmDMOChorus: TfrmDMOChorus
  Left = 279
  Top = 225
  BorderStyle = bsNone
  Caption = 'frmDMOChorus'
  ClientHeight = 481
  ClientWidth = 715
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
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
      Caption = ' DMO Chorus '
      TabOrder = 0
      object PageControl1: TPageControl
        Left = 8
        Top = 16
        Width = 366
        Height = 242
        ActivePage = TabSheet1
        TabOrder = 0
        object TabSheet1: TTabSheet
          Caption = 'Setup 1'
          object Label3: TLabel
            Left = 298
            Top = 31
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label15: TLabel
            Left = 10
            Top = 31
            Width = 60
            Height = 13
            Caption = 'Wet/Dry Mix'
          end
          object Label5: TLabel
            Left = 298
            Top = 87
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label17: TLabel
            Left = 10
            Top = 87
            Width = 29
            Height = 13
            Caption = 'Depth'
          end
          object Label7: TLabel
            Left = 298
            Top = 143
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label19: TLabel
            Left = 10
            Top = 143
            Width = 46
            Height = 13
            Caption = 'Feedback'
          end
          object Label1: TLabel
            Left = 8
            Top = 8
            Width = 60
            Height = 13
            Caption = 'Waveform : '
          end
          object tbWetDryMix: TTrackBar
            Left = 3
            Top = 45
            Width = 350
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
          object tbDepth: TTrackBar
            Left = 3
            Top = 101
            Width = 350
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
            OnChange = tbDepthChange
          end
          object tbFeedback: TTrackBar
            Left = 3
            Top = 157
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 990
            Min = -990
            ParentShowHint = False
            Frequency = 40
            ShowHint = False
            TabOrder = 2
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbFeedbackChange
          end
          object rbSine: TRadioButton
            Left = 128
            Top = 8
            Width = 57
            Height = 17
            Caption = 'Sine'
            TabOrder = 3
            OnClick = rbSineClick
          end
          object rbTriangle: TRadioButton
            Left = 200
            Top = 8
            Width = 89
            Height = 17
            Caption = 'Triangle'
            TabOrder = 4
            OnClick = rbTriangleClick
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Setup 2'
          ImageIndex = 1
          object Label9: TLabel
            Left = 298
            Top = 7
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label21: TLabel
            Left = 10
            Top = 7
            Width = 51
            Height = 13
            Caption = 'Frequency'
          end
          object Label11: TLabel
            Left = 298
            Top = 63
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label23: TLabel
            Left = 10
            Top = 63
            Width = 27
            Height = 13
            Caption = 'Delay'
          end
          object Label24: TLabel
            Left = 10
            Top = 119
            Width = 51
            Height = 13
            Caption = 'LFO Phase'
          end
          object Label13: TLabel
            Left = 298
            Top = 119
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object tbFrequency: TTrackBar
            Left = 3
            Top = 21
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 100
            ParentShowHint = False
            Frequency = 2
            Position = 1
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbFrequencyChange
          end
          object tbDelay: TTrackBar
            Left = 3
            Top = 77
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 200
            ParentShowHint = False
            Frequency = 4
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbDelayChange
          end
          object tbLFOPhase: TTrackBar
            Left = 3
            Top = 133
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 4
            ParentShowHint = False
            ShowHint = False
            TabOrder = 2
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbLFOPhaseChange
          end
        end
      end
    end
  end
end
