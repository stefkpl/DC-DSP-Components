object frmDMOI3DL2Reverb: TfrmDMOI3DL2Reverb
  Left = 310
  Top = 196
  VertScrollBar.Style = ssFlat
  VertScrollBar.Tracking = True
  BorderStyle = bsNone
  Caption = 'frmDMOI3DL2Reverb'
  ClientHeight = 413
  ClientWidth = 749
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
      Caption = ' DMO I3DL2 Reverb '
      TabOrder = 0
      object Label27: TLabel
        Left = 16
        Top = 242
        Width = 31
        Height = 13
        Caption = 'Preset'
      end
      object cmbPreset: TComboBox
        Left = 60
        Top = 238
        Width = 265
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbPresetChange
      end
      object PageControl1: TPageControl
        Left = 8
        Top = 16
        Width = 366
        Height = 215
        ActivePage = TabSheet1
        TabOrder = 1
        object TabSheet1: TTabSheet
          Caption = 'Setup 1'
          object Label3: TLabel
            Left = 298
            Top = 7
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label42: TLabel
            Left = 10
            Top = 7
            Width = 27
            Height = 13
            Caption = 'Room'
          end
          object Label5: TLabel
            Left = 298
            Top = 63
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label44: TLabel
            Left = 10
            Top = 63
            Width = 43
            Height = 13
            Caption = 'Room HF'
          end
          object Label7: TLabel
            Left = 298
            Top = 119
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label46: TLabel
            Left = 10
            Top = 119
            Width = 61
            Height = 13
            Caption = 'Room Rolloff'
          end
          object tbRoom: TTrackBar
            Left = 3
            Top = 21
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 0
            Min = -10000
            ParentShowHint = False
            Frequency = 200
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbRoomChange
          end
          object tbRoomHF: TTrackBar
            Left = 3
            Top = 77
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 0
            Min = -10000
            ParentShowHint = False
            Frequency = 200
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbRoomHFChange
          end
          object tbRoomRollof: TTrackBar
            Left = 3
            Top = 133
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 100
            ParentShowHint = False
            Frequency = 2
            ShowHint = False
            TabOrder = 2
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbRoomRollofChange
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
          object Label48: TLabel
            Left = 10
            Top = 7
            Width = 55
            Height = 13
            Caption = 'Decay Time'
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
          object Label50: TLabel
            Left = 10
            Top = 63
            Width = 46
            Height = 13
            Caption = 'Decay HF'
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
          object Label52: TLabel
            Left = 10
            Top = 119
            Width = 53
            Height = 13
            Caption = 'Reflections'
          end
          object tbDecayTime: TTrackBar
            Left = 3
            Top = 21
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 200
            Min = 1
            ParentShowHint = False
            Frequency = 4
            Position = 1
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbDecayTimeChange
          end
          object tbDecayHF: TTrackBar
            Left = 3
            Top = 77
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 200
            Min = 1
            ParentShowHint = False
            Frequency = 4
            Position = 1
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbDecayHFChange
          end
          object tbReflections: TTrackBar
            Left = 4
            Top = 131
            Width = 349
            Height = 41
            Hint = '0 %'
            Max = 1000
            Min = -10000
            ParentShowHint = False
            Frequency = 100
            ShowHint = False
            TabOrder = 2
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbReflectionsChange
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Setup 3'
          ImageIndex = 2
          object Label14: TLabel
            Left = 298
            Top = 7
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label53: TLabel
            Left = 10
            Top = 7
            Width = 64
            Height = 13
            Caption = 'Reflect Delay'
          end
          object Label16: TLabel
            Left = 298
            Top = 63
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label55: TLabel
            Left = 10
            Top = 63
            Width = 35
            Height = 13
            Caption = 'Reverb'
          end
          object Label18: TLabel
            Left = 298
            Top = 119
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label57: TLabel
            Left = 10
            Top = 119
            Width = 65
            Height = 13
            Caption = 'Reverb Delay'
          end
          object tbReflectDelay: TTrackBar
            Left = 3
            Top = 21
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 3000
            ParentShowHint = False
            Frequency = 70
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbReflectDelayChange
          end
          object tbReverb: TTrackBar
            Left = 3
            Top = 77
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 2000
            Min = -10000
            ParentShowHint = False
            Frequency = 8
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbReverbChange
          end
          object tbReverbDelay: TTrackBar
            Left = 3
            Top = 133
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 1000
            ParentShowHint = False
            Frequency = 25
            ShowHint = False
            TabOrder = 2
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbReverbDelayChange
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Setup 4'
          ImageIndex = 3
          object Label20: TLabel
            Left = 298
            Top = 7
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label59: TLabel
            Left = 10
            Top = 7
            Width = 42
            Height = 13
            Caption = 'Diffusion'
          end
          object Label22: TLabel
            Left = 298
            Top = 63
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label61: TLabel
            Left = 10
            Top = 63
            Width = 36
            Height = 13
            Caption = 'Density'
          end
          object tbDiffusion: TTrackBar
            Left = 3
            Top = 21
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 1000
            ParentShowHint = False
            Frequency = 30
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbDiffusionChange
          end
          object tbDensity: TTrackBar
            Left = 3
            Top = 77
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 1000
            ParentShowHint = False
            Frequency = 30
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbDensityChange
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'Setup 5'
          ImageIndex = 4
          object Label24: TLabel
            Left = 297
            Top = 7
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object Label63: TLabel
            Left = 9
            Top = 7
            Width = 66
            Height = 13
            Caption = 'HF Reference'
          end
          object Label26: TLabel
            Left = 297
            Top = 63
            Width = 52
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '0'
          end
          object lanel12: TLabel
            Left = 9
            Top = 63
            Width = 34
            Height = 13
            Caption = 'Quality'
          end
          object tbHFReference: TTrackBar
            Left = 3
            Top = 19
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 200000
            Min = 200
            ParentShowHint = False
            Frequency = 30
            Position = 200
            ShowHint = False
            TabOrder = 0
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbHFReferenceChange
          end
          object tbQuality: TTrackBar
            Left = 3
            Top = 75
            Width = 350
            Height = 41
            Hint = '0 %'
            Max = 3
            ParentShowHint = False
            ShowHint = False
            TabOrder = 1
            ThumbLength = 22
            TickMarks = tmBoth
            OnChange = tbQualityChange
          end
        end
      end
    end
  end
end
