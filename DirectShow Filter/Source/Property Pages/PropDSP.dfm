object FormPropDSP: TFormPropDSP
  Left = 216
  Top = 119
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'DSP'
  ClientHeight = 439
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 588
    Height = 423
    TabOrder = 0
    object GroupBox2: TGroupBox
      Left = 16
      Top = 16
      Width = 155
      Height = 323
      Caption = ' Filter and Effects '
      TabOrder = 0
      object lbFilters: TCheckListBox
        Left = 15
        Top = 22
        Width = 125
        Height = 285
        OnClickCheck = lbFiltersClickCheck
        BevelWidth = 5
        ItemHeight = 13
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnClick = lbFiltersClick
        OnKeyDown = lbFiltersKeyDown
        OnMouseDown = lbFiltersMouseDown
        OnMouseMove = lbFiltersMouseMove
        OnMouseUp = lbFiltersMouseUp
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 348
      Width = 155
      Height = 57
      Caption = ' Preset '
      TabOrder = 1
      object btnSavePreset: TSpeedButton
        Left = 116
        Top = 20
        Width = 23
        Height = 22
        Hint = 'Quick Save'
        Glyph.Data = {
          42020000424D4202000000000000420000002800000010000000100000000100
          1000030000000002000000000000000000000000000000000000007C0000E003
          00001F000000E001E001E001E001E001E001E001E001E001E001E001E001E001
          E001E001E001E001E00100000000000000000000000000000000000000000000
          00000000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000000000000000
          E03D0000E001E0010000E03DE03DE03DE03DE03DE03DE03DE03DE03DE03DE03D
          E03D0000E001E0010000E03DE03D00000000000000000000000000000000E03D
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          00000000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          F75E0000E001E001000000000000000000000000000000000000000000000000
          00000000E001E001E001E001E001E001E001E001E001E001E001E001E001E001
          E001E001E001}
        Margin = 1
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSavePresetClick
      end
      object cmbPreset: TComboBox
        Left = 15
        Top = 21
        Width = 99
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbPresetChange
      end
    end
    object pnlControlWindow: TPanel
      Left = 182
      Top = 137
      Width = 395
      Height = 280
      BevelOuter = bvNone
      TabOrder = 2
      object GroupBox1: TGroupBox
        Left = 5
        Top = 1
        Width = 383
        Height = 267
        Caption = ' Welcome '
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 20
          Width = 360
          Height = 238
          AutoSize = False
          WordWrap = True
        end
      end
    end
    object GroupBox4: TGroupBox
      Left = 187
      Top = 16
      Width = 383
      Height = 110
      Caption = ' Spectrum Analyzer '
      TabOrder = 3
      object tbFalloff: TTrackBar
        Left = 12
        Top = 16
        Width = 22
        Height = 81
        Hint = 'Spectrum Falloff Speed'
        Max = 21
        Min = 1
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 2
        Position = 5
        ShowHint = True
        TabOrder = 0
        ThumbLength = 14
        OnChange = tbVisualChange
      end
      object pnlVisual: TPanel
        Left = 37
        Top = 19
        Width = 308
        Height = 75
        BevelOuter = bvLowered
        PopupMenu = PopupMenu2
        TabOrder = 1
        object Shape1: TShape
          Left = 1
          Top = 1
          Width = 306
          Height = 73
          Hint = 'Click to Enable Spectrum drawing.'
          Align = alClient
          Brush.Color = clBlack
          ParentShowHint = False
          ShowHint = True
          OnMouseUp = pbVisualMouseUp
        end
        object pbVisual: TPaintBox
          Left = 1
          Top = 1
          Width = 306
          Height = 73
          Hint = 'Click to Disable'
          Align = alClient
          Color = clBlack
          ParentColor = False
          ParentShowHint = False
          PopupMenu = PopupMenu2
          ShowHint = True
          OnMouseUp = pbVisualMouseUp
        end
      end
      object tbVisual: TTrackBar
        Left = 348
        Top = 16
        Width = 22
        Height = 81
        Hint = 'Spectrum Max Amplitude'
        Max = 1000
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 111
        Position = 700
        ShowHint = True
        TabOrder = 2
        ThumbLength = 14
        OnChange = tbVisualChange
      end
    end
  end
  object PopupMenu1: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = PopupMenu1Popup
    Left = 728
    Top = 8
    object Add1: TMenuItem
      Caption = 'Add DSP'
      object Amplify1: TMenuItem
        Caption = 'Amplify'
        OnClick = AddFilter
      end
      object BandPass1: TMenuItem
        Tag = 1
        Caption = 'Band Pass'
        OnClick = AddFilter
      end
      object ChannelOrder1: TMenuItem
        Tag = 2
        Caption = 'Channel Order'
        OnClick = AddFilter
      end
      object Compressor1: TMenuItem
        Tag = 3
        Caption = 'Compressor'
        OnClick = AddFilter
      end
      object DownMix1: TMenuItem
        Tag = 4
        Caption = 'Down Mix'
        OnClick = AddFilter
      end
      object DynamicAmplify1: TMenuItem
        Tag = 5
        Caption = 'Dynamic Amplify'
        OnClick = AddFilter
      end
      object EchoDelay1: TMenuItem
        Tag = 6
        Caption = 'Echo/Delay'
        OnClick = AddFilter
      end
      object Equalizer1: TMenuItem
        Tag = 7
        Caption = 'Equalizer'
        OnClick = AddFilter
      end
      object Flanger2: TMenuItem
        Tag = 8
        Caption = 'Flanger'
        OnClick = AddFilter
      end
      object HighPass1: TMenuItem
        Tag = 9
        Caption = 'High Pass'
        OnClick = AddFilter
      end
      object LowPass1: TMenuItem
        Tag = 10
        Caption = 'Low Pass'
        OnClick = AddFilter
      end
      object Notch1: TMenuItem
        Tag = 11
        Caption = 'Notch'
        OnClick = AddFilter
      end
      object Equalizer21: TMenuItem
        Tag = 29
        Caption = 'Parametric EQ'
        OnClick = AddFilter
      end
      object PhaseInvert1: TMenuItem
        Tag = 12
        Caption = 'Phase Invert'
        OnClick = AddFilter
      end
      object Phaser1: TMenuItem
        Tag = 13
        Caption = 'Phaser'
        OnClick = AddFilter
      end
      object PitchScale1: TMenuItem
        Tag = 14
        Caption = 'Pitch Scale'
        OnClick = AddFilter
      end
      object PitchShift1: TMenuItem
        Tag = 15
        Caption = 'Pitch Shift'
        OnClick = AddFilter
      end
      object Sound3D1: TMenuItem
        Tag = 16
        Caption = 'Sound 3D'
        OnClick = AddFilter
      end
      object empo1: TMenuItem
        Tag = 17
        Caption = 'Tempo'
        OnClick = AddFilter
      end
      object rebleEnhancer1: TMenuItem
        Tag = 18
        Caption = 'Treble Enhancer'
        OnClick = AddFilter
      end
      object rueBass1: TMenuItem
        Tag = 19
        Caption = 'True Bass'
        OnClick = AddFilter
      end
    end
    object AddDMO1: TMenuItem
      Caption = 'Add DMO'
      object Chorus1: TMenuItem
        Tag = 20
        Caption = 'Chorus'
        OnClick = AddFilter
      end
      object Compressor2: TMenuItem
        Tag = 21
        Caption = 'Compressor'
        OnClick = AddFilter
      end
      object Distortion1: TMenuItem
        Tag = 22
        Caption = 'Distortion'
        OnClick = AddFilter
      end
      object Echo1: TMenuItem
        Tag = 23
        Caption = 'Echo'
        OnClick = AddFilter
      end
      object Flanger1: TMenuItem
        Tag = 24
        Caption = 'Flanger'
        OnClick = AddFilter
      end
      object Gargle1: TMenuItem
        Tag = 25
        Caption = 'Gargle'
        OnClick = AddFilter
      end
      object I3DL2Reverb1: TMenuItem
        Tag = 26
        Caption = 'I3DL2 Reverb'
        OnClick = AddFilter
      end
      object ParamEQ1: TMenuItem
        Tag = 27
        Caption = 'Param EQ'
        OnClick = AddFilter
      end
      object WavesReverb1: TMenuItem
        Tag = 28
        Caption = 'Waves Reverb'
        OnClick = AddFilter
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Remove2: TMenuItem
      Caption = 'Remove'
      object Remove1: TMenuItem
        Caption = 'Filter'
        OnClick = Remove1Click
      end
      object RemoveAll1: TMenuItem
        Caption = 'All'
        OnClick = RemoveAll1Click
      end
    end
    object Move1: TMenuItem
      Caption = 'Move'
      object MoveUp1: TMenuItem
        Caption = 'Up'
        OnClick = MoveUp1Click
      end
      object MoveDown1: TMenuItem
        Caption = 'Down'
        OnClick = MoveDown1Click
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object EnableAll1: TMenuItem
      Caption = 'Enable All'
      OnClick = EnableAll1Click
    end
    object DisableAll1: TMenuItem
      Caption = 'Disable All'
      OnClick = DisableAll1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Preset1: TMenuItem
      Caption = 'Preset'
      object Add2: TMenuItem
        Caption = 'Save'
        OnClick = Add2Click
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object LoadAll1: TMenuItem
        Caption = 'Load All Filters'
        OnClick = LoadAll1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object ShowControls1: TMenuItem
      Caption = 'Show Controls'
      OnClick = ShowControls1Click
    end
  end
  object DCSpectrum: TDCSpectrum
    MinY = 0
    MaxY = 71
    MixChannels = True
    OnSpectrumData = DCSpectrumSpectrumData
    FFTSize = fts1024
    PreEmphesis = 92
    PreEmphesisEnabled = True
    SampleSkip = 2
    WindowMode = wmRectangular
    Logarithmic = False
    Left = 728
    Top = 40
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 728
    Top = 72
    object Enabled1: TMenuItem
      Caption = 'Enabled'
      OnClick = Enabled1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Bar1: TMenuItem
      Caption = 'Bar'
      object SetStartColor1: TMenuItem
        Caption = 'Set Top Color'
        OnClick = SetStartColor1Click
      end
      object SetEndColor1: TMenuItem
        Caption = 'Set Bottom Color'
        OnClick = SetEndColor1Click
      end
    end
    object Back1: TMenuItem
      Caption = 'Back'
      object SetTopColor1: TMenuItem
        Caption = 'Set Top Color'
        OnClick = SetTopColor1Click
      end
      object SetBackColor1: TMenuItem
        Caption = 'Set Bottom Color'
        OnClick = SetBackColor1Click
      end
      object SetBackgroundColor1: TMenuItem
        Caption = 'Set Background Color'
        OnClick = SetBackgroundColor1Click
      end
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object SetDefaultStyle1: TMenuItem
      Caption = 'Set Default Style'
      OnClick = SetDefaultStyle1Click
    end
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 728
    Top = 104
  end
  object tmrVisual: TTimer
    Interval = 20
    OnTimer = tmrVisualTimer
    Left = 728
    Top = 136
  end
end
