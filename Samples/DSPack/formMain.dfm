object frmMain: TfrmMain
  Left = 196
  Top = 165
  Width = 725
  Height = 505
  Caption = 'DC-DSP Filter DSPack Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VideoWindow: TVideoWindow
    Left = 48
    Top = 8
    Width = 160
    Height = 120
    FilterGraph = FilterGraph
    VMROptions.Mode = vmrWindowed
    Color = clBlack
  end
  object log: TMemo
    Left = 0
    Top = 386
    Width = 717
    Height = 66
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 452
    Width = 717
    Height = 19
    Panels = <
      item
        Text = '00:00:00:000'
        Width = 100
      end
      item
        Width = 500
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 313
    Width = 717
    Height = 73
    Align = alBottom
    TabOrder = 3
    DesignSize = (
      717
      73)
    object SpeedButton1: TSpeedButton
      Left = 8
      Top = 8
      Width = 23
      Height = 22
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF000000000000000000000000000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
        000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
        000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF000000000000000000000000000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      Layout = blGlyphTop
      Margin = 1
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 32
      Top = 8
      Width = 23
      Height = 22
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00
        FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      Margin = 0
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 56
      Top = 8
      Width = 23
      Height = 22
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      Margin = 0
      OnClick = SpeedButton3Click
    end
    object SpeedButton4: TSpeedButton
      Left = 80
      Top = 8
      Width = 23
      Height = 22
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FF00FFFF00FFFF00FFFF00FF00000000000000000000000000000000
        0000000000000000000000000000000000000000FF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000000000000000000000000000
        0000000000000000000000000000000000000000FF00FFFF00FFFF00FFFF00FF
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FF00FFFF00FFFF00FFFF00FFFF00FF00000000000000000000000000
        0000000000000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF000000000000000000000000000000000000000000000000FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000000000
        0000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
        0000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 399
      Top = 8
      Width = 76
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'DSP Menu'
      OnMouseUp = SpeedButton5MouseUp
    end
    object DSTrackBar: TDSTrackBar
      Left = 1
      Top = 36
      Width = 481
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      Max = 100
      TabOrder = 0
      OnChange = DSTrackBarChange
      FilterGraph = FilterGraph
      TimerInterval = 1
    end
    object SoundLevel: TTrackBar
      Left = 106
      Top = 7
      Width = 127
      Height = 27
      Max = 10000
      Frequency = 400
      TabOrder = 1
      OnChange = SoundLevelChange
    end
    object Panel2: TPanel
      Left = 493
      Top = 8
      Width = 217
      Height = 57
      Anchors = [akTop, akRight]
      BevelOuter = bvLowered
      TabOrder = 2
      object pbVisual: TPaintBox
        Left = 1
        Top = 1
        Width = 215
        Height = 55
        PopupMenu = VisualPopup
        OnClick = pbVisualClick
        OnPaint = pbVisualPaint
      end
    end
    object tbPan: TTrackBar
      Left = 234
      Top = 7
      Width = 127
      Height = 27
      Max = 10000
      Min = -10000
      Frequency = 400
      TabOrder = 3
      OnChange = tbPanChange
    end
  end
  object lbFilters: TListBox
    Left = 549
    Top = 0
    Width = 168
    Height = 313
    Align = alRight
    ItemHeight = 13
    TabOrder = 4
    OnDblClick = lbFiltersDblClick
  end
  object DSPackDCDSPFilter: TDSPackDCDSPFilter
    VisualEnabled = True
    VisualInterval = 25
    VisualAfterDSP = False
    DSPEnabled = True
    DelayEnabled = False
    Delay = 0
    FilterGraph = FilterGraph
    OnVisualData = DSPackDCDSPFilterVisualData
    OnPCMData = DSPackDCDSPFilterPCMData
    OnMediaTypeChanged = DSPackDCDSPFilterMediaTypeChanged
    OnFlush = DSPackDCDSPFilterFlush
    Left = 8
    Top = 40
  end
  object FilterGraph: TFilterGraph
    GraphEdit = False
    LinearVolume = True
    OnDSEvent = FilterGraphDSEvent
    Left = 8
    Top = 8
  end
  object DSPPopup: TPopupMenu
    Left = 8
    Top = 72
    object DMO1: TMenuItem
      Caption = 'DMO'
      object Chorus1: TMenuItem
        Caption = 'Chorus'
        OnClick = Chorus1Click
      end
      object Compressor1: TMenuItem
        Caption = 'Compressor'
        OnClick = Compressor1Click
      end
      object Distortion1: TMenuItem
        Caption = 'Distortion'
        OnClick = Distortion1Click
      end
      object Echo1: TMenuItem
        Caption = 'Echo'
        OnClick = Echo1Click
      end
      object Flanger1: TMenuItem
        Caption = 'Flanger'
        OnClick = Flanger1Click
      end
      object Gargle1: TMenuItem
        Caption = 'Gargle'
        OnClick = Gargle1Click
      end
      object I3DL2Reverb1: TMenuItem
        Caption = 'I3DL2 Reverb'
        OnClick = I3DL2Reverb1Click
      end
      object ParamEQ1: TMenuItem
        Caption = 'Param EQ'
        OnClick = ParamEQ1Click
      end
      object WavesReverb1: TMenuItem
        Caption = 'Waves Reverb'
        OnClick = WavesReverb1Click
      end
    end
    object Internal1: TMenuItem
      Caption = 'Internal'
      object Amplify1: TMenuItem
        Caption = 'Amplify'
        OnClick = Amplify1Click
      end
      object Bandpass1: TMenuItem
        Caption = 'Bandpass'
        OnClick = Bandpass1Click
      end
      object ChannelOrder1: TMenuItem
        Caption = 'Channel Order'
        OnClick = ChannelOrder1Click
      end
      object Compressor2: TMenuItem
        Caption = 'Compressor'
        OnClick = Compressor2Click
      end
      object DownMixMono1: TMenuItem
        Caption = 'Down Mix (Mono)'
        OnClick = DownMixMono1Click
      end
      object DynamicAmplify1: TMenuItem
        Caption = 'Dynamic Amplify'
        OnClick = DynamicAmplify1Click
      end
      object EchoDelay1: TMenuItem
        Caption = 'Echo/Delay'
        OnClick = EchoDelay1Click
      end
      object Equalizer1: TMenuItem
        Caption = 'Equalizer'
        OnClick = Equalizer1Click
      end
      object Flanger2: TMenuItem
        Caption = 'Flanger'
        OnClick = Flanger2Click
      end
      object Highpass1: TMenuItem
        Caption = 'Highpass'
        OnClick = Highpass1Click
      end
      object Lowpass1: TMenuItem
        Caption = 'Lowpass'
        OnClick = Lowpass1Click
      end
      object Notch1: TMenuItem
        Caption = 'Notch'
        OnClick = Notch1Click
      end
      object ParametricEQ1: TMenuItem
        Caption = 'Parametric EQ'
        OnClick = ParametricEQ1Click
      end
      object PhaseInvert1: TMenuItem
        Caption = 'Phase Invert'
        OnClick = PhaseInvert1Click
      end
      object Phaser1: TMenuItem
        Caption = 'Phaser'
        OnClick = Phaser1Click
      end
      object PitchScale1: TMenuItem
        Caption = 'Pitch Scale'
        OnClick = PitchScale1Click
      end
      object PitchShift1: TMenuItem
        Caption = 'Pitch Shift'
        OnClick = PitchShift1Click
      end
      object Sound3D1: TMenuItem
        Caption = 'Sound 3D'
        OnClick = Sound3D1Click
      end
      object empo1: TMenuItem
        Caption = 'Tempo'
        OnClick = empo1Click
      end
      object rebleEnhancer1: TMenuItem
        Caption = 'Treble Enhancer'
        OnClick = rebleEnhancer1Click
      end
      object rueBass1: TMenuItem
        Caption = 'True Bass'
        OnClick = rueBass1Click
      end
    end
    object Winamp21: TMenuItem
      Caption = 'Winamp2'
      object VisualPlugins1: TMenuItem
        Caption = 'Visual Plugins'
        OnClick = VisualPlugins1Click
      end
      object DSPPlugins1: TMenuItem
        Caption = 'DSP Plugins'
        OnClick = DSPPlugins1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 8
    Top = 136
  end
  object DCAmplify: TDCAmplify
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 556
    Top = 5
  end
  object DCBandpass: TDCBandpass
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 556
    Top = 37
  end
  object DCChannelOrder: TDCChannelOrder
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 556
    Top = 69
  end
  object DCDownMix: TDCDownMix
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 556
    Top = 133
  end
  object DCDynamicAmplify: TDCDynamicAmplify
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    AttackTime = 1000
    ReleaseTime = 3000
    MaxAmplification = 10000
    Left = 556
    Top = 165
  end
  object DCEchoDelay: TDCEchoDelay
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    KillMain = False
    NumDelays = 1
    Highpass = False
    DelayAmp = 0
    Delay = 0
    Left = 556
    Top = 197
  end
  object DCEqualizer: TDCEqualizer
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    FFTSize = fts2048
    Left = 556
    Top = 229
  end
  object DCFlanger: TDCFlanger
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 5
  end
  object DCHighpass: TDCHighpass
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 37
  end
  object DCLowpass: TDCLowpass
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 69
  end
  object DCNotch: TDCNotch
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 101
  end
  object DCPhaseInvert: TDCPhaseInvert
    Seperate = True
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 165
  end
  object DCPhaser: TDCPhaser
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 197
  end
  object DCPitchScale: TDCPitchScale
    FFTSize = fts2048
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 229
  end
  object DCPitchShift: TDCPitchShift
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Pitch = 10000
    Left = 620
    Top = 5
  end
  object DCSound3D: TDCSound3D
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Volume = 1000
    Left = 620
    Top = 37
  end
  object DCTempo: TDCTempo
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Tempo = 1000
    Left = 620
    Top = 69
  end
  object DCTrueBass: TDCTrueBass
    Seperate = False
    Enabled = False
    Frequency = 200
    ProcessMessages = False
    SampleSize = 8192
    Left = 620
    Top = 101
  end
  object DCDMOChorus: TDCDMOChorus
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    WetDryMix = 50.000000000000000000
    Depth = 10.000000000000000000
    Feedback = 25.000000000000000000
    Frequency = 1.100000023841858000
    Waveform = 1
    Delay = 16.000000000000000000
    Phase = 3
    Left = 652
    Top = 37
  end
  object DCDMOCompressor: TDCDMOCompressor
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Attack = 10.000000000000000000
    Release = 200.000000000000000000
    Threshold = -20.000000000000000000
    Ratio = 3.000000000000000000
    PreDelay = 4.000000000000000000
    Left = 652
    Top = 69
  end
  object DCDMOEcho: TDCDMOEcho
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    WetDryMix = 50.000000000000000000
    Feedback = 50.000000000000000000
    LeftDelay = 500.000000000000000000
    RightDelay = 500.000000000000000000
    PanDelay = 0
    Left = 652
    Top = 133
  end
  object DCDMOFlanger: TDCDMOFlanger
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    WetDryMix = 50.000000000000000000
    Depth = 100.000000000000000000
    Feedback = -50.000000000000000000
    Frequency = 0.250000000000000000
    Waveform = 1
    Delay = 2.000000000000000000
    Phase = 2
    Left = 652
    Top = 165
  end
  object DCDMOGargle: TDCDMOGargle
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    RateHz = 20
    WaveShape = 0
    Left = 652
    Top = 197
  end
  object DCWAVisualWrapper: TDCWAVisualWrapper
    Plugins.OwnerWindow = 0
    Plugins = <>
    Left = 652
    Top = 5
  end
  object VisualPopup: TPopupMenu
    OnPopup = VisualPopupPopup
    Left = 8
    Top = 104
    object None1: TMenuItem
      Caption = 'None'
      OnClick = None1Click
    end
    object Spectrum1: TMenuItem
      Tag = 1
      Caption = 'Spectrum'
      OnClick = None1Click
    end
    object Waveform1: TMenuItem
      Tag = 2
      Caption = 'Waveform'
      OnClick = None1Click
    end
  end
  object DCDMODistortion: TDCDMODistortion
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Gain = -18.000000000000000000
    Edge = 15.000000000000000000
    PostEQCenterFrequency = 2400.000000000000000000
    PostEQBandwidth = 2400.000000000000000000
    PreLowpassCutoff = 8000.000000000000000000
    Left = 652
    Top = 101
  end
  object DCSpectrum: TDCSpectrum
    MinY = 0
    MaxY = 55
    MixChannels = True
    OnSpectrumData = DCSpectrumSpectrumData
    FFTSize = fts1024
    PreEmphesis = 91
    PreEmphesisEnabled = True
    SampleSkip = 1
    WindowMode = wmRectangular
    Logarithmic = True
    Left = 620
    Top = 165
  end
  object DCWaveform: TDCWaveform
    MinY = 0
    MaxY = 55
    NumSamples = 215
    MixChannels = True
    OnWaveformData = DCWaveformWaveformData
    SampleSkip = 4
    Left = 620
    Top = 197
  end
  object DCWADSPWrapper: TDCWADSPWrapper
    Plugins.OwnerWindow = 0
    Plugins = <>
    Left = 620
    Top = 229
  end
  object DCDMOParamEQ: TDCDMOParamEQ
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Center = 8000.000000000000000000
    Bandwidth = 12.000000000000000000
    Left = 684
    Top = 5
  end
  object DCDMOWavesReverb: TDCDMOWavesReverb
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    ReverbTime = 1000.000000000000000000
    HighFreqRTRatio = 0.001000000047497451
    Left = 684
    Top = 37
  end
  object DCDMOI3DL2Reverb: TDCDMOI3DL2Reverb
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Room = -1000
    RoomHF = -100
    DecayTime = 1.490000009536743000
    DecayHFRatio = 0.829999983310699500
    Reflections = -2602
    ReflectionsDelay = 0.007000000216066837
    Reverb = 200
    ReverbDelay = 0.010999999940395360
    Diffusion = 100.000000000000000000
    Density = 100.000000000000000000
    HFReference = 5000.000000000000000000
    Quality = 2
    Preset = None
    Left = 652
    Top = 229
  end
  object DCCompressor: TDCCompressor
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    AttackTime = 0.150000005960464500
    DecayTime = 0.050000000745058060
    ThresholdDB = -47.930000305175780000
    Ratio = 10.000000000000000000
    GainDB = 40.000000000000000000
    Left = 556
    Top = 101
  end
  object DCTrebleEnhancer: TDCTrebleEnhancer
    Seperate = False
    Enabled = False
    Frequency = 16000
    ProcessMessages = False
    SampleSize = 8192
    Left = 620
    Top = 133
  end
  object DCParametricEQ: TDCParametricEQ
    Seperate = False
    Enabled = False
    ProcessMessages = False
    SampleSize = 8192
    Left = 588
    Top = 133
  end
end
