object frmMain: TfrmMain
  Left = 192
  Top = 160
  Width = 651
  Height = 480
  Caption = 'DC-DSP Filter DirectShow Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlVideo: TPanel
    Left = 8
    Top = 8
    Width = 361
    Height = 177
    Color = clBlack
    TabOrder = 0
    OnResize = pnlVideoResize
  end
  object lbFilters: TListBox
    Left = 8
    Top = 192
    Width = 177
    Height = 97
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = lbFiltersDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 373
    Width = 643
    Height = 73
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      643
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
    object tbVolume: TTrackBar
      Left = 106
      Top = 7
      Width = 127
      Height = 27
      Enabled = False
      Max = 10000
      Frequency = 400
      Position = 10000
      TabOrder = 0
      OnChange = tbVolumeChange
    end
    object tbPan: TTrackBar
      Left = 234
      Top = 7
      Width = 127
      Height = 27
      Enabled = False
      Max = 10000
      Min = -10000
      Frequency = 400
      TabOrder = 1
      OnChange = tbPanChange
    end
    object tbPosition: TTrackBar
      Left = 1
      Top = 35
      Width = 424
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      Max = 100
      Frequency = 0
      TabOrder = 2
      OnChange = tbPositionChange
    end
    object tbAmplify: TTrackBar
      Left = 427
      Top = 1
      Width = 25
      Height = 71
      Anchors = [akTop, akRight]
      Max = 20000
      Orientation = trVertical
      Position = 10000
      TabOrder = 3
      ThumbLength = 17
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = tbAmplifyChange
    end
    object eq0: TTrackBar
      Left = 453
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 4
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq1: TTrackBar
      Left = 469
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 5
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq2: TTrackBar
      Left = 485
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 6
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq3: TTrackBar
      Left = 501
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 7
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq4: TTrackBar
      Left = 517
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 8
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq5: TTrackBar
      Left = 533
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 9
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq6: TTrackBar
      Left = 549
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 10
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq7: TTrackBar
      Left = 565
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 11
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq8: TTrackBar
      Left = 581
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 12
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object eq9: TTrackBar
      Left = 597
      Top = 1
      Width = 17
      Height = 71
      Anchors = [akTop, akRight]
      Max = 100
      Min = -100
      Orientation = trVertical
      TabOrder = 13
      ThumbLength = 11
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = OnEqualizerChange
    end
    object pbVisual: TProgressBar
      Left = 618
      Top = 5
      Width = 17
      Height = 63
      Orientation = pbVertical
      Smooth = True
      TabOrder = 14
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 376
    Top = 8
  end
  object tmrVisual: TTimer
    Enabled = False
    Interval = 40
    OnTimer = tmrVisualTimer
    Left = 376
    Top = 40
  end
  object DCWaveform: TDCWaveform
    MinY = 0
    MaxY = 200
    NumSamples = 512
    MixChannels = True
    OnWaveformData = DCWaveformWaveformData
    SampleSkip = 1
    Left = 376
    Top = 72
  end
end
