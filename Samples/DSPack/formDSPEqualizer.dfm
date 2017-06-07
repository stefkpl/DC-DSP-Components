object frmDSPEqualizer: TfrmDSPEqualizer
  Left = 192
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' Equalizer'
  ClientHeight = 195
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 14
    Top = 114
    Width = 12
    Height = 13
    Caption = '60'
  end
  object Label3: TLabel
    Left = 43
    Top = 114
    Width = 18
    Height = 13
    Caption = '170'
  end
  object Label4: TLabel
    Left = 234
    Top = 114
    Width = 18
    Height = 13
    Caption = '12k'
  end
  object Label5: TLabel
    Left = 106
    Top = 114
    Width = 18
    Height = 13
    Caption = '600'
  end
  object Label6: TLabel
    Left = 266
    Top = 114
    Width = 18
    Height = 13
    Caption = '14k'
  end
  object Label7: TLabel
    Left = 74
    Top = 114
    Width = 18
    Height = 13
    Caption = '310'
  end
  object Label8: TLabel
    Left = 142
    Top = 114
    Width = 12
    Height = 13
    Caption = '1k'
  end
  object Label9: TLabel
    Left = 174
    Top = 114
    Width = 12
    Height = 13
    Caption = '3k'
  end
  object Label11: TLabel
    Left = 206
    Top = 114
    Width = 12
    Height = 13
    Caption = '6k'
  end
  object lab0: TLabel
    Left = 2
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab1: TLabel
    Left = 34
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab2: TLabel
    Left = 66
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab3: TLabel
    Left = 98
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab4: TLabel
    Left = 130
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab5: TLabel
    Left = 162
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab6: TLabel
    Left = 194
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab7: TLabel
    Left = 226
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab8: TLabel
    Left = 260
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object lab9: TLabel
    Left = 292
    Top = 6
    Width = 32
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
  end
  object Label1: TLabel
    Left = 8
    Top = 144
    Width = 30
    Height = 13
    Caption = 'Preset'
  end
  object Label10: TLabel
    Left = 224
    Top = 144
    Width = 39
    Height = 13
    Caption = 'Channel'
  end
  object Label12: TLabel
    Left = 301
    Top = 114
    Width = 18
    Height = 13
    Caption = '16k'
  end
  object eq1: TTrackBar
    Left = 34
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 0
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq2: TTrackBar
    Tag = 1
    Left = 66
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 1
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq3: TTrackBar
    Tag = 2
    Left = 98
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 2
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq4: TTrackBar
    Tag = 3
    Left = 130
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 3
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq5: TTrackBar
    Tag = 4
    Left = 162
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 4
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq6: TTrackBar
    Tag = 5
    Left = 194
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 5
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq7: TTrackBar
    Tag = 6
    Left = 226
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 6
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq8: TTrackBar
    Tag = 7
    Left = 258
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 7
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq9: TTrackBar
    Tag = 8
    Left = 292
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 8
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object eq0: TTrackBar
    Tag = 9
    Left = 4
    Top = 17
    Width = 32
    Height = 96
    Hint = '0 %'
    Max = 100
    Min = -100
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 25
    ShowHint = False
    TabOrder = 9
    ThumbLength = 15
    TickMarks = tmBoth
    OnChange = eq0Change
  end
  object cmbEQChannels: TComboBox
    Left = 275
    Top = 142
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    Text = 'All'
    OnChange = cmbEQChannelsChange
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
  object chkEqualizerEnabled: TCheckBox
    Left = 9
    Top = 170
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 11
    OnClick = chkEqualizerEnabledClick
  end
  object cmbEqualizerPreset: TComboBox
    Left = 58
    Top = 143
    Width = 116
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 12
    OnChange = cmbEqualizerPresetChange
  end
end
