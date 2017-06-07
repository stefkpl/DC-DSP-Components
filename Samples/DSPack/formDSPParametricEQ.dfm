object frmDSPParametricEQ: TfrmDSPParametricEQ
  Left = 359
  Top = 249
  BorderStyle = bsToolWindow
  Caption = ' Parametric EQ '
  ClientHeight = 158
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 344
    Top = 17
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '8000 Hz'
  end
  object Label7: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Gain'
  end
  object Label1: TLabel
    Left = 8
    Top = 17
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object Label8: TLabel
    Left = 344
    Top = 56
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 db'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 8
    Height = 13
    Caption = 'Q'
  end
  object Label3: TLabel
    Left = 344
    Top = 96
    Width = 64
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '1.0'
  end
  object Label15: TLabel
    Left = 312
    Top = 132
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object tbFrequency: TTrackBar
    Left = 64
    Top = 5
    Width = 276
    Height = 37
    Max = 2200000
    Min = 2000
    ParentShowHint = False
    Frequency = 8
    Position = 2000
    ShowHint = False
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = tbFrequencyChange
  end
  object tbGain: TTrackBar
    Left = 64
    Top = 43
    Width = 276
    Height = 37
    Max = 2000
    Min = -2000
    ParentShowHint = False
    Frequency = 100
    ShowHint = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = tbFrequencyChange
  end
  object chkParametricEQEnabled: TCheckBox
    Left = 8
    Top = 128
    Width = 63
    Height = 17
    Caption = 'Enabled'
    TabOrder = 2
    OnClick = chkParametricEQEnabledClick
  end
  object tbQ: TTrackBar
    Left = 64
    Top = 83
    Width = 276
    Height = 37
    Max = 1000
    Min = 1
    ParentShowHint = False
    Frequency = 30
    Position = 1
    ShowHint = False
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = tbFrequencyChange
  end
  object cmbParametricEQChannels: TComboBox
    Left = 360
    Top = 126
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'All'
    OnChange = cmbParametricEQChannelsChange
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
end
