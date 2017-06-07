object frmDSPAmplify: TfrmDSPAmplify
  Left = 860
  Top = 224
  BorderStyle = bsToolWindow
  Caption = ' Amplify'
  ClientHeight = 289
  ClientWidth = 115
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 20
    Top = 88
    Width = 37
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '100 %'
  end
  object Label13: TLabel
    Left = 16
    Top = 46
    Width = 41
    Height = 13
    Caption = 'Pitch (%)'
  end
  object Label1: TLabel
    Left = 16
    Top = 6
    Width = 39
    Height = 13
    Caption = 'Channel'
  end
  object PreAmp: TTrackBar
    Left = 16
    Top = 104
    Width = 41
    Height = 161
    Hint = '0 %'
    Max = 1000
    Orientation = trVertical
    ParentShowHint = False
    Frequency = 250
    ShowHint = False
    TabOrder = 0
    ThumbLength = 22
    TickMarks = tmBoth
    OnChange = PreAmpChange
  end
  object cmbAmpPitch: TComboBox
    Left = 13
    Top = 62
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = '100'
    OnChange = cmbAmpPitchChange
    Items.Strings = (
      '100'
      '200'
      '300'
      '400'
      '500'
      '600'
      '700'
      '800'
      '900'
      '1000')
  end
  object cmbAmpChannels: TComboBox
    Left = 13
    Top = 22
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = 'All'
    OnChange = cmbAmpChannelsChange
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
  object chkPreAmp: TCheckBox
    Left = 6
    Top = 267
    Width = 67
    Height = 17
    Caption = 'Enabled'
    TabOrder = 3
    OnClick = chkPreAmpClick
  end
end
