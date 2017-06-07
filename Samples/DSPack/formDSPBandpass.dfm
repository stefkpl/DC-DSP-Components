object frmDSPBandpass: TfrmDSPBandpass
  Left = 192
  Top = 156
  BorderStyle = bsToolWindow
  Caption = ' Bandpass'
  ClientHeight = 111
  ClientWidth = 394
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
  object Label10: TLabel
    Left = 168
    Top = 84
    Width = 42
    Height = 13
    Caption = 'Channel '
  end
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 20
    Height = 13
    Caption = 'Low'
  end
  object Label3: TLabel
    Left = 272
    Top = 85
    Width = 104
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0 %'
  end
  object Label4: TLabel
    Left = 8
    Top = 42
    Width = 22
    Height = 13
    Caption = 'High'
  end
  object tbLow: TTrackBar
    Left = 44
    Top = 5
    Width = 333
    Height = 37
    Hint = '0 %'
    Max = 10000
    ParentShowHint = False
    Frequency = 250
    ShowHint = False
    TabOrder = 0
    OnChange = tbLowChange
  end
  object chkBandPassEnabled: TCheckBox
    Left = 8
    Top = 82
    Width = 65
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkBandPassEnabledClick
  end
  object cmbBandPassChannels: TComboBox
    Left = 216
    Top = 80
    Width = 49
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'All'
    OnChange = cmbBandPassChannelsChange
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
  object tbHigh: TTrackBar
    Left = 44
    Top = 37
    Width = 333
    Height = 37
    Hint = '0 %'
    Max = 10000
    ParentShowHint = False
    Frequency = 250
    Position = 10000
    ShowHint = False
    TabOrder = 3
    OnChange = tbLowChange
  end
end
