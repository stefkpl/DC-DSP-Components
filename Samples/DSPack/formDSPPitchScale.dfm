object frmDSPPitchScale: TfrmDSPPitchScale
  Left = 278
  Top = 226
  BorderStyle = bsToolWindow
  Caption = ' Pitch Scale'
  ClientHeight = 79
  ClientWidth = 354
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
  object Label12: TLabel
    Left = 8
    Top = 13
    Width = 24
    Height = 13
    Caption = 'Pitch'
  end
  object Label13: TLabel
    Left = 319
    Top = 13
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = '1.00x'
  end
  object Label1: TLabel
    Left = 96
    Top = 56
    Width = 19
    Height = 13
    Caption = 'FFT'
  end
  object Label2: TLabel
    Left = 240
    Top = 56
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object tbPitchScale: TTrackBar
    Left = 72
    Top = 8
    Width = 225
    Height = 33
    Max = 2000
    Min = 500
    Frequency = 200
    Position = 1000
    TabOrder = 0
    OnChange = tbPitchScaleChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 56
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 1
    OnClick = chkEnabledClick
  end
  object cmbFFT: TComboBox
    Left = 128
    Top = 52
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cmbFFTChange
    Items.Strings = (
      '2'
      '4'
      '8'
      '16'
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object spQuality: TSpinEdit
    Left = 288
    Top = 52
    Width = 57
    Height = 22
    MaxValue = 50
    MinValue = 1
    TabOrder = 3
    Value = 1
    OnChange = spQualityChange
  end
end
