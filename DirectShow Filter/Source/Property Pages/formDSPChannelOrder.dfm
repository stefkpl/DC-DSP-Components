object frmDSPChannelOrder: TfrmDSPChannelOrder
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDSPChannelOrder'
  ClientHeight = 475
  ClientWidth = 840
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
      Caption = ' Channel Order '
      TabOrder = 0
      object Label8: TLabel
        Left = 11
        Top = 20
        Width = 55
        Height = 13
        Caption = 'Channel In '
      end
      object Label9: TLabel
        Left = 149
        Top = 20
        Width = 60
        Height = 13
        Caption = 'Channel Out'
      end
      object cmbChannelSwitchIn: TComboBox
        Left = 69
        Top = 17
        Width = 43
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbChannelSwitchInChange
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8')
      end
      object cmbChannelSwitchOut: TComboBox
        Left = 213
        Top = 17
        Width = 43
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbChannelSwitchOutChange
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8')
      end
      object sbDefaultChannel: TButton
        Left = 297
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Default'
        TabOrder = 2
        OnClick = sbDefaultChannelClick
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 56
        Width = 353
        Height = 121
        Caption = ' Current Order '
        TabOrder = 3
        object Label1: TLabel
          Left = 14
          Top = 24
          Width = 328
          Height = 79
          AutoSize = False
          WordWrap = True
        end
      end
    end
  end
end
