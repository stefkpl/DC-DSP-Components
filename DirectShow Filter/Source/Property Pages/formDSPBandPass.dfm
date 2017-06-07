object frmDSPBandPass: TfrmDSPBandPass
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDSPBandPass'
  ClientHeight = 452
  ClientWidth = 791
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
      Caption = ' Band Pass '
      TabOrder = 0
      object Label10: TLabel
        Left = 8
        Top = 135
        Width = 42
        Height = 13
        Caption = 'Channel '
      end
      object Label1: TLabel
        Left = 8
        Top = 14
        Width = 73
        Height = 13
        Caption = 'Low Frequency'
      end
      object Label3: TLabel
        Left = 264
        Top = 75
        Width = 104
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label4: TLabel
        Left = 8
        Top = 74
        Width = 75
        Height = 13
        Caption = 'High Frequency'
      end
      object Label2: TLabel
        Left = 264
        Top = 16
        Width = 104
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object tbLow: TTrackBar
        Left = 3
        Top = 28
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        ParentShowHint = False
        Frequency = 250
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbLowChange
      end
      object cmbBandPassChannels: TComboBox
        Left = 56
        Top = 131
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
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
        Left = 3
        Top = 88
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        ParentShowHint = False
        Frequency = 250
        Position = 10000
        ShowHint = False
        TabOrder = 2
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbLowChange
      end
    end
  end
end
