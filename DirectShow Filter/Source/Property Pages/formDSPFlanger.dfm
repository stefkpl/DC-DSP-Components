object frmDSPFlanger: TfrmDSPFlanger
  Left = 590
  Top = 196
  BorderStyle = bsNone
  Caption = 'frmDSPFlanger'
  ClientHeight = 478
  ClientWidth = 647
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
      Caption = ' Flanger '
      TabOrder = 0
      object Label6: TLabel
        Left = 312
        Top = 15
        Width = 60
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label3: TLabel
        Left = 10
        Top = 15
        Width = 51
        Height = 13
        Caption = 'Frequency'
      end
      object Label8: TLabel
        Left = 312
        Top = 71
        Width = 60
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label5: TLabel
        Left = 10
        Top = 71
        Width = 62
        Height = 13
        Caption = 'Sweeplength'
      end
      object Label15: TLabel
        Left = 276
        Top = 134
        Width = 42
        Height = 13
        Caption = 'Channel '
      end
      object tbFrequency: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        Min = 1
        ParentShowHint = False
        Frequency = 300
        Position = 1
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbFrequencyChange
      end
      object tbDelay: TTrackBar
        Left = 3
        Top = 85
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 100
        Min = 1
        ParentShowHint = False
        Frequency = 3
        Position = 1
        ShowHint = False
        TabOrder = 1
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbDelayChange
      end
      object chkInvertEnabled: TCheckBox
        Left = 10
        Top = 130
        Width = 95
        Height = 17
        Caption = 'Phaseinvert'
        TabOrder = 2
        OnClick = chkInvertEnabledClick
      end
      object cmbChannels: TComboBox
        Left = 322
        Top = 130
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = 'All'
        OnChange = cmbChannelsChange
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
  end
end
