object frmDSPNotch: TfrmDSPNotch
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDSPNotch'
  ClientHeight = 474
  ClientWidth = 743
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
      Caption = ' Notch '
      TabOrder = 0
      object Label2: TLabel
        Left = 288
        Top = 15
        Width = 84
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label3: TLabel
        Left = 278
        Top = 80
        Width = 39
        Height = 13
        Caption = 'Channel'
      end
      object Label4: TLabel
        Left = 10
        Top = 15
        Width = 31
        Height = 13
        Caption = 'Cutoff'
      end
      object tbNotch: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 10000
        ParentShowHint = False
        Frequency = 200
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbNotchChange
      end
      object cmbChannels: TComboBox
        Left = 323
        Top = 77
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
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
