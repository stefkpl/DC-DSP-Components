object frmDSPAmplify: TfrmDSPAmplify
  Left = 349
  Top = 225
  BorderStyle = bsNone
  Caption = 'frmDSPAmplify'
  ClientHeight = 446
  ClientWidth = 750
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
      Caption = ' Amplify '
      TabOrder = 0
      object Label12: TLabel
        Left = 335
        Top = 15
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '100 %'
      end
      object Label13: TLabel
        Left = 8
        Top = 80
        Width = 45
        Height = 13
        Caption = 'Pitch (%)'
      end
      object Label1: TLabel
        Left = 278
        Top = 80
        Width = 39
        Height = 13
        Caption = 'Channel'
      end
      object Label2: TLabel
        Left = 10
        Top = 15
        Width = 41
        Height = 13
        Caption = 'Volume :'
      end
      object PreAmp: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 1000
        ParentShowHint = False
        Frequency = 20
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = PreAmpChange
      end
      object cmbAmpPitch: TComboBox
        Left = 53
        Top = 77
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
        Left = 323
        Top = 77
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
    end
  end
end
