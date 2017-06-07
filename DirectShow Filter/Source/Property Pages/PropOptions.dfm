object FormPropOptions: TFormPropOptions
  Left = 329
  Top = 177
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Options'
  ClientHeight = 439
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PrintScale = poNone
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 588
    Height = 423
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 554
      Height = 113
      Caption = ' Options '
      TabOrder = 0
      object chkVisuals: TCheckBox
        Left = 19
        Top = 27
        Width = 169
        Height = 17
        Caption = 'Show Visuals before any DSP'
        TabOrder = 0
        OnClick = chkVisualsClick
      end
      object chkInstance: TCheckBox
        Left = 219
        Top = 27
        Width = 177
        Height = 17
        Caption = 'Limit to one Instance per Graph'
        TabOrder = 1
        OnClick = chkInstanceClick
      end
      object chkTrayicon: TCheckBox
        Left = 427
        Top = 27
        Width = 113
        Height = 17
        Caption = 'Show Trayicon'
        TabOrder = 2
        OnClick = chkTrayiconClick
      end
      object cbROT: TCheckBox
        Left = 19
        Top = 51
        Width = 169
        Height = 17
        Caption = 'Enable Running Object Table'
        TabOrder = 3
        OnClick = cbROTClick
      end
      object cbBalloonHint: TCheckBox
        Left = 219
        Top = 51
        Width = 297
        Height = 17
        Caption = 'Show Balloonhint when multiple Audiostreams present'
        TabOrder = 4
        OnClick = cbBalloonHintClick
      end
      object CheckBox1: TCheckBox
        Left = 19
        Top = 75
        Width = 142
        Height = 17
        Caption = 'Enable Visual Buffering'
        TabOrder = 5
        OnClick = CheckBox1Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 144
      Width = 554
      Height = 89
      Caption = ' Audiostream Delay (works only if a Video Stream is present) '
      TabOrder = 1
      object Label1: TLabel
        Left = 481
        Top = 65
        Width = 56
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0.000 sec'
      end
      object Label3: TLabel
        Left = 441
        Top = 65
        Width = 34
        Height = 13
        Caption = 'Delay: '
      end
      object SpeedButton1: TSpeedButton
        Left = 254
        Top = 62
        Width = 44
        Height = 17
        Caption = 'Reset'
        Transparent = False
        OnClick = SpeedButton1Click
      end
      object tbStreamDelay: TTrackBar
        Left = 12
        Top = 25
        Width = 530
        Height = 33
        Hint = '0 %'
        Max = 5000
        Min = -5000
        ParentShowHint = False
        Frequency = 250
        ShowHint = False
        TabOrder = 0
        OnChange = tbStreamDelayChange
      end
      object chkStreamDelay: TCheckBox
        Left = 16
        Top = 62
        Width = 65
        Height = 17
        Caption = 'Enabled'
        TabOrder = 1
        OnClick = chkStreamDelayClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 248
      Width = 554
      Height = 81
      Caption = ' Stream Switching '
      TabOrder = 2
      object chkEnableStreamSwitching: TCheckBox
        Left = 16
        Top = 20
        Width = 65
        Height = 17
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = chkEnableStreamSwitchingClick
      end
      object chkStreamInterface: TCheckBox
        Left = 16
        Top = 36
        Width = 225
        Height = 17
        Caption = 'Disable when only one Stream is present'
        TabOrder = 1
        OnClick = chkStreamInterfaceClick
      end
      object chkForceReconnect: TCheckBox
        Left = 88
        Top = 20
        Width = 145
        Height = 17
        Caption = 'Force Pin reconnect'
        TabOrder = 2
        OnClick = chkForceReconnectClick
      end
      object chkForceStopFilter: TCheckBox
        Left = 16
        Top = 52
        Width = 209
        Height = 17
        Caption = 'Force not used Filter to Stop'
        TabOrder = 3
        OnClick = chkForceStopFilterClick
      end
      object GroupBox5: TGroupBox
        Left = 283
        Top = 16
        Width = 257
        Height = 46
        Caption = ' Audio Stream '
        TabOrder = 4
        object cmbStreams: TComboBox
          Left = 8
          Top = 15
          Width = 241
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cmbStreamsChange
        end
      end
    end
    object GroupBox4: TGroupBox
      Left = 16
      Top = 340
      Width = 554
      Height = 65
      Caption = ' Bitrate Conversion '
      TabOrder = 3
      object Label4: TLabel
        Left = 249
        Top = 27
        Width = 55
        Height = 13
        Caption = 'change to: '
      end
      object chkEnableBitRate: TCheckBox
        Left = 16
        Top = 28
        Width = 65
        Height = 17
        Caption = 'Enabled'
        TabOrder = 0
        OnClick = chkEnableBitRateClick
      end
      object cmbBitRate: TComboBox
        Left = 312
        Top = 23
        Width = 225
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbBitRateChange
        Items.Strings = (
          '8 Bit integer'
          '16 Bit integer'
          '24 Bit integer'
          '32 Bit integer'
          '32 Bit Floating Point')
      end
      object chkBitRateBeforeDSP: TCheckBox
        Left = 88
        Top = 28
        Width = 89
        Height = 17
        Caption = 'before DSP'
        TabOrder = 2
        OnClick = chkBitRateBeforeDSPClick
      end
    end
  end
end
