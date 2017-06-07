object FormPropWinampVis: TFormPropWinampVis
  Left = 240
  Top = 138
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Winamp Visual'
  ClientHeight = 462
  ClientWidth = 614
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
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 588
    Height = 423
    TabOrder = 0
    object Label3: TLabel
      Left = 96
      Top = 288
      Width = 3
      Height = 13
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 340
      Width = 554
      Height = 65
      Caption = ' Options '
      TabOrder = 0
      object Label2: TLabel
        Left = 416
        Top = 29
        Width = 69
        Height = 13
        Caption = 'Interval (ms) :'
      end
      object SpinEdit1: TSpinEdit
        Left = 490
        Top = 27
        Width = 44
        Height = 22
        MaxValue = 200
        MinValue = 10
        TabOrder = 0
        Value = 10
        OnChange = SpinEdit1Change
      end
      object chkAutostart: TCheckBox
        Left = 16
        Top = 28
        Width = 73
        Height = 17
        Caption = 'Autostart'
        TabOrder = 1
        OnClick = chkAutostartClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 16
      Width = 554
      Height = 67
      Caption = ' Plugins Directory '
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 17
        Top = 26
        Width = 23
        Height = 22
        Hint = 'Delete Window from list'
        Glyph.Data = {
          42020000424D4202000000000000420000002800000010000000100000000100
          1000030000000002000000000000000000000000000000000000007C0000E003
          00001F000000F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          003CFF7FF75EF75EF75EF75E003CFF7FF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75E003C003C003CFF7FF75EF75EF75EF75EF75EF75E003C
          FF7FF75EF75EF75EF75E003C003C003CFF7FF75EF75EF75EF75EF75E003CFF7F
          F75EF75EF75EF75EF75EF75E003C003C003CFF7FF75EF75EF75E003C003CFF7F
          F75EF75EF75EF75EF75EF75EF75E003C003C003CFF7FF75E003C003CFF7FF75E
          F75EF75EF75EF75EF75EF75EF75EF75E003C003C003C003C003CFF7FF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75E003C003C003CFF7FF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75E003C003C003C003C003CFF7FF75EF75E
          F75EF75EF75EF75EF75EF75EF75E003C003C003CFF7FF75E003CFF7FF75EF75E
          F75EF75EF75EF75EF75E003C003C003C003CFF7FF75EF75EF75E003C003CFF7F
          F75EF75EF75EF75E003C003C003C003CFF7FF75EF75EF75EF75EF75E003C003C
          FF7FF75EF75EF75E003C003CFF7FF75EF75EF75EF75EF75EF75EF75EF75E003C
          003CFF7FF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75E}
        ParentShowHint = False
        ShowHint = True
        OnClick = OnDeleteDirectory
      end
      object edPluginPath: TEdit
        Left = 48
        Top = 26
        Width = 401
        Height = 21
        TabStop = False
        ReadOnly = True
        TabOrder = 0
      end
      object Button2: TButton
        Left = 463
        Top = 26
        Width = 75
        Height = 21
        Caption = 'Browse ...'
        TabOrder = 1
        OnClick = SpeedButton1Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 96
      Width = 554
      Height = 233
      Caption = ' Plugins '
      TabOrder = 2
      object lbPlugins: TListBox
        Left = 16
        Top = 24
        Width = 521
        Height = 161
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbPluginsClick
      end
      object cmbPlugins: TComboBox
        Left = 16
        Top = 196
        Width = 273
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbPluginsChange
      end
      object Button1: TButton
        Left = 463
        Top = 196
        Width = 75
        Height = 21
        Caption = 'Configure'
        TabOrder = 2
        OnClick = SpeedButton2Click
      end
      object Button3: TButton
        Left = 384
        Top = 196
        Width = 75
        Height = 21
        Caption = 'Stop'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 304
        Top = 196
        Width = 75
        Height = 21
        Caption = 'Start'
        TabOrder = 4
        OnClick = Button4Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Ctl3D = False
    Filter = 'Winamp Visual Plugins|vis_*.dll'
    Title = 'Choose Plugins Directory ...'
    Left = 568
    Top = 8
  end
end
