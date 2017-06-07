object FormPropWinamp: TFormPropWinamp
  Left = 241
  Top = 141
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Winamp DSP'
  ClientHeight = 464
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PrintScale = poNone
  Scaled = False
  OnClose = FormClose
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
    object Label2: TLabel
      Left = 96
      Top = 288
      Width = 3
      Height = 13
    end
    object GroupBox1: TGroupBox
      Left = 104
      Top = 340
      Width = 466
      Height = 65
      Caption = ' Auto hide Winamp DSP Windows '
      TabOrder = 0
      object btnDeleteWindow: TSpeedButton
        Left = 401
        Top = 24
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
        OnClick = btnDeleteWindowClick
      end
      object btnChooseWindow: TSpeedButton
        Left = 424
        Top = 24
        Width = 23
        Height = 22
        Hint = 
          'Left click this Button, hold it and move it to'#13#10'that Window that' +
          ' should be Autoclosed.'#13#10'Release the Mousebutton and click on OK'#13 +
          #10'on the next Dialog.'
        Glyph.Data = {
          42020000424D4202000000000000420000002800000010000000100000000100
          1000030000000002000000000000000000000000000000000000007C0000E003
          00001F0000001F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C
          1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C
          1F7C1F7C1F7C000000000000000000001F7C1F7C1F7C1F7C1F7C000000000000
          000000001F7C0000FF7F0000000000001F7C1F7C1F7C1F7C1F7C0000FF7F0000
          000000001F7C0000FF7F0000000000001F7C1F7C1F7C1F7C1F7C0000FF7F0000
          000000001F7C00000000000000000000000000001F7C00000000000000000000
          000000001F7C00000000FF7F000000000000000000000000FF7F000000000000
          000000001F7C00000000FF7F0000000000001F7C00000000FF7F000000000000
          000000001F7C00000000FF7F0000000000001F7C00000000FF7F000000000000
          000000001F7C1F7C000000000000000000000000000000000000000000000000
          00001F7C1F7C1F7C1F7C0000FF7F0000000000001F7C0000FF7F000000000000
          1F7C1F7C1F7C1F7C1F7C000000000000000000001F7C00000000000000000000
          1F7C1F7C1F7C1F7C1F7C1F7C0000000000001F7C1F7C1F7C0000000000001F7C
          1F7C1F7C1F7C1F7C1F7C1F7C0000FF7F00001F7C1F7C1F7C0000FF7F00001F7C
          1F7C1F7C1F7C1F7C1F7C1F7C0000000000001F7C1F7C1F7C0000000000001F7C
          1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C1F7C
          1F7C1F7C1F7C}
        Layout = blGlyphTop
        Margin = 0
        ParentShowHint = False
        ShowHint = True
        OnMouseDown = btnChooseWindowMouseDown
        OnMouseMove = btnChooseWindowMouseMove
        OnMouseUp = btnChooseWindowMouseUp
      end
      object btnShowWindow: TSpeedButton
        Left = 377
        Top = 24
        Width = 23
        Height = 22
        Hint = 'Show the selected Window from the list.'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333333C0C33333333333CC0F0C333333333C30F0F0C3333333C00FFF0F0
          C3333CC0FFFFFF0F0C33C30F0FFFFFF0F0C300FFF0FFFFFF0F0C0FFFFF0FFFFF
          F0F030FFFFF0FFFFFF03330FFFFF0FFF00333330FFFFF000333333330FFF0333
          3333333330003333333333333333333333333333333333333333}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnShowWindowClick
      end
      object btnHideWindow: TSpeedButton
        Left = 353
        Top = 24
        Width = 23
        Height = 22
        Hint = 'Hide the selected Window from the list.'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333C33333333333333C0C333333333333C0F0C3333333333C0FFF0C33
          333333CCC0FFF0C333333CCCCC0FFF0C33334CCCCCC0FFF0C333C4CCCCCC0FFF
          0C333C4CC0CCC0FFF0C333C4CCC0CC0F0C33333C4C00CCC033333333C4CCCCCC
          333333333C4CCCC33333333333C4C33333333333333333333333}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnHideWindowClick
      end
      object cmbWindows: TComboBox
        Left = 16
        Top = 25
        Width = 329
        Height = 21
        Hint = 
          'This is used to automatically close unused Winamp2 '#13#10'DSP Plugin ' +
          'Windows during rendering a Mediafile.'
        Style = csDropDownList
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
    object chkPluginEnabled: TCheckBox
      Left = 24
      Top = 367
      Width = 65
      Height = 17
      Caption = 'Enabled'
      TabOrder = 1
      OnClick = chkPluginEnabledClick
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 16
      Width = 554
      Height = 67
      Caption = ' Plugins Directory '
      TabOrder = 2
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
        OnClick = OnDeleteKlick
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
        OnClick = btnSetDirectoryExecute
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 96
      Width = 554
      Height = 233
      Caption = ' Plugins '
      TabOrder = 3
      object lbPlugins: TListBox
        Left = 16
        Top = 24
        Width = 521
        Height = 161
        ItemHeight = 13
        Items.Strings = (
          'None')
        TabOrder = 0
        OnClick = lbPluginsClick
      end
      object cmbPlugins: TComboBox
        Left = 16
        Top = 196
        Width = 433
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbPluginsChange
      end
      object Button1: TButton
        Left = 462
        Top = 196
        Width = 75
        Height = 21
        Caption = 'Configure'
        TabOrder = 2
        OnClick = SpeedButton2Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Ctl3D = False
    Filter = 'Winamp DSP Plugins|dsp_*.dll'
    Title = 'Choose Plugins Directory ...'
    Left = 608
    Top = 8
  end
end
