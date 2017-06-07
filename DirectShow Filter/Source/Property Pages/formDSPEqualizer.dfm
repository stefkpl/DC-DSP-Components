object frmDSPEqualizer: TfrmDSPEqualizer
  Left = 271
  Top = 169
  BorderStyle = bsNone
  Caption = 'frmDSPEqualizer'
  ClientHeight = 466
  ClientWidth = 714
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
      Caption = ' Equalizer '
      TabOrder = 0
      object Label2: TLabel
        Left = 18
        Top = 123
        Width = 12
        Height = 13
        Caption = '60'
      end
      object Label3: TLabel
        Left = 51
        Top = 123
        Width = 18
        Height = 13
        Caption = '170'
      end
      object Label4: TLabel
        Left = 274
        Top = 123
        Width = 17
        Height = 13
        Caption = '12k'
      end
      object Label5: TLabel
        Left = 126
        Top = 123
        Width = 18
        Height = 13
        Caption = '600'
      end
      object Label6: TLabel
        Left = 311
        Top = 123
        Width = 17
        Height = 13
        Caption = '14k'
      end
      object Label7: TLabel
        Left = 89
        Top = 123
        Width = 18
        Height = 13
        Caption = '310'
      end
      object Label8: TLabel
        Left = 167
        Top = 123
        Width = 11
        Height = 13
        Caption = '1k'
      end
      object Label9: TLabel
        Left = 206
        Top = 123
        Width = 11
        Height = 13
        Caption = '3k'
      end
      object Label11: TLabel
        Left = 242
        Top = 123
        Width = 11
        Height = 13
        Caption = '6k'
      end
      object lab0: TLabel
        Left = 7
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab1: TLabel
        Left = 43
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab2: TLabel
        Left = 80
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab3: TLabel
        Left = 117
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab4: TLabel
        Left = 155
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab5: TLabel
        Left = 193
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab6: TLabel
        Left = 229
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab7: TLabel
        Left = 265
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab8: TLabel
        Left = 301
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object lab9: TLabel
        Left = 337
        Top = 14
        Width = 32
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
      end
      object Label1: TLabel
        Left = 9
        Top = 154
        Width = 31
        Height = 13
        Caption = 'Preset'
      end
      object Label10: TLabel
        Left = 272
        Top = 185
        Width = 39
        Height = 13
        Caption = 'Channel'
      end
      object Label12: TLabel
        Left = 347
        Top = 123
        Width = 17
        Height = 13
        Caption = '16k'
      end
      object Label13: TLabel
        Left = 258
        Top = 217
        Width = 54
        Height = 13
        Caption = 'Range (db)'
      end
      object btnResetPreset: TSpeedButton
        Left = 347
        Top = 151
        Width = 23
        Height = 22
        Hint = 'Reset all Presets'
        Glyph.Data = {
          42020000424D4202000000000000420000002800000010000000100000000100
          1000030000000002000000000000000000000000000000000000007C0000E003
          00001F000000F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EEF3D0F000F000F000F000F00EF3DF75EF75E
          F75EF75EF75EF75EF75EF75E0F000F000F000F000F000F000F000F000F00F75E
          F75EF75EF75EF75EF75E0F000F000F00F75EF75EF75EF75EF75E0F000F000F00
          F75EF75EF75EF75EEF3D0F000F00F75EF75EF75EF75EF75EF75EF75E0F000F00
          EF3DF75EF75EF75E0F000F00F75EF75EF75EF75EE001E001F75EF75EEF3D0F00
          0F00F75EF75EF75E0F000F00F75EF75EF75EE001E001E001E001F75EF75E0F00
          0F00F75EF75EF75E0F000F00F75EF75EF75EE001E001E001E001F75EF75E0F00
          0F00F75EF75EF75E0F000F00F75EF75EF75EF75EE001E001F75EF75EF75EF75E
          F75EF75EF75EF75E0F000F00EF3DF75EF75EF75EF75EF75EF75EF75EF75E0F00
          0F00F75EF75EF75EEF3D0F000F00F75EF75E0F00F75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75E0F000F000F00F75E0F000F00F75EF75EF75E0F00EF3D
          F75EF75EF75EF75EF75EF75E0F000F000F000F000F000F00F75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EEF3D0F000F000F000F00F75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75E0F000F00F75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75E0F00F75EF75EF75EF75EF75EF75E
          F75EF75EF75E}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnResetPresetClick
      end
      object btnDeletePreset: TSpeedButton
        Left = 321
        Top = 151
        Width = 23
        Height = 22
        Hint = 'Delete Preset'
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
        OnClick = btnDeletePresetClick
      end
      object btnSavePreset: TSpeedButton
        Left = 295
        Top = 151
        Width = 23
        Height = 22
        Hint = 'Save Preset'
        Glyph.Data = {
          42020000424D4202000000000000420000002800000010000000100000000100
          1000030000000002000000000000000000000000000000000000007C0000E003
          00001F000000E001E001E001E001E001E001E001E001E001E001E001E001E001
          E001E001E001E001E00100000000000000000000000000000000000000000000
          00000000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000F75EF75E0000
          E03D0000E001E0010000E03DE03D000000000000000000000000000000000000
          E03D0000E001E0010000E03DE03DE03DE03DE03DE03DE03DE03DE03DE03DE03D
          E03D0000E001E0010000E03DE03D00000000000000000000000000000000E03D
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          E03D0000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          00000000E001E0010000E03D0000F75EF75EF75EF75EF75EF75EF75EF75E0000
          F75E0000E001E001000000000000000000000000000000000000000000000000
          00000000E001E001E001E001E001E001E001E001E001E001E001E001E001E001
          E001E001E001}
        Margin = 1
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSavePresetClick
      end
      object eq1: TTrackBar
        Left = 42
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 0
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq2: TTrackBar
        Tag = 1
        Left = 79
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 1
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq3: TTrackBar
        Tag = 2
        Left = 116
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 2
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq4: TTrackBar
        Tag = 3
        Left = 154
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 3
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq5: TTrackBar
        Tag = 4
        Left = 192
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 4
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq6: TTrackBar
        Tag = 5
        Left = 228
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 5
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq7: TTrackBar
        Tag = 6
        Left = 264
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 6
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq8: TTrackBar
        Tag = 7
        Left = 300
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 7
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq9: TTrackBar
        Tag = 8
        Left = 336
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 8
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object eq0: TTrackBar
        Tag = 9
        Left = 6
        Top = 26
        Width = 35
        Height = 96
        Hint = '0 %'
        Max = 100
        Min = -100
        Orientation = trVertical
        ParentShowHint = False
        Frequency = 25
        ShowHint = False
        TabOrder = 9
        ThumbLength = 17
        TickMarks = tmBoth
        OnChange = eq0Change
      end
      object cmbEQChannels: TComboBox
        Left = 323
        Top = 183
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        Text = 'All'
        OnChange = cmbEQChannelsChange
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
      object cmbEqualizerPreset: TComboBox
        Left = 50
        Top = 152
        Width = 239
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        Sorted = True
        TabOrder = 11
        OnChange = cmbEqualizerPresetChange
      end
      object cmbEqualizerMaxDB: TComboBox
        Left = 323
        Top = 215
        Width = 49
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 7
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        Text = '12'
        OnChange = cmbEqualizerMaxDBChange
        Items.Strings = (
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16'
          '17'
          '18'
          '19'
          '20')
      end
    end
  end
end
