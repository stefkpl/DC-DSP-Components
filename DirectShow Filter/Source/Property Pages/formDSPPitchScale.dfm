object frmDSPPitchScale: TfrmDSPPitchScale
  Left = 263
  Top = 165
  BorderStyle = bsNone
  Caption = 'frmDSPPitchScale'
  ClientHeight = 478
  ClientWidth = 716
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
      Caption = ' Pitch Scale '
      TabOrder = 0
      object Label13: TLabel
        Left = 335
        Top = 15
        Width = 37
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object Label6: TLabel
        Left = 10
        Top = 15
        Width = 23
        Height = 13
        Caption = 'Pitch'
      end
      object Label1: TLabel
        Left = 8
        Top = 80
        Width = 40
        Height = 13
        Caption = 'FFT Size'
      end
      object Label2: TLabel
        Left = 264
        Top = 80
        Width = 34
        Height = 13
        Caption = 'Quality'
      end
      object tbPitchScale: TTrackBar
        Left = 3
        Top = 29
        Width = 374
        Height = 41
        Hint = '0 %'
        Max = 2000
        Min = 500
        ParentShowHint = False
        Frequency = 36
        Position = 500
        ShowHint = False
        TabOrder = 0
        ThumbLength = 22
        TickMarks = tmBoth
        OnChange = tbPitchScaleChange
      end
      object cmbFFT: TComboBox
        Left = 56
        Top = 76
        Width = 81
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
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
        Left = 312
        Top = 76
        Width = 57
        Height = 22
        MaxValue = 50
        MinValue = 1
        TabOrder = 2
        Value = 1
        OnChange = spQualityChange
      end
    end
  end
end
