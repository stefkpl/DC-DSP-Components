object frmDSPPhaseInvert: TfrmDSPPhaseInvert
  Left = 365
  Top = 250
  BorderStyle = bsNone
  Caption = 'frmDSPPhaseInvert'
  ClientHeight = 458
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
      Caption = ' Phase Invert '
      TabOrder = 0
      object Label1: TLabel
        Left = 58
        Top = 39
        Width = 58
        Height = 13
        Caption = 'Channel 0 : '
      end
      object Label2: TLabel
        Left = 58
        Top = 55
        Width = 58
        Height = 13
        Caption = 'Channel 1 : '
      end
      object Label3: TLabel
        Left = 58
        Top = 71
        Width = 58
        Height = 13
        Caption = 'Channel 2 : '
      end
      object Label4: TLabel
        Left = 58
        Top = 87
        Width = 58
        Height = 13
        Caption = 'Channel 3 : '
      end
      object Label6: TLabel
        Left = 58
        Top = 103
        Width = 58
        Height = 13
        Caption = 'Channel 4 : '
      end
      object Label7: TLabel
        Left = 210
        Top = 39
        Width = 58
        Height = 13
        Caption = 'Channel 5 : '
      end
      object Label8: TLabel
        Left = 210
        Top = 55
        Width = 58
        Height = 13
        Caption = 'Channel 6 : '
      end
      object Label9: TLabel
        Left = 210
        Top = 71
        Width = 58
        Height = 13
        Caption = 'Channel 7 : '
      end
      object Label10: TLabel
        Left = 210
        Top = 87
        Width = 58
        Height = 13
        Caption = 'Channel 8 : '
      end
      object Label11: TLabel
        Left = 210
        Top = 103
        Width = 58
        Height = 13
        Caption = 'Channel 9 : '
      end
      object ch0: TCheckBox
        Left = 116
        Top = 38
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 0
        OnClick = chkPhaseInvEnabledClick
      end
      object ch1: TCheckBox
        Tag = 1
        Left = 116
        Top = 54
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 1
        OnClick = chkPhaseInvEnabledClick
      end
      object ch2: TCheckBox
        Tag = 2
        Left = 116
        Top = 70
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 2
        OnClick = chkPhaseInvEnabledClick
      end
      object ch3: TCheckBox
        Tag = 3
        Left = 116
        Top = 86
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 3
        OnClick = chkPhaseInvEnabledClick
      end
      object ch4: TCheckBox
        Tag = 4
        Left = 116
        Top = 102
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 4
        OnClick = chkPhaseInvEnabledClick
      end
      object ch5: TCheckBox
        Tag = 5
        Left = 268
        Top = 38
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 5
        OnClick = chkPhaseInvEnabledClick
      end
      object ch6: TCheckBox
        Tag = 6
        Left = 268
        Top = 54
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 6
        OnClick = chkPhaseInvEnabledClick
      end
      object ch7: TCheckBox
        Tag = 7
        Left = 268
        Top = 70
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 7
        OnClick = chkPhaseInvEnabledClick
      end
      object ch8: TCheckBox
        Tag = 8
        Left = 268
        Top = 86
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 8
        OnClick = chkPhaseInvEnabledClick
      end
      object ch9: TCheckBox
        Tag = 9
        Left = 268
        Top = 102
        Width = 53
        Height = 17
        Caption = 'Invert'
        TabOrder = 9
        OnClick = chkPhaseInvEnabledClick
      end
    end
  end
end
