object frmDMOI3DL2Reverb: TfrmDMOI3DL2Reverb
  Left = 194
  Top = 160
  BorderStyle = bsToolWindow
  Caption = ' DMO I3DL2 Reverb'
  ClientHeight = 319
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 544
    Top = 288
    Width = 121
    Height = 22
    Caption = 'Show Property Page'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 8
    Top = 13
    Width = 28
    Height = 13
    Caption = 'Room'
  end
  object Label3: TLabel
    Left = 264
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 8
    Top = 53
    Width = 45
    Height = 13
    Caption = 'Room HF'
  end
  object Label5: TLabel
    Left = 264
    Top = 53
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label6: TLabel
    Left = 8
    Top = 93
    Width = 58
    Height = 13
    Caption = 'Room Rollof'
  end
  object Label7: TLabel
    Left = 264
    Top = 93
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label8: TLabel
    Left = 8
    Top = 133
    Width = 57
    Height = 13
    Caption = 'Decay Time'
  end
  object Label9: TLabel
    Left = 264
    Top = 133
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label10: TLabel
    Left = 8
    Top = 173
    Width = 48
    Height = 13
    Caption = 'Decay HF'
  end
  object Label11: TLabel
    Left = 264
    Top = 173
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label12: TLabel
    Left = 8
    Top = 213
    Width = 53
    Height = 13
    Caption = 'Reflections'
  end
  object Label13: TLabel
    Left = 264
    Top = 213
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label1: TLabel
    Left = 8
    Top = 253
    Width = 64
    Height = 13
    Caption = 'Reflect Delay'
  end
  object Label14: TLabel
    Left = 264
    Top = 253
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label15: TLabel
    Left = 352
    Top = 13
    Width = 35
    Height = 13
    Caption = 'Reverb'
  end
  object Label16: TLabel
    Left = 608
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label17: TLabel
    Left = 352
    Top = 53
    Width = 65
    Height = 13
    Caption = 'Reverb Delay'
  end
  object Label18: TLabel
    Left = 608
    Top = 53
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label19: TLabel
    Left = 352
    Top = 93
    Width = 41
    Height = 13
    Caption = 'Diffusion'
  end
  object Label20: TLabel
    Left = 608
    Top = 93
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label21: TLabel
    Left = 352
    Top = 133
    Width = 35
    Height = 13
    Caption = 'Density'
  end
  object Label22: TLabel
    Left = 608
    Top = 133
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label23: TLabel
    Left = 352
    Top = 173
    Width = 67
    Height = 13
    Caption = 'HF Reference'
  end
  object Label24: TLabel
    Left = 608
    Top = 173
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label25: TLabel
    Left = 352
    Top = 213
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object Label26: TLabel
    Left = 608
    Top = 213
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label27: TLabel
    Left = 352
    Top = 253
    Width = 30
    Height = 13
    Caption = 'Preset'
  end
  object tbRoom: TTrackBar
    Left = 80
    Top = 8
    Width = 169
    Height = 33
    Max = 0
    Min = -10000
    Frequency = 300
    TabOrder = 0
    OnChange = tbRoomChange
  end
  object tbRoomHF: TTrackBar
    Left = 80
    Top = 48
    Width = 169
    Height = 33
    Max = 0
    Min = -10000
    Frequency = 300
    TabOrder = 1
    OnChange = tbRoomHFChange
  end
  object tbRoomRollof: TTrackBar
    Left = 80
    Top = 88
    Width = 169
    Height = 33
    Max = 100
    Frequency = 40
    Position = 7
    TabOrder = 2
    OnChange = tbRoomRollofChange
  end
  object tbDecayTime: TTrackBar
    Left = 80
    Top = 128
    Width = 169
    Height = 33
    Max = 200
    Min = 1
    Frequency = 6
    Position = 1
    TabOrder = 3
    OnChange = tbDecayTimeChange
  end
  object tbDecayHF: TTrackBar
    Left = 80
    Top = 168
    Width = 169
    Height = 33
    Max = 200
    Min = 1
    Frequency = 6
    Position = 2
    TabOrder = 4
    OnChange = tbDecayHFChange
  end
  object tbReflections: TTrackBar
    Left = 80
    Top = 208
    Width = 169
    Height = 33
    Max = 1000
    Min = -10000
    Position = 7
    TabOrder = 5
    OnChange = tbReflectionsChange
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 296
    Width = 73
    Height = 17
    Caption = 'Enabled'
    TabOrder = 6
    OnClick = chkEnabledClick
  end
  object tbReflectDelay: TTrackBar
    Left = 80
    Top = 248
    Width = 169
    Height = 33
    Max = 3000
    Frequency = 100
    TabOrder = 7
    OnChange = tbReflectDelayChange
  end
  object tbReverb: TTrackBar
    Left = 424
    Top = 8
    Width = 169
    Height = 33
    Max = 2000
    Min = -10000
    Frequency = 20
    Position = 6
    TabOrder = 8
    OnChange = tbReverbChange
  end
  object tbReverbDelay: TTrackBar
    Left = 424
    Top = 48
    Width = 169
    Height = 33
    Max = 1000
    Frequency = 20
    Position = 8
    TabOrder = 9
    OnChange = tbReverbDelayChange
  end
  object tbDiffusion: TTrackBar
    Left = 424
    Top = 88
    Width = 169
    Height = 33
    Max = 1000
    Frequency = 40
    TabOrder = 10
    OnChange = tbDiffusionChange
  end
  object tbDensity: TTrackBar
    Left = 424
    Top = 128
    Width = 169
    Height = 33
    Max = 1000
    Frequency = 2
    TabOrder = 11
    OnChange = tbDensityChange
  end
  object tbHFReference: TTrackBar
    Left = 424
    Top = 168
    Width = 169
    Height = 33
    Max = 200000
    Min = 200
    Frequency = 4
    Position = 200
    TabOrder = 12
    OnChange = tbHFReferenceChange
  end
  object tbQuality: TTrackBar
    Left = 424
    Top = 208
    Width = 169
    Height = 33
    Max = 3
    TabOrder = 13
    OnChange = tbQualityChange
  end
  object cmbPreset: TComboBox
    Left = 432
    Top = 248
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 14
    OnChange = cmbPresetChange
  end
end
