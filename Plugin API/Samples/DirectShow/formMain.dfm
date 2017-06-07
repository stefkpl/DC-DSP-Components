object frmMain: TfrmMain
  Left = 213
  Top = 167
  Width = 540
  Height = 480
  Caption = 'DC-DSP Filter - Plugin API Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlVideo: TPanel
    Left = 56
    Top = 16
    Width = 297
    Height = 137
    BevelOuter = bvNone
    Color = clBlack
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnMouseDown = pnlVideoMouseDown
    OnMouseMove = pnlVideoMouseMove
    OnMouseUp = pnlVideoMouseUp
    OnResize = pnlVideoResize
  end
  object pnlControls: TPanel
    Left = 0
    Top = 375
    Width = 532
    Height = 78
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      532
      78)
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Play'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Pause'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 168
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button5: TButton
      Left = 248
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 448
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Plugins'
      TabOrder = 4
      OnClick = Button6Click
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 40
      Width = 521
      Height = 32
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = TrackBar1Change
    end
  end
  object OpenDialog: TOpenDialog
    Left = 8
    Top = 8
  end
end
