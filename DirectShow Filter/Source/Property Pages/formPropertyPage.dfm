object frmPropertyPage: TfrmPropertyPage
  Left = 197
  Top = 161
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'DC-DSP Filter'
  ClientHeight = 405
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 312
    Width = 521
    Height = 37
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      521
      37)
    object btClose: TButton
      Left = 440
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
      OnClick = btCloseClick
    end
  end
  object pgPages: TPageControl
    Left = 0
    Top = 0
    Width = 521
    Height = 305
    TabOrder = 1
  end
end
