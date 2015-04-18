object Form1: TForm1
  Left = 229
  Top = 201
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 854
    Height = 602
    Align = alClient
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Load image'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Save image as...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = 'bmp'
    Left = 56
    Top = 80
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = 'bmp'
    Left = 96
    Top = 80
  end
end
