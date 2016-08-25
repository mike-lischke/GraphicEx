object Form1: TForm1
  Left = 537
  Top = 244
  Caption = 'Form1'
  ClientHeight = 416
  ClientWidth = 786
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 81
    Top = 0
    Width = 705
    Height = 416
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 81
    Height = 416
    Align = alLeft
    TabOrder = 0
    object Button1: TButton
      Left = 0
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 64
  end
end
