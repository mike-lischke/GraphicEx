object PropertyDialog: TPropertyDialog
  Left = 376
  Top = 261
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Image properties'
  ClientHeight = 370
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 348
    Top = 336
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 268
    Top = 336
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 200
    Height = 157
    Caption = ' Options: '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object TiledCheckBox: TCheckBox
      Left = 12
      Top = 20
      Width = 153
      Height = 17
      Caption = 'Tiled image'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object EndianCheckBox: TCheckBox
      Left = 12
      Top = 44
      Width = 149
      Height = 17
      Caption = 'Big endian'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object MinCheckBox: TCheckBox
      Left = 12
      Top = 68
      Width = 141
      Height = 17
      Caption = 'White is minimum color'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object ReversedCheckBox: TCheckBox
      Left = 12
      Top = 92
      Width = 133
      Height = 17
      Caption = 'Inversed bit order'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object InterlacedCheckBox: TCheckBox
      Left = 12
      Top = 116
      Width = 133
      Height = 17
      Caption = 'Interlaced image'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 268
    Width = 200
    Height = 57
    Caption = ' Size (width x height): '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object Label1: TLabel
      Left = 92
      Top = 28
      Width = 5
      Height = 13
      Caption = 'x'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object WidthEdit: TEdit
      Left = 12
      Top = 24
      Width = 53
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object HeightEdit: TEdit
      Left = 120
      Top = 24
      Width = 53
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '0'
    end
    object WidthUpDown: TUpDown
      Left = 65
      Top = 24
      Width = 12
      Height = 21
      Associate = WidthEdit
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 2
      Wrap = False
    end
    object HeightUpDown: TUpDown
      Left = 173
      Top = 24
      Width = 12
      Height = 21
      Associate = HeightEdit
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 3
      Wrap = False
    end
  end
  object GroupBox3: TGroupBox
    Left = 220
    Top = 268
    Width = 200
    Height = 57
    Caption = ' Resolution (dpi): '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object Label2: TLabel
      Left = 96
      Top = 28
      Width = 5
      Height = 13
      Caption = 'x'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Edit1: TEdit
      Left = 12
      Top = 24
      Width = 53
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object Edit2: TEdit
      Left = 120
      Top = 24
      Width = 53
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '0'
    end
    object XResUpDown: TUpDown
      Left = 65
      Top = 24
      Width = 12
      Height = 21
      Associate = Edit1
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 2
      Wrap = False
    end
    object YResUpDown: TUpDown
      Left = 173
      Top = 24
      Width = 12
      Height = 21
      Associate = Edit2
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 3
      Wrap = False
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 172
    Width = 201
    Height = 90
    Caption = ' Compression: '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object CompressionComboBox: TComboBox
      Left = 12
      Top = 24
      Width = 177
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      Items.Strings = (
        'No compression'
        'Run length encoding'
        'Packbits (Macintosh RLE)'
        'LZW'
        'CCITT Fax group 3 (T.4, 1d)'
        'CCITT modified Huffman RLE'
        'CCITT Fax group 4 (T.6, 2d)'
        'CCITT modified Huffman (Word alignment)'
        'Deflate (LZ77)'
        'JPEG (JPEG DCT)'
        'OJPEG (old style JPEG)'
        'ThunderScan RLE'
        'NEXT (2-bit RLE)'
        'IT8 CT with padding'
        'IT8 Linework RLE'
        'IT8 Monochrome picture'
        'IT8 Binary line art'
        'Pixarfilm (Pixar companded 10bit LZW)'
        'Pixarlog (Pixar companded 11bit ZIP)'
        'Kodak DCS encoding'
        'ISO JBIG'
        'PCD Huffman encoding')
    end
  end
  object GroupBox5: TGroupBox
    Left = 220
    Top = 8
    Width = 201
    Height = 157
    Caption = ' Colors: '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 39
      Height = 13
      Caption = 'Scheme'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 68
      Width = 74
      Height = 13
      Caption = 'Bits per sample:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 88
      Width = 85
      Height = 13
      Caption = 'Samples per pixel:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 108
      Width = 62
      Height = 13
      Caption = 'Bits per pixel:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object BPSLabel: TLabel
      Left = 184
      Top = 68
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object SPPLabel: TLabel
      Left = 184
      Top = 88
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object BPPLabel: TLabel
      Left = 184
      Top = 108
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DescriptionLabel: TLabel
      Left = 8
      Top = 132
      Width = 95
      Height = 13
      Caption = 'DescriptionLabel'
    end
    object ColorComboBox: TComboBox
      Left = 8
      Top = 36
      Width = 185
      Height = 21
      Style = csDropDownList
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      Items.Strings = (
        'Indexed'
        'Gray'
        'Gray with alpha channel'
        'RGB'
        'RGB with alpha channel'
        'BGR'
        'BGR with alpha channel'
        'CMY'
        'CMYK'
        'CIE L*a*b*'
        'YCbCr'
        'Photo YCC')
    end
  end
  object GroupBox6: TGroupBox
    Left = 220
    Top = 172
    Width = 201
    Height = 90
    Caption = ' Gamma: '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    object FileGammaLabel: TLabel
      Left = 16
      Top = 44
      Width = 56
      Height = 13
      Caption = 'File gamma:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 16
      Top = 64
      Width = 74
      Height = 13
      Caption = 'Display gamma:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DisplayGammaLabel: TLabel
      Left = 157
      Top = 64
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label7'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object FileGammaEdit: TEdit
      Left = 112
      Top = 40
      Width = 77
      Height = 19
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '0'
    end
    object GammaCheckBox: TCheckBox
      Left = 16
      Top = 20
      Width = 97
      Height = 17
      Caption = 'gamma used'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
end
