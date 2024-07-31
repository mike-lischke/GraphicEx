GraphicEx
=========

GraphicEx is an addendum to Delphi's Graphics.pas to enable your application to load many common image formats. This library is primarily designed to load images as background (buttons, forms, toolbars) and textures (DirectX, OpenGL) or for image browsing and editing purposes as long as you don't need to save images. Currently only TTargaGraphic also supports saving an image. GraphicEx is open source under the Mozilla Public License (MPL).

The code here includes some bug fixes and enhancements I have never made available on my homepage. Since I cannot work on this lib any longer I open it up for forks and contributions by others if they are interested.

Image Formats
============

    TIFF images (*.tif; *.tiff), extended base line implementation
        1..16 bits per sample
        indexed, grayscale, RGB(A), CMYK, L*a*b*
        uncompressed, packed bits, LZW, CCITT T.4, CCIT T.4 2D, CCIT T.6, Thunderscan, Deflate, new style JPEG
    GFI fax images (*.fax), uses TTIFFGraphic to read
    SGI images (*.bw, *.rgb, *.rgba, *.sgi)
        1..16 bits per sample
        indexed, grayscale, RGB(A)
        uncompressed, RLE
    Autodesk images files (*.cel; *.pic)old style only
        8 bits per sample, indexed and uncompressed
    Truevision images (*.tga; *.vst; *.icb; *.vda; *.win), write support included
        5 and 8 bits per sample
        grayscale, indexed, 15 bits RGB (555), 24 bits RGB(A)(888)
        uncompressed, RLE
    ZSoft Paintbrush images (*.pcx, *.pcc; *.scr)
        1..8 bits per sample
        grayscale, indexed, RGB
        uncompressed, RLE
    Kodak Photo-CD images (*.pcd)
        8 bits per sample in YCbCr in any resolution (192 x 128 up to 6144 x 4096)
    Portable pixel/gray map images (*.ppm, *.pgm, *.pbm)
        1 and 8 bits per sample
        grayscale, indexed, RGB uncompressed
    Dr. Halo images (*.cut, *.pal)
        8 bits per sample indexed, RLE compressed
    CompuServe images (*.gif)
        1, 4, 8 bits per sample indexed, LZW compressed
    SGI Alias/Wavefront images (*.rla, *.rpf)
        8 bits per sample RGB(A), RLE compressed
    Standard Windows bitmap images (*.bmp, *.rle, *.dib)
    Photoshop images (*.psd, *.pdd)
        1, 8, 16 bits per sample
        indexed, RGB, CMYK, CIE L*a*b*
        uncompressed and packed bits
    Paintshop Pro images (*.psp)
        1, 4, 8 bits per sample
        indexed, grayscale, RGB
        uncompressed, RLE and LZ77
        single-layered files only!
    Portable network graphic images (*.png)
        1, 2, 4, 8, 16 bits per sample
        indexed, grayscale alpha, RGB(A), LZ77 compressd

IDE and compiler versions
=========================

Library was tested under various IDE from Delphi 7 up to Delphi 10 Seattle, both win32 and win64. Right now, it works only with VCL, but support of FMX is also planned in some near future. 


Compilation
===========

Almost all the code here is native Delphi code, so compiling is straight forward. The only exception is DelphiZlib which is included in this repository as 3rd party. Good thing, it is already compiled for both win32 and win64, so C compiler is not necessary after all, Delphi is enough.

Just add all the units in root folder of GraphicEx to your project and also from subfolders and that's it. Now all the graphic formats are available: line like

Image1.Picture.LoadFromFile(OpenPictureDialog1.Filename);

is everything which is needed to load any of files listed above.

Multi-language support
======================

Recent version greatly simplifies writing international application: there is just no worries about translating GraphicEx strings and error messages: they are already translated and will be fetched in correct language at runtime. So far, there are translations to German, French, Italian, Spanish, Catalan, Polish and Russian and even two English versions: US and UK which differ by spelling of 'color'.
