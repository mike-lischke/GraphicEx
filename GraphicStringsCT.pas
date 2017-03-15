unit GraphicStringsCT;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStringsCT contains the catalan version of the strings used in GraphicEx, which can be localized.
// Translation done by Ivan Llanas (ivan@cbi.es; ivan@llanas.com)
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface
//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, windows;

const  _CNLI = 'No es pot carregar la imatge. ';

initialization
  with AutoResourceString(MakeLangId(LANG_CATALAN, SUBLANG_NEUTRAL)) do begin
    // image file descriptions
    Add(gesAllImages, 'Totes les imatges');
    Add(gesRegistration, 'Intent de registrar %s per segona vegada.');

    Add(gesBitmaps, 'Imatges de Windows');
    Add(gesRLEBitmaps, 'Imatges RLE de Windows');
    Add(gesDIBs, 'Imatges DIB de Windows');
    Add(gesEPS, 'Imatges Encapsulated Postscript');
    Add(gesIcons, 'Icones de Windows');
    Add(gesMetaFiles, 'Metaarxius de Windows');
    Add(gesEnhancedMetaFiles, 'Metaarxius millorats de Windows');
    Add(gesJPGImages, 'Imatges JPG');
    Add(gesJPEGImages, 'Imatges JPEG');
    Add(gesJPEImages, 'Imatges JPE');
    Add(gesJFIFImages, 'Imatges JFIF');
    Add(gesTruevision, 'Imatges Truevision');
    Add(gesTIFF, 'Imatges en formato Tagged');
    Add(gesMacTIFF,  'Imatges TIFF Macintosh');
    Add(gesPCTIF, 'Imatges TIF PC');
    Add(gesGFIFax, 'Imatges fax GFI fax');
    Add(gesSGI, 'Imatges SGI');
    Add(gesSGITrueColor, 'Imatges SGI de color verdadero');
    Add(gesZSoft, 'Imatges ZSoft Paintbrush');
    Add(gesZSoftWord, 'Imatges de captura de pantalla de Word 5.x');
    Add(gesAliasWaveFront, 'Imatges Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Imatges SGI de color verdader amb alfa');
    Add(gesSGIMono, 'Imatges SGI en blanc i negre');
    Add(gesPhotoshop, 'Imatges de Photoshop');
    Add(gesPortable, 'Imatges Portable map');
    Add(gesPortablePixel, 'Imatges Portable pixel map');
    Add(gesPortableGray, 'Imatges Portable gray map');
    Add(gesPortableMono, 'Imatges Portable bitmap');
    Add(gesAutoDesk, 'Imatges de Autodesk');
    Add(gesKodakPhotoCD, 'Imatges Kodak Photo-CD');
    Add(gesCompuserve, 'Imatges GIF de CompuServe');
    Add(gesHalo, 'Imatges de Dr. Halo');
    Add(gesPaintShopPro, 'Imatges de Paintshop Pro');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Imatges Portable network graphic');

    // image specific error messaAdd(ges

    Add(gesInvalidImage, _CNLI + 'Format d''imatge %s invàlid.');
    Add(gesInvalidColorFormat, 'Format de color invàlid en arxiu %s.');
    Add(gesStreamReadError, 'Error de lectura en arxiu %s.');
    Add(gesUnsupportedImage, _CNLI + 'Formato d''imatge %s no soportat.');
    Add(gesUnsupportedFeature, _CNLI + '%s no soportat per arxius %s.');
    Add(gesInvalidCRC, _CNLI + 'Error de CRC en arxiu %s.');
    Add(gesCompression, _CNLI + 'Error de compressió en arxiu %s.');
    Add(gesExtraCompressedData, _CNLI + 'Dades extra comprimides en arxiu %s.');
    Add(gesInvalidPalette, _CNLI + 'La paleta de l''arxiu %s es invàlida.');
    Add(gesUnknownCriticalChunk, _CNLI + 'Paquet inesperat però crític.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'L''esquema de compressió és');
    Add(gesRLAPixelFormat, 'Altres formats a part de RGB i RGBA són');
    Add(gesPSPFileType, 'Altres versions a part de 3 o 4 són');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'La conversió entre formats de pixel indexats i no indexats no està soportada.');
    Add(gesConversionUnsupported, 'Error en la conversió de color. No s''ha pogut trobar el mètode apropiat.');
    Add(gesInvalidSampleDepth, 'Profunditat de color invàlida. Els bits per mostra han de ser 1, 2, 4, 8 o 16.');
    Add(gesInvalidPixelDepth, 'El nombre de mostres per pixel no correspon a l''esquema de color donat.');
    Add(gesInvalidSubSampling, 'Valor de submostrat invàlido. Els valors permesos són 1, 2 i 4.');
    Add(gesVerticalSubSamplingError, 'El valor de submostrat vertical ha de ser <= valor de submostrat horitzontal.');

    // progress strings
    Add(gesPreparing, 'Preparant...');
    Add(gesLoadingData, 'Carregant...');
    Add(gesUpsampling, 'Composant...');
    Add(gesTransfering, 'Transferint...');

    // compression errors
    Add(gesLZ77Error, 'Error en descompressió LZ77.');
    Add(gesJPEGEOI, 'Error en descompressió JPEG. Inesperat final d''entrada.');
    Add(gesJPEGStripSize, 'Mida de tira/quadre JPEG impropi.');
    Add(gesJPEGComponentCount, 'Nombre de component JPEG impropi.');
    Add(gesJPEGDataPrecision, 'Precissió de dades JPEG impròpia.');
    Add(gesJPEGSamplingFactors, 'Factor de mostra JPEG impropi.');
    Add(gesJPEGBogusTableField, 'Camp de taula JPEG Bogus.');
    Add(gesJPEGFractionalLine, 'Línia JPEG fraccional no soportada.');

    // miscellaneous
    Add(gesWarning, 'Atenció');

  end;

//----------------------------------------------------------------------------------------------------------------------

end.
