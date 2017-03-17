unit GraphicStringsES;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the spanish version of the strings used in GraphicEx, which can be localized.
// Translation done by Ivan Llanas (ivan@cbi.es; ivan@llanas.com)
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, Windows;

const _CNLI = 'No se puede cargar la imagen. ';
//----------------------------------------------------------------------------------------------------------------------
initialization
  with AutoResourceString(MakeLangId(LANG_SPANISH, SUBLANG_SPANISH)) do begin
    // image file descriptions
    Add(gesAllImages, 'Todas las imágenes');
    Add(gesRegistration, 'Intento de registrar %s por segunda vez.');

    Add(gesBitmaps, 'Imágenes de Windows');
    Add(gesRLEBitmaps, 'Imágenes RLE de Windows');
    Add(gesDIBs, 'Imágenes DIB de Windows');
    Add(gesEPS, 'Imágenes Encapsulated Postscript');
    Add(gesIcons, 'Iconos de Windows');
    Add(gesMetaFiles, 'Metaarchivos de Windows');
    Add(gesEnhancedMetaFiles, 'Metaarchivos mejorados de Windows');
    Add(gesJPGImages, 'Imágenes JPG');
    Add(gesJPEGImages, 'Imágenes JPEG');
    Add(gesJPEImages, 'Imágenes JPE');
    Add(gesJFIFImages, 'Imágenes JFIF');
    Add(gesTruevision, 'Imágenes Truevision');
    Add(gesTIFF, 'Imágenes en formato Tagged');
    Add(gesMacTIFF,  'Imágenes TIFF Macintosh');
    Add(gesPCTIF, 'Imágenes TIF PC');
    Add(gesGFIFax, 'Imágenes fax GFI fax');
    Add(gesSGI, 'Imágenes SGI');
    Add(gesSGITrueColor, 'Imágenes SGI de color verdadero');
    Add(gesZSoft, 'Imágenes ZSoft Paintbrush');
    Add(gesZSoftWord, 'Imágenes de captura de pantalla de Word 5.x');
    Add(gesAliasWaveFront, 'Imágenes Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Imágenes SGI de color verdadero con alfa');
    Add(gesSGIMono, 'Imágenes SGI en blanco y negro');
    Add(gesPhotoshop, 'Imágenes de Photoshop');
    Add(gesPortable, 'Imágenes Portable map');
    Add(gesPortablePixel, 'Imágenes Portable pixel map');
    Add(gesPortableGray, 'Imágenes Portable gray map');
    Add(gesPortableMono, 'Imágenes Portable bitmap');
    Add(gesAutoDesk, 'Imágenes de Autodesk');
    Add(gesKodakPhotoCD, 'Imágenes Kodak Photo-CD');
    Add(gesCompuserve, 'Imágenes GIF de CompuServe');
    Add(gesHalo, 'Imágenes de Dr. Halo');
    Add(gesPaintShopPro, 'Imágenes de Paintshop Pro');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Imágenes Portable network graphic');

    // image specific error messaAdd(ges
    Add(gesInvalidImage, _CNLI + 'Formato de imagen %s inválido.');
    Add(gesInvalidColorFormat, 'Formato de color inválido en archivo %s.');
    Add(gesStreamReadError, 'Error de lectura en archivo %s.');
    Add(gesUnsupportedImage, _CNLI + 'Formato de imagen %s no soportado.');
    Add(gesUnsupportedFeature, _CNLI + '%s no soportado para archivos %s.');
    Add(gesInvalidCRC, _CNLI + 'Error de CRC en archivo %s.');
    Add(gesCompression, _CNLI + 'Error de compresión en archivo %s.');
    Add(gesExtraCompressedData, _CNLI + 'Datos extra comprimidos en archivo %s.');
    Add(gesInvalidPalette, _CNLI + 'La paleta del archivo %s es inválida.');
    Add(gesUnknownCriticalChunk, _CNLI + 'Paquete inesperado pero crítico.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'El esquema de compresión es');
    Add(gesRLAPixelFormat, 'Otros formatos a parte de RGB y RGBA son');
    Add(gesPSPFileType, 'Otras versiones a parte de 3 o 4 son');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'La conversión entre formatos de pixel indexados y no indexados no está soportada.');
    Add(gesConversionUnsupported, 'Error en la conversión de color. No se pudo encontrar el método apropiado.');
    Add(gesInvalidSampleDepth, 'Profundidad de color inválida. Los bits por muestra deben ser 1, 2, 4, 8 o 16.');
    Add(gesInvalidPixelDepth, 'El número de muestras por pixel no corresponde al esquema de color dado.');
    Add(gesInvalidSubSampling, 'Valor de submuestreado inválido. Los valores permitidos son 1, 2 y 4.');
    Add(gesVerticalSubSamplingError, 'El valor de submuestreado vertical debe ser <= valor de submuestreado horizontal.');

    // progress strings
    Add(gesPreparing, 'Preparando...');
    Add(gesLoadingData, 'Cargando...');
    Add(gesUpsampling, 'Componiendo...');
    Add(gesTransfering, 'Transfiriendo...');

    // compression errors
    Add(gesLZ77Error, 'Error en descompresión LZ77.');
    Add(gesJPEGEOI, 'Error en descompresión JPEG. Inesperado final de entrada.');
    Add(gesJPEGStripSize, 'Tamaño de tira/cuadro JPEG impropio.');
    Add(gesJPEGComponentCount, 'Número de componente JPEG impropio.');
    Add(gesJPEGDataPrecision, 'Precisión de datos JPEG impropia.');
    Add(gesJPEGSamplingFactors, 'Factor de muestra JPEG impropio.');
    Add(gesJPEGBogusTableField, 'Campo de tabla JPEG Bogus.');
    Add(gesJPEGFractionalLine, 'Línea JPEG fraccional no soportada.');

  // miscellaneous
    Add(gesWarning, 'Atención');
  end;

end.
