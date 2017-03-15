unit GraphicStringsFR;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the french version of the strings used in GraphicEx, which can be localized.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface



//----------------------------------------------------------------------------------------------------------------------
implementation

uses GraphicStrings, AutoResourceStr, Windows;

initialization
  with AutoResourceString(MakeLangId(LANG_FRENCH, SUBLANG_FRENCH)) do begin
    Add(gesAllImages, 'Toutes les Images');
    Add(gesRegistration, 'Tentative de re-enregistrement des fichiers %s.');

    Add(gesBitmaps, 'Bitmaps Windows');
    Add(gesRLEBitmaps, 'Bitmaps Windows (Run length encoded)');
    Add(gesDIBs, 'Bitmaps Windows (Device independant)');
    Add(gesEPS, 'Images Postscript Encapsulées');
    Add(gesIcons, 'Icone Windows');
    Add(gesMetaFiles, 'Metafiles Windows');
    Add(gesEnhancedMetaFiles, 'Metafiles Windows améliorés');
    Add(gesJPGImages, 'Images JPG');
    Add(gesJPEGImages, 'Images JPEG');
    Add(gesJPEImages, 'Images JPE Images');
    Add(gesJFIFImages, 'Images JFIF Images');
    Add(gesTruevision, 'Images Truevision');
    Add(gesTIFF, 'Images Tagged image file format');
    Add(gesMacTIFF,  'Images TIFF Macintosh');
    Add(gesPCTIF, 'Images PC TIF');
    Add(gesGFIFax, 'Images GFI fax');
    Add(gesSGI, 'Images SGI');
    Add(gesSGITrueColor, 'Images SGI true color');
    Add(gesZSoft, 'Images ZSoft Paintbrush');
    Add(gesZSoftWord, 'Capture d''ecrant Word 5.x');
    Add(gesAliasWaveFront, 'Images Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Images SGI true color avec canal alpha');
    Add(gesSGIMono, 'Images SGI noir/blanc');
    Add(gesPhotoshop, 'Images Photoshop');
    Add(gesPortable, 'Images Portable map');
    Add(gesPortablePixel, 'Images Portable pixel map');
    Add(gesPortableGray, 'Images Portable gray map');
    Add(gesPortableMono, 'Images Portable bitmap');
    Add(gesAutoDesk, 'Images Autodesk');
    Add(gesKodakPhotoCD, 'Images Kodak Photo-CD');
    Add(gesCompuserve, 'Images CompuServe');
    Add(gesHalo, 'Images Dr. Halo');
    Add(gesPaintShopPro, 'Images Paintshop Pro');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Images Portable network graphic');

    // image specific error messaAdd(ges
    Add(gesInvalidImage, 'Ne peux pas charger l''image. Format de fichier %s invalide ou inattendue.');
    Add(gesInvalidColorFormat, 'Format de couleur invalide dans le fichier %s.');
    Add(gesStreamReadError, 'Erreur de lecture de flux dans le fichier %s.');
    Add(gesUnsupportedImage, 'Ne peux pas charger l''image. Format de fichier %s non supporté.');
    Add(gesUnsupportedFeature, 'Ne peux pas charger l''image. %s pas supporté par les fichiers %s.');
    Add(gesInvalidCRC, 'Ne peux pas charger l''image. Erreur de CRC dans le fichier %s.');
    Add(gesCompression, 'Ne peux pas charger l''image. Erreur de compression dans le fichier %s.');
    Add(gesExtraCompressedData, 'Ne peux pas charger l''image. Surplus de données compressé trouvé dans le fichier %s.');
    Add(gesInvalidPalette, 'Ne peux pas charger l''image. La palette du fichier %s est invalide.');
    Add(gesUnknownCriticalChunk, 'Ne peux pas charger l''image PNG. Morceau inattendue, mais critique détecté.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'Le procédé de compression n''est');
    Add(gesRLAPixelFormat, 'Les format d''Images différents de RGB ou RGBA ne sont');
    Add(gesPSPFileType, 'Les fichiers de version différents de 3 ou 4 ne sont');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'La conversion entre les formats de pixels indexé et non-indexé n''est pas supportée.');
    Add(gesConversionUnsupported, 'la conversion des couleurs a échoué. Méthode approprié non trouvé.');
    Add(gesInvalidSampleDepth, 'Profondeur des couleurs invalide. Elle doit être de 1, 2, 4, 8, or 16 bits par échantillon.');
    Add(gesInvalidPixelDepth, 'La profondeur des pixels de l''échantillon ne correspond pas au format des couleurs.');
    Add(gesInvalidSubSampling, 'Valeur du sous échantillon est invalide. Les valeurs correctes sont 1, 2 et 4.');
    Add(gesVerticalSubSamplingError, 'La valeur du sous échantillon vertical doit être <= à la valeur du sous échantillon horizontal.');

    // progress strings
    Add(gesPreparing, 'Préparation...');
    Add(gesLoadingData, 'Chargement des données...');
    Add(gesUpsampling, 'Upsampling...');
    Add(gesTransfering, 'Transfert...');

    // compression errors
    Add(gesLZ77Error, 'Erreur de décompressionLZ77.');
    Add(gesJPEGEOI, 'Erreur de décompression JPEG. Fin inattendue des entrées.');
    Add(gesJPEGStripSize, 'Traille strip/tile incorrecte.');
    Add(gesJPEGComponentCount, 'Nombre d''élément JPEG incorrecte.');
    Add(gesJPEGDataPrecision, 'Précision des données JPEG incorrecte.');
    Add(gesJPEGSamplingFactors, 'Echantillon JPEG invalides.');
    Add(gesJPEGBogusTableField, 'Champs de la table JPEG fantôme.');
    Add(gesJPEGFractionalLine, 'Fractional JPEG scanline non supportée.');

    // miscellaneous
    Add(gesWarning, 'Attention');
  end;
//----------------------------------------------------------------------------------------------------------------------

end.
