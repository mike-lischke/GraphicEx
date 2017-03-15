unit GraphicStringsDE;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the german version of the strings used in GraphicEx, which can be localized.
// Translation done by Mike Lischke (public@delphi-gems.com).
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface
//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, windows;

initialization
  with AutoResourceString(MakeLangId(LANG_GERMAN, SUBLANG_GERMAN)) do begin
    Add(gesAllImages, 'Alle Bilder');
    Add(gesRegistration, 'Das Format %s ist schon registriert.');

    Add(gesBitmaps, 'Windows bitmaps');
    Add(gesRLEBitmaps, 'Run length encoded Windows bitmaps');
    Add(gesDIBs, 'Geräteunabhängige Windows bitmaps');
    Add(gesIcons, 'Windows icons');
    Add(gesMetaFiles, 'Windows metafiles');
    Add(gesEnhancedMetaFiles, 'Windows erweiterte metafiles');
    Add(gesJPGImages, 'JPG Bilder');
    Add(gesJPEGImages, 'JPEG Bilder');
    Add(gesTruevision, 'Truevision Bilder');
    Add(gesTIFF, 'Tagged image file format');
    Add(gesMacTIFF,  'Macintosh TIFF Bilder');
    Add(gesPCTIF, 'PC TIF Bilder');
    Add(gesSGI, 'SGI Bilder');
    Add(gesSGITrueColor, 'SGI True Color Bilder');
    Add(gesZSoft, 'ZSoft Paintbrush Bilder');
    Add(gesZSoftWord, 'Word 5.x Snapschuss Bilder');
    Add(gesAliasWaveFront, 'Alias/Wavefront Bilder');
    Add(gesSGITrueColorAlpha, 'SGI True Color Bilder mit Transparenz');
    Add(gesSGIMono, 'SGI schwarz/weiss Bilder');
    Add(gesPhotoshop, 'Photoshop Bilder');
    Add(gesPortable, 'Portable map Bilder');
    Add(gesPortablePixel, 'Portable pixel map Bilder');
    Add(gesPortableGray, 'Portable gray map Bilder');
    Add(gesPortableMono, 'Portable bitmap Bilder');
    Add(gesAutoDesk, 'Autodesk Bilder');
    Add(gesKodakPhotoCD, 'Kodak Photo-CD Bilder');
    Add(gesCompuserve, 'CompuServe Bilder');
    Add(gesHalo, 'Dr. Halo Bilder');
    Add(gesPaintShopPro, 'Paintshop Pro Bilder');
    Add(gesPaintshopProFrames, 'Paintshop Pro Frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro Tubes');
    Add(gesPortableNetworkGraphic, 'Portable network graphic Bilder');

    // image specific error messaAdd(ges
    Add(gesInvalidImage, 'Bild konnte nicht geladen werden. UngültiAdd(ges oder unerwartetes %s Bildformat.');
    Add(gesInvalidColorFormat, 'UngültiAdd(ges Farbformat in %s Bild.');
    Add(gesStreamReadError, 'Stream Lesefehler in %s Datei.');
    Add(gesUnsupportedImage, 'Bild konnte nicht geladen werden. Nicht unterstütztes %s Bildformat.');
    Add(gesUnsupportedFeature, 'Bild konnte nicht geladen werden. %s nicht unterstützt für %s Dateien.');
    Add(gesInvalidCRC, 'Bild konnte nicht geladen werden. Ein CRC Fehler ist in der %s Datei aufgetreten.');
    Add(gesCompression, 'Bild konnte nicht geladen werden. Kompressionsfehler in %s Datei gefunden.');
    Add(gesExtraCompressedData, 'Bild konnte nicht geladen werden. Zuviele komprimierte Daten in %s Datei gefunden.');
    Add(gesInvalidPalette, 'Bild konnte nicht geladen werden. Palette in %s Datei ist ungültig.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'Das Kompressionsschema wird');
//    Add(gesPCDImagesize, 'Bildgrößen außer Base16, Base4 oder Base werden');
    Add(gesRLAPixelFormat, 'Bildformate außer RGB und RGBA werden');
    Add(gesPSPFileType, 'Dateiversionen außer 3 oder 4 werden');

    // errors which apply only to specific image types
    Add(gesUnknownCriticalChunk, 'PNG Bild konnte nicht geladen werden. Unerwarteten, aber notwendigen Chunk gefunden.');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'Konversion zwischen indizierten and nicht-indizierten Farbformaten wird nicht unterstützt.');
    Add(gesConversionUnsupported, 'Farbkonversion schlug fehl. Konnte keine Methode zur Konversion finden.');
    Add(gesInvalidSampleDepth, 'Farbtiefe ist ungültig. Bits pro Sample muß entweder 1, 2, 4, 8 or 16 sein.');
    Add(gesInvalidPixelDepth, 'Sample Anzahl pro Pixel korrespondiert nicht mit dem einAdd(gestellten Farbschema.');
  end;
//----------------------------------------------------------------------------------------------------------------------

end.
