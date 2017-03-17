unit GraphicStringsIT;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStringsIT contains the italian version of the strings used in GraphicEx, which can be localized.
// Translation done by Salvatore Meschini (salvatoremeschini@tiscalinet.it).
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface
//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, windows;

initialization
  with AutoResourceString(MakeLangId(LANG_ITALIAN, SUBLANG_ITALIAN)) do begin
    Add(gesAllImages, 'Tutte le immagini');
    Add(gesRegistration, 'Tentata doppia registrazione di %s');

    Add(gesBitmaps, 'Bitmap di Windows');
    Add(gesRLEBitmaps, 'Bitmap di Windows Run Length Encoded');
    Add(gesDIBs, 'Bitmap di Windows Indipendenti dal Dispositivo');
    Add(gesEPS, 'Immagini Encapsulated Postscript');
    Add(gesIcons, 'Icone di Windows');
    Add(gesMetaFiles, 'Metafiles di Windows');
    Add(gesEnhancedMetaFiles, 'Enhanced Meta Files di Windows');
    Add(gesJPGImages, 'Immagini JPG');
    Add(gesJPEGImages, 'Immagini JPEG');
    Add(gesJPEImages, 'Immagini JPE');
    Add(gesJFIFImages, 'Immagini JFIF');
    Add(gesTruevision, 'Immagini Truevision');
    Add(gesTIFF, 'Immagini TIFF');
    Add(gesMacTIFF,  'Immagini Macintosh TIFF');
    Add(gesPCTIF, 'Immagini PC TIF');
    Add(gesGFIFax, 'Immagini GFI fax');
    Add(gesSGI, 'Immagini SGI');
    Add(gesSGITrueColor, 'Immagini SGI true color');
    Add(gesZSoft, 'Immagini ZSoft Paintbrush');
    Add(gesZSoftWord, 'Immagini schermate Word 5.x');
    Add(gesAliasWaveFront, 'Immagini Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Immagini SGI true color con alpha');
    Add(gesSGIMono, 'Immagini SGI bianco/nero');
    Add(gesPhotoshop, 'Immagini Photoshop');
    Add(gesPortable, 'Immagini Portable map');
    Add(gesPortablePixel, 'Immagini Portable pixel map');
    Add(gesPortableGray, 'Immagini Portable gray map');
    Add(gesPortableMono, 'Immagini Portable bitmap');
    Add(gesAutoDesk, 'Immagini Autodesk');
    Add(gesKodakPhotoCD, 'Immagini Kodak Photo-CD');
    Add(gesCompuserve, 'Immagini CompuServe');
    Add(gesHalo, 'Immagini Dr. Halo');
    Add(gesPaintShopPro, 'Immagini Paintshop Pro');
    Add(gesPortableNetworkGraphic, 'Immagini Portable network graphic');

    // image specific error messaAdd(ges
    Add(gesInvalidImage, 'Impossibile caricare l''immagine. Il formato %s è non valido o non corretto.');
    Add(gesInvalidColorFormat, 'Il formato dei colori del file %s non è valido.');
    Add(gesStreamReadError, 'Errore nella lettura del file %s.');
    Add(gesUnsupportedImage, 'Impossibile caricare l''immagine. Il formato %s non è supportato.');
    Add(gesUnsupportedFeature, 'Impossibile caricare l''immagine. %s non supportato per file %s.');
    Add(gesInvalidCRC, 'Impossibile caricare l''immagine. Errore CRC nel file %s.');
    Add(gesCompression, 'Impossibile caricare l''immagine. Errore di compressione nel file %s.');
    Add(gesExtraCompressedData, 'Impossibile caricare l''immagine. Trovati dati extra compressi nel file %s.');
    Add(gesInvalidPalette, 'Impossibile caricare l''immagine. Palette nel file %s non valida.');
    Add(gesUnknownCriticalChunk, 'Impossibile caricare l''immagine PNG. Rilevato blocco critico ma non previsto.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'Lo schema di compressione è');
    Add(gesRLAPixelFormat, 'Formati diversi da RGB e RGBA sono');
    Add(gesPSPFileType, 'Versioni dei file diverse da 3 o 4 sono');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'La conversione tra formati di pixel indicizzati e non-indicizzati non è supportata.');
    Add(gesConversionUnsupported, 'Conversione di colore fallita. Impossibile trovare un metodo adatto.');
    Add(gesInvalidSampleDepth, 'Profondità di colore non valida. I bit per campione devono essere 1, 2, 4, 8 o 16.');
    Add(gesInvalidPixelDepth, 'Il conteggio dei campioni per pixel non corrisponde allo schema dei colori.');
    Add(gesInvalidSubSampling, 'Valore di sottocampionamento non valido. Valori consentiti 1, 2 e 4.');
    Add(gesVerticalSubSamplingError, 'Il valore di sottocampionamento verticale deve essere <= di quello valore orizzontale.');

    // progress strings
    Add(gesPreparing, 'Preparazione...');
    Add(gesLoadingData, 'Caricamento dati...');
    Add(gesUpsampling, 'Campionamento...');
    Add(gesTransfering, 'Trasferimento...');

    // compression errors
    Add(gesLZ77Error, 'Errore decompressione LZ77.');
    Add(gesJPEGEOI, 'Errore decompressione JPEG. Fine del flusso di ingresso non attesa.');
    Add(gesJPEGStripSize, 'Dimensione blocco JPEG impropria.');
    Add(gesJPEGComponentCount, 'Conteggio componente JPEG improprio.');
    Add(gesJPEGDataPrecision, 'Precisione dati JPEG impropria.');
    Add(gesJPEGSamplingFactors, 'Fattori di campionamento JPEG impropri.');
    Add(gesJPEGBogusTableField, 'Campo tabelle JPEG fittizio.');
    Add(gesJPEGFractionalLine, 'Linea di scansione frazionale JPEG non supportata.');

    // miscellaneous
    Add(gesWarning, 'Avvertimento');


  end;
//----------------------------------------------------------------------------------------------------------------------

end.
