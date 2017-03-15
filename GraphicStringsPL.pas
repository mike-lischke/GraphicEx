unit GraphicStringsPL;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the polish version of the strings used in GraphicEx, which can be localized.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface

//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, Windows;

initialization
  with AutoResourceString(MakeLangId(LANG_POLISH, SUBLANG_NEUTRAL)) do begin
    Add(gesAllImages, 'Wszystkie obrazy');
    Add(gesRegistration, 'Próba podwójnej rejestracji %s.');

    Add(gesBitmaps, 'Mapy bitowe Windows');
    Add(gesRLEBitmaps, 'Mapy bitowe run length encoded Windows');
    Add(gesDIBs, 'Mapy bitowe Device independant Windows');
    Add(gesEPS, 'Obrazy Encapsulated Postscript');
    Add(gesIcons, 'Ikony Windows');
    Add(gesMetaFiles, 'Meta pliki Windows');
    Add(gesEnhancedMetaFiles, 'Ulepszone meta pliki Windows');
    Add(gesJPGImages, 'Obrazy JPG');
    Add(gesJPEGImages, 'Obrazy JPEG');
    Add(gesJPEImages, 'Obrazy JPE');
    Add(gesJFIFImages, 'Obrazy JFIF');
    Add(gesTruevision, 'Obrazy Truevision');
    Add(gesTIFF, 'Tagged image file format Images');
    Add(gesMacTIFF,  'Obrazy Macintosh TIFF');
    Add(gesPCTIF, 'Obrazy PC TIF');
    Add(gesGFIFax, 'Obrazy GFI fax');
    Add(gesSGI, 'Obrazy SGI');
    Add(gesSGITrueColor, 'Obrazy SGI true color');
    Add(gesZSoft, 'Obrazy ZSoft Paintbrush');
    Add(gesZSoftWord, 'Zrzut ekranu Word 5.x');
    Add(gesAliasWaveFront, 'Obrazy Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Obrazy SGI true color with alpha');
    Add(gesSGIMono, 'Czarno-białe obrazy SGI');
    Add(gesPhotoshop, 'Obrazy Photoshop');
    Add(gesPortable, 'Obrazy Portable map');
    Add(gesPortablePixel, 'Obrazy Portable pixel map');
    Add(gesPortableGray, 'Obrazy Portable gray map');
    Add(gesPortableMono, 'Obrazy Portable bitmap');
    Add(gesAutoDesk, 'Obrazy Autodesk');
    Add(gesKodakPhotoCD, 'Obrazy Kodak Photo-CD');
    Add(gesCompuserve, 'Obrazy CompuServe');
    Add(gesHalo, 'Obrazy Dr. Halo');
    Add(gesPaintShopPro, 'Obrazy Paintshop Pro');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Obrazy Portable network graphic');

    // image specific error messaAdd(ges
    Add(gesInvalidImage, 'Nie mogę otworzyć obrazu. Błędny lub niespodziewany %s format.');
    Add(gesInvalidColorFormat, 'Błędny format kolorów w pliku %s.');
    Add(gesStreamReadError, 'Błąd odczytu strumienia danych z pliku %s.');
    Add(gesUnsupportedImage, 'Nie mogę otworzyć obrazu. Nieobsługiwany format %s.');
    Add(gesUnsupportedFeature, 'Nie mogę otworzyć obrazu. %s nie obsługuje plików %s.');
    Add(gesInvalidCRC, 'Nie mogę otworzyć obrazu. Błąd CRC w pliku %s.');
    Add(gesCompression, 'Nie mogę otworzyć obrazu. Błąd kompresji w pliku %s.');
    Add(gesExtraCompressedData, 'Nie mogę otworzyć obrazu. Nieznany typ kompresji w pliku %s.');
    Add(gesInvalidPalette, 'Nie mogę otworzyć obrazu. Błędna paleta kolorów w pliku %s.');
    Add(gesUnknownCriticalChunk, 'Nie mogę otworzyć obrazu PNG. Napodkano krytczny wyjątek.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'Metoda kompresji to');
    Add(gesRLAPixelFormat, 'Formaty inne niż RGB i RGBA to');
    Add(gesPSPFileType, 'Pliki w wersji innej niż 3 lub 4 to');

    // color manager error messaAdd(ges
    Add(gesIndexedNotSupported, 'Konwersja pomiędzy indeksowanym i nieindeksowanym formatem punktów nie jest obsługiwana.');
    Add(gesConversionUnsupported, 'Błąd konwersji koloru. Nie mogę znaleźć odpowiedniej metody.');
    Add(gesInvalidSampleDepth, 'Błędna głębia kolorów. Powinno być 1, 2, 4, 8 lub 16 bitów na próbkę.');
    Add(gesInvalidPixelDepth, 'Ilość próbek na punkt nLanguageie odpowiada danemu schematowi kolorów.');
    Add(gesInvalidSubSampling, 'Błędna wartość Subsampling. Dozwolona jest 1, 2 i 4.');
    Add(gesVerticalSubSamplingError, 'Pionowa wartość Subsampling nie może być większa niż pozioma.');

    // progress strings
    Add(gesPreparing, 'Przygotowywanie...');
    Add(gesLoadingData, 'Czytanie danych...');
    Add(gesUpsampling, 'Próbkowanie...');
    Add(gesTransfering, 'Transferowanie...');

    // compression errors
    Add(gesLZ77Error, 'Błąd dekompresji LZ77.');
    Add(gesJPEGEOI, 'Błąd dekompresji JPEG. Niespodziewany koniec danych wejściowych.');
    Add(gesJPEGStripSize, 'Niewłaściwy rozmiar JPEG strip/tile.');
    Add(gesJPEGComponentCount, 'Niewłaściwy JPEG component count.');
    Add(gesJPEGDataPrecision, 'Niewłaściwa precyzja danych JPEG.');
    Add(gesJPEGSamplingFactors, 'Niewłaściwe próbkowanie JPEG.');
    Add(gesJPEGBogusTableField, 'Błąd zakresu w tablicy JPEG.');
    Add(gesJPEGFractionalLine, 'Fractional JPEG scanline nie jest obsługiwany.');

    // miscellaneous
    Add(gesWarning, 'Uwaga');
  end;
//----------------------------------------------------------------------------------------------------------------------

end.
