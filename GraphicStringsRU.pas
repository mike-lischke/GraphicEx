unit GraphicStringsRU;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains the russian version of the strings used in GraphicEx, which can be localized.
// Translation done by Shashlov Kirill.
//
// Rename the file to GraphicStrings.pas to use it as your favourite language file.
//
//----------------------------------------------------------------------------------------------------------------------

interface
//----------------------------------------------------------------------------------------------------------------------

implementation

uses GraphicStrings, AutoResourceStr, windows;

initialization

  with AutoResourceString(MakeLangId(LANG_RUSSIAN, SUBLANG_NEUTRAL)) do begin
    // Описания файлов изображений
    Add(gesAllImages, 'Все изображения');
    Add(gesRegistration, 'Попытка зарегистрировать %s дважды.');

    Add(gesBitmaps,'Точечные рисунки Windows');
    Add(gesRLEBitmaps, 'Точечные рисунки RLE');
    Add(gesDIBs, 'Аппаратно-независимые точечные рисунки Windows');
    Add(gesEPS, 'Внедрённые изображения Postscript');
    Add(gesIcons, 'Иконки Windows');
    Add(gesMetaFiles, 'Метафайлы Windows');
    Add(gesEnhancedMetaFiles, 'Расширенные метафайлы Windows');
    Add(gesJPGImages, 'Изображения JPG');
    Add(gesJPEGImages, 'Изображения JPEG');
    Add(gesJPEImages, 'Изображения JPE');
    Add(gesJFIFImages, 'Изображения JFIF');
    Add(gesTruevision, 'Изображения Truevision');
    Add(gesTIFF, 'Изображения TIFF');
    Add(gesMacTIFF,  'Изображения TIFF для Macintosh');
    Add(gesPCTIF, 'Изображения TIF (PC)');
    Add(gesGFIFax, 'Факс-изображения GFI');
    Add(gesSGI, 'Изображения SGI');
    Add(gesSGITrueColor, 'Полноцветные изображения SGI');
    Add(gesZSoft, 'Изображения ZSoft Paintbrush');
    Add(gesZSoftWord, 'Снимки экрана Word 5.x');
    Add(gesAliasWaveFront, 'Изображения Alias/Wavefront');
    Add(gesSGITrueColorAlpha, 'Полноцветные изображения SGI с альфа-каналом');
    Add(gesSGIMono, 'Чёрно-белые изображения SGI');
    Add(gesPhotoshop, 'Изображения Photoshop');
    Add(gesPortable, 'Изображения Portable map');
    Add(gesPortablePixel, 'Изображения Portable pixel map');
    Add(gesPortableGray, 'Изображения Portable gray map');
    Add(gesPortableMono, 'Изображения Portable bitmap');
    Add(gesAutoDesk, 'Изображения Autodesk');
    Add(gesKodakPhotoCD, 'Изображения Kodak Photo-CD');
    Add(gesCompuserve, 'Изображения CompuServe');
    Add(gesHalo, 'Изображения Dr. Halo');
    Add(gesPaintShopPro, 'Изображения Paintshop Pro');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Изображения Portable network graphic (PNG)');

    // Специфичные ошибки при работе с изображениями
    Add(gesInvalidImage, 'Невозможно загрузить изображение. Неверный или неподдерживаемый формат файла %s.');
    Add(gesInvalidColorFormat, 'Неверный формат цвета в файле %s.');
    Add(gesStreamReadError, 'Ошибка чтения потока при чтении файла %s.');
    Add(gesUnsupportedImage, 'Невозможно загрузить изображение. Неподдерживаемый формат изображения %s.');
    Add(gesUnsupportedFeature, 'Невозможно загрузить изображение. %s не поддерживается для файлов %s.');
    Add(gesInvalidCRC, 'Невозможно загрузить изображение. Неверная контрольная сумма файла %s.');
    Add(gesCompression, 'Невозможно загрузить изображение. Ошибка сжатия в файле %s.');
    Add(gesExtraCompressedData, 'Невозможно загрузить изображение. Файл %s содержит лишние данные.');
    Add(gesInvalidPalette, 'Невозможно загрузить изображение. Неверная палитра в файле %s.');
    Add(gesUnknownCriticalChunk, 'Невозможно загрузить PNG изображение. Формат необходимой секции данных не поддерживается.');

    // Параметры (обычно используются вместе с сообщениями об отсутствии поддержки)
    Add(gesCompressionScheme, 'Схема сжатия:');
    Add(gesRLAPixelFormat, 'Форматы изображений, отличные от RGB and RGBA:');
    Add(gesPSPFileType, 'Версии формата файла, отличные от 3й или 4й:');

    // Ошибки при работе с цветом
    Add(gesIndexedNotSupported, 'Преобразование между индексированными и неиндексированными форматами пикселов не поддерживается.');
    Add(gesConversionUnsupported, 'Невозможно преобразовать цвет. Нет подходящего метода.');
    Add(gesInvalidSampleDepth, 'Неверная глубина цвета. Должно быть 1, 2, 4, 8 или 16 бит на сэмпл.');
    Add(gesInvalidPixelDepth, 'Количество сэмплов на пиксел не соответствует данной цветовой схеме.');
    Add(gesInvalidSubSampling, 'Неверное значение сэмплирования. Допустимы 1, 2 и 4.');
    Add(gesVerticalSubSamplingError, 'Значение вертикального сэмплирования должно быть меньше или равно значению горизонтального сэмплирования.');

    // Состояния
    Add(gesPreparing, 'Подготовка...');
    Add(gesLoadingData, 'Загрузка данных...');
    Add(gesUpsampling, 'Пресэмплирование...');
    Add(gesTransfering, 'Передача...');

    // Ошибки сжатия
    Add(gesLZ77Error, 'Ошибка LZ77-декомпрессии.');
    Add(gesJPEGEOI, 'Ошибка декомпрессии JPEG. Неожиданное окончание данных.');
    Add(gesJPEGStripSize, 'Несоответствующий strip/tile размер JPEG.');
    Add(gesJPEGComponentCount, 'Несоответствующее количество компонент JPEG.');
    Add(gesJPEGDataPrecision, 'Несоответствующая точность данных JPEG.');
    Add(gesJPEGSamplingFactors, 'Несоответствующий фактор сэмплирования JPEG.');
    Add(gesJPEGBogusTableField, 'Ошибочное поле JPEG.');
    Add(gesJPEGFractionalLine, 'Частичные скан-линии JPEG не поддерживаются.');

    // Разное
    Add(gesWarning, 'Внимание!');

  end;

//----------------------------------------------------------------------------------------------------------------------

end.
