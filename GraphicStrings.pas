unit GraphicStrings;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStrings contains variables for localization strings.
// You should include one or more units containing actual strings:
// GraphicStringsEn, GraphicStringsDe etc.
// Language will be selected automatically at runtime.
// Translation done by Mike Lischke (mike@lischke-online.de).
//
// AutoResourceString lib by nabbla (nabbla@yandex.ru)
//
//----------------------------------------------------------------------------------------------------------------------

interface

var
  // image file descriptions
  gesAllImages,
  gesRegistration,

  gesBitmaps,
  gesRLEBitmaps,
  gesDIBs,
  gesEPS,
  gesIcons,
  gesMetaFiles,
  gesEnhancedMetaFiles,
  gesJPGImages,
  gesJPEGImages,
  gesJPEImages,
  gesJFIFImages,
  gesTruevision,
  gesTIFF,
  gesMacTIFF,
  gesPCTIF,
  gesGFIFax,
  gesSGI,
  gesSGITrueColor,
  gesZSoft,
  gesZSoftWord,
  gesAliasWaveFront,
  gesSGITrueColorAlpha,
  gesSGIMono,
  gesPhotoshop,
  gesPortable,
  gesPortablePixel,
  gesPortableGray,
  gesPortableMono,
  gesAutoDesk,
  gesKodakPhotoCD,
  gesCompuserve,
  gesHalo,
  gesPaintshopPro,
  gesPaintshopProFrames,
  gesPaintshopProTubes,
  gesPortableNetworkGraphic,
  gesArtsAndLettersGraphic,

  // image specific error messages
  gesInvalidImage,
  gesInvalidSaveFormat,
  gesInvalidSaveOnlyLoadFormat,
  gesInvalidColorFormat,
  gesStreamReadError,
  gesUnsupportedImage,
  gesUnsupportedFeature,
  gesInvalidCRC,
  gesCompression,
  gesExtraCompressedData,
  gesInvalidPalette,
  gesUnknownCriticalChunk,
  gesInvalidPSDLayerData,
  gesInvalidPSDResourceData,

  // features (usually used together with unsupported feature string)
  gesCompressionScheme,
  gesRLAPixelFormat,
  gesPSPFileType,

  // color manager error messages
  gesIndexedNotSupported,
  gesConversionUnsupported,
  gesInvalidSampleDepth,
  gesInvalidPixelDepth,
  gesInvalidSubSampling,
  gesVerticalSubSamplingError,

  // progress strings
  gesPreparing,
  gesLoadingData,
  gesUpsampling,
  gesTransfering,

  // compression errors
  gesLZ77Error,
  gesJPEGEOI,
  gesJPEGStripSize,
  gesJPEGComponentCount,
  gesJPEGDataPrecision,
  gesJPEGSamplingFactors,
  gesJPEGBogusTableField,
  gesJPEGFractionalLine,

  // miscellaneous
  gesWarning
                   :Variant;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
