unit GraphicStringsEN;

// The original code is GraphicStrings.pas, released November 1, 1999.
//
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// GraphicStringsEN contains variables for localization strings.
// You should include one or more units containing actual strings:
// GraphicStringsEn, GraphicStringsDe etc.
// Language will be selected automatically at runtime.
// Translation done by Mike Lischke (mike@lischke-online.de).
//
// AutoResourceString lib by nabbla (nabbla@yandex.ru)
//
//----------------------------------------------------------------------------------------------------------------------

interface
uses AutoResourceStr, GraphicStrings; //so first GraphicStrings will be freed
//(while custom variant is still working), only then freeing custom variant class

implementation

uses windows;

initialization
  with AutoResourceString(MakeLangId(LANG_ENGLISH, SUBLANG_ENGLISH_US)) do begin
    // image file descriptions
    Add(gesAllImages, 'All images');
    Add(gesRegistration, 'Attempt to register %s twice.');
    Add(gesBitmaps, 'Windows bitmaps');
    Add(gesRLEBitmaps, 'Run length encoded Windows bitmaps');
    Add(gesDIBs, 'Device independant Windows bitmaps');
    Add(gesEPS, 'Encapsulated Postscript images');
    Add(gesIcons, 'Windows icons');
    Add(gesMetaFiles, 'Windows metafiles');
    Add(gesEnhancedMetaFiles, 'Windows enhanced meta files');
    Add(gesJPGImages, 'JPG images');
    Add(gesJPEGImages, 'JPEG images');
    Add(gesJPEImages, 'JPE images');
    Add(gesJFIFImages, 'JFIF images');
    Add(gesTruevision, 'Truevision images');
    Add(gesTIFF, 'Tagged image file format images');
    Add(gesMacTIFF, 'Macintosh TIFF images');
    Add(gesPCTIF, 'PC TIF images');
    Add(gesGFIFax, 'GFI fax images');
    Add(gesSGI, 'SGI images');
    Add(gesSGITrueColor, 'SGI true color images');
    Add(gesZSoft, 'ZSoft Paintbrush images');
    Add(gesZSoftWord, 'Word 5.x screen capture images');
    Add(gesAliasWaveFront, 'Alias/Wavefront images');
    Add(gesSGITrueColorAlpha, 'SGI true color images with alpha');
    Add(gesSGIMono, 'SGI black/white images');
    Add(gesPhotoshop, 'Photoshop images');
    Add(gesPortable, 'Portable map images');
    Add(gesPortablePixel, 'Portable pixel map images');
    Add(gesPortableGray, 'Portable gray map images');
    Add(gesPortableMono, 'Portable bitmap images');
    Add(gesAutoDesk, 'Autodesk images');
    Add(gesKodakPhotoCD, 'Kodak Photo-CD images');
    Add(gesCompuserve, 'CompuServe images');
    Add(gesHalo, 'Dr. Halo images');
    Add(gesPaintshopPro, 'Paintshop Pro images');
    Add(gesPaintshopProFrames, 'Paintshop Pro frames');
    Add(gesPaintshopProTubes, 'Paintshop Pro tubes');
    Add(gesPortableNetworkGraphic, 'Portable network graphic images');
    Add(gesArtsAndLettersGraphic, 'Arts & Letters thumbnail images');

    // image specific error messages
    Add(gesInvalidImage, 'Cannot load image. Invalid or unexpected %s image format.');
    Add(gesInvalidSaveFormat, 'Cannot save image. Invalid or unexpected %s image format.');
    Add(gesInvalidSaveOnlyLoadFormat, 'Cannot save image. Only load is supported for %s image format.');
    Add(gesInvalidColorFormat, 'Invalid color format in %s file.');
    Add(gesStreamReadError, 'Stream read error in %s file.');
    Add(gesUnsupportedImage, 'Cannot load image. Unsupported %s image format.');
    Add(gesUnsupportedFeature, 'Cannot load image. %s not supported for %s files.');
    Add(gesInvalidCRC, 'Cannot load image. CRC error found in %s file.');
    Add(gesCompression, 'Cannot load image. Compression error found in %s file.');
    Add(gesExtraCompressedData, 'Cannot load image. Extra compressed data found in %s file.');
    Add(gesInvalidPalette, 'Cannot load image. Palette in %s file is invalid.');
    Add(gesUnknownCriticalChunk, 'Cannot load PNG image. Unexpected but critical chunk detected.');
    Add(gesInvalidPSDLayerData, 'Image is invalid. Layer data is corrupt.');
    Add(gesInvalidPSDResourceData, 'Image is invalid. Resource data is corrupt.');

    // features (usually used together with unsupported feature string)
    Add(gesCompressionScheme, 'The compression scheme is');
    Add(gesRLAPixelFormat, 'Image formats other than RGB and RGBA are');
    Add(gesPSPFileType, 'File versions other than 3 or 4 are');

    // color manager error messages
    Add(gesIndexedNotSupported, 'Conversion between indexed and non-indexed pixel formats is not supported.');
    Add(gesConversionUnsupported, 'Color conversion failed. Could not find a proper method.');
    Add(gesInvalidSampleDepth, 'Color depth is invalid. Bits per sample must be 1, 2, 4, 8 or 16.');
    Add(gesInvalidPixelDepth, 'Sample count per pixel does not correspond to the given color scheme.');
    Add(gesInvalidSubSampling, 'Subsampling value is invalid. Allowed are 1, 2 and 4.');
    Add(gesVerticalSubSamplingError, 'Vertical subsampling value must be <= horizontal subsampling value.');

    // progress strings
    Add(gesPreparing, 'Preparing...');
    Add(gesLoadingData, 'Loading data...');
    Add(gesUpsampling, 'Upsampling...');
    Add(gesTransfering, 'Transfering...');

    // compression errors
    Add(gesLZ77Error, 'LZ77 decompression error.');
    Add(gesJPEGEOI, 'JPEG decompression error. Unexpected end of input.');
    Add(gesJPEGStripSize, 'Improper JPEG strip/tile size.');
    Add(gesJPEGComponentCount, 'Improper JPEG component count.');
    Add(gesJPEGDataPrecision, 'Improper JPEG data precision.');
    Add(gesJPEGSamplingFactors, 'Improper JPEG sampling factors.');
    Add(gesJPEGBogusTableField, 'Bogus JPEG tables field.');
    Add(gesJPEGFractionalLine, 'Fractional JPEG scanline unsupported.');

    // miscellaneous
    Add(gesWarning, 'Warning');
  end;
//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------

end.