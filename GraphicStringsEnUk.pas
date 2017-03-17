unit GraphicStringsEnUk;

//the only difference between UK english and US english important here
// is spelling of 'color'/'colour'.
//So in this unit we redefine strings which have 'color' in it.
//no need to redefine another strings: AutoResourceStr will use strings from En-US
//automatically


interface
uses AutoResourceStr, GraphicStrings;


implementation

uses Windows, GraphicStringsEN;

initialization
  with AutoResourceString(MakeLangId(LANG_ENGLISH, SUBLANG_ENGLISH_UK)) do begin
    Add(gesSGITrueColor, 'SGI true colour images');
    Add(gesSGITrueColorAlpha, 'SGI true colour images with alpha');
    Add(gesInvalidColorFormat, 'Invalid colour format in %s file.');
    Add(gesConversionUnsupported, 'Colour conversion failed. Could not find a proper method.');
    Add(gesInvalidSampleDepth, 'Colour depth is invalid. Bits per sample must be 1, 2, 4, 8 or 16.');
    Add(gesInvalidPixelDepth, 'Sample count per pixel does not correspond to the given colour scheme.');
  end;
end.
