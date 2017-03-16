unit AutoResourceStr;

//custom variant type which might be more handy than classic resourcestring.
//usage:

//1. Create unit where all the string variables are defined, let's name it 'LocStrings.pas'
//In interface section, write
// var
// String1,
// String2,
// String3, //some more informative names would be preferable, of course
// :Variant;
//that's all. Now you should add this unit to 'uses' clause, when these strings are needed.

//2. Create one or more 'language units', let's name them 'LocStringsEN.pas' and 'LocStringsDE.pas'
//their interface section is empty.
//in implementation section, add 'LocStrings' and 'AutoRersourceStr' to uses clause
//add Initialization section, in which write smth like this:
//with AutoResourceString($409) do begin; //select English / USA
//  Add(String1, 'Button1');
//  Add(String2, 'Button2');
//end;

//3. Enjoy!
//4. But there is more: you can change language on the run without risky reloading of resources
//(it is risky because it reverts all the properties to their initial value, not only change captions)
//

interface

uses classes,VariantWrapper;

type

  //class which is stored inside AutoResourceStr variant
  //contains strings of different locale (it's marked in Objects property,
  //which is which)
  //and also doubly linked to another AutoResourceStr.
  TAutoResourceStrData = class(TAbstractWrapperData)
    private
      fPrevious: TAutoResourceStrData;  //we want them all to be linked,
      //that way we can enumerate them all.
      fNext: TAutoResourceStrData;  //needed for fast deletion
      fStrings: TStrings; //each one corresponds to particular language
    public
      constructor Create; override;
      procedure Assign(source: TPersistent); override;
      destructor Destroy; override;
      //we don't need fast creation/destroying here (implemented by object pool),
      //probably all the variants are initialized when program starts
      // and are kept in memory till the end
      function GetAsString: string; override;

      //methods useful with numerical types but no need for us
      procedure DoNegate; override; //raise EAssertionError
      procedure DoAdd(const Right: TAbstractWrapperData); override;
      procedure DoMultiply(const Right: TAbstractWrapperData); override;
      procedure DoDivide(const Right: TAbstractWrapperData); override;

      //called when we add string on another language to current one,
      //or change current language, so we must iterate through the whole list
      procedure SelectMostAppropriateLang;
    end;

  //helper class which defines our custom variant.
  //most of dirty work is done in VariantWrapper.pas,
  //rules are pretty simple right now: we can't cast another variants to our own
  //we cannot concatenate two AutoResourceStr though that could be beneficial,
  //but requires more complex mechanics (late binding)
  //all we can do is to cast AutoResourceStr to string
  TAutoResourceStrVarType = class(TAbstractWrapperVariantType)
    protected
      //if there is string at the right, let it remain string, we'll convert into string ourselves
      function RightPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    public
      //we can convert only to string
      procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
  end;

  //16-byte (24-byte in x64) variant contents. As usual, it's just VType and link to object.
  TAutoResourceStrVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TAutoResourceStrData;
    Reserved4: Pointer;
  end;

  IAutoResourceString = interface //sort of syntactic sugar: it's short-lived object which is created
  //for some particular language. It stores this language and adds strings in it to some autoResourceStrings
    ['{033F7084-BC70-449D-9A34-6D6B58FC8DAD}']
  //we could use just one singleton object instead, but with interface we theoretically may have
  //thread-safe implementation. Not sure it is needed so much, but why not...
    procedure Add(var V: Variant; const text: String);
  end;

  //also, it contains class functions for more advanced features. We could use normal functions,
  //but name conflicts are possible, or problems of memorizing function names if they are too long
  //(which is best guarantee that name conflicts don't arise)
  TAutoResourceString = class (TInterfacedObject, IAutoResourceString) //helper object
    private
      fLang: Integer;
    public
      constructor Create(aLang: Integer);
      procedure Add(var V: Variant; const text: string);
      class function GetLanguage: Integer;
      class procedure SetLanguage(LangID: Integer);
      //these ones determine which languages are fully present in all AutoResourceStrs
      class function LanguageCount: Integer;
      class function Languages(index: Integer): Integer;
      class procedure MakeReport(FileName: string); //tells which strings are still missing
  end;

function AutoResourceString(LangID: Integer): IAutoResourceString; overload;
//function AutoResourceString(Language: string); overload;

function GetLanguageNameInCurLocale(Lang_ID: Integer): string;

  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF RTLVersion >= 30.0}
      {$DEFINE HAS_LANG_MACROS}
    {$IFEND}
 {$ENDIF}

 {$IFNDEF HAS_LANG_MACROS}
  function MAKELANGID(p, s: WORD): WORD;
  function PRIMARYLANGID(lgid: WORD): WORD;
  function SUBLANGID(lgid: WORD): WORD;
 {$ENDIF}



implementation

uses SysUtils, Windows;

var AutoResourceStrVarType: TAutoResourceStrVarType;
    CurrentLanguage: Integer; //at initialization is set to system locale,
                              //may be changed at runtime
    TopAutoResourceStr: TAutoResourceStrData;  //'pointer' to all our strings, nil at first

    LanguageArr: Array of Integer; //list of fully implemented languages
    LanguagesChanged: Boolean; //to avoid recalculating every time
    //set to true when new AutoResourceStr is created (that moment array in fact disappears,
    //as this newly constructed Str has no languages at all, until populated)
    //and when new language is added to existing Str.

(*
    Some WinAPI calls. Intention is to grab them all there, so programmer
    could use platform-independent code and we handle it low-level
                                                                      *)

function GetLanguageNameInCurLocale(Lang_ID: Integer): string;
var size: Integer;
begin
  Size := GetLocaleInfo(Lang_ID, LOCALE_SLANGUAGE, nil, 0);
  SetLength(Result, size);
  GetLocaleInfo(Lang_ID, LOCALE_SLANGUAGE, @Result[1], size);
  Result := String(PChar(Result)); //make sure that 0 termination is exactly at string end
end;

function GetShortLanguage(Lang_ID: Integer): string;
var size: Integer;
begin
  Size := GetLocaleInfo(Lang_ID, LOCALE_SLANGUAGE, nil, 0);
  SetLength(Result, size);
  GetLocaleInfo(Lang_ID, LOCALE_SABBREVLANGNAME, @Result[1], size);
  Result := String(PChar(Result));
end;

function GetDefaultLanguageID: Integer;
var loc_str: PChar;
    size: Integer;
begin
  Size := GetLocaleInfo(LANG_USER_DEFAULT, LOCALE_ILANGUAGE, nil, 0);
  loc_Str := AllocMem(Size);
  GetLocaleInfo(LANG_USER_DEFAULT, LOCALE_ILANGUAGE, loc_str, size);
  Result := StrToInt('$' + loc_str);
  FreeMem(loc_str);
end;

 {$IFNDEF HAS_LANG_MACROS}
  function MAKELANGID(p, s: WORD): WORD;
  begin
    Result := WORD(s shl 10) or p;
  end;

  function PRIMARYLANGID(lgid: WORD): WORD;
  begin
    Result := WORD(lgid and $3FF);
  end;

  function SUBLANGID(lgid: WORD): WORD;
  begin
    Result := lgid shr 10;
  end;
 {$ENDIF}


(*
      TAutoResourceStrData
                                *)
constructor TAutoResourceStrData.Create;
begin
  inherited;
  fStrings := TStringList.Create;

  //link into 2-way list
  fPrevious := TopAutoResourceStr;
  TopAutoResourceStr := self;
  if Assigned(fPrevious) then
    fPrevious.fNext := self;
  LanguagesChanged := true;
end;

procedure TAutoResourceStrData.Assign(source: TPersistent);
var src: TAutoResourceStrData absolute source;
begin
  if source is TAutoResourceStrData then
    fStrings.Assign(src.fstrings)
  else
    inherited Assign(source);
end;

destructor TAutoResourceStrData.Destroy;
begin
  //remove ourself from 2-way list

  if Assigned(fPrevious) then
    fPrevious.fNext := fNext;
  if Assigned(fNext) then
    fNext.fPrevious := fPrevious
  else
    TopAutoResourceStr := nil;

  //cleaning up
  fStrings.Free;
  inherited;
end;

procedure TAutoResourceStrData.DoAdd(const Right: TAbstractWrapperData);
begin
  raise EAssertionFailed.Create('AutoResourceStrData.DoAdd has no possible meaning');
end;

procedure TAutoResourceStrData.DoDivide(const Right: TAbstractWrapperData);
begin
  raise EAssertionFailed.Create('AutoResourceStrData.DoDivide has no possible meaning');
end;

procedure TAutoResourceStrData.DoMultiply(const Right: TAbstractWrapperData);
begin
  raise EAssertionFailed.Create('AutoResourceStrData.DoMultiply has no possible meaning');
end;

procedure TAutoResourceStrData.DoNegate;
begin
  raise EAssertionFailed.Create('AutoResourceStrData.DoNegate has no possible meaning');
end;

function TAutoResourceStrData.GetAsString: string;
begin
  //when language is changed or string on another language added to AutoResourceStr,
  //we reorder stringlist, so the most appropriate is on top
  Result := fStrings[0];
end;

procedure TAutoResourceStrData.SelectMostAppropriateLang;
var i: Integer;
begin
  //first, try to find exact match (lang + sublang)
  i := fStrings.IndexOfObject(TObject(CurrentLanguage));
  if i >= 0 then begin
    fStrings.Exchange(0, i);
    Exit;
  end;

  //if we got to that point then we have no exact match. Let's find at least
  //the same group, for example English (USA) instead of English (UK)
  for i := 0 to fStrings.Count - 1 do
    if PrimaryLangId(Integer(fStrings.Objects[i])) = PrimaryLangId(CurrentLanguage) then begin
      fStrings.Exchange(0, i);
      Exit;
    end;
  //still no match? Well, it'll fetch the first language which was added into AutoResourceStr
  //by choosing order in which locale strings are added, we may set priorities.
end;

(*
    TAutoResourceStrVarType
                                  *)
procedure TAutoResourceStrVarType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: Word);
begin
  //we only can convert to strings
  //we expect that most appropriate string will be at zero elem.
  if Source.VType = VarType then begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest,TAutoResourceStrVarData(Source).Data.fStrings[0]);
      varString{$IFDEF UNICODE}, varUString{$ENDIF}:
        VarDataFromStr(Dest,TAutoResourceStrVarData(Source).Data.fStrings[0]);
      else
        RaiseInvalidOp;
    end;
  end
    else inherited; //we were given empty variant probably
end;

function TAutoResourceStrVarType.RightPromotion(const V: TVarData; const Operator: Integer; out RequiredVarType: Word): Boolean;
begin
  Result := false; //if mountain doesn't go to Mohammed then Mohammed goes to mountain
  //we refuse to work with string, but then they'll ask string is it ok to work with us :)
end;

(*
    TAutoResourceString
                            *)
constructor TAutoResourceString.Create(aLang: Integer);
begin
  fLang := aLang;
end;

procedure TAutoResourceString.Add(var V: Variant; const text: string);
var VarData: TAutoResourceStrVarData absolute V;
begin
  //two main cases: V is unassigned (never was initialized) or it is VarAutoResourceString already
  if VarData.VType <> AutoResourceStrVarType.VarType then begin
    VarClear(V);
    VarData.Data := TAutoResourceStrData.Create;
    VarData.VType := AutoResourceStrVarType.VarType;
  end;
  //now it's of correct type and initialized.
  VarData.Data.fStrings.AddObject(text, TObject(fLang));
  VarData.Data.SelectMostAppropriateLang;
  LanguagesChanged := true;
end;

function AutoResourceString(LangID: Integer): IAutoResourceString;
begin
  Result := TAutoResourceString.Create(LangID);
end;

class function TAutoResourceString.GetLanguage: Integer;
begin
  Result := CurrentLanguage;
end;

class procedure TAutoResourceString.SetLanguage(LangID: Integer);
var i: TAutoResourceStrData;
begin
  if CurrentLanguage <> LangID then begin
    CurrentLanguage := LangID;
    //iterate through the list
    i := TopAutoResourceStr;
    while Assigned(i) do begin
      i.SelectMostAppropriateLang;
      i := i.fNext;
    end;
  end;
end;

//allows to make a 'list of available languages'.
//but it is very strict by this implementation: if one lib has
//English, German, French, Russian locales,  while another one only English,
//then only English is available.
procedure PopulateLanguages;
var cur: TAutoResourceStrData;
    i, j: Integer;
    count: Integer;
begin
  //it's working like 'AND' operator: there can't be more languages
  //than in very first entry.
  LanguagesChanged := false;
  if TopAutoResourceStr = nil then begin
    SetLength(LanguageArr, 0); //no AutoResourceStr == no languages at all
    Exit;
  end;
  count := TopAutoResourceStr.fStrings.Count;
  SetLength(LanguageArr, count);
  for i := 0 to count - 1 do
    LanguageArr[i] := Integer(TopAutoResourceStr.fStrings.Objects[i]);

  cur := TopAutoResourceStr.fPrevious;
  while cur <> nil do begin
    i := count - 1;
    while i >= 0 do begin
      //check, does this language exist (at least the same primary language)
      j := cur.fStrings.Count - 1;
      while j >= 0 do begin
        if PrimaryLangId(LanguageArr[i]) = PrimaryLangId(Integer(cur.fStrings.Objects[j])) then
          break;
        dec(j);
      end;
      if j < 0 then begin //language i not found in AutoResourceStr cur
        LanguageArr[i] := LanguageArr[count - 1]; //place the last one here
        dec(count);
      end;
      dec(i);
    end;
    cur := cur.fPrevious;
  end;
  SetLength(LanguageArr, count);
end;

class function TAutoResourceString.LanguageCount: Integer;
begin
  if LanguagesChanged then PopulateLanguages;
  Result := Length(LanguageArr);
end;

class function TAutoResourceString.Languages(index: Integer): Integer;
begin
  if LanguagesChanged then PopulateLanguages;
  Result := LanguageArr[index];
end;

//to help programmer understand, which strings weren't properly localized
class procedure TAutoResourceString.MakeReport(FileName: string);
var list: TList;
    cur: TAutoResourceStrData;
    i, j, count, LangID: Integer;
    FileStream: TFileStream;
    s: string;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    //step 1: search all the languages which were used in strings at least once
    list := TList.Create;
    cur := TopAutoResourceStr;
    while cur <> nil do begin
      for i := 0 to cur.fStrings.Count - 1 do begin
        LangID := Integer(cur.fStrings.Objects[i]);
        if list.IndexOf(Pointer(LangID)) < 0 then
          list.Add(Pointer(LangID));
      end;
      cur := cur.fPrevious;
    end;
    //we can't know variable names (it's not part of RTTI)
    //so we should keep their values at different languages together,
    //otherwise empty values are hard to identify.
    //also, let's use '->' symbol and short name of language where redirection
    //(like, from en-uk to en-us) is used

    //first, columns headers
    s := 'Count';
    for i := 0 to list.Count - 1 do
      s := s + #9 + GetShortLanguage(Integer(list[i]));
    s := s + #13 + #10;
    FileStream.Write(s[1], Length(s)*SizeOf(Char));
    //now, strings one after another
    count := 0;
    cur := TopAutoResourceStr;
    while cur <> nil do begin
      inc(count);
      s := IntToStr(count);
      for i := 0 to list.Count - 1 do begin
        j := cur.fStrings.IndexOfObject(TObject(list[i]));
        if j >= 0 then
          s := s + #9 + cur.fStrings[j]
        else begin //maybe it has at least the same prime language (En-Uk for En-Us)
          s := s + #9 + '-';
          for j := 0 to cur.fStrings.Count - 1 do
            if PrimaryLangId(Integer(list[i])) = PrimaryLangId(Integer(cur.fStrings.Objects[j])) then begin
              s := s + '>'+GetShortLanguage(Integer(cur.fStrings.Objects[j]));
              break;
            end;
        end;
      end;
      s := s + #13 + #10;
      FileStream.Write(s[1], Length(s)*SizeOf(Char));
      cur := cur.fPrevious;
    end;
    list.Free;
  finally
    FileStream.Free;
  end;
end;

initialization
  AutoResourceStrVarType:=TAutoResourceStrVarType.Create;
  CurrentLanguage := GetDefaultLanguageId;
finalization
  FreeAndNil(AutoResourceStrVarType);
end.
