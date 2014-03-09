unit proj_common;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes;

{$IFNDEF UNICODE} type
  UnicodeString = WideString; {$ENDIF}

function PosFrom(SubStr,Str: string; From: integer): integer;  
function SelectDirectory(const Caption, InitialDir: String; const Root: WideString;
                         ShowStatus: Boolean; out Directory: String): Boolean;
function valExt(str: string; var intVar: int64): boolean; overload;
function valExt(s:PChar; var position:integer; var intVar: int64): boolean; overload;
function valExt(str: string; var realVar: real): boolean; overload;
function valExt(s:PChar; var position:integer; var realVar: real): boolean; overload;

function ExtractFileDirW(const FileName: UnicodeString; Pure: boolean = false)
  : UnicodeString;
function AppPath: UnicodeString;
function AppDir: UnicodeString;

type
  TLinesOption = (loTrim, loNoEmptyLines);
  TLinesOptionSet = set of TLinesOption;

  TUnicodeLines = class
  public
    Options: TLinesOptionSet;
    Lines: array of UnicodeString;
    procedure SetTextStr(const EachString: UnicodeString);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
  end;

implementation

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Math, SysUtils, ShlObj, ActiveX, Forms;

function PosFrom(SubStr,Str: string; From: integer):integer;
var
  posStr,posSub: integer;
  L: integer;
begin
  L := Length(Str);
  if (From<0)or(From-1+Length(Substr)>Length(Str)) then
  begin
    result:=0;
    exit;
  end;
  result:=From;
  repeat
    posStr:=result;
    posSub:=1;
    while (Str[posStr]<>SubStr[posSub])and(posStr<=L) do inc(posStr);
    if posStr>L then
    begin
      result:=0;
      exit;
    end;
    result := posStr;
    posSub := 1;
    while (Str[posStr]=SubStr[posSub])and(posSub<=Length(SubStr)) do
    begin
      inc(posStr);
      inc(posSub);
    end;
    if posSub>Length(SubStr) then break;
    inc(result);
  until false;
end;

function valExt(str: string; var intVar: int64): boolean; overload;
var
  position:integer;
begin
  position:=0;
  result:=valExt(PChar(str), position, intVar);
end;

function valExt(s:PChar; var position:integer; var intVar: int64): boolean; overload;
var
  Code: integer;
  sign: boolean;
begin
  while not (s[position] in ['0'..'9',#0]) do inc(position);
  if s[position]=#0 then result:=false
  else
  begin
    Assert(s[position] in ['0'..'9']);
    sign:= (position>0)and(s[position-1]='-');
    intVar:=0;
    while s[position] in ['0'..'9'] do
    begin
      intVar:=intVar*10+ord(s[position])-ord('0');
      inc(position);
    end;
    if sign then intVar:=-intVar;
    result:=true;
  end;
end;

function valExt(str: string; var realVar: real): boolean; overload;
var
  position:integer;
begin
  position:=0;
  result:=valExt(PChar(str), position, realVar);
end;

function valExt(s:PChar; var position:integer; var realVar: real): boolean; overload;
var
  Code: integer;
  sign: integer;
  decfactor: real;
  exponent: int64;
begin
  while not (s[position] in ['0'..'9',#0]) do inc(position);
  if s[position]=#0 then result:=false
  else
  begin
    Assert(s[position] in ['0'..'9']);
    if (position>0)and(s[position-1]='-') then sign:=-1 else sign:=1;
    realVar:=0;
    while s[position] in ['0'..'9'] do
    begin
      realVar:=realVar*10+ord(s[position])-ord('0');
      inc(position);
    end;
    realVar:=sign*realVar;
    if s[position]={$IFNDEF FPC}{$IF RTLVersion>=24.00}FormatSettings.{$IFEND}{$ENDIF}DecimalSeparator then
    begin
      decfactor:=0.1;
      inc(position);
      while s[position] in ['0'..'9'] do
      begin
        realVar:=realVar+sign*(ord(s[position])-ord('0'))*decfactor;
        decfactor:=decfactor/10;
        inc(position);
      end;
    end;
    if s[position] in ['e','E'] then
    begin
      if not valExt(s,position,exponent) then
      begin
        result:=false;
        exit;
      end;
      realVar:=realVar * Power(10,exponent);
    end;
    result:=true;
  end;
end;

  // Default preserve \ at end, if Pure return directory without endig \
// except root dir like c:\ or \
function ExtractFileDirW(const FileName: UnicodeString; Pure: boolean = false)
  : UnicodeString;
var
  pos: integer;
begin
  pos := Length(FileName);
  while (pos > 0) and (FileName[pos] <> PathDelim)
{$IFDEF MSWINDOWS} and (FileName[pos] <> ':'){$ENDIF}
    do
    dec(pos);
  if pos = 0 then
    result := ''
  else
  begin
    Assert((pos >= 1) and ((FileName[pos] = PathDelim){$IFDEF MSWINDOWS} or
      (FileName[pos] = ':'){$ENDIF}));
    if Pure and (pos >= 2)
{$IFDEF MSWINDOWS} and (FileName[pos] = PathDelim) and
      (FileName[pos - 1] <> ':'){$ENDIF}
    then
      SetString(result, PWideChar(FileName), pos - 1)
    else
      SetString(result, PWideChar(FileName), pos);
  end;
end;

function GetModuleFileNameW(hModule: HINST; filename: PWideChar; size: Cardinal): Cardinal;
  stdcall; external 'kernel32.dll';

function AppPath: UnicodeString;
var
  Buffer: array [0 .. MAX_PATH] of WideChar;
  Len: LongWord;
begin
  Len := GetModuleFileNameW(0, Buffer, MAX_PATH);
  Buffer[Len] := #0;
  SetString(result, Buffer, Len);
end;

function AppDir: UnicodeString;
begin
  result := ExtractFileDirW(AppPath());
end;

const
  UTF8BOM: array [0 .. 2] of Byte = ($EF, $BB, $BF);

  { TUnicodeLines }

procedure TUnicodeLines.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure CheckConstainsZero(SA: AnsiString);
var
  i: integer;
begin
  for i := 1 to Length(SA) do
    if SA[i] = #0 then
    begin
      raise Exception.Create
        ('File has zero, it is binary file or Unicode, must be Latin or UTF8!');
    end;
end;

procedure CheckAbove127(SA: AnsiString);
var
  i: integer;
begin
  for i := 1 to Length(SA) do
    if SA[i] > #127 then
    begin
      raise Exception.Create
        ('File has non latin chars and has not BOM, must be UTF8 with BOM, not Ansi');
    end;
end;

procedure TUnicodeLines.LoadFromStream(Stream: TStream);
var
  ByteOrderMask: array [0 .. 2] of Byte;
  BytesRead: integer;
  Size: int64;
  SA: AnsiString;
  SU: UnicodeString;
begin
  Size := Stream.Size - Stream.Position;
  BytesRead := Stream.Read(ByteOrderMask[0], SizeOf(ByteOrderMask));
  if (BytesRead >= 3) and (ByteOrderMask[0] = UTF8BOM[0]) and
    (ByteOrderMask[1] = UTF8BOM[1]) and (ByteOrderMask[2] = UTF8BOM[2]) then
  begin
    SetLength(SA, (Size - 3) div SizeOf(AnsiChar));
    Stream.Read(SA[1], Size - BytesRead);
    CheckConstainsZero(SA);
    SU := UTF8Decode(SA);
  end
  else
  begin
    SetLength(SA, Size div SizeOf(AnsiChar));
    System.Move(ByteOrderMask[0], SA[1], BytesRead);
    Stream.Read(SA[1 + BytesRead], Size - BytesRead);
    CheckConstainsZero(SA);
    CheckAbove127(SA);
    SU := SA;
  end;
  SetTextStr(SU);
end;

const
  WideNull = WideChar(#0);
  WideLineFeed = WideChar(#10);
  WideCarriageReturn = WideChar(#13);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

procedure TUnicodeLines.SetTextStr(const EachString: UnicodeString);
var
  Head, Tail: PWideChar;
  LineCnt: integer;
  SU: UnicodeString;
begin
  SetLength(Lines, 0);
  LineCnt := 0;
  Head := PWideChar(EachString);
  while Head^ <> WideNull do
  begin
    Tail := Head;
    while not(Tail^ in [WideNull, WideLineFeed, WideCarriageReturn,
      WideVerticalTab, WideFormFeed]) and (Tail^ <> WideLineSeparator) and
      (Tail^ <> WideParagraphSeparator) do
      Inc(Tail);
    SetString(SU, Head, Tail - Head);
    if loTrim in Options then
      SU := Trim(SU);
    if (SU <> '') or not(loNoEmptyLines in Options) then
    begin
      Inc(LineCnt);
      SetLength(Lines, LineCnt);
      Lines[LineCnt - 1] := SU;
    end;
    Head := Tail;
    if Head^ <> WideNull then
    begin
      Inc(Head);
      if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
        Inc(Head);
    end;
  end;
end;

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
// callback function used in SelectDirectory to set the status text and choose an initial dir
var
  Path: array[0..MAX_PATH] of Char;
  X, Y: Integer;
  R: TRect;
begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        // Initialization has been done, now set our initial directory which is passed in lpData
        // (and set btw. the status text too).
        // Note: There's no need to cast lpData to a PChar since the following call needs a
        //       LPARAM parameter anyway.
        SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, lpData);

        // place the dialog screen centered
        GetWindowRect(hwnd, R);
        X := (Screen.Width - (R.Right - R.Left)) div 2;
        Y := (Screen.Height - (R.Bottom - R.Top)) div 2;
        SetWindowPos(hwnd, 0, X, Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER); 
      end;
    BFFM_SELCHANGED:
      begin
        // Set the status window to the currently selected path.
        if SHGetPathFromIDList(Pointer(lParam), Path) then SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Integer(@Path));
      end;
  end;
  Result := 0;
end;

function SelectDirectory(const Caption, InitialDir: String; const Root: WideString;
                         ShowStatus: Boolean; out Directory: String): Boolean;

// Another browse-for-folder function with the ability to select an intial directory
// (other SelectDirectory functions are in FileCtrl.pas).
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList,
  ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  Windows: Pointer;
  Path: String;
begin
  Result := False;
  Directory := '';
  Path := InitialDir;
  if (Length(Path) > 0) and (Path[Length(Path)] = '\') then Delete(Path, Length(Path), 1);
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      IDesktopFolder.ParseDisplayName(Application.Handle, nil, PWideChar(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if ShowStatus then ulFlags := ulFlags or BIF_STATUSTEXT;
        lParam := Integer(PChar(Path));
        lpfn := BrowseCallbackProc;
      end;

      // make the browser dialog modal
      Windows := DisableTaskWindows(Application.Handle);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(Windows);
      end;

      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

end.
