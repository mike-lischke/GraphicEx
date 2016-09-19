unit GraphicCompression;

// The original code is GraphicCompression.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2003 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
//
// GraphicCompression contains various encoder/decoder classes used to handle compressed
// data in the various image classes.
//
// Currently supported methods are:
// - LZW (Lempel-Ziff-Welch)
//   + TIF
//   + GIF
// - RLE (run length encoding)
//   + TGA,
//   + PCX,
//   + packbits
//   + SGI
//   + CUT
//   + RLA
//   + PSP
// - CCITT
//   + raw G3 (fax T.4)
//   + modified G3 (CCITT RLE)
//   + modified G3 w/ word alignment (CCITT RLEW)
// - LZ77
// - Thunderscan
// - JPEG
// - PCD Huffmann encoding (photo CD)
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I Compilers.inc}
{$I GraphicConfiguration.inc}

uses                                                
  Windows, Classes, SysUtils, Graphics,
  zLibEx, ZLibExApi;  // general inflate/deflate and LZ77 compression support

type
  // abstract decoder class to define the base functionality of an encoder/decoder
  TDecoder = class
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); virtual; abstract;
    procedure DecodeEnd; virtual;
    procedure DecodeInit; virtual;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); virtual; abstract;
    procedure EncodeInit; virtual;
    procedure EncodeEnd; virtual;
  end;

  // generally, there should be no need to cover the decoder classes by conditional symbols
  // because the image classes which use the decoder classes are already covered and if they
  // aren't compiled then the decoders are also not compiled (more precisely: not linked)
  TTargaRLEDecoder = class(TDecoder)
  private
    FColorDepth: Cardinal;
  public
    constructor Create(ColorDepth: Cardinal); 
    
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  // Lempel-Ziff-Welch encoder/decoder class
  // TIFF LZW compression / decompression is a bit different to the common LZW code
  TTIFFLZWDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPackbitsRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPCXRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TSGIRLEDecoder = class(TDecoder)
  private
    FSampleSize: Byte; // 8 or 16 bits
  public
    constructor Create(SampleSize: Byte);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TCUTRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPSPRLEDecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  // Note: We need a different LZW decoder class for GIF because the bit order is reversed compared to that
  //       of TIFF and the code size increment is handled slightly different.
  TGIFLZWDecoder = class(TDecoder)
  private
    FInitialCodeSize: Byte;
  public
    constructor Create(InitialCodeSize: Byte);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TRLADecoder = class(TDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TStateEntry = record
    NewState: array[Boolean] of Cardinal;
    RunLength: Integer;
  end;
  TStateArray = array of TStateEntry;

  TCCITTDecoder = class(TDecoder)
  private
    FOptions: Integer; // determines some options how to proceed
                       // Bit 0: if set then two-dimensional encoding was used, otherwise one-dimensional
                       // Bit 1: if set then data is uncompressed
                       // Bit 2: if set then fill bits are used before EOL codes so that EOL codes always end at
                       //        at a byte boundary (not used in this context)
    FIsWhite,          // alternating flag used while coding
    FSwapBits: Boolean; // True if the order of all bits in a byte must be swapped
    FWhiteStates,
    FBlackStates,
    F2DStates: TStateArray;
    FWidth: Cardinal; // need to know how line length for modified huffman encoding

    // coding/encoding variables
    FBitsLeft,
    FMask,
    FBits: Byte;
    FPackedSize,
    FRestWidth: Cardinal;
    FSource,
    FTarget: PByte;
    FFreeTargetBits: Byte;
    FWordAligned: Boolean;

    //some fields for 2D compression. Will be used in Fax3 (T4) as well as Fax4 (T6) decoders
    fChangingElems: array [Boolean] of array of Integer;  //coordinates of these pixels on prev. row
    fcurChangingElem: Integer; //even (0,2,...) means changing from white to black,
                            //odd (1,3,...) are black to white
    fprevChangingElem: Integer;
    fRowUsed: Boolean;
    fBitPos: Integer;

    procedure MakeStates;
  protected
    procedure ReverseBits(Source: Pointer; PackedSize: Integer);
    function FillRun(RunLength: Cardinal): Boolean;
    function DoFiniteStateMachine(const states: TStateArray): Integer;
    function FindRunLength: Integer;
    function Find2DCode:    Integer;
    function NextBit: Boolean;

    //2D decompression routines
    procedure UpdateChangingElem;
  public
    constructor Create(Options: Integer; SwapBits, WordAligned: Boolean; Width: Cardinal);
  end;

  TCCITTFax3Decoder = class(TCCITTDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TCCITTFax4Decoder = class (TCCITTDecoder)
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TCCITTMHDecoder = class(TCCITTDecoder) // modified Huffman RLE
  public
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TLZ77Decoder = class(TDecoder)
  private
    FStream: TZStreamRec;
    FZLibResult,         // contains the return code of the last ZLib operation
    FFlushMode: Integer; // one of flush constants declard in ZLib.pas
                         // this is usually Z_FINISH for PSP and Z_PARTIAL_FLUSH for PNG
    FAutoReset: Boolean; // TIF, PSP and PNG share this decoder, TIF needs a reset for each
                         // decoder run
    function GetAvailableInput: Integer;
    function GetAvailableOutput: Integer;

  public
    constructor Create(FlushMode: Integer; AutoReset: Boolean);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure DecodeEnd; override;
    procedure DecodeInit; override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;

    property AvailableInput: Integer read GetAvailableInput;
    property AvailableOutput: Integer read GetAvailableOutput;
    property ZLibResult: Integer read FZLibResult;
  end;

  TThunderDecoder = class(TDecoder)
  private
    FWidth: Cardinal; // width of a scanline in pixels
  public
    constructor Create(Width: Cardinal);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  TPCDDecoder = class(TDecoder)
  private
    FData: PByte;  // decoder must read some data
  public
    constructor Create(Raw: Pointer);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math,
  GraphicEx,
  GraphicStrings,
  GraphicColor;

const // LZW encoding and decoding support
  NoLZWCode = 4096;

type
  EGraphicCompression = class(Exception);

//----------------------------------------------------------------------------------------------------------------------

procedure CompressionError(ErrorString: String); overload;

begin
  raise EGraphicCompression.Create(ErrorString);
end;

//----------------- TDecoder (generic decoder class) -------------------------------------------------------------------

procedure TDecoder.DecodeEnd;

// called after all decompression has been done

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.DecodeInit;

// called before any decompression can start

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.EncodeEnd;

// called after all compression has been done

begin
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDecoder.EncodeInit;

// called before any compression can start

begin
end;

//----------------- TTargaRLEDecoder -----------------------------------------------------------------------------------

constructor TTargaRLEDecoder.Create(ColorDepth: Cardinal);

begin
  FColorDepth := ColorDepth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..MaxInt div 4 - 1] of Cardinal;

var
  I: Integer;
  SourcePtr,
  TargetPtr: PByte;
  RunLength: Cardinal;
  SourceCardinal: Cardinal;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  // unrolled decoder loop to speed up process
  case FColorDepth of
    8:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          FillChar(TargetPtr^, RunLength, SourcePtr^);
          Inc(TargetPtr, RunLength);
          Inc(SourcePtr);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, RunLength);
          Inc(SourcePtr, RunLength);
          Inc(TargetPtr, RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    15,
    16:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 2);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 2 * RunLength);
          Inc(SourcePtr, 2 * RunLength);
          Inc(TargetPtr, 2 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    24:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          for I := 0 to RunLength - 1 do
          begin
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Inc(SourcePtr);
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^;
            Dec(SourcePtr, 2);
            Inc(TargetPtr);
          end;
          Inc(SourcePtr, 3);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 3 * RunLength);
          Inc(SourcePtr, 3 * RunLength);
          Inc(TargetPtr, 3 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
    32:
      while UnpackedSize > 0 do
      begin
        RunLength := 1 + (SourcePtr^ and $7F);
        if SourcePtr^ > $7F then
        begin
          Inc(SourcePtr);
          SourceCardinal := PCardinalArray(SourcePtr)[0];
          for I := 0 to RunLength - 1 do
            PCardinalArray(TargetPtr)[I] := SourceCardinal;

          Inc(TargetPtr, 4 * RunLength);
          Inc(SourcePtr, 4);
        end
        else
        begin
          Inc(SourcePtr);
          Move(SourcePtr^, TargetPtr^, 4 * RunLength);
          Inc(SourcePtr, 4 * RunLength);
          Inc(TargetPtr, 4 * RunLength);
        end;
        Dec(UnpackedSize, RunLength);
      end;
  end;

  Source := SourcePtr;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetPixel(P: PByte; BPP: Byte): Cardinal;

// Retrieves a pixel value from a Buffer. The actual size and order of the bytes is not important
// since we are only using the value for comparisons with other pixels.

begin
  Result := P^;
  Inc(P);
  Dec(BPP);
  while BPP > 0 do
  begin
    Result := Result shl 8;
    Result := Result or P^;
    Inc(P);
    Dec(BPP);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountDiffPixels(P: PByte; BPP: Byte; Count: Integer): Integer;

// counts pixels in Buffer until two identical adjacent ones found

var
  N: Integer;
  Pixel,
  NextPixel: Cardinal;

begin
  N := 0;
  NextPixel := 0; // shut up compiler
  if Count = 1 then Result := Count
               else
  begin
    Pixel := GetPixel(P, BPP);
    while Count > 1 do
    begin
      Inc(P, BPP);
      NextPixel := GetPixel(P, BPP);
      if NextPixel = Pixel then Break;
      Pixel := NextPixel;
      Inc(N);
      Dec(Count);
    end;
    if NextPixel = Pixel then
      Result := N
    else
      Result := N + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CountSamePixels(P: PByte; BPP: Byte; Count: Integer): Integer;

var
  Pixel,
  NextPixel: Cardinal;

begin
  Result := 1;
  Pixel := GetPixel(P, BPP);
  Dec(Count);
  while Count > 0 do
  begin
    Inc(P, BPP);
    NextPixel := GetPixel(P, BPP);
    if NextPixel <> Pixel then
      Break;
    Inc(Result);
    Dec(Count);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTargaRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

// Encodes "Count" bytes pointed to by Source into the Buffer supplied with Target and returns the
// number of bytes stored in Target. BPP denotes bytes per pixel color depth.
// Note: The target Buffer must provide enough space to hold the compressed data. Using a size of
//       twice the size of the input Buffer is sufficent.

var
  DiffCount, // pixel count until two identical
  SameCount: Integer; // number of identical adjacent pixels
  SourcePtr,
  TargetPtr: PByte;
  BPP: Integer;

begin
  BytesStored := 0;
  SourcePtr := Source;
  TargetPtr := Dest;
  BytesStored := 0;
  // +1 for 15 bits to get the correct 2 bytes per pixel
  BPP := (FColorDepth + 1) div 8;
  while Count > 0 do
  begin
    DiffCount := CountDiffPixels(SourcePtr, BPP, Count);
    SameCount := CountSamePixels(SourcePtr, BPP, Count);
    if DiffCount > 128 then
      DiffCount := 128;
    if SameCount > 128  then
      SameCount := 128;

    if DiffCount > 0 then
    begin
      // create a raw packet
      TargetPtr^ := DiffCount - 1; Inc(TargetPtr);
      Dec(Count, DiffCount);
      Inc(BytesStored, (DiffCount * BPP) + 1);
      while DiffCount > 0 do
      begin
        TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
        if BPP > 1 then
        begin
          TargetPtr^ := SourcePtr^;
          Inc(SourcePtr);
          Inc(TargetPtr);
        end;
        if BPP > 2 then
        begin
          TargetPtr^ := SourcePtr^;
          Inc(SourcePtr);
          Inc(TargetPtr);
        end;
        if BPP > 3 then
        begin
          TargetPtr^ := SourcePtr^;
          Inc(SourcePtr);
          Inc(TargetPtr);
        end;
        Dec(DiffCount);
      end;
    end;

    if SameCount > 1 then
    begin
      // create a RLE packet
      TargetPtr^ := (SameCount - 1) or $80; Inc(TargetPtr);
      Dec(Count, SameCount);
      Inc(BytesStored, BPP + 1);
      Inc(SourcePtr, (SameCount - 1) * BPP);
      TargetPtr^ := SourcePtr^; Inc(SourcePtr); Inc(TargetPtr);
      if BPP > 1 then
      begin
        TargetPtr^ := SourcePtr^;
        Inc(SourcePtr);
        Inc(TargetPtr);
      end;
      if BPP > 2 then
      begin
        TargetPtr^ := SourcePtr^;
        Inc(SourcePtr);
        Inc(TargetPtr);
      end;
      if BPP > 3 then
      begin
        TargetPtr^ := SourcePtr^;
        Inc(SourcePtr);
        Inc(TargetPtr);
      end;
    end;
  end;
end;

//----------------- TTIFFLZWDecoder ------------------------------------------------------------------------------------

procedure TTIFFLZWDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  Target: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  Target := Dest;
  SourcePtr := Source;

  // initialize parameter
  ClearCode := 1 shl 8;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeSize := 9;
  CodeMask := (1 shl CodeSize) - 1;

  // init code table
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := NoLZWCode;
    Suffix[I] := I;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;  
  while (PackedSize > 0) and (UnpackedSize > 0) do
  begin
    // read code from bit stream
    Inc(Data, Cardinal(SourcePtr^) shl (24 - Bits));
    Inc(Bits, 8);
    while Bits >= CodeSize do
    begin
      // current code
      Code := (Data and ($FFFFFFFF - CodeMask)) shr (32 - CodeSize);
      // mask it
      Data := Data shl CodeSize;
      Dec(Bits, CodeSize);

      if Code = EOICode then
        Exit;

      // handling of clear codes
      if Code = ClearCode then
      begin
        // reset of all variables
        CodeSize := 9;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then
        Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then
      begin
        FirstChar := Suffix[Code];
        Target^ := FirstChar;
        Inc(Target);
        Dec(UnpackedSize);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;  

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;
      if FreeCode < 4096 then
        Inc(FreeCode);

      // increase code size if necessary
      if (FreeCode = CodeMask) and
         (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(UnpackedSize);
      until Cardinal(StackPointer) <= Cardinal(@Stack);
    end;
    Inc(SourcePtr);
    Dec(PackedSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFLZWDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TPackbitsRLEDecoder --------------------------------------------------------------------------------

procedure TPackbitsRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// decodes a simple run-length encoded strip of size PackedSize

var
  SourcePtr,
  TargetPtr: PByte;
  N: Integer;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while (UnpackedSize > 0) and (PackedSize > 0) do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N < 0 then // replicate next Byte -N + 1 times
    begin
      if N = -128 then
        Continue; // nop
      N := -N + 1;
      if N > UnpackedSize then
        N := UnpackedSize;
      FillChar(TargetPtr^, N, SourcePtr^);
      Inc(SourcePtr);
      Dec(PackedSize);
      Inc(TargetPtr, N);
      Dec(UnpackedSize, N);
    end
    else
    begin // copy next N + 1 bytes literally
      Inc(N);
      if N > UnpackedSize then
        N := UnpackedSize;
      if N > PackedSize then
        N := PackedSize;
      Move(SourcePtr^, TargetPtr^, N);
      Inc(TargetPtr, N);
      Inc(SourcePtr, N);
      Dec(PackedSize, N);
      Dec(UnpackedSize, N);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPackbitsRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TPCXRLEDecoder -------------------------------------------------------------------------------------

procedure TPCXRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  Count: Integer;
  SourcePtr,
  TargetPtr: PByte;

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while UnpackedSize > 0 do
  begin
    if (SourcePtr^ and $C0) = $C0 then
    begin
      // RLE-Code
      Count := SourcePtr^ and $3F;
      Inc(SourcePtr);
      if UnpackedSize < Count then
        Count := UnpackedSize;
      FillChar(TargetPtr^, Count, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, Count);
      Dec(UnpackedSize, Count);
    end
    else
    begin
      // not compressed
      TargetPtr^ := SourcePtr^;
      Inc(SourcePtr);
      Inc(TargetPtr);
      Dec(UnpackedSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCXRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin

end;

//----------------- TSGIRLEDecoder -------------------------------------------------------------------------------------

constructor TSGIRLEDecoder.Create(SampleSize: Byte);

begin
  FSampleSize := SampleSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  Source8,
  Target8: PByte;
  Source16,
  Target16: PWord;
  Pixel: Byte;
  Pixel16: Word;
  RunLength: Cardinal;

begin
  if FSampleSize = 8 then
  begin
    Source8 := Source;
    Target8 := Dest;
    while True do
    begin
      Pixel := Source8^;
      Inc(Source8);
      RunLength := Pixel and $7F;
      if RunLength = 0 then
        Break;

      if (Pixel and $80) <> 0 then
      begin
        Move(Source8^, Target8^, RunLength);
        Inc(Target8, RunLength);
        Inc(Source8, RunLength);
      end
      else
      begin
        Pixel := Source8^;
        Inc(Source8);
        FillChar(Target8^, RunLength, Pixel);
        Inc(Target8, RunLength);
      end;
    end;
  end
  else
  begin
    // 16 bits per sample
    Source16 := Source;
    Target16 := Dest;
    while True do
    begin
      // SGI images are stored in big endian style, swap this one repeater value for it
      Pixel16 := Swap(Source16^);
      Inc(Source16);
      RunLength := Pixel16 and $7F;
      if RunLength = 0 then
        Break;

      if (Pixel16 and $80) <> 0 then
      begin
        Move(Source16^, Target16^, 2 * RunLength);
        Inc(Source16, RunLength);
        Inc(Target16, RunLength);
      end
      else
      begin
        Pixel16 := Source16^;
        Inc(Source16);
        while RunLength > 0 do
        begin
          Target16^ := Pixel16;
          Inc(Target16);
          Dec(RunLength);
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TCUTRLE --------------------------------------------------------------------------------------------

procedure TCUTRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  TargetPtr: PByte;
  Pixel: Byte;
  RunLength: Cardinal;
  Run: PByte absolute Source; //PByte alias for Source

begin
  TargetPtr := Dest;
  // Skip first two bytes per row (I don't know their meaning).
  Inc(Run, 2);
  while True do
  begin
    Pixel := Run^;
    Inc(Run);
    if Pixel = 0 then
      Break;

    RunLength := Pixel and $7F;
    if (Pixel and $80) = 0 then
    begin
      Move(Run^, TargetPtr^, RunLength);
      Inc(TargetPtr, RunLength);
      Inc(Run, RunLength);
    end
    else
    begin
      Pixel := Run^;
      Inc(Run);
      FillChar(TargetPtr^, RunLength, Pixel);
      Inc(TargetPtr, RunLength);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TPSPRLEDecoder -------------------------------------------------------------------------------------

procedure TPSPRLEDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  SourcePtr,
  TargetPtr: PByte;
  RunLength: Cardinal;

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while PackedSize > 0 do
  begin
    RunLength := SourcePtr^;
    Inc(SourcePtr);
    Dec(PackedSize);
    if RunLength < 128 then
    begin
      Move(SourcePtr^, TargetPtr^, RunLength);
      Inc(TargetPtr, RunLength);
      Inc(SourcePtr, RunLength);
      Dec(PackedSize, RunLength);
    end
    else
    begin
      Dec(RunLength, 128);
      FillChar(TargetPtr^, RunLength, SourcePtr^);
      Inc(SourcePtr);
      Inc(TargetPtr, RunLength);
      Dec(PackedSize);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSPRLEDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TGIFLZWDecoder -------------------------------------------------------------------------------------

constructor TGIFLZWDecoder.Create(InitialCodeSize: Byte);

begin
  FInitialCodeSize := InitialCodeSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFLZWDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  I: Integer;
  Data,           // current data
  Bits,           // counter for bit management
  Code: Cardinal; // current code value
  SourcePtr: PByte;
  InCode: Cardinal; // Buffer for passed code

  CodeSize: Cardinal;
  CodeMask: Cardinal;
  FreeCode: Cardinal;
  OldCode: Cardinal;
  Prefix: array[0..4095] of Cardinal; // LZW prefix
  Suffix,                             // LZW suffix
  Stack: array [0..4095] of Byte;     // stack
  StackPointer: PByte;
  Target: PByte;
  FirstChar: Byte;  // Buffer for decoded byte
  ClearCode,
  EOICode: Word;

begin
  Target := Dest;
  SourcePtr := Source;

  // initialize parameter
  CodeSize := FInitialCodeSize + 1;
  ClearCode := 1 shl FInitialCodeSize;
  EOICode := ClearCode + 1;
  FreeCode := ClearCode + 2;
  OldCode := NoLZWCode;
  CodeMask := (1 shl CodeSize) - 1;

  // init code table
  for I := 0 to ClearCode - 1 do
  begin
    Prefix[I] := NoLZWCode;
    Suffix[I] := I;
  end;

  // initialize stack
  StackPointer := @Stack;
  FirstChar := 0;

  Data := 0;
  Bits := 0;
  while (UnpackedSize > 0) and (PackedSize > 0) do
  begin
    // read code from bit stream
    Inc(Data, SourcePtr^ shl Bits);
    Inc(Bits, 8);
    while (Bits >= CodeSize) and (UnpackedSize > 0) do
    begin
      // current code
      Code := Data and CodeMask;
      // prepare next run
      Data := Data shr CodeSize;
      Dec(Bits, CodeSize);

      // decoding finished?
      if Code = EOICode then
        Break;

      // handling of clear codes
      if Code = ClearCode then
      begin
        // reset of all variables
        CodeSize := FInitialCodeSize + 1;
        CodeMask := (1 shl CodeSize) - 1;
        FreeCode := ClearCode + 2;
        OldCode := NoLZWCode;
        Continue;
      end;

      // check whether it is a valid, already registered code
      if Code > FreeCode then
        Break;

      // handling for the first LZW code: print and keep it
      if OldCode = NoLZWCode then
      begin
        FirstChar := Suffix[Code];
        Target^ := FirstChar;
        Inc(Target);
        Dec(UnpackedSize);
        OldCode := Code;
        Continue;
      end;

      // keep the passed LZW code
      InCode := Code;

      // the first LZW code is always smaller than FFirstCode
      if Code = FreeCode then
      begin
        StackPointer^ := FirstChar;
        Inc(StackPointer);
        Code := OldCode;
      end;

      // loop to put decoded bytes onto the stack
      while Code > ClearCode do
      begin
        StackPointer^ := Suffix[Code];
        Inc(StackPointer);
        Code := Prefix[Code];
      end;

      // place new code into code table
      FirstChar := Suffix[Code];
      StackPointer^ := FirstChar;
      Inc(StackPointer);
      Prefix[FreeCode] := OldCode;
      Suffix[FreeCode] := FirstChar;

      // increase code size if necessary
      if (FreeCode = CodeMask) and (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;
      if FreeCode < 4095 then
        Inc(FreeCode);

      // put decoded bytes (from the stack) into the target Buffer
      OldCode := InCode;
      repeat
        Dec(StackPointer);
        Target^ := StackPointer^;
        Inc(Target);
        Dec(UnpackedSize);
      until StackPointer = @Stack;
    end;
    Inc(SourcePtr);
    Dec(PackedSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFLZWDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TRLADecoder ----------------------------------------------------------------------------------------

procedure TRLADecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// decodes a simple run-length encoded strip of size PackedSize
// this is very similar to TPackbitsRLEDecoder

var
  SourcePtr,
  TargetPtr: PByte;
  N: SmallInt;

begin
  TargetPtr := Dest;
  SourcePtr := Source;
  while PackedSize > 0 do
  begin
    N := ShortInt(SourcePtr^);
    Inc(SourcePtr);
    Dec(PackedSize);
    if N >= 0 then // replicate next Byte N + 1 times
    begin
      FillChar(TargetPtr^, N + 1, SourcePtr^);
      Inc(TargetPtr, N + 1);
      Inc(SourcePtr);
      Dec(PackedSize);
    end
    else
    begin // copy next -N bytes literally
      Move(SourcePtr^, TargetPtr^, -N);
      Inc(TargetPtr, -N);
      Inc(SourcePtr, -N);
      Inc(PackedSize, N);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLADecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TCCITTDecoder --------------------------------------------------------------------------------------

constructor TCCITTDecoder.Create(Options: Integer; SwapBits, WordAligned: Boolean; Width: Cardinal);
begin
  FOptions := Options;
  FSwapBits := SwapBits;
  FWidth := Width;
  FWordAligned := WordAligned;
  MakeStates;
end;

//----------------------------------------------------------------------------------------------------------------------

var ReverseTable: array [0..255] of Byte;


const
  G3_EOL = -1;
  G3_INVALID = -2;


//----------------------------------------------------------------------------------------------------------------------
procedure TCCITTDecoder.ReverseBits(Source: Pointer; PackedSize: Integer);
{$IFDEF ResortToPurePascal}
var i: Integer;
begin
  for i := PackedSize-1 downto 0 do
      PByteArray(Source)^[i] := ReverseTable[PByteArray(Source)^[i]];
{$ELSE}
  asm
  //Source is in EDX
  //PackedSize is in ECX
         PUSH EBX
         LEA EBX, ReverseTable
  @@1:
         MOV AL, [EDX]
         {$ifdef COMPILER_6}
           XLATB
         {$else}
           XLAT
         {$endif COMPILER_6}
         MOV [EDX], AL
         INC EDX
         DEC ECX
         JNZ @@1
         POP EBX
{$ENDIF}
end;


function TCCITTDecoder.FillRun(RunLength: Cardinal): Boolean;

// fills a number of bits with 1s (for black, white only increments pointers),
// returns True if the line has been filled entirely, otherwise False

var
  Run: Cardinal;

begin
  Run := Min(FFreeTargetBits, RunLength);
  // fill remaining bits in the current byte
  if Run in [1..7] then
  begin
    Dec(FFreeTargetBits, Run);
    if not FIsWhite then
      FTarget^ := FTarget^ or (((1 shl Run) - 1) shl FFreeTargetBits);
    if FFreeTargetBits = 0 then
    begin
      Inc(FTarget);
      FFreeTargetBits := 8;
    end;
    Run := RunLength - Run;
  end
  else
    Run := RunLength;

  // fill entire bytes whenever possible
  if Run > 0 then
  begin
    if not FIsWhite then
      FillChar(FTarget^, Run div 8, $FF);
    Inc(FTarget, Run div 8);
    Run := Run mod 8;
  end;

  // finally fill remaining bits
  if Run > 0 then
  begin
    FFreeTargetBits := 8 - Run;
    if not FIsWhite then
      FTarget^ := ((1 shl Run) - 1) shl FFreeTargetBits;
  end;

  // this will throw an exception if the sum of the run lengths for a row is not
  // exactly the row size (the documentation speaks of an unrecoverable error)
  if Cardinal(RunLength) > FRestWidth then
    RunLength := FRestWidth;
  Dec(FRestWidth, RunLength);
  Result := FRestWidth = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.DoFiniteStateMachine(const states: TStateArray): Integer;

// Executes the state machine to find the run length for the next bit combination.
// Returns the run length of the found code.

var
  State,
  NewState: Cardinal;
  Bit: Boolean;

begin
  State := 0;
  Result := 0;
  repeat
    // advance to next byte in the input Buffer if necessary
    if FBitsLeft = 0 then
    begin
      if FPackedSize = 0 then
        Break;
      FBits := FSource^;
      Inc(FSource);
      Dec(FPackedSize);
      FMask := $80;
      FBitsLeft := 8;
    end;
    Bit := (FBits and FMask) <> 0;

    // advance the state machine
    NewState := states[State].NewState[Bit];
    if NewState = 0 then
    begin
      Inc(Result, states[State].RunLength);
      if states[State].RunLength < 64 then
        Break
      else
      begin
        NewState := states[0].NewState[Bit];
      end;
    end;
    State := NewState;

    // address next bit
    FMask := FMask shr 1;
    if FBitsLeft > 0 then
      Dec(FBitsLeft);

  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.FindRunLength: Integer;
begin
  if FIsWhite then
    Result := DoFiniteStateMachine(FWhiteStates)
  else
    Result := DoFiniteStateMachine(FBlackStates);
end;

function TCCITTDecoder.Find2DCode: Integer;
begin
  Result := DoFiniteStateMachine(F2DStates);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCCITTDecoder.NextBit: Boolean;

// Reads the current bit and returns True if it is set, otherwise False.
// This method is only used in the process to synchronize the bit stream in descentants.

begin
  // advance to next byte in the input Buffer if necessary
  if (FBitsLeft = 0) and (FPackedSize > 0) then
  begin
    FBits := FSource^;
    Inc(FSource);
    Dec(FPackedSize);
    FMask := $80;
    FBitsLeft := 8;
  end;
  Result := (FBits and FMask) <> 0;

  // address next bit
  FMask := FMask shr 1;
  if FBitsLeft > 0 then
    Dec(FBitsLeft);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TCodeEntry = record
    Code, Len: Cardinal;
  end;

const // CCITT code tables
  WhiteCodes: array[0..103] of TCodeEntry = (
    (Code : $0035; Len : 8),
    (Code : $0007; Len : 6),
    (Code : $0007; Len : 4),
    (Code : $0008; Len : 4),
    (Code : $000B; Len : 4),
    (Code : $000C; Len : 4),
    (Code : $000E; Len : 4),
    (Code : $000F; Len : 4),
    (Code : $0013; Len : 5),
    (Code : $0014; Len : 5),
    (Code : $0007; Len : 5),
    (Code : $0008; Len : 5),
    (Code : $0008; Len : 6),
    (Code : $0003; Len : 6),
    (Code : $0034; Len : 6),
    (Code : $0035; Len : 6),
    (Code : $002A; Len : 6),
    (Code : $002B; Len : 6),
    (Code : $0027; Len : 7),
    (Code : $000C; Len : 7),
    (Code : $0008; Len : 7),
    (Code : $0017; Len : 7),
    (Code : $0003; Len : 7),
    (Code : $0004; Len : 7),
    (Code : $0028; Len : 7),
    (Code : $002B; Len : 7),
    (Code : $0013; Len : 7),
    (Code : $0024; Len : 7),
    (Code : $0018; Len : 7),
    (Code : $0002; Len : 8),
    (Code : $0003; Len : 8),
    (Code : $001A; Len : 8),
    (Code : $001B; Len : 8),
    (Code : $0012; Len : 8),
    (Code : $0013; Len : 8),
    (Code : $0014; Len : 8),
    (Code : $0015; Len : 8),
    (Code : $0016; Len : 8),
    (Code : $0017; Len : 8),
    (Code : $0028; Len : 8),
    (Code : $0029; Len : 8),
    (Code : $002A; Len : 8),
    (Code : $002B; Len : 8),
    (Code : $002C; Len : 8),
    (Code : $002D; Len : 8),
    (Code : $0004; Len : 8),
    (Code : $0005; Len : 8),
    (Code : $000A; Len : 8),
    (Code : $000B; Len : 8),
    (Code : $0052; Len : 8),
    (Code : $0053; Len : 8),
    (Code : $0054; Len : 8),
    (Code : $0055; Len : 8),
    (Code : $0024; Len : 8),
    (Code : $0025; Len : 8),
    (Code : $0058; Len : 8),
    (Code : $0059; Len : 8),
    (Code : $005A; Len : 8),
    (Code : $005B; Len : 8),
    (Code : $004A; Len : 8),
    (Code : $004B; Len : 8),
    (Code : $0032; Len : 8),
    (Code : $0033; Len : 8),
    (Code : $0034; Len : 8),
    (Code : $001B; Len : 5),
    (Code : $0012; Len : 5),
    (Code : $0017; Len : 6),
    (Code : $0037; Len : 7),
    (Code : $0036; Len : 8),
    (Code : $0037; Len : 8),
    (Code : $0064; Len : 8),
    (Code : $0065; Len : 8),
    (Code : $0068; Len : 8),
    (Code : $0067; Len : 8),
    (Code : $00CC; Len : 9),
    (Code : $00CD; Len : 9),
    (Code : $00D2; Len : 9),
    (Code : $00D3; Len : 9),
    (Code : $00D4; Len : 9),
    (Code : $00D5; Len : 9),
    (Code : $00D6; Len : 9),
    (Code : $00D7; Len : 9),
    (Code : $00D8; Len : 9),
    (Code : $00D9; Len : 9),
    (Code : $00DA; Len : 9),
    (Code : $00DB; Len : 9),
    (Code : $0098; Len : 9),
    (Code : $0099; Len : 9),
    (Code : $009A; Len : 9),
    (Code : $0018; Len : 6),
    (Code : $009B; Len : 9),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

  BlackCodes: array[0..103] of TCodeEntry = (
    (Code : $0037; Len : 10),
    (Code : $0002; Len : 3),
    (Code : $0003; Len : 2),
    (Code : $0002; Len : 2),
    (Code : $0003; Len : 3),
    (Code : $0003; Len : 4),
    (Code : $0002; Len : 4),
    (Code : $0003; Len : 5),
    (Code : $0005; Len : 6),
    (Code : $0004; Len : 6),
    (Code : $0004; Len : 7),
    (Code : $0005; Len : 7),
    (Code : $0007; Len : 7),
    (Code : $0004; Len : 8),
    (Code : $0007; Len : 8),
    (Code : $0018; Len : 9),
    (Code : $0017; Len : 10),
    (Code : $0018; Len : 10),
    (Code : $0008; Len : 10),
    (Code : $0067; Len : 11),
    (Code : $0068; Len : 11),
    (Code : $006C; Len : 11),
    (Code : $0037; Len : 11),
    (Code : $0028; Len : 11),
    (Code : $0017; Len : 11),
    (Code : $0018; Len : 11),
    (Code : $00CA; Len : 12),
    (Code : $00CB; Len : 12),
    (Code : $00CC; Len : 12),
    (Code : $00CD; Len : 12),
    (Code : $0068; Len : 12),
    (Code : $0069; Len : 12),
    (Code : $006A; Len : 12),
    (Code : $006B; Len : 12),
    (Code : $00D2; Len : 12),
    (Code : $00D3; Len : 12),
    (Code : $00D4; Len : 12),
    (Code : $00D5; Len : 12),
    (Code : $00D6; Len : 12),
    (Code : $00D7; Len : 12),
    (Code : $006C; Len : 12),
    (Code : $006D; Len : 12),
    (Code : $00DA; Len : 12),
    (Code : $00DB; Len : 12),
    (Code : $0054; Len : 12),
    (Code : $0055; Len : 12),
    (Code : $0056; Len : 12),
    (Code : $0057; Len : 12),
    (Code : $0064; Len : 12),
    (Code : $0065; Len : 12),
    (Code : $0052; Len : 12),
    (Code : $0053; Len : 12),
    (Code : $0024; Len : 12),
    (Code : $0037; Len : 12),
    (Code : $0038; Len : 12),
    (Code : $0027; Len : 12),
    (Code : $0028; Len : 12),
    (Code : $0058; Len : 12),
    (Code : $0059; Len : 12),
    (Code : $002B; Len : 12),
    (Code : $002C; Len : 12),
    (Code : $005A; Len : 12),
    (Code : $0066; Len : 12),
    (Code : $0067; Len : 12),
    (Code : $000F; Len : 10),
    (Code : $00C8; Len : 12),
    (Code : $00C9; Len : 12),
    (Code : $005B; Len : 12),
    (Code : $0033; Len : 12),
    (Code : $0034; Len : 12),
    (Code : $0035; Len : 12),
    (Code : $006C; Len : 13),
    (Code : $006D; Len : 13),
    (Code : $004A; Len : 13),
    (Code : $004B; Len : 13),
    (Code : $004C; Len : 13),
    (Code : $004D; Len : 13),
    (Code : $0072; Len : 13),
    (Code : $0073; Len : 13),
    (Code : $0074; Len : 13),
    (Code : $0075; Len : 13),
    (Code : $0076; Len : 13),
    (Code : $0077; Len : 13),
    (Code : $0052; Len : 13),
    (Code : $0053; Len : 13),
    (Code : $0054; Len : 13),
    (Code : $0055; Len : 13),
    (Code : $005A; Len : 13),
    (Code : $005B; Len : 13),
    (Code : $0064; Len : 13),
    (Code : $0065; Len : 13),
    (Code : $0008; Len : 11),
    (Code : $000C; Len : 11),
    (Code : $000D; Len : 11),
    (Code : $0012; Len : 12),
    (Code : $0013; Len : 12),
    (Code : $0014; Len : 12),
    (Code : $0015; Len : 12),
    (Code : $0016; Len : 12),
    (Code : $0017; Len : 12),
    (Code : $001C; Len : 12),
    (Code : $001D; Len : 12),
    (Code : $001E; Len : 12),
    (Code : $001F; Len : 12)
    // EOL codes are added "manually"
  );

  TwoDimCodes: array [0..10] of TCodeEntry = (
    (Code : $0001; Len : 4),  //Pass
    (Code : $0001; Len : 3),  //Horizontal (start)
    (Code : $0001; Len : 1),  //V(0)
    (Code : $0003; Len : 3),  //VR(1)
    (Code : $0003; Len : 6),  //VR(2)
    (Code : $0003; Len : 7),  //VR(3)
    (Code : $0002; Len : 3),  //VL(1)
    (Code : $0002; Len : 6),  //VL(2)
    (Code : $0002; Len : 7),  //VL(3)
    (Code : $0001; Len : 7),  //2-D extensions
    (Code : $0001; Len : 9)  //1-D extensions
  );

  cePass = 0;
  ceHorizontal = 1;


procedure TCCITTDecoder.MakeStates;

// creates state arrays for white and black codes
// These state arrays are so designed that they have at each state (starting with state 0) a new state index
// into the same array according to the bit for which the state is current.

  //--------------- local functions -------------------------------------------

  procedure AddCode(var Target: TStateArray; Bits: Cardinal; BitLen, RL: Integer);

  // interprets the given string as a sequence of bits and makes a state chain from it

  var
    State,
    NewState: Integer;
    Bit: Boolean;

  begin
    // start state
    State := 0;
    // prepare bit combination (bits are given right align, but must be scanned from left)
    Bits := Bits shl (32 - BitLen);
    while BitLen > 0 do
    begin
      // determine next state according to the bit string
      {$IFDEF ResortToPurePascal}
        Bit := (Bits and $80000000) <> 0;
        Bits := Bits shl 1;
      {$ELSE}
        asm
          SHL [Bits], 1
          SETC [Bit]
        end;
      {$ENDIF}
      NewState := Target[State].NewState[Bit];
      // Is it a not yet assigned state?
      if NewState = 0 then
      begin
        // if yes then create a new state at the end of the array
        NewState := Length(Target);
        Target[State].NewState[Bit] := NewState;
        SetLength(Target, Length(Target) + 1);
      end;
      State := NewState;

      Dec(BitLen);
    end;
    // at this point State indicates the final state where we must store the run length for the
    // particular bit combination
    Target[State].RunLength := RL;
  end;

  //--------------- end local functions ---------------------------------------

var
  I: Integer;

begin
  // set an initial entry in each state array
  SetLength(FWhiteStates, 1);
  SetLength(FBlackStates, 1);
  SetLength(F2DStates,    1);
  // with codes
  for I := 0 to 63 do
    with WhiteCodes[I] do AddCode(FWhiteStates, Code, Len, I);
  for I := 64 to 103 do
    with WhiteCodes[I] do AddCode(FWhiteStates, Code, Len, (I - 63) * 64);

  AddCode(FWhiteStates, 1, 12, G3_EOL);
  AddCode(FWhiteStates, 1, 9, G3_INVALID);
  AddCode(FWhiteStates, 1, 10, G3_INVALID);
  AddCode(FWhiteStates, 1, 11, G3_INVALID);
  AddCode(FWhiteStates, 0, 12, G3_INVALID);

  // black codes
  for I := 0 to 63 do
    with BlackCodes[I] do AddCode(FBlackStates, Code, Len, I);
  for I := 64 to 103 do
    with BlackCodes[I] do AddCode(FBlackStates, Code, Len, (I - 63) * 64);

  AddCode(FBlackStates, 1, 12, G3_EOL);
  AddCode(FBlackStates, 1, 9, G3_INVALID);
  AddCode(FBlackStates, 1, 10, G3_INVALID);
  AddCode(FBlackStates, 1, 11, G3_INVALID);
  AddCode(FBlackStates, 0, 12, G3_INVALID);

  //2D mode
  for I := 0 to 10 do
    with TwoDimCodes[I] do AddCode(F2DStates, Code, Len, I);
end;

procedure TCCITTDecoder.UpdateChangingElem;
begin
  if Length(fChangingElems[not fRowUsed])<=fCurChangingElem then
    SetLength(fChangingElems[not fRowUsed],Length(fChangingElems[not fRowUsed])*2+1); //asymptotically fast realloc
  fChangingElems[not fRowUsed,fCurChangingElem]:=fBitPos;
  inc(fCurChangingElem);
end;

//----------------- TCCITTFax3Decoder ----------------------------------------------------------------------------------

procedure TCCITTFax3Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  RunLength: Integer;
  EOLCount: Integer;
  //--------------- local functions -------------------------------------------

  procedure SynchBOL;

  // synch bit stream to next line start

  var
    Count: Integer;

  begin
    // if no EOL codes have been read so far then do it now
    if EOLCount = 0 then
    begin
      // advance until 11 consecutive 0 bits have been found
      Count := 0;
      while (Count < 11) and (FPackedSize > 0) do
      begin
        if NextBit then
          Count := 0
        else
          Inc(Count);
      end;
    end;

    // read 8 bit until at least one set bit is found
    repeat
      Count := 0;
      while (Count < 8) and (FPackedSize > 0) do
      begin
        if NextBit then
          Count := 9
        else
          Inc(Count);
      end;
    until (Count > 8) or (FPackedSize = 0);

    // here we are already beyond the set bit and can restart scanning
    EOLCount := 0;
  end;

  //---------------------------------------------------------------------------

  procedure AdjustEOL;

  begin
    FIsWhite := False;
    if FFreeTargetBits in [1..7] then
      Inc(FTarget);
    FFreeTargetBits := 8;
    FRestWidth := FWidth;
  end;

  //--------------- end local functions ---------------------------------------

begin
  // make all bits white
  FillChar(Dest^, UnpackedSize, 0);

  // swap all bits here, in order to avoid frequent tests in the main loop
  if FSwapBits then
    ReverseBits(Source, PackedSize);
  // setup initial states
  // a row always starts with a (possibly zero-length) white run
  FSource := Source;
  FBitsLeft := 0;
  FPackedSize := PackedSize;

  // target preparation
  FTarget := Dest;
  FRestWidth := FWidth;
  FFreeTargetBits := 8;
  EOLCount := 0;
  //we don't care, is FRowUsed true or false, it's totally symmetric

  // main loop
  repeat
    // synchronize to start of next line
    fBitPos := 0;
    fCurChangingElem:=0;
    fPrevChangingElem:=0;
    SynchBOL;
    if ((FOptions and 1)<>0) and (not NextBit) then
    begin
      FIsWhite := True;
      //begin 2-dimension decoding here
      repeat
        RunLength :=Find2DCode;
        if RunLength = cePass then begin
          FillRun(fChangingElems[fRowUsed,fPrevChangingElem+1]-fBitPos);  //we need b2 here
          fBitPos := fChangingElems[fRowUsed,fPrevChangingElem+1];

          if Cardinal(fBitPos) >= FWidth then
            Break;

          inc(fPrevChangingElem,2);
        end
        else if RunLength = ceHorizontal then begin
          //two passes: black and then white
          RunLength:=FindRunLength;
          FillRun(RunLength);
          inc(fBitPos, RunLength);

          UpdateChangingElem;

          FIsWhite := not FIsWhite;
          RunLength:=FindRunLength;
          FillRun(RunLength);
          inc(fBitPos, RunLength);

          UpdateChangingElem;

          if Cardinal(fBitPos) >= FWidth then
            Break;

          while fChangingElems[fRowUsed,fPrevChangingElem]<=fBitPos do
            inc(fPrevChangingElem,2); //we might want to add 'dummy' at the end of line
          FIsWhite := not FIsWhite;
        end
        else if (RunLength > 1) and (RunLength < 9) then begin
          //vertical coding
          if RunLength <6 then
            RunLength := (fChangingElems[fRowUsed, fPrevChangingElem]-fBitPos)+(RunLength-2)  //R0 to R3
          else
            RunLength := (fChangingElems[fRowUsed, fPrevChangingElem]-fBitPos)-(RunLength-5);  //L1 to L3
          if RunLength<0 then break; //some garbage got here
          FillRun(RunLength);
          inc(fBitPos, RunLength);

          UpdateChangingElem;

          if Cardinal(fBitPos) >= FWidth then
            Break;
          FIsWhite := not FIsWhite;
          //find b positions again, that is moving CurChangingElem

          dec(fPrevChangingElem);  //as we may turn left, then maybe previous changing elem of same color is better
          if fPrevChangingElem=-1 then
            fPrevChangingElem:=1;

          while fChangingElems[fRowUsed,fPrevChangingElem]<=fBitPos do
            inc(fPrevChangingElem,2);
        end
        else
          raise Exception.Create('special codes for reverting fax3 2D to uncompressed mode not implemented');
      until false;

    end
    else begin
      // a line always starts with a white run
      FIsWhite := True;
      // decode one line
      repeat
        RunLength:=FindRunLength;
        //populating array for 2D compression (if needed)
        if ((FOptions and 1) <> 0) then begin
          inc(fBitPos, RunLength);
          UpdateChangingElem;
        end;

        if RunLength >= 0 then
        begin
          if FillRun(RunLength) then
            Break;
          FIsWhite := not FIsWhite;
        end
        else
          if RunLength = G3_EOL then
            Inc(EOLCount)
          else
            Break;
      until (RunLength = G3_EOL) or (FPackedSize = 0);
    end;
    fBitPos:=fWidth;
    UpdateChangingElem;
    AdjustEOL;
    fRowUsed:=not fRowUsed;
  until (FPackedSize = 0) or (FTarget - PChar(Dest) >= UnpackedSize);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCCITTFax3Decoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin

end;

//------------------TCCITTFax4Decoder ------------------------------------------------------------------------------------
procedure TCCITTFax4Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
var RunLength: Integer;

  procedure AdjustEOL;
  begin
    FIsWhite := False;
    if FFreeTargetBits in [1..7] then
      Inc(FTarget);
    FFreeTargetBits := 8;
    FRestWidth := FWidth;
  end;

begin
  // make all bits white
  FillChar(Dest^, UnpackedSize, 0);

  // swap all bits here, in order to avoid frequent tests in the main loop
  if FSwapBits then
    ReverseBits(Source, PackedSize);
  // setup initial states
  // a row always starts with a (possibly zero-length) white run
  FSource := Source;
  FBitsLeft := 0;
  FPackedSize := PackedSize;

  // target preparation
  FTarget := Dest;
  FRestWidth := FWidth;
  FFreeTargetBits := 8;
  fRowUsed:=false; //could be true as well, it's symmetric
  fBitPos:=FWidth;
  UpdateChangingElem; //this way we form white reference line
  UpdateChangingElem;
  fRowUsed:=true;

  // main loop
  repeat
    // synchronize to start of next line
    fBitPos := 0;
    fCurChangingElem:=0;
    fPrevChangingElem:=0;
    FIsWhite := True;
    repeat
      RunLength :=Find2DCode;
      if RunLength = cePass then begin
        FillRun(fChangingElems[fRowUsed,fPrevChangingElem+1]-fBitPos);  //we need b2 here
        fBitPos := fChangingElems[fRowUsed,fPrevChangingElem+1];

        if Cardinal(fBitPos) >= FWidth then
          Break;

        inc(fPrevChangingElem,2);
      end
      else if RunLength = ceHorizontal then begin
        //two passes: black and then white
        RunLength:=FindRunLength;
        FillRun(RunLength);
        inc(fBitPos, RunLength);

        UpdateChangingElem;

        FIsWhite := not FIsWhite;
        RunLength:=FindRunLength;
        FillRun(RunLength);
        inc(fBitPos, RunLength);

        UpdateChangingElem;

        if Cardinal(fBitPos) >= FWidth then
          Break;

        while fChangingElems[fRowUsed,fPrevChangingElem]<=fBitPos do
          inc(fPrevChangingElem,2); //we might want to add 'dummy' at the end of line
        FIsWhite := not FIsWhite;
      end
      else if (RunLength > 1) and (RunLength < 9) then begin
        //vertical coding
        if RunLength <6 then
          RunLength := (fChangingElems[fRowUsed, fPrevChangingElem]-fBitPos)+(RunLength-2)  //R0 to R3
        else
          RunLength := (fChangingElems[fRowUsed, fPrevChangingElem]-fBitPos)-(RunLength-5);  //L1 to L3
        if RunLength<0 then break; //some garbage got here
        FillRun(RunLength);
        inc(fBitPos, RunLength);

        UpdateChangingElem;

        if Cardinal(fBitPos) >= FWidth then
          Break;
        FIsWhite := not FIsWhite;
        //find b positions again, that is moving CurChangingElem

        dec(fPrevChangingElem);  //as we may turn left, then maybe previous changing elem of same color is better
        if fPrevChangingElem=-1 then
          fPrevChangingElem:=1;

        while fChangingElems[fRowUsed,fPrevChangingElem]<=fBitPos do
          inc(fPrevChangingElem,2);
      end
      else
        raise Exception.Create('special codes for reverting fax4 to uncompressed mode not implemented');
    until false;
    //if BitPos>=FWidth then Exit;
    fBitPos:=fWidth;
    UpdateChangingElem;
    AdjustEOL;
    fRowUsed:=not fRowUsed;
  until (FPackedSize = 0) or (FTarget - PChar(Dest) >= UnpackedSize);
end;

procedure TCCITTFax4Decoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin

end;


//----------------- TCCITTMHDecoder ------------------------------------------------------------------------------------

procedure TCCITTMHDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  RunLength: Integer;

  //--------------- local functions -------------------------------------------

  procedure AdjustEOL;

  begin
    FIsWhite := False;
    if FFreeTargetBits in [1..7] then
      Inc(FTarget);
    FFreeTargetBits := 8;
    FRestWidth := FWidth;
    if FBitsLeft < 8 then
      FBitsLeft := 0; // discard remaining bits
    if FWordAligned and Odd(Cardinal(FTarget)) then
      Inc(FTarget);
  end;

  //--------------- end local functions ---------------------------------------

begin
  // make all bits white
  FillChar(Dest^, UnpackedSize, 0);

  // swap all bits here, in order to avoid frequent tests in the main loop
  if FSwapBits then
    ReverseBits(Source, PackedSize);
  // setup initial states
  // a row always starts with a (possibly zero-length) white run
  FIsWhite := True;
  FSource := Source;
  FBitsLeft := 0;
  FPackedSize := PackedSize;

  // target preparation
  FTarget := Dest;
  FRestWidth := FWidth;
  FFreeTargetBits := 8;

  // main loop
  repeat
    RunLength:=FindRunLength;
    if RunLength > 0 then
      if FillRun(RunLength) then
        AdjustEOL;
    FIsWhite := not FIsWhite;
  until FPackedSize = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCCITTMHDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TLZ77Decoder ---------------------------------------------------------------------------------------

constructor TLZ77Decoder.Create(FlushMode: Integer; AutoReset: Boolean);

begin
  FillChar(FStream, SizeOf(FStream), 0);
  FFlushMode := FlushMode;
  FAutoReset := AutoReset;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

begin
  FStream.next_in := Source;
  FStream.avail_in := PackedSize;
  if FAutoReset then
    FZLibResult := InflateReset(FStream);
  if FZLibResult = Z_OK then
  begin
    FStream.next_out := Dest;
    FStream.avail_out := UnpackedSize;
    FZLibResult := Inflate(FStream, FFlushMode);
    // advance pointers so used input can be calculated
    Source := FStream.next_in;
    Dest := FStream.next_out;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.DecodeEnd;

begin
  if InflateEnd(FStream) < 0 then
    CompressionError(gesLZ77Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.DecodeInit;

begin
  if InflateInit(FStream) < 0 then
    CompressionError(gesLZ77Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLZ77Decoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

function TLZ77Decoder.GetAvailableInput: Integer;

begin
  Result := FStream.avail_in;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLZ77Decoder.GetAvailableOutput: Integer;

begin
  Result := FStream.avail_out;
end;

//----------------- TThunderDecoder ------------------------------------------------------------------------------------

// ThunderScan uses an encoding scheme designed for 4-bit pixel values.  Data is encoded in bytes, with
// each byte split into a 2-bit code word and a 6-bit data value.  The encoding gives raw data, runs of
// pixels, or pixel values encoded as a delta from the previous pixel value.  For the latter, either 2-bit
// or 3-bit delta values are used, with the deltas packed into a single byte.

const
  THUNDER_DATA = $3F;       // mask for 6-bit data
  THUNDER_CODE = $C0;       // mask for 2-bit code word
  // code values
  THUNDER_RUN = 0;          // run of pixels w/ encoded count
  THUNDER_2BITDELTAS = $40;	// 3 pixels w/ encoded 2-bit deltas
    DELTA2_SKIP = 2;        // skip code for 2-bit deltas
  THUNDER_3BITDELTAS = $80; // 2 pixels w/ encoded 3-bit deltas
    DELTA3_SKIP = 4;        // skip code for 3-bit deltas
  THUNDER_RAW = $C0;        // raw data encoded

  TwoBitDeltas: array[0..3] of Integer = (0, 1, 0, -1);
  ThreeBitDeltas: array[0..7] of Integer = (0, 1, 2, 3, 0, -3, -2, -1);

constructor TThunderDecoder.Create(Width: Cardinal);

begin
  FWidth := Width;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThunderDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

var
  SourcePtr,
  TargetPtr: PByte;
  N, Delta: Integer;
  NPixels: Cardinal;
  LastPixel: Integer;

  //--------------- local function --------------------------------------------

  procedure SetPixel(Delta: Integer);

  begin
    Lastpixel := Delta and $0F;
    if Odd(NPixels) then
    begin
      TargetPtr^ := TargetPtr^ or LastPixel;
      Inc(TargetPtr);
    end
    else
      TargetPtr^ := LastPixel shl 4;

    Inc(NPixels);
  end;

  //--------------- end local function ----------------------------------------

begin
  SourcePtr := Source;
  TargetPtr := Dest;
  while UnpackedSize > 0 do
  begin
    LastPixel := 0;
    NPixels := 0;
    // Usually Width represents the byte number of a strip, but the thunder
    // algo is only defined for 4 bits per pixel formats where 2 pixels take up
    // one byte.
    while (PackedSize > 0) and (NPixels < 2 * FWidth) do
    begin
      N := SourcePtr^;
      Inc(SourcePtr);
      Dec(PackedSize);
      case N and THUNDER_CODE of
        THUNDER_RUN:
          // pixel run, replicate the last pixel n times, where n is the lower-order 6 bits
          begin
            if Odd(NPixels) then
            begin
              TargetPtr^ := TargetPtr^ or Lastpixel;
              Lastpixel := TargetPtr^;
              Inc(TargetPtr);
              Inc(NPixels);
              Dec(N);
            end
            else
              LastPixel := LastPixel or LastPixel shl 4;

            Inc(NPixels, N);
            while N > 0 do
            begin
              TargetPtr^ := LastPixel;
              Inc(TargetPtr);
              Dec(N, 2);
            end;

            if N = -1 then
            begin
              Dec(TargetPtr);
              TargetPtr^ := TargetPtr^ and $F0;
            end;

            LastPixel := LastPixel and $0F;
          end;
        THUNDER_2BITDELTAS: // 2-bit deltas
          begin
            Delta := (N shr 4) and 3;
            if Delta <> DELTA2_SKIP then
              SetPixel(LastPixel + TwoBitDeltas[Delta]);
            Delta := (N shr 2) and 3;
            if Delta <> DELTA2_SKIP then
              SetPixel(LastPixel + TwoBitDeltas[Delta]);
            Delta := N and 3;
            if Delta <> DELTA2_SKIP then
              SetPixel(LastPixel + TwoBitDeltas[Delta]);
          end;
        THUNDER_3BITDELTAS: // 3-bit deltas
          begin
            Delta := (N shr 3) and 7;
            if Delta <> DELTA3_SKIP then
              SetPixel(LastPixel + ThreeBitDeltas[Delta]);
            Delta := N and 7;
            if Delta <> DELTA3_SKIP then
              SetPixel(LastPixel + ThreeBitDeltas[Delta]);
          end;
        THUNDER_RAW: // raw data
          SetPixel(N);
      end;
    end;

    Dec(UnpackedSize, FWidth);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TThunderDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

//----------------- TPCDDecoder ----------------------------------------------------------------------------------------

constructor TPCDDecoder.Create(Raw: Pointer);

begin
  FData := Raw;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);

// recovers the Huffman encoded luminance and chrominance deltas
// Note: This decoder leaves a bit the way like the other decoders work.
//       Source points to an array of 3 pointers, one for luminance (Y, Luma), one for blue
//       chrominance (Cb, Chroma1) and one for red chrominance (Cr, Chroma2). These pointers
//       point to source and target at the same time (in place decoding).
//       PackedSize contains the width of the current subimage and UnpackedSize its height.
//       Dest is not used and can be nil.

type
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..2] of Pointer;

  PPCDTable = ^TPCDTable;
  TPCDTable = record
    Length: Word;
    Sequence: Cardinal;
    Key: Byte;
    Mask: Integer;
  end;

  PQuantumArray = ^TQuantumArray;
  TQuantumArray = array[0..3 * 256 - 1] of Byte;

var
  Luma,
  Chroma1,
  Chroma2: PChar; // hold the actual pointers, PChar to easy pointer maths
  Width,
  Height: Cardinal;

  PCDTable: array[0..2] of PPCDTable;
  I, J, K: Cardinal;
  R: PPCDTable;
  RangeLimit: PQuantumArray;
  P, Q,
  Buffer: PChar;
  Accumulator,
  Bits,
  Length,
  Plane,
  Row: Cardinal;
  PCDLength: array[0..2] of Cardinal;

  //--------------- local function --------------------------------------------

  procedure PCDGetBits(N: Cardinal);

  begin
    Accumulator := Accumulator shl N;
    Dec(Bits, N);
    while Bits <= 24 do
    begin
      if P >= (Buffer + $800) then
      begin
        Move(FData^, Buffer^, $800);
        Inc(FData, $800);
        P := Buffer;
      end;
      Accumulator := Accumulator or (Cardinal(P^) shl (24 - Bits));
      Inc(Bits, 8);
      Inc(P);
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  Limit: Cardinal;
  
begin
  // place the used source values into local variables with proper names to make
  // their usage clearer
  Luma := PPointerArray(Source)[0];
  Chroma1 := PPointerArray(Source)[1];
  Chroma2 := PPointerArray(Source)[2];
  Width := PackedSize;
  Height := UnpackedSize;
  
  // initialize Huffman tables
  ZeroMemory(@PCDTable, SizeOf(PCDTable));
  GetMem(Buffer, $800);
  try
    Accumulator := 0;
    Bits := 32;
    P := Buffer + $800;
    Limit := 1;
    if Width > 1536 then
      Limit := 3;
    for I := 0 to Limit - 1 do
    begin
      PCDGetBits(8);
      Length := (Accumulator and $FF) + 1;
      GetMem(PCDTable[I], Length * SizeOf(TPCDTable));

      R := PCDTable[I];
      for J := 0 to Length - 1 do
      begin
        PCDGetBits(8);
        R.Length := (Accumulator and $FF) + 1;
        if R.Length > 16 then
        begin
          for K := 0 to 2 do
            if Assigned(PCDTable[K]) then
              FreeMem(PCDTable[K]);
          Exit;
        end;
        PCDGetBits(16);
        R.Sequence := (Accumulator and $FFFF) shl 16;
        PCDGetBits(8);
        R.Key := Accumulator and $FF;
      R.Mask := not ((1 shl (32 - R.Length)) - 1);
        {asm
          // asm implementation to avoid overflow errors and for faster execution
          MOV EDX, [R]
          MOV CL, 32
          SUB CL, [EDX].TPCDTable.Length
          MOV EAX, 1
          SHL EAX, CL
          DEC EAX
          NOT EAX
          MOV [EDX].TPCDTable.Mask, EAX
        end;}
        Inc(R);
      end;
      PCDLength[I] := Length;
    end;

    // initialize range limits
    GetMem(RangeLimit, 3 * 256);
    try
      for I := 0 to 255 do
      begin
        RangeLimit[I] := 0;
        RangeLimit[I + 256] := I;
        RangeLimit[I + 2 * 256] := 255;
      end;
      Inc(PByte(RangeLimit), 255);

      // search for sync byte
      PCDGetBits(16);
      PCDGetBits(16);
      while (Accumulator and $00FFF000) <> $00FFF000 do
        PCDGetBits(8);
      while (Accumulator and $FFFFFF00) <> $FFFFFE00 do
        PCDGetBits(1);

      // recover the Huffman encoded luminance and chrominance deltas
      Length := 0;
      Plane := 0;
      Q := Luma;
      repeat
        if (Accumulator and $FFFFFF00) = $FFFFFE00 then
        begin
          // determine plane and row number
          PCDGetBits(16);
          Row := (Accumulator shr 9) and $1FFF;
          if Row = Height then
            Break;
          PCDGetBits(8);
          Plane := Accumulator shr 30;
          PCDGetBits(16);
          case Plane of
            0:
              Q := Luma + Row * Width;
            2:
              begin
                Q := Chroma1 + (Row shr 1) * Width;
                Dec(Plane);
              end;
            3:
              begin
                Q := Chroma2 + (Row shr 1) * Width;
                Dec(Plane);
              end;
          else
            Abort; // invalid/corrupt image
          end;

          Length := PCDLength[Plane];
          Continue;
        end;

        // decode luminance or chrominance deltas
        R := PCDTable[Plane];
        I := 0;
        while (I < Length) and ((Accumulator and R.Mask) <> R.Sequence) do
        begin
          Inc(I);
          Inc(R);
        end;
      
        if R = nil then
        begin
          // corrupt PCD image, skipping to sync byte
          while (Accumulator and $00FFF000) <> $00FFF000 do
            PCDGetBits(8);
          while (Accumulator and $FFFFFF00) <> $FFFFFE00 do
            PCDGetBits(1);
          Continue;
        end;

        if R.Key < 128 then
          Q^ := Char(RangeLimit[ClampByte(Byte(Q^) + R.Key)])
        else
          Q^ := Char(RangeLimit[ClampByte(Byte(Q^) + R.Key - 256)]);
        Inc(Q);
        PCDGetBits(R.Length);
      until False;                                     
    finally
      for I := 0 to 2 do
        if Assigned(PCDTable[I]) then
          FreeMem(PCDTable[I]);
      Dec(PByte(RangeLimit), 255);
      if Assigned(RangeLimit) then
        FreeMem(RangeLimit);
    end;
  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);

begin
end;

procedure PopulateReverseTable;
var i,j,k: Integer;
begin
  //Gold-Rader algorithm
  j:=0;
  for i := 0 to 254 do begin  //usually it is used to 'permutate' input data for FFT,
                        //then we can skip elems which stay intact
                        //but here we must populate whole table
    k:=128;
    ReverseTable[i]:=j;
    while k<=j do begin
      dec(j,k);
      k:=k shr 1;
    end;
    inc(j,k);
  end;
  ReverseTable[255]:=255;
end;


initialization
  PopulateReverseTable;

end.

