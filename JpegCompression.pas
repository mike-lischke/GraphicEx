unit JpegCompression;

(*
    implementation of JPEG decoder from scratch
    by now only baseline JPEG is supported, but may be extended later.
    So far it is used to decode TIFF files with JPEG compression,
    but after arithmetic decoder is implemented it would be handy on its own,
    as Delphi JPEG library still doesn't support arithmetic
    (like the most of other programs, too...)

    By nabbla (nabbla@yandex.ru)

*)


interface

uses GraphicCompression;

type

  TQuantTableArray = array [0..63] of Integer;
  PQuantTableArray = ^TQuantTableArray;

  THuffmanTables = record
    Bits: Array [0..15] of Integer;
    HuffVal: Array of Integer;
    HuffSize: Array of Integer;
    HuffCode: Array of Integer;
    MinCode: Array [0..15] of Integer;
    MaxCode: Array [0..15] of Integer;
    ValPTR: Array [0..15] of Integer;
  end;
  PHuffmanTables = ^THuffmanTables;

  TJpegComponentDescriptor = record
    ComponentID: Byte;
    HSampling, VSampling: Byte;
    QuantID: Byte;
    SampleOffset: Integer;  //first component has offset 0, second may have offset 4 (if 4:1:1), third: 5 etc.
    Run: PByte; //pointer to current position at output
  end;

  TJpegScanHeader = record
    ComponentSelector: Byte;
    Td: Byte; //DC huff codes selector
    Ta: Byte; //AC huff codes selector
  end;

  TRealArray64 = array [0..63] of Real; //to store coefs. for DCT
  PRealArray64 = ^TRealArray64;

  TRealArray512 = array [0..511] of Real; //to store several blocks for DCT when subsampling is used
  PRealArray512 = ^TRealArray512;

  TDecodeBlockProc = procedure(compNum, QuantID: Integer) of Object;  //we'll have Huffman and (later) Arithm here

  TJPEGDecoder = class(TDecoder)
  private
    // anonymously declared because I cannot take GraphicEx.pas in the uses clause above
    FImageProperties: Pointer;
    //we multiply them by scaling factors for AAN iDCT
    FQuantTables: array [0..3] of TRealArray64;
    //that's FHuffmanTables[IsDC][id], New/Dispose should be used
    //to initialize/finalize dynamic arrays
    FHuffmanTables: array [boolean] of array [0..3] of PHuffmanTables;
    FSource: Pointer;
    FDest: Pointer;
    fPackedSize: Integer; //for reader of next values
    fUnpackedSize: Integer;
    fBitCount: Integer; //for NextBit function
    fCurrentByte: Byte;

    fFrameType: Word; //baseline/sequential/progressive/lossless/hierarchical, huffman vs arithm etc

    fPrecision: Byte; //8 or 12
    fBytesPerSample: Integer; //1 for 8-bit precision, 2 for 12-bit
    fColorComponents: array of TJpegComponentDescriptor;
    fInterleavedBlockSize: Integer; //size in bytes of one interleaved block in output
    fBlocksPerRow: Integer; //number of input interleaved blocks (4 blocks Y + 1 Cb + 1 Cr counts as one) per row
    fBlocksPerCol: Integer;
    fRowSize: Integer;  //size in bytes of one row

    fX, fY : Integer; //width and height

    PRED: array [0..3] of Integer;  //previous DC coefficient for each component.
      //After each RST it must reset to 0
      //no more than 4 components per scan is allowed, so we use static array here
    //settings of current scan
    Ns: Word; //number of components in current scan
    ScanHeaders: array of TJpegScanHeader;
    Ss: Byte; //start of spectral/predictor selection
    Se: Byte; //end of spectral/predictor selection (for progressive mode)
    Ah: Byte; //high bit of DCT coefs
    Al: Byte; //low bit of DCT coefs (for another type of progressive mode)

    fBuffer: TRealArray64;  //for DCT mode of course

    fDecodeBlockProc: TDecodeBlockProc;

    function NextByte: Byte;
    function NextWord: Word; //will swap
    function NextBit: Byte;

    function HuffDecode(isDC: Boolean; ID: Byte): Integer;
    function HuffReceive(SSSS: Integer): Integer;
    function HuffExtend(V,T: Integer): Integer;

    procedure DecodeQuantTable;
    procedure DecodeHuffmanTable;
    procedure DecodeSOF;
    procedure DecodeSequentialDCTScan;
    procedure DecodeDNL;
    procedure DecodeHuffmanBlock(CompNum, QuantID: Integer);
    procedure DecodeArithmBlock(CompNum, QuantID: Integer); //will raise exception so far
  public
    constructor Create(Properties: Pointer);
    destructor Destroy; override;
//    procedure DecodeTables(const Source: Pointer; const size: Cardinal);

    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

procedure DCT(var fltData: TRealArray64);
procedure IDCT(var fltData: TRealArray64);

implementation

uses GraphicEx, SysUtils, math, GraphicStrings;

//----------------------------------------------------------------------------------------------------------------------
//----------------- TTIFFJPEGDecoder ---------------------------------------------------------------------------------------
// We're introverts a little: easier to decode JPEG ourselves then call somebody...

const JPEG_SOI   = $FFD8; //start of image
      JPEG_QUANT = $FFDB; //quantization table
      //huffman coding
      JPEG_SOF0  = $FFC0; //start of baseline DCT
      JPEG_SOF1  = $FFC1; //start of extended sequential DCT
      JPEG_SOF2  = $FFC2; //start of progressive DCT
      JPEG_SOF3  = $FFC3; //start of lossless (sequential)
      JPEG_HUF   = $FFC4; //huffman table
      JPEG_SOF5  = $FFC5; //differential sequential DCT
      JPEG_SOF6  = $FFC6; //differential progressive DCT
      JPEG_SOF7  = $FFC7; //differential lossless (sequential)
      //arithmetic coding
      JPEG_JPG   = $FFC8; //JPEG extensions (reserved)
      JPEG_SOF9  = $FFC9; //extended sequential DCT
      JPEG_SOF10 = $FFCA; //progressive DCT
      JPEG_SOF11 = $FFCB; //lossless (sequential)
      JPEG_DAC   = $FFCC; //arithmetic coding conditioning table
      JPEG_SOF13 = $FFCD; //differential sequential DCT
      JPEG_SOF14 = $FFCE; //differential progressive DCT
      JPEG_SOF15 = $FFCF; //differential lossless (sequential)

      JPEG_SOS   = $FFDA; //start of scan
      JPEG_EOI   = $FFD9; //end of image

      JPEG_DNL   = $DC; //define number of lines

type
  TZigZagCoords = record
    R,C: Byte
  end;

const ZigZagPath: array [0..63] of TZigZagCoords = (
  (R: 0; C: 0), (R: 0; C: 1), (R: 1; C: 0), (R: 2; C: 0),
  (R: 1; C: 1), (R: 0; C: 2), (R: 0; C: 3), (R: 1; C: 2),
  (R: 2; C: 1), (R: 3; C: 0), (R: 4; C: 0), (R: 3; C: 1),
  (R: 2; C: 2), (R: 1; C: 3), (R: 0; C: 4), (R: 0; C: 5),
  (R: 1; C: 4), (R: 2; C: 3), (R: 3; C: 2), (R: 4; C: 1),
  (R: 5; C: 0), (R: 6; C: 0), (R: 5; C: 1), (R: 4; C: 2),
  (R: 3; C: 3), (R: 2; C: 4), (R: 1; C: 5), (R: 0; C: 6),
  (R: 0; C: 7), (R: 1; C: 6), (R: 2; C: 5), (R: 3; C: 4),
  (R: 4; C: 3), (R: 5; C: 2), (R: 6; C: 1), (R: 7; C: 0),
  (R: 7; C: 1), (R: 6; C: 2), (R: 5; C: 3), (R: 4; C: 4),
  (R: 3; C: 5), (R: 2; C: 6), (R: 1; C: 7), (R: 2; C: 7),
  (R: 3; C: 6), (R: 4; C: 5), (R: 5; C: 4), (R: 6; C: 3),
  (R: 7; C: 2), (R: 7; C: 3), (R: 6; C: 4), (R: 5; C: 5),
  (R: 4; C: 6), (R: 3; C: 7), (R: 4; C: 7), (R: 5; C: 6),
  (R: 6; C: 5), (R: 7; C: 4), (R: 7; C: 5), (R: 6; C: 6),
  (R: 5; C: 7), (R: 6; C: 7), (R: 7; C: 6), (R: 7; C: 7));

//  C + R*8
var ZigZag1D: array [0..63] of Integer;


const IDCT_Scales: array [0..7] of Real = (
  1,                              //1*cos(0*pi/16)
  1.38703984532215,               //sqrt(2)*cos(k*pi/16), k=1..7
  1.30656296487638,
  1.17587560241936,
  1,
  0.785694958387102,
  0.541196100146197,
  0.275899379282943);

const SQR_2     = 1.4142135623731;
      INV_SQR_2 = 0.707106781186547;

//implements AAN algorithm (Arai, Y., T. Agui, and M. Nakajima, (1988).
//A Fast DCT-SQ Scheme for Images, Trans IEICE, 71, pp. 1095-1097.)
//this code is mostly from LibJPEG, but rewritten on Pascal
procedure DCT(var fltData: TRealArray64);
var tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: Real;
    tmp10, tmp11, tmp12, tmp13: Real;
    z1, z2, z3, z4, z5, z11, z13: Real;
    ctr: Integer;
begin
  //Pass 1: process rows.
  for ctr := 7 downto 0 do begin
    tmp0 := fltData[0 + ctr * 8] + fltData[7 + ctr * 8];
    tmp7 := fltData[0 + ctr * 8] - fltData[7 + ctr * 8];
    tmp1 := fltData[1 + ctr * 8] + fltData[6 + ctr * 8];
    tmp6 := fltData[1 + ctr * 8] - fltData[6 + ctr * 8];
    tmp2 := fltData[2 + ctr * 8] + fltData[5 + ctr * 8];
    tmp5 := fltData[2 + ctr * 8] - fltData[5 + ctr * 8];
    tmp3 := fltData[3 + ctr * 8] + fltData[4 + ctr * 8];
    tmp4 := fltData[3 + ctr * 8] - fltData[4 + ctr * 8];

    //Even part

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    fltData[0 + ctr * 8] := tmp10 + tmp11;
    fltData[4 + ctr * 8] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * Inv_Sqr_2;
    fltData[2 + ctr * 8] := tmp13 + z1;
    fltData[6 + ctr * 8] := tmp13 - z1;

    //Odd part
    tmp10 := tmp4 + tmp5;
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    //The rotator is modified from fig 4-8 to avoid extra negations.
    z5 := (tmp10 - tmp12) * 0.382683433;
    z2 := 0.541196100 * tmp10 + z5;
    z4 := 1.306562965 * tmp12 + z5;
    z3 := tmp11 * 0.707106781;

    z11 := tmp7 + z3;
    z13 := tmp7 - z3;

    fltData[5 + ctr * 8] := z13 + z2;
    fltData[3 + ctr * 8] := z13 - z2;
    fltData[1 + ctr * 8] := z11 + z4;
    fltData[7 + ctr * 8] := z11 - z4;

  end;

  //Pass 2: process columns.
  for ctr := 7 downto 0 do begin
    tmp0 := fltData[ctr] + fltData[ctr + 56];
    tmp7 := fltData[ctr] - fltData[ctr + 56];
    tmp1 := fltData[ctr + 8] + fltData[ctr + 48];
    tmp6 := fltData[ctr + 8] - fltData[ctr + 48];
    tmp2 := fltData[ctr + 16] + fltData[ctr + 40];
    tmp5 := fltData[ctr + 16] - fltData[ctr + 40];
    tmp3 := fltData[ctr + 24] + fltData[ctr + 32];
    tmp4 := fltData[ctr + 24] - fltData[ctr + 32];

    //Even part

    tmp10 := tmp0 + tmp3;
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    fltData[ctr] := tmp10 + tmp11;
    fltData[ctr + 32] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * INV_SQR_2;
    fltData[ctr + 16] := tmp13 + z1;
    fltData[ctr + 48] := tmp13 - z1;

    //Odd part

    tmp10 := tmp4 + tmp5;
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    //The rotator is modified from fig 4-8 to avoid extra negations.
    z5 := (tmp10 - tmp12) * 0.382683433;
    z2 := 0.541196100 * tmp10 + z5;
    z4 := 1.306562965 * tmp12 + z5;
    z3 := tmp11 * INV_SQR_2;

    z11 := tmp7 + z3;
    z13 := tmp7 - z3;

    fltData[ctr + 40] := z13 + z2;
    fltData[ctr + 24] := z13 - z2;
    fltData[ctr + 8] := z11 + z4;
    fltData[ctr + 56] := z11 - z4;

  end;
end;

procedure IDCT(var fltData: TRealArray64);
var tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: Real;
    tmp10, tmp11, tmp12, tmp13: Real;
    z5, z10, z11, z12, z13: Real;
    WorkArray: array [0..63] of Real;
    ctr: Integer;
    dcval: Real;
begin
  //Pass 1: process columns from input, store into work array.
  for ctr := 7 downto 0 do
    (*
     * Due to quantization, we will usually find that many of the input
     * coefficients are zero, especially the AC terms.  We can exploit this
     * by short-circuiting the IDCT calculation for any column in which all
     * the AC terms are zero.  In that case each output is equal to the
     * DC coefficient (with scale factor as needed).
     * With typical images and quantization tables, half or more of the
     * column DCT calculations can be simplified this way.
     *)

    if (fltData[8+ctr] = 0)  and (fltData[16+ctr] = 0) and
       (fltData[24+ctr] = 0) and (fltData[32+ctr] = 0) and
       (fltData[40+ctr] = 0) and (fltData[48+ctr] = 0) and
       (fltData[56+ctr] = 0) then
    begin
      dcval               := fltData[ctr];
      WorkArray[ctr]      := dcval;
      WorkArray[8 + ctr]  := dcval;
      WorkArray[16 + ctr] := dcval;
      WorkArray[24 + ctr] := dcval;
      WorkArray[32 + ctr] := dcval;
      WorkArray[40 + ctr] := dcval;
      WorkArray[48 + ctr] := dcval;
      WorkArray[56 + ctr] := dcval;
    end
    else begin
      //even part
      tmp0 := fltData[ctr];
      tmp1 := fltData[ctr + 16];
      tmp2 := fltData[ctr + 32];
      tmp3 := fltData[ctr + 48];

      tmp10 := tmp0 + tmp2; //phase 3
      tmp11 := tmp0 - tmp2;

      tmp13 := tmp1 + tmp3; //phases 5-3
      tmp12 := (tmp1 - tmp3) * SQR_2 - tmp13;

      tmp0 := tmp10 + tmp13;  //phase 2
      tmp3 := tmp10 - tmp13;
      tmp1 := tmp11 + tmp12;
      tmp2 := tmp11 - tmp12;
      //odd part
      tmp4 := fltData[ctr + 8];
      tmp5 := fltData[ctr + 24];
      tmp6 := fltData[ctr + 40];
      tmp7 := fltData[ctr + 56];

      z13 := tmp6 + tmp5; //phase 6
      z10 := tmp6 - tmp5;
      z11 := tmp4 + tmp7;
      z12 := tmp4 - tmp7;

      tmp7 := z11 + z13;  //phase 5
      tmp11 := (z11 - z13) * SQR_2;

      z5 := (z10 + z12) * 1.847759065;
      tmp10 := 1.082392200 * z12 - z5;
      tmp12 := -2.613125930 * z10 + z5;

      tmp6 := tmp12 - tmp7;
      tmp5 := tmp11 - tmp6;
      tmp4 := tmp10 + tmp5;

      WorkArray[ctr]      := tmp0 + tmp7;
      WorkArray[ctr + 56] := tmp0 - tmp7;
      WorkArray[ctr + 8]  := tmp1 + tmp6;
      WorkArray[ctr + 48] := tmp1 - tmp6;
      WorkArray[ctr + 16] := tmp2 + tmp5;
      WorkArray[ctr + 40] := tmp2 - tmp5;
      WorkArray[ctr + 32] := tmp3 + tmp4;
      WorkArray[ctr + 24] := tmp3 - tmp4;
    end;

  (* Pass 2: process rows from work array, store into output array. *
   * Note that we must descale the results by a factor of 8 == 2**3. *)
  for ctr := 0 to 7 do begin
    (* Rows of zeroes can be exploited in the same way as we did with columns.
     * However, the column calculation has created many nonzero AC terms, so
     * the simplification applies less often (typically 5% to 10% of the time).
     * And testing floats for zero is relatively expensive, so we don't bother.
     *)

    //Even part

    tmp10 := WorkArray[0 + ctr * 8] + WorkArray[4 + ctr * 8];
    //we got faith in compiler: it will address neatly in one call
    tmp11 := WorkArray[0 + ctr * 8] - WorkArray[4 + ctr * 8];

    tmp13 := WorkArray[2 + ctr * 8] + WorkArray[6 + ctr * 8];
    tmp12 := (WorkArray[2 + ctr * 8] - WorkArray[6 + ctr * 8]) * SQR_2 - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    //Odd part

    z13   := WorkArray[5 + ctr * 8] + WorkArray[3 + ctr * 8];
    z10   := WorkArray[5 + ctr * 8] - WorkArray[3 + ctr * 8];
    z11   := WorkArray[1 + ctr * 8] + WorkArray[7 + ctr * 8];
    z12   := WorkArray[1 + ctr * 8] - WorkArray[7 + ctr * 8];

    tmp7  := z11 + z13;
    tmp11 := (z11 - z13) * SQR_2;

    z5    := (z10 + z12) * 1.847759065;
    tmp10 := 1.082392200 * z12 - z5;
    tmp12 := -2.613125930 * z10 + z5;

    tmp6 := tmp12 - tmp7;
    tmp5 := tmp11 - tmp6;
    tmp4 := tmp10 + tmp5;

    //We'll descale later, when converting to int.
    fltData[0 + ctr * 8] := tmp0 + tmp7;
    fltData[7 + ctr * 8] := tmp0 - tmp7;
    fltData[1 + ctr * 8] := tmp1 + tmp6;
    fltData[6 + ctr * 8] := tmp1 - tmp6;
    fltData[2 + ctr * 8] := tmp2 + tmp5;
    fltData[5 + ctr * 8] := tmp2 - tmp5;
    fltData[4 + ctr * 8] := tmp3 + tmp4;
    fltData[3 + ctr * 8] := tmp3 - tmp4;
  end;
end;



//first, low-level access to data
function TJPEGDecoder.NextByte: Byte;
begin
  if fPackedSize = 0 then
    GraphicExError(gesJPEGEOI);
  Result := PByte(fSource)^;
  inc(PByte(fSource));
  dec(fPackedSize);
end;

function TJPEGDecoder.NextWord: Word;
begin
  if fPackedSize<2 then
    GraphicExError(gesJPEGEOI);
  Result := ReadBigEndianWord(PAnsiChar(fSource));  //it will advance 2 bytes
  dec(fPackedSize,2);
end;

function TJPEGDecoder.NextBit: Byte;
begin
  if fBitCount = 0 then begin
    fBitCount := 8;
    fCurrentByte := NextByte;
    if fCurrentByte = $FF then begin
      fCurrentByte := NextByte;
      if fCurrentByte <> 0 then
        if fCurrentByte = JPEG_DNL then
          DecodeDNL
        else
          GraphicExError('unexpected marker %d at the middle of entropy-coded data', [fCurrentByte])
      else
        fCurrentByte := $FF;  //after all
    end;
  end;
  Result := fCurrentByte shr 7;
  fCurrentByte := (fCurrentByte shl 1) and $FF;
  dec(fBitCount);
end;

//little higher level
function TJPEGDecoder.HuffDecode(isDC: Boolean; ID: Byte): Integer;
var i, j: Integer;
    code: Integer;
begin
  i := 0;
  code := NextBit;
  while code > fHuffmanTables[isDC, ID]^.MaxCode[i] do begin
    inc(i);
    code := (code shl 1) or NextBit;
  end;
  j := fHuffmanTables[isDC, ID]^.ValPTR[i];
  j := j + code - fHuffmanTables[isDC, ID]^.MinCode[i];
  Result := fHuffmanTables[isDC, ID]^.HuffVal[j];
end;

function TJPEGDecoder.HuffReceive(SSSS: Integer): Integer;  //just receiving SSSS count of new bits
var i, v: Integer;
begin
  v := 0;
  for i := 0 to SSSS-1 do
    v := (v shl 1) or NextBit;
  Result := v;
end;

function TJPEGDecoder.HuffExtend(V: Integer; T: Integer): Integer;
var i: Integer;
begin
  if T = 0 then
    Result := 0
  else begin
    i := 1 shl (T-1);
    while V < i do begin
      i := ((-1) shl T) + 1;
      inc(V, i);
    end;
    Result := V;
  end;
end;



(*
      TJPEGDecoder
                        *)
constructor TJPEGDecoder.Create(Properties: Pointer);
var ptr, nilptr: Pointer;
begin
  FImageProperties := Properties;
  with PImageProperties(Properties)^ do
    if Assigned(JPEGTables) then begin
      ptr := @JPEGTables[0];
      nilptr := nil;
      Decode(ptr, nilptr, Length(JPEGTables),0);
    end;
end;

destructor TJPEGDecoder.Destroy;
var i: Integer;
begin
//  for i := 0 to 3 do
//    if (FQuantTables[i]<>nil) then FreeMem(FQuantTables[i]);
  for i := 0 to 3 do begin
    if Assigned(fHuffmanTables[true, i]) then Dispose(fHuffmanTables[true, i]);
    if Assigned(fHuffmanTables[false, i]) then Dispose(fHuffmanTables[false, i]);
  end;
  inherited Destroy;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TJPEGDecoder.DecodeQuantTable;
var B: Byte;
    SectionSize: Word;
    ID: Byte;
    i,k: Integer;
begin
  SectionSize := NextWord;
  B := NextByte;
  ID := B and $0F;
  if ID > 3 then
    GraphicExError('quant table ID must be 0..3');
  if (B and $F0) = 0 then begin
    if SectionSize <> 67 then
      GraphicExError('quant table of size 67 expected for 1-byte vals');
    for i := 0 to 63 do
      FQuantTables[ID, i] := NextByte;
  end
  else if (B and $F0) = $10 then begin
    if SectionSize <> 131 then
      GraphicExError('quant table of size 131 expected for 2-byte vals');
    for i := 0 to 63 do
      FQuantTables[ID, i] := NextWord;
  end
  else
    GraphicExError('sample sizes of 1 or 2 bytes expected for quant table');

  //now we'll scale our tables for advanced IDCT method
  for k := 0 to 63 do
    FQuantTables[ID, k] := FQuantTables[ID, k] * IDCT_scales[ZigZagPath[k].R] * IDCT_scales[ZigZagPath[k].C];


end;

procedure TJPEGDecoder.DecodeHuffmanTable;
var SectionSize: Word;
    B: Byte;
    ID: Byte;
    i, j, k: Integer;
    IsDC: Boolean;
    CodesCount: Integer;
begin
  SectionSize := NextWord; //at least 3 bytes left
  B := NextByte;
  ID := B and $0F;
  if ID > 3 then
    GraphicExError(gesJPEGBogusTableField);  //'Huffman table ID must be 0..3'
  isDC := (B and $F0) = 0;
  if fHuffmanTables[isDC, ID] = nil then
    New(fHuffmanTables[isDC, ID]);
  if SectionSize < 19 then
    GraphicExError(gesJPEGBogusTableField); //'Incorrect size of Huffman table (less than 19 bytes)'
  CodesCount := 0;
  with fHuffmanTables[isDC, ID]^ do begin
    for i := 0 to 15 do begin
      Bits[i] := NextByte;  //how many symbols with i+1 bit len
      inc(CodesCount, Bits[i]);
    end;
    if SectionSize <> 2+1+16+CodesCount then
      GraphicExError(gesJPEGBogusTableField); //'Incorrect size of Huffman table'
    SetLength(HuffVal, CodesCount);
    for i := 0 to CodesCount - 1 do
      HuffVal[i] := NextByte;
    //let's compute codes from data we have

    //first, codelen for each symbol
    SetLength(HuffSize, CodesCount);
    i := 0;
    j := 1;
    k := 0;
    repeat
      if j <= Bits[i] then begin
        HuffSize[k] := i + 1;
        inc(k);
        inc(j);
        continue;
      end;
      inc(i);
      j := 1;
    until i > 15;
    //second, code itself
    k := 0;
    i := 0; //i will present new code, "CODE" in algorithm
    j := HuffSize[0]; //"SI" in algorithm
    SetLength(HuffCode, CodesCount);
    repeat
      HuffCode[k] := i;
      inc(i);
      inc(k);
      if k = CodesCount then
        Break;
      if HuffSize[k] = j then
        Continue;
      repeat
        i := i shl 1;
        inc(j);
      until HuffSize[k] = j;
    until false;
    //two more
    j:=0;
    for i := 0 to 15 do begin
      if bits[i] = 0 then
        MaxCode[i] := -1
      else begin
        ValPTR[i] := j;
        MinCode[i] := HuffCode[j];
        j := j + Bits[i] - 1;
        MaxCode[i] := HuffCode[j];
        inc(j);
      end;
    end;
  end;
end;

procedure TJPEGDecoder.DecodeSOF;
var HL: Word;
    Nf: Byte;
    i, j: Integer;
    B: Byte;
    Ident: Word;
    MaxHSamplingFactor, MaxVSamplingFactor: Integer;
    Exists: Boolean;
    SamplingSum: Integer;
begin
  HL := NextWord;
  if HL < 11 then
    GraphicExError(gesJPEGBogusTableField); //'start of frame header is too short (less then 11 bytes)'
  fPrecision := NextByte;
  if (fPrecision <> 8) and (fPrecision <> 12) then
    GraphicExError(gesJPEGDataPrecision); //'only sample sizes of 8 or 12 are allowed in JPEG'
  if fPrecision = 8 then
    fBytesPerSample := 1
  else
    fBytesPerSample := 2;
  if (fframeType = JPEG_SOF0) and (fPrecision = 12) then
    GraphicExError(gesJPEGDataPrecision); //'12-bit samples not allowed in baseline JPEG'
  fY := NextWord;  //number of lines (0 means implicit)
  fX := NextWord;  //number of sample per line
  if fX = 0 then
    GraphicExError(gesJPEGComponentCount); //'zero samples per line is not allowed'
  Nf := NextByte; //number of image components in frame
  if (Nf > 4) and ((fframeType and $03) = 2) then
    GraphicExError(gesJPEGComponentCount); //'number of image components more than 4 not allowed in progressive JPEG'
  SetLength(fColorComponents, Nf);
  for i := 0 to Nf-1 do begin
    fColorComponents[i].ComponentID := NextByte;
    for j := i - 1 downto 0 do
      if fColorComponents[i].ComponentID = fColorComponents[j].ComponentID then
        GraphicExError(gesJPEGBogusTableField); //'two image components with same ID not allowed'
    B := NextByte;
    fColorComponents[i].HSampling := (B and $F0) shr 4;
    if fColorComponents[i].HSampling > 4 then
      GraphicExError(gesJPEGSamplingFactors); //'horizontal sampling more than 4 not allowed'
    fColorComponents[i].VSampling := B and $0F;
    if fColorComponents[i].VSampling > 4 then
      GraphicExError(gesJPEGSamplingFactors); //'vertical sampling more than 4 not allowed'
    fColorComponents[i].QuantID := NextByte;
    if fColorComponents[i].QuantId > 3 then
      GraphicExError(gesJPEGBogusTableField); //'quantization table ID must be 0..3'
    if (fColorComponents[i].QuantId <> 0) and ((fframeType and $03) = 3) then
      GraphicExError(gesJPEGBogusTableField); //'quantization table ID<>0 not allowed for lossless'
//    if FQuantTables[fColorComponents[i].QuantId]=nil then
//      GraphicExError(Format('quantization table %d not present',[fColorComponents[i].QuantId]));
  end;

  fInterleavedBlockSize := 0;
  maxHSamplingFactor := 1;
  maxVSamplingFactor := 1;
  for i := 0 to Nf-1 do begin
    fColorComponents[i].SampleOffset := fInterleavedBlockSize;
    fColorComponents[i].Run := fDest;
    inc(fColorComponents[i].Run, fInterleavedBlockSize);
    inc(fInterleavedBlockSize, fColorComponents[i].HSampling * fColorComponents[i].VSampling * fBytesPerSample);
    maxHSamplingFactor := max(maxHSamplingFactor, fColorComponents[i].HSampling);
    maxVSamplingFactor := max(maxVSamplingFactor, fColorComponents[i].VSampling);
  end;
  fRowSize := (fX div maxHSamplingFactor) * fInterleavedBlockSize;
  fBlocksPerRow := (fX + 8 * maxHSamplingFactor - 1) div (8 * maxHSamplingFactor);
  fBlocksPerCol := (fY + 8 * maxVSamplingFactor - 1) div (8 * maxVSamplingFactor);


  //here we begin scans
  while fPackedSize >= 10 do begin
    Ident := NextWord;
    if Ident = JPEG_SOS then begin
      HL := NextWord; //length of SOS header
      Ns := NextByte;
      if (Ns > 4) or (Ns = 0) then
        GraphicExError(gesJPEGComponentCount); //'incorrect number of image components per scan (should be 1..4)'
      if HL <> 6 + 2 * Ns then
        GraphicExError(gesJPEGBogusTableField); //'incorrect size of SOS header'
      SetLength(ScanHeaders, Ns);
      SamplingSum := 0;
      for I := 0 to Ns - 1 do begin
        //component selector
        ScanHeaders[i].ComponentSelector := NextByte;
        exists := false;
        for j := 0 to Nf - 1 do
          if ScanHeaders[i].ComponentSelector = fColorComponents[j].ComponentId then begin
            exists := true;
            inc(SamplingSum, fColorComponents[j].HSampling*fColorComponents[j].VSampling);
            ScanHeaders[i].ComponentSelector := j;  //it's much simpler to use!
            break;
          end;
        if not exists then
          GraphicExError(gesJPEGComponentCount); //Format('component %d not present in frame header', [ScanHeaders[i].ComponentSelector])
        if SamplingSum > 10 then
          GraphicExError(gesJPEGSamplingFactors); //'too much subsampling involved (must be <= 10)'
        for j := i-1 downto 0 do
          if ScanHeaders[i].ComponentSelector = ScanHeaders[j].ComponentSelector then
            GraphicExError(gesJPEGBogusTableField); //Format('component %d is duplicated in scan header',[ScanHeaders[i].ComponentSelector])
        //oof, it's all right here...
        //DC huffman selector
        B := NextByte;
        ScanHeaders[i].Td := (B and $F0) shr 4;
        if (ScanHeaders[i].Td > 4) or ((ScanHeaders[i].Td > 2) and (fFrameType = JPEG_SOF0)) then
          GraphicExError(gesJPEGBogusTableField); //'incorrect DC huffman table ID in scan header'
          //check if corresponding Huff table exists. We still don't know how to represent them
        ScanHeaders[i].Ta := B and $0F;
        if (ScanHeaders[i].Ta > 4) or ((ScanHeaders[i].Ta > 2) and (fFrameType = JPEG_SOF0)) then
          GraphicExError(gesJPEGBogusTableField); //'incorrect AC huffman table ID in scan header'
        if ((fFrameType and $03) = 3) and (ScanHeaders[i].Ta <>0) then
          GraphicExError(gesJPEGBogusTableField); //'inappropriate value Ta<>0 for lossless'
        //check if corresp. table exists
        //oof
      end;  //reading all components of scan header
      //Start/End of spectral selection. For progressive DCT
      Ss := NextByte;
      if Ss > 63 then
        GraphicExError(gesJPEGBogusTableField); //'incorect start of spectral selection, must be 0..63'
      if ((fFrameType and $3) = 3) and ((Ss > 7) or (Ss = 0)) then
        GraphicExError(gesJPEGBogusTableField); //'incorrect number of predictor for lossless, must be 1..7'
      if ((fFrameType and $2) = 0) and (Ss <> 0) then
        GraphicExError(gesJPEGBogusTableField); //'start of spectral selection must be 0 for sequential DCT'
      Se := NextByte;
      if ((fFrameType and $3) = 3) and (Se <> 0) then
        GraphicExError(gesJPEGBogusTableField); //'spectral selection end must be 0 for lossless'
      if ((fFrameType and $2) = 0) and (Se <> 63) then
        GraphicExError(gesJPEGBogusTableField); //'spectral selection end must be 63 for sequential DCT'
      if ((fFrameType and $3) = 2) and (Se < Ss) then
        GraphicExError(gesJPEGBogusTableField); //'end of spectral selection must be greater than its start'
      if ((fFrameType and $3) = 2) and (Ss = 0) and (Se <> 0) then
        GraphicExError(gesJPEGBogusTableField); //'if start of spectral selection is zero, the end must be 0 also'
      //oof
      //successive approximation bit position high/low
      B := NextByte;
      Ah := (B and $F0) shr 4;
      if Ah > 13 then
        GraphicExError(gesJPEGBogusTableField); //'successive approximation bit position high must be 0..13'
      if ((fFrameType and $3) <> 2) and (Ah <> 0) then
        GraphicExError(gesJPEGBogusTableField); //'successive approximation bit position must be 0 for all modes except progressive'
      Al := B and $0F;
      if Al > 15 then
        GraphicExError('successive approximation bit position low must be 0..15');
      if ((fFrameType and $3) = 2) and (Al > 13) then
        GraphicExError('successive approximation bit position low must be 0..13');
      if ((fFrameType and $2) = 0) and (Al <> 0) then
        GraphicExError('successive approximation bit position low must be 0 for baseline/sequential');
      //oof
      //ok, now we can read data itself.
      if (fFrameType and $4) = 0 then //we use Huffman codes
        fDecodeBlockProc := DecodeHuffmanBlock
      else
        fDecodeBlockProc := DecodeArithmBlock;

      if (fFrameType = JPEG_SOF0) or (fFrameType = JPEG_SOF1) or
        (fFrameType = JPEG_SOF9) then
        DecodeSequentialDCTScan
      else
        GraphicExError('only sequential DCT mode is supported so far');
      Exit;
    end;
  end;
end;

function ClampByte(value: Real): Byte;
begin
  if value < 0 then
    Result := 0
  else if value > 255 then
    Result := 255
  else
    Result := Round(value);
end;

function ClampWord(value: Real): Word;
begin
  if value < 0 then
    Result := 0
  else if value > $FFFF then
    Result := $FFFF
  else
    Result := Round(value);
end;


//might be handy even for progressive mode and differential mode, say, all modes
//except lossless, there are no 8x8 blocks in lossless..
procedure TJPEGDecoder.DecodeHuffmanBlock(CompNum, QuantID: Integer);
var T: Integer;
    Diff: Integer;
    ZZ: Array [0..63] of Integer; //0: DC, others: AC coef in zig-zag order
    i, k: Integer;
    RS: Integer;
    SSSS: Integer;
    RRRR: Integer;
    Quant: PRealArray64;
begin
  Quant := @FQuantTables[QuantID];
  //ok, let's try
  //DC first
  T := HuffDecode(true, ScanHeaders[compNum].Td);
  Diff := HuffReceive(T);
  Diff := HuffExtend(Diff, T);
  ZZ[0] := Diff + PRED[compNum];  //current DC coefficient
  PRED[compNum] := ZZ[0]; //getting ready for next one

  //AC, 63 of them
  k := 1;
  for i := 1 to 63 do
    ZZ[i] := 0;
  repeat
    RS := HuffDecode(false,ScanHeaders[compNum].Ta);
    SSSS := RS and $0F;
    RRRR := RS shr 4;
    if SSSS = 0 then
      if RRRR = 15 then begin
        inc(k, 16);  //skipped 16 zero coef at once
        continue
      end
      else
        break;  //EOB already
    inc(k, RRRR);
    //range check error possible on these lines
    //because k is more than 63 already.
    //why not EOB???
    if k>63 then
      break;

    ZZ[k] := HuffReceive(SSSS);
    ZZ[k] := HuffExtend(ZZ[k], SSSS);
    inc(k);
  until k = 64;
  //OK, spectrum is almost ready

  for i := 63 downto 0 do
    fBuffer[ZigZag1D[i]] := ZZ[i] * Quant^[i];

  IDCT(fBuffer);
end;

procedure TJPEGDecoder.DecodeArithmBlock(CompNum: Integer; QuantID: Integer);
begin
  GraphicExError('Sorry, arithmetic decoder is under construction');
end;

procedure TJPEGDecoder.DecodeSequentialDCTScan;
var compNum: Integer;
  i: Integer;
  sampX, sampY: Integer;
  subsampX, subsampY: Integer; //we convert BIG interleave (8x8 blocks) into LITTLE one (pixels)

  BiggerBuffer: TRealArray512;  //won't struggle with fixed point arithmetic here
  PBuf: PRealArray512; //may point to BiggerBuffer as well as to fBuffer
  //later we can use SSE/SSE2 etc to make it extremely effective
  Run: PByte;
  WordRun: PWord absolute Run;
  BlockNum: Integer;
  x, y: Integer;

  VSamp, HSamp: Integer;
  offs: Integer;
  bufIndex: Integer;  //for debug purposes
  BufferWidth: Integer;

  ColsToGo, RowsToGo: Integer;  //normally 8x8 but will be less on the edges
begin
  //MCU's here are interleaved, component after component
  //dealing with sequential mode here. We can do IDCT on the fly
  //into Dest.
  BlockNum := 0;
  //let's also reset decoder
  for i := 0 to 3 do
    PRED[i] := 0; //only 4 components possible in one scan
  repeat
    for compNum := 0 to Ns-1 do begin
      Run := fColorComponents[ScanHeaders[compNum].ComponentSelector].Run;
      HSamp := fColorComponents[ScanHeaders[compNum].ComponentSelector].HSampling;
      VSamp := fColorComponents[ScanHeaders[compNum].ComponentSelector].VSampling;
      for sampY := 0 to VSamp-1 do
        for sampX := 0 to HSamp-1 do begin
          fDecodeBlockProc(compNum,fColorComponents[ScanHeaders[compNum].ComponentSelector].QuantID);

          if (HSamp <> 1) or (VSamp <> 1) then begin
            offs := sampX * 8 * VSamp + sampY * 64 * HSamp;
            //example: HSamp=VSamp=2. This way, subsampX, subsampY = 0..1,
            //BiggerBuffer runs from 0 to 3, while we extract (0;0), (1;0), (0;1) and (1;1)
            //from fBuffer.
            y := 0;
            while y < 64 do begin
              x := 0;
              while x < 8 do begin
                for subsampY := 0 to VSamp - 1 do
                  for subsampX := 0 to HSamp - 1 do begin
                    BufIndex := subsampX + subsampY * 8 + x + y;
                    BiggerBuffer[offs] := fBuffer[BufIndex];
                    inc(offs);
                  end;
                inc(x, HSamp);
              end;
              inc(y, VSamp * 8);
              inc(offs, 8 * Vsamp * (Hsamp - 1));
            end;
          end;
        end; //loop over several luma samples per one chroma sample
      //ok, now we move this block to output
      if (HSamp = 1) and (VSamp = 1) then
        PBuf := @fBuffer
      else
        PBuf := @BiggerBuffer;
      BufferWidth := 8 * HSamp;
      ColsToGo := 8 * HSamp;
      if ((BlockNum mod fBlocksPerRow) = fBlocksPerRow - 1) and ((fX mod ColsToGo) <> 0) then
        ColsToGo := fx mod ColsToGo;
      RowsToGo := 8 * VSamp;
      if (BlockNum >= fBlocksPerRow * (fY div RowsToGo)) then
        RowsToGo := fY mod RowsToGo;
      i := 0;
      if fprecision = 8 then begin //1 byte per sample to dest
        for y := 0 to (RowsToGo div VSamp) - 1 do begin   //we must ensure earlier that fX and fY have integer number
          for x := 0 to (ColsToGo div HSamp) - 1 do begin // of HSamp,VSamp in it
            for subsampY := 0 to VSamp * HSamp - 1 do begin
              Run^ := ClampByte(PBuf^[i] / 8 + 128);
              inc(i);
              inc(Run);
            end;
            inc(Run, fInterleavedBlockSize - HSamp * VSamp);
          end;
          inc(i,BufferWidth - ColsToGo);
          inc(Run, fRowSize - (ColsToGo div HSamp) * fInterleavedBlockSize);
        end;
        if (BlockNum mod fBlocksPerRow) = fBlocksPerRow - 1 then //move down and left
          dec(Run, fRowSize - (ColsToGo div HSamp) * fInterleavedBlockSize)
        else
          dec(Run, fRowSize * (RowsToGo div VSamp) - (ColsToGo div HSamp) * fInterleavedBlockSize);
        end
      else
        GraphicExError('12-bit samples support under construction');
      fColorComponents[ScanHeaders[compNum].ComponentSelector].Run := Run;
    end; //loop over all the color components
    inc(BlockNum);
  until BlockNum >= fBlocksPerRow * fBlocksPerCol;
end;

procedure TJPEGDecoder.DecodeDNL;
begin

end;

procedure TJPEGDecoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
var Tag: Word;
    CharRun: PAnsiChar absolute Source;
    Run: PByte absolute Source;
    WordRun: PWord absolute Source;
    SectionSize: Integer;
begin
  fSource := Source;
  fPackedSize := PackedSize;
  fDest := Dest;
  fUnpackedSize := UnpackedSize;
  fBitCount := 0;

  Tag := NextWord;
  if Tag <> JPEG_SOI then
    GraphicExError('SOI expected at beginning of JPEG image/tables'); //should make rsrcstr

  while fPackedSize > 6 do begin  //we expect 2 bytes EOI at the end and also 2 bytes tag and 2 bytes size
    Tag := NextWord;
    case Tag of
      JPEG_QUANT: DecodeQuantTable;
      JPEG_HUF: DecodeHuffmanTable;
      JPEG_SOF0..JPEG_SOF3, JPEG_SOF5..JPEG_SOF7, JPEG_SOF9..JPEG_SOF11,
        JPEG_SOF13..JPEG_SOF15:
        begin
          fFrameType := Tag;
          DecodeSOF;
          Exit;
        end;
      else
        begin
          SectionSize := ReadBigEndianWord(PAnsiChar(fSource));
          inc(PByte(fSource), SectionSize - 2);  //jump to next label
          dec(fPackedSize, SectionSize);
        end;
    end;



  end;
  if NextWord <> JPEG_EOI then
    GraphicExError('EOI expected at the end of JPEG image/tables');

end;

procedure TJPEGDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin
end;

//----------------------------------------------------------------------------------------------------------------------
procedure PopulateZigZag1D;
var i: Integer;
begin
  for i := 0 to 63 do
    ZigZag1D[i] := ZigZagPath[i].C + ZigZagPath[i].R * 8;
end;


initialization
  PopulateZigZag1D;
end.
