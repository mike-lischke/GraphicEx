unit JpegCompression;

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
    SampleCount: Byte;  //number of samples in one interleaved block
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

  TJPEGDecoder = class(TDecoder)
  private
    FImageProperties: Pointer; // anonymously declared because I cannot take GraphicEx.pas in the uses clause above
    FQuantTables: array [0..3] of TRealArray64;  //we multiply them by scaling factors for one of fastest IDCT
    FHuffmanTables: array [boolean] of array [0..3] of PHuffmanTables;  //FHuffmanTables[IsDC][id]

    FSource: Pointer;
    FDest: Pointer;
    fPackedSize: Integer; //for reader of next values
    fUnpackedSize: Integer;
    fBitCount: Integer; //for NextBit function
    fCurrentByte: Byte;

    fFrameType: Word;

    fPrecision: Byte; //8 or 12
    fBytesPerSample: Integer; //1 for 8-bit precision, 2 for 12-bit
    fColorComponents: array of TJpegComponentDescriptor;
    fInterleavedBlockSize: Integer; //size in bytes of one interleaved block in output
    fBlocksPerRow: Integer; //number of input interleaved blocks (4 blocks Y + 1 Cb + 1 Cr counts as one) per row
    fRowSize: Integer;  //size in bytes of one row

    PRED: array [0..3] of Integer;  //previous DC coefficient for each component.
      //After each RST it must reset to 0
      //no more than 4 components per scan is allowed, so we use static array here
  //settings of current scan
    Ns: Word; //number of components in current scan
    ScanHeaders: array of TJpegScanHeader;
    exists: Boolean;
    SamplingSum: Integer;
    Ss: Byte; //start of spectral/predictor selection
    Se: Byte; //end of spectral/predictor selection
    Ah: Byte;
    Al: Byte;

    function NextByte: Byte;
    function NextWord: Word; //will swap
    function NextBit: Byte;

    function HuffDecode(isDC: Boolean; ID: Byte): Integer;
    function HuffReceive(SSSS: Integer): Integer;
    function HuffExtend(V,T: Integer): Integer;

    procedure DecodeQuantTable;
    procedure DecodeHuffmanTable;
    procedure DecodeSOF;
    procedure DecodeHuffmanScan;
    procedure DecodeDNL;
  public
    constructor Create(Properties: Pointer);
    destructor Destroy; override;
//    procedure DecodeTables(const Source: Pointer; const size: Cardinal);

    procedure Decode(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

procedure DCT(var fltData: TRealArray64);
procedure IDCT(var fltData: TRealArray64);

implementation

uses GraphicEx, SysUtils, math;

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

      JPEG_SOS   = $FFDA;
      JPEG_EOI   = $FFD9;

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
//  (0, 1, 8, 16, 9, 2, 3, 10,
//   17,

const IDCT_Scales: array [0..7] of Real = (
  1,
  1.38703984532215,
  1.30656296487638,
  1.17587560241936,
  1,
  0.785694958387102,
  0.541196100146197,
  0.275899379282943);

const SQR_2     = 1.4142135623731;
      INV_SQR_2 = 0.707106781186547;

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

    if (fltData[8+ctr] = 0) and (fltData[16+ctr] = 0) and
       (fltData[24+ctr] = 0) and (fltData[32+ctr] = 0) and
       (fltData[40+ctr] = 0) and (fltData[48+ctr] = 0) and
       (fltData[56+ctr] = 0) then begin
      dcval := fltData[ctr];
      WorkArray[ctr] := dcval;
      WorkArray[8 + ctr] := dcval;
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

    tmp10 := WorkArray[0 + ctr * 8] + WorkArray[4 + ctr * 8]; //we got faith in compiler: it will address neatly in one call
    tmp11 := WorkArray[0 + ctr * 8] - WorkArray[4 + ctr * 8];

    tmp13 := WorkArray[2 + ctr * 8] + WorkArray[6 + ctr * 8];
    tmp12 := (WorkArray[2 + ctr * 8] - WorkArray[6 + ctr * 8]) * SQR_2 - tmp13;

    tmp0 := tmp10 + tmp13;
    tmp3 := tmp10 - tmp13;
    tmp1 := tmp11 + tmp12;
    tmp2 := tmp11 - tmp12;

    //Odd part

    z13 := WorkArray[5 + ctr * 8] + WorkArray[3 + ctr * 8];
    z10 := WorkArray[5 + ctr * 8] - WorkArray[3 + ctr * 8];
    z11 := WorkArray[1 + ctr * 8] + WorkArray[7 + ctr * 8];
    z12 := WorkArray[1 + ctr * 8] - WorkArray[7 + ctr * 8];

    tmp7 := z11 + z13;
    tmp11 := (z11 - z13) * SQR_2;

    z5 := (z10 + z12) * 1.847759065;
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
    GraphicExError('unexpected end of data');
  Result := PByte(fSource)^;
  inc(PByte(fSource));
  dec(fPackedSize);
end;

function TJPEGDecoder.NextWord: Word;
begin
  if fPackedSize<2 then
    GraphicExError('unexpected end of data');
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
      if fCurrentByte <>0 then
        if fCurrentByte = JPEG_DNL then
          DecodeDNL
        else
          GraphicExError('unexpected marker %d at the middle of entropy-coded data',[fCurrentByte]);
    end;
  end;
  Result := fCurrentByte shr 7;
  fCurrentByte := (fCurrentByte shl 1) and $FF;
  dec(fBitCount);
end;

//little higher level
function TJPEGDecoder.HuffDecode(isDC: Boolean; ID: Byte): Integer;
var i,j: Integer;
    code: Integer;
begin
  i := 0;
  code := NextBit;
  while code>fHuffmanTables[isDC,ID]^.MaxCode[i] do begin
    inc(i);
    code := (code shl 1) or NextBit;
  end;
  j := fHuffmanTables[isDC,ID]^.ValPTR[i];
  j := j + code - fHuffmanTables[isDC, ID]^.MinCode[i];
  Result := fHuffmanTables[isDC,ID]^.HuffVal[j];
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
    Result:=0
  else begin
    i := 1 shl (T-1);
    while V < i do begin
      i := ((-1) shl T) + 1;
      inc(V,i);
    end;
    Result:=V;
  end;
end;






constructor TJPEGDecoder.Create(Properties: Pointer);

begin
  FImageProperties := Properties;
  with PImageProperties(Properties)^ do
    if Assigned(JPEGTables) then
      Decode(@JPEGTables[0], nil, Length(JPEGTables),0);
end;

destructor TJPEGDecoder.Destroy;
//var i: Integer;
begin
//  for i := 0 to 3 do
//    if Assigned(FQuantTables[i]) then FreeMem(FQuantTables[i]);
  inherited Destroy;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TJPEGDecoder.DecodeQuantTable;
var B: Byte;
    SectionSize: Word;
    ID: Byte;
    i,j,k: Integer;
begin
  SectionSize := NextWord;
  B := NextByte;
  ID := B and $0F;
  if ID>3 then
    GraphicExError('quant table ID must be 0..3');
  if (B and $F0) = 0 then begin
    if SectionSize <> 67 then
      GraphicExError('quant table of size 67 expected for 1-byte vals');
    for i := 0 to 63 do
      FQuantTables[ID, i]:=NextByte;
  end
  else if (B and $F0) = $10 then begin
    if SectionSize <> 131 then
      GraphicExError('quant table of size 131 expected for 2-byte vals');
    for i := 0 to 63 do
      FQuantTables[ID,i]:=NextWord;
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
    i,j,k: Integer;
    IsDC: Boolean;
    CodesCount: Integer;
begin
  SectionSize := NextWord; //at least 3 bytes left
  B := NextByte;
  ID := B and $0F;
  if ID>3 then
    GraphicExError('Huffman table ID must be 0..3');
  isDC := (B and $F0) = 0;
  if fHuffmanTables[isDC, ID] = nil then
    GetMem(fHuffmanTables[isDC, ID],SizeOf(THuffmanTables));
  if SectionSize < 19 then
    GraphicExError('Incorrect size of Huffman table (less than 19 bytes)');
  CodesCount := 0;
  with fHuffmanTables[isDC, ID]^ do begin
    for i := 0 to 15 do begin
      Bits[i]:=NextByte;  //how many symbols with i+1 bit len
      inc(CodesCount, Bits[i]);
    end;
    if SectionSize <> 2+1+16+CodesCount then
      GraphicExError('Incorrect size of Huffman table');
    SetLength(HuffVal,CodesCount);
    for i := 0 to CodesCount-1 do
      HuffVal[i]:=NextByte;
    //let's compute codes from data we have

    //first, codelen for each symbol
    SetLength(HuffSize,CodesCount);
    i := 0;
    j := 1;
    k := 0;
    repeat
      if j <= Bits[i] then begin
        HuffSize[k] := i+1;
        inc(k);
        inc(j);
        continue;
      end;
      inc(i);
      j:=1;
    until i>15;
    //second, code itself
    k := 0;
    i := 0; //i will present new code, "CODE" in algorithm
    j := HuffSize[0]; //"SI" in algorithm
    SetLength(HuffCode,CodesCount);
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
    Y, X: Word;
    Nf: Byte;
    i,j: Integer;
    B: Byte;
    Ident: Word;
    LeastCommonDivider: Integer;
begin
  HL := NextWord;
  if HL<11 then
    GraphicExError('start of frame header is too short (less then 11 bytes)');
  fPrecision := NextByte;
  if (fPrecision <> 8) and (fPrecision <> 12) then
    GraphicExError('only sample sizes of 8 or 12 are allowed in JPEG');
  if fPrecision = 8 then
    fBytesPerSample := 1
  else
    fBytesPerSample := 2;
  if (fframeType = JPEG_SOF0) and (fPrecision = 12) then
    GraphicExError('12-bit samples not allowed in baseline JPEG');
  Y := NextWord;  //number of lines (0 means implicit)
  X := NextWord;  //number of sample per line
  if X=0 then
    GraphicExError('zero samples per line is not allowed');
  Nf := NextByte; //number of image components in frame
  if (Nf>4) and ((fframeType and $03)=2) then
    GraphicExError('number of image components more than 4 not allowed in progressive JPEG');
  SetLength(fColorComponents, Nf);
  for i := 0 to Nf-1 do begin
    fColorComponents[i].ComponentID:=NextByte;
    for j := i - 1 downto 0 do
      if fColorComponents[i].ComponentID = fColorComponents[j].ComponentID then
        GraphicExError('two image components with same ID not allowed');
    B := NextByte;
    fColorComponents[i].HSampling := (B and $F0) shr 4;
    if fColorComponents[i].HSampling > 4 then
      GraphicExError('horizontal sampling more than 4 not allowed');
    fColorComponents[i].VSampling := B and $0F;
    if fColorComponents[i].VSampling > 4 then
      GraphicExError('vertical sampling more than 4 not allowed');
    fColorComponents[i].QuantID := NextByte;
    if fColorComponents[i].QuantId > 3 then
      GraphicExError('quantization table ID must be 0..3');
    if (fColorComponents[i].QuantId <> 0) and ((fframeType and $03) = 3) then
      GraphicExError('quantization table ID<>0 not allowed for lossless');
//    if FQuantTables[fColorComponents[i].QuantId]=nil then
//      GraphicExError(Format('quantization table %d not present',[fColorComponents[i].QuantId]));
  end;

  //now we determine layout of interleaved output
  LeastCommonDivider:=1;
  for i := 0 to Nf-1 do
    LeastCommonDivider := max(LeastCommonDivider, fColorComponents[i].HSampling * fColorComponents[i].VSampling);
  //we know that we have at least one channel with VSampling=HSampling=1 (otherwise we'd better lower resolution)
  //and also that only 1,2,4 are allowed. So, no problem.
  fInterleavedBlockSize:=0;
  for i := 0 to Nf-1 do begin
    fColorComponents[i].SampleCount := LeastCommonDivider div (fColorComponents[i].HSampling * fColorComponents[i].VSampling);
    fColorComponents[i].SampleOffset := fInterleavedBlockSize;
    fColorComponents[i].Run := fDest;
    inc(fColorComponents[i].Run,fInterleavedBlockSize);
    inc(fInterleavedBlockSize, fColorComponents[i].SampleCount * fBytesPerSample);
  end;
  fRowSize := X * fInterleavedBlockSize;
  fBlocksPerRow := X div (8 * LeastCommonDivider);


  //here we begin scans
  while fPackedSize >= 10 do begin
    Ident := NextWord;
    if Ident = JPEG_SOS then begin
      HL := NextWord; //length of SOS header
      Ns := NextByte;
      if (Ns>4) or (Ns=0) then
        GraphicExError('incorrect number of image components per scan (should be 1..4)');
      if HL<>6 + 2 * Ns then
        GraphicExError('incorrect size of SOS header');
      SetLength(ScanHeaders, Ns);
      SamplingSum := 0;
      for I := 0 to Ns-1 do begin
        //component selector
        ScanHeaders[i].ComponentSelector := NextByte;
        exists := false;
        for j := 0 to Nf-1 do
          if ScanHeaders[i].ComponentSelector = fColorComponents[j].ComponentId then begin
            exists := true;
            inc(SamplingSum, fColorComponents[j].HSampling*fColorComponents[j].VSampling);
            break;
          end;
        if not exists then
          GraphicExError(Format('component %d not present in frame header', [ScanHeaders[i].ComponentSelector]));
        if SamplingSum>10 then
          GraphicExError('too much subsampling involved (must be <= 10)');
        for j := i-1 downto 0 do
          if ScanHeaders[i].ComponentSelector = ScanHeaders[j].ComponentSelector then
            GraphicExError(Format('component %d is duplicated in scan header',[ScanHeaders[i].ComponentSelector]));
        //oof, it's all right here...
        //DC huffman selector
        B := NextByte;
        ScanHeaders[i].Td := (B and $F0) shr 4;
        if (ScanHeaders[i].Td > 4) or ((ScanHeaders[i].Td > 2) and (fFrameType = JPEG_SOF0)) then
          GraphicExError('incorrect DC huffman table ID in scan header');
          //check if corresponding Huff table exists. We still don't know how to represent them
        ScanHeaders[i].Ta := B and $0F;
        if (ScanHeaders[i].Ta > 4) or ((ScanHeaders[i].Ta > 2) and (fFrameType = JPEG_SOF0)) then
          GraphicExError('incorrect AC huffman table ID in scan header');
        if ((fFrameType and $03) = 3) and (ScanHeaders[i].Ta <>0) then
          GraphicExError('inappropriate value Ta<>0 for lossless');
        //check if corresp. table exists
        //oof
      end;  //reading all components of scan header
      //Start/End of spectral selection. For progressive DCT
      Ss := NextByte;
      if Ss > 63 then
        GraphicExError('incorect start of spectral selection, must be 0..63');
      if ((fFrameType and $3) = 3) and ((Ss > 7) or (Ss = 0)) then
        GraphicExError('incorrect number of predictor for lossless, must be 1..7');
      if ((fFrameType and $2) = 0) and (Ss <>0) then
        GraphicExError('start of spectral selection must be 0 for sequential DCT');
      Se := NextByte;
      if ((fFrameType and $3) = 3) and (Se <>0) then
        GraphicExError('spectral selection end must be 0 for lossless');
      if ((fFrameType and $2) = 0) and (Se <>63) then
        GraphicExError('spectral selection end must be 63 for sequential DCT');
      if ((fFrameType and $3) = 2) and (Se < Ss) then
        GraphicExError('end of spectral selection must be greater than its start');
      if ((fFrameType and $3) = 2) and (Ss = 0) and (Se<>0) then
        GraphicExError('if start of spectral selection is zero, the end must be 0 also');
      //oof
      //successive approximation bit position high/low
      B := NextByte;
      Ah := (B and $F0) shr 4;
      if Ah > 13 then
        GraphicExError('successive approximation bit position high must be 0..13');
      if ((fFrameType and $3) <>2) and (Ah<>0) then
        GraphicExError('successive approximation bit position must be 0 for all modes except progressive');
      Al := B and $0F;
      if Al > 15 then
        GraphicExError('successive approximation bit position low must be 0..15');
      if ((fFrameType and $3) = 2) and (Al > 13) then
        GraphicExError('successive approximation bit position low must be 0..13');
      if ((fFrameType and $2) = 0) and (Al <> 0) then
        GraphicExError('successive approximation bit position low must be 0 for baseline/sequential');
      //oof
      //ok, now we can read data itself.
      if (fFrameType = JPEG_SOF0) or (fFrameType = JPEG_SOF1) then
        DecodeHuffmanScan
      else
        GraphicExError('only baseline and extended sequential huffman supported so far');

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

procedure TJPEGDecoder.DecodeHuffmanScan;
var compNum: Integer;
  i, j, dbg: Integer;
  T: Integer;
  Diff: Integer;
  k: Integer;
  RS: Integer;
  SSSS: Integer;
  RRRR: Integer;
  ZZ: Array [0..63] of Integer; //0: DC, others: AC coef in zig-zag order
  Buffer: TRealArray64;  //won't struggle with fixed point arithmetic here
  //later we can use SSE/SSE2 etc to make it extremely effective
  Quant: PRealArray64;
  Run: PByte;
  WordRun: PWord absolute Run;
  CurChannelId: Integer;
  BlockNum: Integer;
begin
  //MCU's here are interleaved, component after component
  //dealing with sequential mode here. We can do IDCT on the fly
  //into Dest.
  BlockNum := 0;
  //let's also reset decoder
  for i := 0 to 3 do
    PRED[i] := 0; //only 4 components possible in one scan
  while true do begin

//  for dbg := 0 to 10 do begin
//    if dbg=88 then
//      assert(dbg=88);

    for compNum := 0 to Ns-1 do begin
      //let's fetch appropriate quant table
      Quant := nil; //to make compiler happy. We know already that right component exists
      //otherwise we'd have exception long time ago.
      for i := 0 to Length(fColorComponents)-1 do
        if fColorComponents[i].ComponentID = ScanHeaders[compNum].ComponentSelector then begin
          CurChannelId := i;
          Quant := @FQuantTables[fColorComponents[i].QuantID];
          Run := fColorComponents[i].Run; //continue where we began
          break;
        end;
      for j := fColorComponents[CurChannelId].SampleCount-1 downto 0 do begin
        //ok, let's try
        //DC first
        T := HuffDecode(true,ScanHeaders[compNum].Td);
        Diff := HuffReceive(T);
        Diff := HuffExtend(Diff, T);
        ZZ[0] := Diff + PRED[compNum];  //current DC coefficient
        PRED[compNum]:=ZZ[0]; //getting ready for next one

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
              inc(k,16);  //skipped 16 zero coef at once
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
          Buffer[ZigZag1D[i]] := ZZ[i] * Quant^[i];

//        for i := 0 to 63 do
//          if (i mod 8) <> 0 then
//            Buffer[i] := 0;

        //ok, now it's in right order (row after row) and scaled back.
        //still don't trust it too much...
        IDCT(Buffer);

//        for i := 0 to 63 do
//          Buffer[i] := Buffer[i] / 8;

        i := 0;
        //and at least, divide by 8, add 128 or 2048 and scale appropriately
        if fprecision=8 then
          if fColorComponents[CurChannelId].SampleCount=1 then begin //very important case, could make it separate to speed-up operations
            for RS := 0 to 7 do begin
              for k := 0 to 7 do begin
                Run^ := ClampByte(Buffer[i] / 8 + 128);
                inc(i);
                inc(Run, fInterleavedBlockSize);
              end;
              inc(Run, fRowSize - 8 * fInterleavedBlockSize);
            end;
            if (BlockNum mod fBlocksPerRow) = fBlocksPerRow - 1 then //move down and left
              dec(Run, fRowSize - 8 * fInterleavedBlockSize)
            else
              dec(Run, fRowSize * 8 - 8 * fInterleavedBlockSize);
          end
          else  //this part is wrong
            repeat
              for k := 0 to fColorComponents[CurChannelId].SampleCount-1 do begin
                Run^ := ClampByte(Buffer[i] / 8 + 128);
                inc(i);
                inc(Run);
              end;  //end of interleaved block, now we must jump
              inc(Run, fInterleavedBlockSize-fColorComponents[CurChannelID].SampleCount);
            until i=64
        else
          repeat
            for k := 0 to fColorComponents[CurChannelId].SampleCount-1 do begin
              WordRun^ := ClampWord(Buffer[i] * 2 + 32768);
              inc(i);
              inc(Run);
            end;
            inc(PByte(Run), fInterleavedBlockSize - fColorComponents[CurChannelID].SampleCount*2);
          until i=64;

        fColorComponents[CurChannelID].Run:=Run;
        if PAnsiChar(Run) >= PAnsiChar(fDest) + fUnpackedSize + 1 then
          Exit;  //decoded all the blocks already
      end;
    end; //loop over all the color components
    inc(BlockNum);
  end; //loop until we read all the blocks
end;

procedure TJPEGDecoder.DecodeDNL;
begin

end;



procedure TJPEGDecoder.Decode(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
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

  Tag := NextWord;
  if Tag <> JPEG_SOI then
    GraphicExError('SOI expected at beginning of JPEG image/tables'); //should make rsrcstr

  while fPackedSize>6 do begin  //we expect 2 bytes EOI at the end and also 2 bytes tag and 2 bytes size
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
          inc(PByte(fSource), SectionSize-2);  //jump to next label
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
