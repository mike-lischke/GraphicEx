unit JpegCompression;

interface

uses GraphicCompression;

type

  TQuantTableArray = array [0..63] of Integer;
  PQuantTableArray = ^TQuantTableArray;

  TJPEGDecoder = class(TDecoder)
  private
    FImageProperties: Pointer; // anonymously declared because I cannot take GraphicEx.pas in the uses clause above
    FQuantTables: array [0..3] of PQuantTableArray;  //FQuantTables[id]
                                                      //at first they are all nil, we allocate needed one
                                                      //and must free them later

    procedure DecodeQuantTable(var Source: Pointer; var PackedSize: Integer);
    procedure DecodeHuffmanTable(var Source: Pointer; var PackedSize: Integer);
    procedure DecodeSOF(frameType: Word; var Source: Pointer; var PackedSize: Integer);
  public
    constructor Create(Properties: Pointer);
    destructor Destroy; override;
//    procedure DecodeTables(const Source: Pointer; const size: Cardinal);

    procedure Decode(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer); override;
    procedure Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;


implementation

uses GraphicEx, SysUtils;

//----------------------------------------------------------------------------------------------------------------------
//----------------- TTIFFJPEGDecoder ---------------------------------------------------------------------------------------


// We're introverts a little: easier to decode JPEG ourselves then call somebody...
constructor TJPEGDecoder.Create(Properties: Pointer);

begin
  FImageProperties := Properties;
  with PImageProperties(Properties)^ do
    if Assigned(JPEGTables) then
      Decode(@JPEGTables[0], nil, Length(JPEGTables),0);
end;

destructor TJPEGDecoder.Destroy;
var i: Integer;
begin
  for i := 0 to 15 do
    if Assigned(FQuantTables[i]) then FreeMem(FQuantTables[i]);
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

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


procedure TJPEGDecoder.DecodeQuantTable(var Source: Pointer; var PackedSize: Integer);
var CharRun: PAnsiChar absolute Source;
    Run:         PByte absolute Source;
    SectionSize: Word;
    ID: Byte;
    i: Integer;
begin
  SectionSize := ReadBigEndianWord(CharRun); //At least 3 bytes left
  if PackedSize < SectionSize then
    GraphicExError('unexpected end of data');
  ID := Run^ and $0F;
  if ID>3 then
    GraphicExError('quant table ID must be 0..3');
  if FQuantTables[ID]=nil then
    GetMem(FQuantTables[ID],64*SizeOf(Integer));
  if (Run^ and $F0) = 0 then begin
    if SectionSize <> 67 then
      GraphicExError('quant table of size 67 expected for 1-byte vals');
    inc(Run);
    for i := 0 to 63 do begin
      FQuantTables[ID, i]:=Run^;
      inc(Run);
    end;
  end
  else if (Run^ and $F0) = $10 then begin
    if SectionSize <> 131 then
      GraphicExError('quant table of size 131 expected for 2-byte vals');
    inc(Run);
    for i := 0 to 63 do
      FQuantTables[ID,i]:=ReadBigEndianWord(CharRun);
  end
  else
    GraphicExError('sample sizes of 1 or 2 bytes expected for quant table');
  dec(PackedSize,SectionSize);
end;

procedure TJPEGDecoder.DecodeHuffmanTable(var Source: Pointer; var PackedSize: Integer);
var CharRun: PAnsiChar absolute Source;
    Run:         PByte absolute Source;
    SectionSize: Word;
    ID: Byte;
    i,j,k: Integer;
    IsDC: Boolean;
    Bits: Array [0..15] of Integer;
    HuffVal: Array of Integer;
    HuffSize: Array of Integer;
    HuffCode: Array of Integer;
    CodesCount: Integer;
begin
  SectionSize := ReadBigEndianWord(CharRun); //at least 3 bytes left
  if PackedSize < SectionSize then
    GraphicExError('Unexpected end of data');
  ID := Run^ and $0F;
  inc(Run);
  if ID>3 then
    GraphicExError('Huffman table ID must be 0..3');
  isDC := (Run^ and $F0) = 0;
  if SectionSize < 19 then
    GraphicExError('Incorrect size of Huffman table (less than 19 bytes)');
  CodesCount := 0;
  for i := 0 to 15 do begin
    Bits[i]:=Run^;  //how many symbols with i+1 bit len
    inc(CodesCount, Run^);
    inc(Run);
  end;
  if SectionSize <> 2+1+16+CodesCount then
    GraphicExError('Incorrect size of Huffman table');
  SetLength(HuffVal,CodesCount);
  for i := 0 to CodesCount-1 do begin
    HuffVal[i]:=Run^;
    inc(Run);
  end;
  dec(PackedSize, SectionSize);
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
//ok, now 2 more tables which are just reordered versions. Maybe we don't need them
end;

type
  TJpegScanHeader = record
    ComponentSelector: Byte;
    Td: Byte; //DC huff codes selector
    Ta: Byte; //AC huff codes selector
  end;



procedure TJPEGDecoder.DecodeSOF(frameType: Word; var Source: Pointer; var PackedSize: Integer);
var CharRun: PAnsiChar absolute Source;
    Run:     PByte absolute Source;
    HL: Word;
    Precision: Byte;
    Y, X: Word;
    Nf: Byte;
    i,j: Integer;
    ComponentID: array of Byte;
    HSampling, VSampling: array of Byte;
    QuantID: array of Byte;

    Ident: Word;
    Ns: Word; //number of components in current scan
    ScanHeaders: array of TJpegScanHeader;
    exists: Boolean;
    SamplingSum: Integer;
    Ss: Byte; //start of spectral/predictor selection
    Se: Byte; //end of spectral/predictor selection
    Ah: Byte;
    Al: Byte;
begin
  HL := ReadBigEndianWord(CharRun);
  if HL<11 then
    GraphicExError('start of frame header is too short (less then 11 bytes)');
  if PackedSize<HL then
    GraphicExError('unexpected end of data'); //now we can bravely read header in the loop
  Precision := Run^;
  inc(Run);
  if (Precision <> 8) and (Precision <> 12) then
    GraphicExError('only sample sizes of 8 or 12 are allowed in JPEG');
  if (frameType = JPEG_SOF0) and (Precision = 12) then
    GraphicExError('12-bit samples not allowed in baseline JPEG');
  Y := ReadBigEndianWord(CharRun);  //number of lines (0 means implicit)
  X := ReadBigEndianWord(CharRun);  //number of sample per line
  if X=0 then
    GraphicExError('zero samples per line is not allowed');
  Nf := Run^; //number of image components in frame
  inc(Run);
  if (Nf>4) and ((frameType and $03)=2) then
    GraphicExError('number of image components more than 4 not allowed in progressive JPEG');
  SetLength(ComponentID,Nf);
  SetLength(HSampling, Nf);
  SetLength(VSampling, Nf);
  SetLength(QuantID, Nf);
  for i := 0 to Nf-1 do begin
    ComponentID[i]:=Run^;
    for j := i - 1 downto 0 do
      if ComponentID[j] = Run^ then
        GraphicExError('two image components with same ID not allowed');
    inc(Run);
    HSampling[i] := (Run^ and $F0) shr 4;
    if HSampling[i] > 4 then
      GraphicExError('horizontal sampling more than 4 not allowed');
    VSampling[i] := Run^ and $0F;
    if VSampling[i] > 4 then
      GraphicExError('vertical sampling more than 4 not allowed');
    inc(Run);
    QuantID[i] := Run^;
    if QuantId[i] > 3 then
      GraphicExError('quantization table ID must be 0..3');
    if (QuantId[i] <> 0) and ((frameType and $03)=3) then
      GraphicExError('quantization table ID <>0 not allowed for lossless');
    if FQuantTables[QuantId[i]]=nil then
      GraphicExError(Format('quantization table %d not present',[QuantId[i]]));
    inc(Run);
  end;
  dec(PackedSize, HL);

  //here we begin scans
  while PackedSize >= 10 do begin
    Ident := ReadBigEndianWord(CharRun);
    dec(PackedSize,2);
    if Ident = JPEG_SOS then begin
      HL := ReadBigEndianWord(CharRun); //length of SOS header
      if PackedSize<HL then
        GraphicExError('unexpected end of data');
      Ns := Run^;
      inc(Run);
      if (Ns>4) or (Ns=0) then
        GraphicExError('incorrect number of image components per scan (should be 1..4)');
      if HL<>6 + 2 * Ns then
        GraphicExError('incorrect size of SOS header');
      SetLength(ScanHeaders, Ns);
      SamplingSum := 0;
      for I := 0 to Ns-1 do begin
        //component selector
        ScanHeaders[i].ComponentSelector := Run^;
        exists := false;
        for j := Nf-1 downto 0 do
          if Run^ = ComponentId[j] then begin
            exists := true;
            inc(SamplingSum, HSampling[j]*VSampling[j]);
            break;
          end;
        if not exists then
          GraphicExError(Format('component %d not present in frame header', [Run^]));
        if SamplingSum>10 then
          GraphicExError('too much subsampling involved (must be <= 10)');
        for j := i-1 downto 0 do
          if Run^ = ScanHeaders[j].ComponentSelector then
            GraphicExError(Format('component %d is duplicated in scan header',[Run^]));
        //oof, it's all right here...
        inc(Run);
        //DC huffman selector
        ScanHeaders[i].Td := (Run^ and $F0) shr 4;
        if (ScanHeaders[i].Td > 4) or ((ScanHeaders[i].Td > 2) and (FrameType = JPEG_SOF0)) then
          GraphicExError('incorrect DC huffman table ID in scan header');
          //check if corresponding Huff table exists. We still don't know how to represent them
        ScanHeaders[i].Ta := Run^ and $0F;
        if (ScanHeaders[i].Ta > 4) or ((ScanHeaders[i].Ta > 2) and (FrameType = JPEG_SOF0)) then
          GraphicExError('incorrect AC huffman table ID in scan header');
        if ((FrameType and $03) = 3) and (ScanHeaders[i].Ta <>0) then
          GraphicExError('inappropriate value Ta<>0 for lossless');
          //check if corresp. table exists
        //oof
        inc(Run);
      end;  //reading all components of scan header
      //Start/End of spectral selection. For progressive DCT
      Ss := Run^;
      if Ss > 63 then
        GraphicExError('incorect start of spectral selection, must be 0..63');
      if ((FrameType and $3) = 3) and ((Ss > 7) or (Ss = 0)) then
        GraphicExError('incorrect number of predictor for lossless, must be 1..7');
      if ((FrameType and $2) = 0) and (Ss <>0) then
        GraphicExError('start of spectral selection must be 0 for sequential DCT');
      inc(Run);
      Se := Run^;
      if ((FrameType and $3) = 3) and (Se <>0) then
        GraphicExError('spectral selection end must be 0 for lossless');
      if ((FrameType and $2) = 0) and (Se <>63) then
        GraphicExError('spectral selection end must be 63 for sequential DCT');
      if ((FrameType and $3) = 2) and (Se < Ss) then
        GraphicExError('end of spectral selection must be greater than its start');
      if ((FrameType and $3) = 2) and (Ss = 0) and (Se<>0) then
        GraphicExError('if start of spectral selection is zero, the end must be 0 also');
      //oof
      inc(Run);
      //successive approximation bit position high/low
      Ah := (Run^ and $F0) shr 4;
      if Ah > 13 then
        GraphicExError('successive approximation bit position high must be 0..13');
      if ((FrameType and $3) <>2) and (Ah<>0) then
        GraphicExError('successive approximation bit position must be 0 for all modes except progressive');
      Al := Run^ and $0F;
      if Al > 15 then
        GraphicExError('successive approximation bit position low must be 0..15');
      if ((FrameType and $3) = 2) and (Al > 13) then
        GraphicExError('successive approximation bit position low must be 0..13');
      if ((FrameType and $2) = 0) and (Al <> 0) then
        GraphicExError('successive approximation bit position low must be 0 for baseline/sequential');
      //oof
      inc(run);
      dec(PackedSize, HL);
      //ok, now we can read data itself.



    end;





  end;



end;


procedure TJPEGDecoder.Decode(Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
var Tag: Word;
    CharRun: PAnsiChar absolute Source;
    Run: PByte absolute Source;
    WordRun: PWord absolute Source;
    SectionSize: Integer;
begin
  Tag:=ReadBigEndianWord(CharRun);
  if Tag<>JPEG_SOI then
    GraphicExError('SOI expected at beginning of JPEG image/tables'); //should make rsrcstr
  dec(PackedSize,2);  //keep track of bytes left
  while PackedSize>6 do begin  //we expect 2 bytes EOI at the end and also 2 bytes tag and 2 bytes size
    Tag:=ReadBigEndianWord(CharRun);
    dec(PackedSize,2); //keep it in sync for simplification
    case Tag of
      JPEG_QUANT: DecodeQuantTable(Source, PackedSize);
      JPEG_HUF: DecodeHuffmanTable(Source, PackedSize);
      JPEG_SOF0..JPEG_SOF3, JPEG_SOF5..JPEG_SOF7, JPEG_SOF9..JPEG_SOF11,
        JPEG_SOF13..JPEG_SOF15: DecodeSOF(Tag, Source, PackedSize);
      else
        begin
          SectionSize:=ReadBigEndianWord(CharRun);
          inc(Run,SectionSize-2);  //jump to next label
          dec(PackedSize,SectionSize);
        end;
    end;



  end;
  if ReadBigEndianWord(CharRun)<>JPEG_EOI then
    GraphicExError('EOI expected at the end of JPEG image/tables');

end;

procedure TJPEGDecoder.Encode(Source, Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin
end;

//----------------------------------------------------------------------------------------------------------------------


end.
