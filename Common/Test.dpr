{$C+} //Assertions On 
program Test;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  Scanf in 'Scanf.pas',
  Scanf_c in 'Scanf_c.pas',
  LibStub in 'LibStub.pas';

procedure test_scanf;
var
  day, year: integer;
  weekday, month: array[0..19] of Char;
  dtmIn: string;

  procedure check;
  begin
    Assert(weekday='Sunday');
    Assert(month='March');
    Assert(day=2);
    Assert(year=2014);
  end;

  procedure print;
  begin
    writeln(Format('%s %d, %d = %s',[month, day, year, weekday]));
  end;
var
  formatStr: string;
  stream: TmemoryStream;
  pi: single;
  c: char;
begin
  formatStr := '%s %s %d  %d';
  dtmIn := 'ABC March 123 2014';
  sscanf(PChar(dtmIn), PChar(formatStr), [@weekday, @month, @day, @year]);
  Assert(day=123);

  dtmIn := 'Sunday March 2 2014';
  sscanf(PChar(dtmIn), PChar(formatStr), [@weekday, @month, @day, @year]);
  check;
  print;

  sscanf('3.1415926', '%f', [@pi]);
  sscanf('a', '%c', [@c]);

  stream:=TMemoryStream.Create;
  stream.Write(AnsiString(dtmIn)[1], Length(dtmIn)); //stream if rather Ansi than Unicode
  stream.Position:=0;
  fscanf(stream, PChar(formatStr), [@weekday, @month, @day, @year]);
  stream.Free;
  check;
  print;

  StrDeFmt(PChar(dtmIn), PChar(formatStr), [@weekday, @month, @day, @year]);
  check;
  print;

  DeFormat(dtmIn, formatStr, [@weekday, @month, @day, @year]);
  check;
  print;

  DeFormatBuf(dtmIn, Length(dtmIn), formatStr, Length(formatStr), [@weekday, @month, @day, @year]);
  check;
  print;
end;

procedure test_conv;
begin
  Assert(int64ToStr(2014)='2014');
  Assert(int64ToHex(2014)='$7DE');
  Assert(int64ToOct(2014)='03736');
end;

procedure test_thousand_sep;
var
  S: string;
  valExtended: Extended;
  valCurrency: Currency;
  b: Boolean;
begin
  S:='123'+{$IF RTLVersion>=24.00}FormatSettings.{$ifend}ThousandSeparator+'456'
          +{$IF RTLVersion>=24.00}FormatSettings.{$ifend}DecimalSeparator+'78901';
  writeln(S);
  b:=TextToFloatS(PChar(S), valExtended, fvExtended);
  Assert(b);
  Assert(valExtended=123456.78901);

  //S:='-'+S+'e20';
  S:='1234e-3';
  writeln(S);
  b:=TextToFloatS(PChar(S), valExtended, fvExtended);
  Assert(b);
  Assert(valExtended=1.234);

  S:='123'+{$IF RTLVersion>=24.00}FormatSettings.{$ifend}ThousandSeparator+'456';
  Assert(TextToFloatS(PChar(S), valCurrency, fvCurrency));
  Assert(valCurrency=123456);

  valCurrency:=StrToCurrS(S);
  Assert(valCurrency=123456);

  valExtended:=StrToFloatS(S);
  Assert(valExtended=123456);
  
  valCurrency:=StrToCurrF(S);
  Assert(valCurrency=123456);
end;

begin
  test_conv;
  test_thousand_sep;
  test_scanf;
end.

