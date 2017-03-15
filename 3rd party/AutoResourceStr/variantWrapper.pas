unit VariantWrapper;

(*
  Sort of 'stub' for custom variants, here we deal with low-level details.

  To make new custom variant type, do the following:
  - make a descendant from TAbstractWrapperVariantType and override its Cast and CastTo methods
  - make a descendant from TAbstractWrapperData: add private fields storing information needed,
  override DoAdd, DoSubtract, DoMultiply etc. (if some of actions are inappropriate,
  better override them anyway and raise EAssertionError, it is much more informative
  than EAbstractError which would occur otherwise)
  - if perfomance is important, you may override TAbstractWrapperData.Release
  and TAbstractWrapperData.GetInstance methods. Instead of allocating and freeing
  memory each time, you can organize object pool: when released, it comes back
  into pool, when new instance is needed, taken from there if available (otherwise
  create a new one). When program ends, free all remaining objects.

  Also in this lib we give access to procedures _VarAdd(), _VarSub(), _VarMul(), _VarDiv etc,
  which may help to speed-up implementation of custom variant.

*)

(*
  Описываем абстрактный тип Variant'a, который "оборачивает" в себя другой Variant,
  добиваясь расширенной функциональности. Например, мы ставим размерности величин,
  а сама величина - Variant, содержащий либо целое, либо действительное, либо комплексное,
  или вовсе кватернион.
  По идее, таких "оберток" может быть несколько - сначала размерность, а потом
  множестенные значения, а потом комплексное число.

  также "вытаскиваем наружу" с помощью небольшого "хака" функции для работы с
  variant'ами, используемые компилятором - _varAdd, _varSub и т.д., что позволяет
  заметно ускорить выполнение

  *)
interface

uses classes,TypInfo,variants;

type

  TAbstractWrapperData = class(TPersistent) //чтобы можно было и запоминать в файле
    public
      constructor Create; virtual;  //does nothing but critical here,
      //otherwise TAbstractWrapperData.Create is static and isn't overriden by descendants
      procedure FastAssign(source: TAbstractWrapperData); virtual; //doesn't check for consistent types, so may be faster
      procedure Release; virtual; //by default, just destroys object, but descendants may implement object pool
      class function GetInstance: TAbstractWrapperData; virtual; //by default, just calls Create, but descendants may take vacant object from object pool
      procedure DoNegate; virtual; abstract;
      procedure DoAdd(const Right: TAbstractWrapperData); virtual; abstract;
      procedure DoSubtract(const Right: TAbstractWrapperData); virtual;
      procedure DoMultiply(const Right: TAbstractWrapperData); virtual; abstract;
      procedure DoDivide(const Right: TAbstractWrapperData); virtual; abstract;
      function GetAsString: string; virtual; abstract;
  end;

  TWrapperDataClass=class of TAbstractWrapperData;

  TAbstractWrapperVariantType = class(TPublishableVariantType)
  protected
    function LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean; override;
    function GetInstance(const V: TVarData): TObject; override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure UnaryOp(var Right: TVarData; const Operator: Integer); override;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
  end;

  TWrapperVarData = record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Data: TAbstractWrapperData;
    Reserved4: LongInt;
  end;


  //If we need to add some value to variant, that is,
  //A := A + B;
  //this easy code will firstly make a copy of A, then add B to it,
  //then clear A, assign copy to A and finally destroy copy.
  //so you can use procedures below instead, they are much faster.
  procedure CallVarAdd(var Left: Variant; const Right: Variant);
  procedure CallVarSub(var Left: Variant; const Right: Variant);
  procedure CallVarMul(var Left: Variant; const Right: Variant);
  procedure CallVarDiv(var Left: Variant; const Right: Variant);

implementation
uses sysUtils;


procedure CallVarAdd(var Left: Variant; const Right: Variant);
asm
  jmp Variants.@VarAdd;
end;

procedure CallVarSub(var Left: Variant; const Right: Variant);
asm
  jmp Variants.@VarSub;
end;

procedure CallVarMul(var Left: Variant; const Right: Variant);
asm
  jmp Variants.@VarMul;
end;

procedure CallVarDiv(var Left: Variant; const Right: Variant);
asm
  jmp Variants.@VarrDiv;
end;



(*
    TAbstractWrapperData
                              *)
constructor TAbstractWrapperData.Create;
begin
  inherited;
end;

procedure TAbstractWrapperData.Release;
begin
  Free;
//  Destroy;  //can be overriden to put this object into object pool where it can be re-used
end;

class function TAbstractWrapperData.getInstance: TAbstractWrapperData;
begin
  Result:=Create; //can be overriden to take new instance from object pool if available
  //it may be very efficient indeed.
end;

procedure TAbstractWrapperData.DoSubtract(const Right: TAbstractWrapperData);
var WrapperClass: TWrapperDataClass;
    Clone: TAbstractWrapperData;
begin
  //here we expect that A-B = A+(-B)
  //in most cases it's correct.
  //what's interesting, we're not expecting that - (-B) = B.
  //previous implementation didn't copy object but just negated it 2 times,
  //but not sure it never affects Right when working with floating point for ex.
  WrapperClass:=TWrapperDataClass(Right.ClassType);
  Clone:=WrapperClass.GetInstance;
  try
    Clone.FastAssign(Right);
    Clone.DoNegate;
    DoAdd(Clone);
  finally
    Clone.Release;
  end;
end;

procedure TAbstractWrapperData.FastAssign(source: TAbstractWrapperData);
begin
  Assign(source); //not so fast here, but can be overriden
end;

(*
    TAbstractWrapperVariantType
                                  *)
function TAbstractWrapperVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result:=TWrapperVarData(V).Data;
end;

procedure TAbstractWrapperVariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  TWrapperVarData(V).Data.Release;
end;

procedure TAbstractWrapperVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var WrapperClass: TWrapperDataClass;  //задается в Create конкретного класса
begin
//it seems that custom variant is never copied indirectly,
//(at least as it's implemented in variants.pas for all Delphi versions up to 10 Seattle)
//that's why commented this part for better perfomance

//  if Indirect and VarDataIsByRef(Source) then
//    VarDataCopyNoInd(Dest, Source)
//  else
// для custom variant'а никогда не будет передачи по ссылке
    with TWrapperVarData(Dest) do
    begin
      VType := VarType;
      WrapperClass:=TWrapperDataClass(TWrapperVarData(Source).Data.ClassType);
      Data:=WrapperClass.GetInstance;  //нужно при создании конкретного VariantType указать WrapperClass!
      Data.FastAssign(TWrapperVarData(Source).Data);
    end;
end;

procedure TAbstractWrapperVariantType.UnaryOp(var Right: TVarData; const Operator: Integer);
begin
//унарный минус и, возможно, логическое not.
  if Right.vtype=VarType then
    if Operator=opNegate then
      TWrapperVarData(Right).Data.DoNegate
    else
      RaiseInvalidOp
  else
    RaiseInvalidOp;
end;

procedure TAbstractWrapperVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp);
//var CastedRight: TVarData;
begin
//сложить, вычесть, умножить, поделить, остаток от деления и битовые операции (сдвиги, лог. и пр)
  case Left.VType of
    varString:
      case Operator of
        opAdd:
          Variant(Left) := Variant(Left) + TWrapperVarData(Right).Data.GetAsString;
      else
        RaiseInvalidOp;
      end;
  else
    if Left.VType = VarType then
      case Operator of
        opAdd:
          TWrapperVarData(Left).data.DoAdd(TWrapperVarData(Right).Data);
        opSubtract:
          TWrapperVarData(Left).data.DoSubtract(TWrapperVarData(Right).Data);
        opMultiply:
          TWrapperVarData(Left).data.DoMultiply(TWrapperVarData(Right).Data);
        opDivide:
          TWrapperVarData(Left).data.DoDivide(TWrapperVarData(Right).Data);
      else
        RaiseInvalidOp;
      end
    else
      RaiseInvalidOp;
  end;
end;

function TAbstractWrapperVariantType.LeftPromotion(const V: TVarData; const Operator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
//во что преобразовать переменную слева от бинарной операции, чтобы действие могло выполниться
  if (Operator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varString
  else
    RequiredVarType := VarType;
  Result := True;
//слева допускаем только строку, иначе "ассимилируем"
end;

end.


