program Pas2Dart;

uses SysUtils, StrUtils, Math, Classes, PParser, PasTree;

type
  TPasTree = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
    function FindElement(const AName: String): TPasElement; override;
  end;

function TPasTree.CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TPasTree.FindElement(const AName: String): TPasElement;
begin // dummy implementation, see TFPDocEngine.FindElement for a real example
  Result := nil;
end;

var
  AliasTypes, EnumTypes: TStringList;
  G: Text;
  InFunction: Boolean = False;
  ByRefArgs: TStringList;
  FuncWithByRefs: TStringList;
  FuncsWithoutParams: String = '"DESTROY"';

const
  LF  = ^M + ^J;
  TAB = '  ';

function ListToStr(Lista: TStringList): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Lista.Count - 1 do
    Result := Result + Lista[I] + IfThen(I < (Lista.Count - 1), ', ')
end;

function IsClassName(ClassName: String): Boolean;
begin
  Result := (Length(ClassName) > 2) and (ClassName[1] = 'T') and (ClassName[2] in ['A'..'Z', '_']) and (ClassName[Length(ClassName)] in ['a'..'z', '0'..'9'])
end;

function ConvertClassName(ClassName: String): String;
begin
  Result := '';
  if ClassName = '' then Exit;
  Result := UpCase(ClassName[1]) + Copy(ClassName, 2, Length(ClassName));
  if IsClassName(Result) then
    Result := Copy(Result, 2, Length(Result));
end;

function TypeIsPrimitive(PasType: String): Boolean;
const
  PrimitiveTypes: array[0..19]  of String = ('string', 'boolean', 'char', 'integer', 'byte', 'single', 'real', 'double', 'extended', 'word',
    'longint', 'cardinal', 'smallint', 'int64', 'comp', 'variant', 'const', 'TDate', 'pointer', 'TObject');
begin
  Result := AnsiIndexText(PasType, PrimitiveTypes) >= 0;
end;

function ConvertType(PasType: String; ConvertClass: Boolean = True): String;
var
  I: Integer;
const
  PascalTypes: array[0..26]  of String =     ('string', 'boolean', 'char', 'integer','byte', 'single', 'real', 'double', 'extended', 'word',
    'longint', 'cardinal', 'smallint', 'int64', 'comp', 'variant', 'const', 'TDate', 'TList', 'TStringList', 'Text', 'procedure', 'pointer',
    'TObject', 'TFPList', 'TObjectList', 'currency');
  DartTypes: array[-1..26] of String = ('', 'String', 'bool', 'int', 'int',   'int',   'double', 'double', 'double', 'Decimal', 'int',
    'int',     'int',      'int',    'int',  'BigInt', 'dynamic', 'Object', 'DateTime', 'List',   'List<String>', 'File', 'Function', 'Object',
    'Object',  'List',  'List',  'Decimal');
begin
  if Pos('File ', PasType) <> 0 then
    Result := 'File'
  else
  begin
    I := AnsiIndexText(PasType, PascalTypes);
    Result := IfThen(I >= 0, DartTypes[I], AliasTypes.Values[PasType]);
    Result := IfThen(Result = '', IfThen(ConvertClass, ConvertClassName(PasType), PasType), Result);
  end;
end;

function CamelCase(Ident: String): String;
begin
  Result := LowerCase(Ident[1]) + Copy(Ident, 2, Length(Ident));
end;

function IsAllUpper(Ident: String): Boolean;
var
  C: Char;
begin
  Result := true;
  for C in Ident do
    if not(C in ['A'..'Z', '_']) then
    begin
      Result := false;
      Exit;
    end;
end;

function ConvertMember(PasMember: String): String;
var
  I: Integer;
const
  PascalMembers: array[0..9] of String = ('write', 'writeln', 'exit',  'inc', 'dec', 'halt', 'count', 'MAXINT', 'paramstr', 'paramcount');
  DartMembers: array[0..9] of String = ('print', 'print', 'return', '++',  '--',  'exit', 'length', '256', 'args', 'args.length');
begin
  Result := PasMember;
  if Pos('''', Result) <> 0 then
    Exit;
  if Length(Result) > 1 then
  begin
    if IsAllUpper(Result) then Exit;
    if Result[2] in ['A'..'Z'] then
      case LowerCase(Result[1]) of
        'f':
        begin
          Result := '_' + CamelCase(Copy(Result, 2, 100));
          Exit;
        end;
        'e', 't':
        begin
          Result := Copy(Result, 2, 100);
          Exit;
        end;
      end;
  end;
  Result := CamelCase(Result);
  I := AnsiIndexText(Result, PascalMembers);
  if I >= 0 then
  begin
    Result := DartMembers[I];
    if (Result = 'return') and (InFunction or (ByRefArgs.Count > 0)) then
      Result := Result + IfThen(ByRefArgs.Count > 0, ' [' + ListToStr(ByRefArgs) + ']', ' result');
  end;
end;

type
  TFechamento = (ComChaves, SemChaves, SoInicio, SoFinal, ComChavesSemSalto);

procedure WriteBlock(Block: TPasImplBlock; Indent: String; Aditional: String = ''; Fechamento: TFechamento = ComChaves); forward;
procedure WriteImplElement(Comando:TPasImplElement; Indent: String; Aditional: String = ''; Fechamento: TFechamento = SemChaves); forward;
function WriteDecls(Decl: TPasDeclarations; Indent: String; IsClosure: Boolean = False): Boolean; forward;
function WriteExpr(Expr: TPasExpr; RemoveCreate: Boolean = False): String; forward;

function WriteList(Left: String; Lista: TPasExprArray; Right: String): String;
var
  I: Integer;
  Expr: TPasExpr;
  BExpr: TBinaryExpr;
begin
  Result := '';
  Write(G, Left);
  for I := 0 to High(Lista) do
  begin
    Expr := Lista[I];
    if (Expr is TBinaryExpr) and (Expr.Kind in [pekRange, pekSet]) then
    begin
      BExpr := TBinaryExpr(Expr);
      if BExpr.Left.Kind = pekNumber then
        Write(G, 'for (var n = ', WriteExpr(BExpr.Left), '; n <= ', WriteExpr(BExpr.Right), '; n++) n')
      else
        Write(G, 'for (var c = ', WriteExpr(BExpr.Left), '.codeUnitAt(0); c <= ', WriteExpr(BExpr.Right), '.codeUnitAt(0); c++) ' +
        'String.fromCharCode(c)');
      Write(G, IfThen(I <> High(Lista), ', '));
    end
    else
      Write(G, WriteExpr(Expr), IfThen(I <> High(Lista), ', '));
  end;
  Write(G, Right);
end;

function IsFuncsWithoutParams(Func: String): String;
begin
  Result := IfThen(Pos('"' + UpperCase(Func) + '"', FuncsWithoutParams) = 0, '', '()');
end;

function ConvertCharLiteral(Value: String): String;
begin
  Result := '''\u00';
  if Pos('$', Value) <> 0 then
    Result := Result + Copy(Value, 3, 2) + ''''
  else
    Result := Result + IntToHex(StrToInt(Copy(Value, 2, 3)), 2) + '''';
end;

function ConvertPrefixToSufix(Value: String): String;
const
  Map = 'length=length,trim=trim(),upcase=toUpperCase(),uppercase=toUpperCase(),lowercase=toLowerCase(),trimleft=trimLeft(),trimright=trimRight()';
begin
 // trim
end;

function WriteExpr(Expr: TPasExpr; RemoveCreate: Boolean = False): String;
var
  I: Integer;
  GetOp: array[TExprOpCode] of String = ('..', ' + ', ' - ', ' * ', ' / ', ' ~/ ', ' % ', ' ** ', ' >> ', ' << ', '!', ' && ', ' || ',
    ' ^ ', ' == ', ' != ', ' < ', ' > ', ' <= ', ' >= ', '.contains(', ' is ', ' as ', '.difference(', '', '', '', '.');
  DartBool: array[Boolean] of String = ('false', 'true');
  CompleteEnum, Member: String;
begin
  Result := '';
  if not Assigned(Expr) then Exit;
  if Expr is TBinaryExpr then
    with TBinaryExpr(Expr) do
      if (OpCode = eopSubIdent) and (Right is TParamsExpr) and (Upcase(TPrimitiveExpr(TParamsExpr(Right).Value).Value) = 'CREATE') then
        Write(G, WriteExpr(Left), WriteExpr(Right, True))
      else
        case OpCode of
          eopIs : Write(G, WriteExpr(Left), GetOp[OpCode], ConvertClassName(TPrimitiveExpr(Right).Value));
          eopIn : Write(G, WriteExpr(Right), '.contains(', WriteExpr(Left), ')');
        else
          if Left is TInheritedExpr then
            Write(G, 'super', WriteExpr(Right, true))
          else
          begin
            if (Left is TBinaryExpr) and (TBinaryExpr(Left).OpCode in ([eopAdd..eopAs] - [OpCode])) then
              Write(G, '(', WriteExpr(Left), ')')
            else
              Write(G, WriteExpr(Left));
            if (Right is TPrimitiveExpr) and (TPrimitiveExpr(Right).Kind = pekIdent) and (OpCode = eopSubIdent) and
               (LowerCase(TPrimitiveExpr(Right).Value) = 'create') then
              begin
                Write(G, '()');
                Exit;
              end;
            Write(G, GetOp[OpCode]);
            if (Right is TBinaryExpr) and (TBinaryExpr(Right).OpCode in ([eopAdd..eopAs] - [OpCode])) then
              Write(G, '(', WriteExpr(Right), ')')
            else
              Write(G, WriteExpr(Right));
          end;
        end
  else
  if Expr is TUnaryExpr then
    if (Expr.Opcode = eopNot) and not(TUnaryExpr(Expr).Operand is TPrimitiveExpr) then
      Write(G, GetOp[Expr.OpCode], '(', WriteExpr(TUnaryExpr(Expr).Operand), ')')
    else
      Write(G, Trim(GetOp[Expr.OpCode]), WriteExpr(TUnaryExpr(Expr).Operand))
  else
  if Expr is TPrimitiveExpr then
    with TPrimitiveExpr(Expr) do
    begin
      Member := ConvertMember(Value);
      case Kind of
        pekNumber: Value := ReplaceText(Value, '$', '0x');
        pekString:
           case Value[1] of
            '#': Value := ConvertCharLiteral(Value);
            '''':
              if Value <> '''''' then
              begin
                Value := ReplaceText(Value, '\', '\\');
                Value := ReplaceText(Value, '%s', '$s');
                Value := ReplaceText(Value, '%d', '$d');
                Value := '''' + ReplaceText(Copy(Value, 2, Length(Value) - 2), '''''', '"') + '''';
              end;
            '^': Value := '''\u' + (Ord(Value[2]) - Ord('@')).ToHexString(4) + '''';
          end;
        pekIdent:
        begin
          CompleteEnum := EnumTypes.Values[Value];
          if CompleteEnum <> '' then
          begin
            Write(G, CompleteEnum + '.' + Member);
            Exit;
          end;
          if RemoveCreate then
            if Member = 'create' then
            begin
              Write(G, '()');
              Exit;
            end
            else
              Write(G, '.');
        end;
      end;
      Write(G, ConvertType(Member, False), IsFuncsWithoutParams(Value))
    end
  else
  if Expr is TBoolConstExpr then
    Write(G, DartBool[TBoolConstExpr(Expr).Value])
  else
  if Expr is TNilExpr then
    Write(G, 'null')
  else
  if Expr is TInheritedExpr then
    Write(G, 'super()')
  else
  if Expr is TSelfExpr then
    Write(G, 'this')
  else
  if Expr is TParamsExpr then //Writeln(G, param1,param2,..,paramn);
    with TParamsExpr(Expr) do
      case Kind of
        pekFuncParams:
          if RemoveCreate then
            WriteList('(', Params, ')')
          else
            if (Length(Params) = 1) and (Value.Kind = pekIdent) then
              if IsClassName(TPrimitiveExpr(Value).Value) then
                WriteList('(', Params, ' as ' + ConvertClassName(TPrimitiveExpr(Value).Value) + ')')
              else
                if ConvertMember(TPrimitiveExpr(Value).Value)[1] in ['-', '+'] then
                  Write(G, WriteExpr(Params[0]), ConvertMember(TPrimitiveExpr(Value).Value))
                else
                  WriteList(WriteExpr(Value) + '(', Params, ')')
            else
              WriteList(WriteExpr(Value) + '(', Params, ')');
        pekSet:
          WriteList(WriteExpr(Value) + '{', Params, '}');
        else
          WriteList(WriteExpr(Value) + '[', Params, ']')
        end
  else
  if Expr is TArrayValues then  //const AnArrayConst: Array[1..3] of Integer = (1,2,3);
    with TArrayValues(Expr) do
      WriteList('[', Values, ']')
  else
  if Expr is TRecordValues then
    with TRecordValues(Expr) do
    begin
      Write(G, '{');
      for I := 0 to High(Fields) do
        with TRecordValuesItem(Fields[I]) do
          Write(G, Name, ': ', WriteExpr(ValueExp), IfThen(I <> High(Fields), ','));
      Write(G, '}');
    end
  else
    Writeln(G, 'Unknown expression: ', Expr.ClassName);
end;

procedure WriteCommandBlock(Comando: TPasImplBlock; Indent: String; Prefixo: String; Adicional: String = ''; Fechamento: TFechamento = ComChaves);
begin
  if Assigned(Comando) then
  begin
    Write(G, Indent, Prefixo);
    WriteBlock(TPasImplBlock(Comando), Indent, Adicional, Fechamento);
  end;
end;

function Explode(Delimitador: Char; const Texto: String): TStringList;
begin
  Result := TStringList.Create;
  with Result do
  begin
    StrictDelimiter    := True;
    Delimiter          := Delimitador;
    QuoteChar          := #0;
    NameValueSeparator := #0;
    Sorted             := False;
    CaseSensitive      := False;
    Duplicates         := dupAccept;
    if Texto = '' then
      Clear
    else
      DelimitedText := Texto;
  end;
end;

function WriteByRefFunction(Func: TPasExpr; Variable, Indent: String): Boolean;
var
  Decl, Atrib: String;
  I, P: Integer;
  Args, FuncParams: TStringList;
  IfSmt: Boolean;
  FuncParam: TPasExpr;
begin
  Result := False;
  FuncParam := Func;
  if Func is TBinaryExpr then
    with TBinaryExpr(Func) do
      if (OpCode = eopSubIdent) and (Right is TParamsExpr) then
        FuncParam := Right;
  if FuncParam is TParamsExpr then
    with TParamsExpr(FuncParam) do
    begin
      Decl := TPrimitiveExpr(Value).Value + IntToStr(High(Params) + 1);
      I := FuncWithByRefs.IndexOf(Decl);
      if I >= 0 then
      begin
        Args := TStringList(FuncWithByRefs.Objects[I]);
        Decl := GetDeclaration(False);
        Decl := Copy(Decl, 2, Length(Decl) - 2);
        FuncParams := Explode(',', Decl);
        Atrib := '';
        I := 0;
        if Variable <> '' then
        begin
          I := 1;
          Atrib := Variable + ', ';
        end;
        for I := I to Args.Count - 1 do
        begin
          P := PtrInt(Args.Objects[I]);
          if (P < FuncParams.Count) and IsValidIdent(Trim(FuncParams[P])) then
            Atrib := Atrib + Indent + ConvertMember(Trim(FuncParams[P])) + ' = ret[' + IntToStr(I) + '];' + LF
          else
            Continue;
        end;
        IfSmt := Pos('return', Variable) = 1;
        if IfSmt then
          Writeln(G, Indent, 'bool ', Variable, LF);
        Writeln(G, Indent, 'var ret = ', WriteExpr(Func), ';');
        Write(G, Atrib);
        if IfSmt then
          Write(G, Indent, 'if (', Variable, ')');
        Result := True;
      end;
    end;
end;

procedure WriteSmt(Smt: TPasImplStatement; Indent: String);
var
  I: Integer;
  ExceptObj: TPasExpr;
begin
  if Smt is TPasImplSimple then
    with TPasImplSimple(Smt) do
    begin
      if Expr is TBinaryExpr then
      begin
        if (TBinaryExpr(Expr).Right is TPrimitiveExpr) and
          AnsiEndsText('free', TPrimitiveExpr(TBinaryExpr(Expr).Right).Value) then
        Exit;
      end
      else
      if (Expr is TParamsExpr) and (TPasExpr(TParamsExpr(Expr).Value) is TPrimitiveExpr) and
        AnsiContainsText('FreeAndNil', TPrimitiveExpr(TPasExpr(TParamsExpr(Expr).Value)).Value) then
        Exit;
      if not WriteByRefFunction(Expr, '', Indent) then
        Writeln(G, Indent, WriteExpr(Expr), ';')
    end
  else
  if Smt is TPasImplAssign then
    with TPasImplAssign(Smt) do
    begin
      if ((Left is TPrimitiveExpr) and not WriteByRefFunction(Right, ConvertMember(TPrimitiveExpr(Left).Value), Indent)) or
        not(Left is TPrimitiveExpr) then
        Writeln(G, Indent, WriteExpr(Left), ' = ', WriteExpr(Right), ';');
    end
  else
  if Smt is TPasImplCaseStatement then
    with TPasImplCaseStatement(Smt) do
    begin
      Write(G, Indent);
      for I := 0 to Expressions.Count - 1 do
        Write(G, 'case ', WriteExpr(TPasExpr(Expressions[I])), ': ');
      WriteImplElement(Body, Indent + TAB, LF, SemChaves);
      Writeln(G, Indent + TAB, 'break;');
    end
  else
  if Smt is TPasImplWithDo then
    with TPasImplWithDo(Smt) do
    begin
      Write(G, Indent);
      for I := 0 to Expressions.Count - 1 do
        Write(G, WriteExpr(TPasExpr(Expressions[I])), '.with', IfThen(I < Expressions.Count - 1, ' { '));   //****
      WriteImplElement(Body, Indent, '', ComChavesSemSalto);
      Writeln(G, IfThen(Expressions.Count > 1, StringOfChar('}', Expressions.Count - 1)));
    end
  else
  if Smt is TPasImplWhileDo then
    with TPasImplWhileDo(Smt) do
    begin
      Write(G, Indent, 'while (', WriteExpr(ConditionExpr), ')');
      WriteImplElement(Body, Indent + TAB, '', ComChaves);
    end
  else
  if Smt is TPasImplExceptOn then
    with TPasImplExceptOn(Smt) do
    begin
      Write(G, Indent, 'on ', ConvertType(TypeName), ' catch (', ConvertMember(VariableName), ')');
      WriteImplElement(Body, Indent, IfThen(TPasImplElement(Body) is TPasImplRaise, 'throw '), ComChaves)
    end
  else
  if Smt is TPasImplForLoop then
    with TPasImplForLoop(Smt) do
    begin
      if LoopType = ltIn then
        Write(G, Indent, 'for (var ', WriteExpr(VariableName), ' in ', WriteExpr(StartExpr), ')')
      else
      begin
        Write(G, Indent, 'for (var ', WriteExpr(VariableName), ' = ');
        WriteExpr(StartExpr);
        Write(G, '; ', WriteExpr(VariableName), IfThen(Down, ' >= ', ' <= '));
        WriteExpr(EndExpr);
        Write(G, '; ', WriteExpr(VariableName), IfThen(Down, '--', '++'), ')');
      end;
      WriteImplElement(Body, Indent, '', ComChaves);
    end
  else
  if Smt is TPasImplRaise then
  begin
    ExceptObj := TPasImplRaise(Smt).ExceptObject;
    if ExceptObj = nil then
      Writeln(G, Indent, 'rethrow;')
    else
      Writeln(G, Indent, 'throw ', WriteExpr(ExceptObj), ';')
  end
  else
    WriteBlock(Smt, Indent + TAB);
end;

procedure WriteImplElement(Comando: TPasImplElement; Indent: String; Aditional: String = ''; Fechamento: TFechamento = SemChaves);
begin
  if not Assigned(Comando) then
  begin
    Writeln(G, ' ;');
    Exit;
  end;
  Write(G, Aditional);
  if Fechamento in [ComChaves, ComChavesSemSalto] then
  begin
    Writeln(G, ' {');
    Indent := Indent + TAB;
  end;
  if Comando is TPasImplStatement then
    WriteSmt(TPasImplStatement(Comando), Indent)
  else
  if Comando is TPasImplIfElse then
    with TPasImplIfElse(Comando) do
    begin
      if not WriteByRefFunction(ConditionExpr, 'return' + IntToStr(SourceLinenumber), Indent) then
        Write(G, Indent, 'if (', WriteExpr(ConditionExpr), ')');
      WriteImplElement(IfBranch, Indent, '', ComChavesSemSalto);
      if Assigned(ElseBranch) then
        WriteImplElement(ElseBranch, Indent, ' else', ComChaves)
      else
        Writeln(G);
    end
  else
  if Comando is TPasImplCaseOf then
  begin
    Write(G, Indent, 'switch (', WriteExpr(TPasImplCaseOf(Comando).CaseExpr), ')');
    WriteBlock(TPasImplCaseOf(Comando), Indent);
  end
  else
  if Comando is TPasImplCaseElse then
  begin
    Writeln(G, Indent, 'default:');
    WriteBlock(TPasImplCaseOf(Comando), Indent + TAB, '', SemChaves)
  end
  else
  if Comando is TPasImplRepeatUntil then
  begin
    WriteCommandBlock(TPasImplBlock(Comando), Indent, 'do', '', ComChavesSemSalto);
    Writeln(G, ' while (!', WriteExpr(TPasImplRepeatUntil(Comando).ConditionExpr), ');')
  end
  else
  if Comando is TPasImplTry then
    with TPasImplTry(Comando) do
    begin
      Write(G, Indent, 'try');
      WriteBlock(TPasImplBlock(Comando), Indent, '', ComChavesSemSalto);
      WriteImplElement(TPasImplElement(FinallyExcept), Indent);
      if Assigned(ElseBranch) then
        WriteImplElement(TPasImplElement(ElseBranch), Indent);
    end
  else
  if Comando is TPasImplTryFinally then
  begin
    Write(G, ' finally {');
    WriteCommandBlock(TPasImplBlock(Comando), Indent, LF, '', SoFinal)
  end
  else
  if (Comando is TPasImplTryExcept) or (Comando is TPasImplTryExceptElse) then
  begin
    Write(G, ' catch (e) {');
    WriteCommandBlock(TPasImplBlock(Comando), Indent, LF, '', SoFinal)
  end
  else
  if Comando is TPasImplLabelMark then
    Write(G, Indent, TPasImplLabelMark(Comando).LabelId, ':')
  else
  if Comando is TPasImplBlock then
    WriteBlock(TPasImplBlock(Comando), Indent, '', TFechamento(IfThen(Fechamento in [ComChaves, ComChavesSemSalto], Byte(SemChaves), Byte(Fechamento))));
  if Fechamento in [ComChaves, SoFinal, ComChavesSemSalto] then
    Write(G, Copy(Indent, 1, Length(Indent) - Length(TAB)), '}', IfThen(Fechamento <> ComChavesSemSalto, LF));
end;
     
procedure WriteBlock(Block: TPasImplBlock; Indent: String; Aditional: String = ''; Fechamento: TFechamento = ComChaves);
var
  I: Integer;
begin
  if not Assigned(Block) then Exit;
  case Fechamento of
    ComChaves, SoInicio, ComChavesSemSalto: Writeln(G, ' {');
    SemChaves: Indent := Copy(Indent, 1, Length(Indent) - Length(TAB));
  end;
  with Block do
    if Assigned(Elements) then
      if Block is TPasImplBlock then
        for I := 0 to Elements.Count - 1 do
          WriteImplElement(TPasImplElement(Elements[I]), Indent + TAB);
  if Aditional <> '' then
    Writeln(G, Indent + TAB, Aditional);
  case Fechamento of
    ComChaves, SoFinal: Writeln(G, Indent, '}');
    ComChavesSemSalto:  Write(G, Indent, '}');
  end;
end;

function GetArrayTypePos(ArrayType: TPasArrayType): String;
begin
  with ArrayType do
    if ElType = nil then
      Result := 'List'
    else
      Result := DupeString('List<', High(Ranges) + 1) + ConvertType(ElType.Name) + DupeString('>', High(Ranges) + 1)
end;

procedure WriteArrayTypePre(ArrayType: TPasArrayType);
begin
  Write(G, GetArrayTypePos(ArrayType));
  Write(G, ' ');
end;

procedure WriteVar(Variavel: TPasVariable; Indent: String); forward;

procedure WriteRecord(Registro: TPasRecordType; Indent: String);
var
  I, J: Integer;
begin
  with Registro do
  begin
    Write(G, Indent, 'class ' + ConvertClassName(Name) + ' {');
    for I := 0 to Members.Count - 1 do
      WriteVar(TPasVariable(Members[I]), LF + Indent + TAB);
    WriteVar(TPasVariable(VariantEl), Indent + TAB);
    Writeln(G, LF, '}', LF);
    if Assigned(Variants) then
      for I := 0 to Variants.Count - 1 do
        with TPasVariant(Variants[I]) do
        begin
          Writeln(G, LF, Indent, 'class ' + ConvertClassName(Name) + ' extends ' + ConvertClassName(Registro.Name) + '{');
          with TPasRecordType(Members) do
            for J := 0 to Members.Count - 1 do
              WriteVar(TPasVariable(Members[I]), Indent + TAB);
          Writeln(G, LF, '}', LF);
       end;
  end
end;

function WritePropertyType(VarType: TPasType): String;
begin
  Result := '';
  if Assigned(VarType) then
    if VarType is TPasArrayType then
      Result := GetArrayTypePos(TPasArrayType(VarType))
    else
    if VarType is TPasSetType then
      Result := 'Set<' + ConvertType(ConvertClassName(TPasSetType(VarType).EnumType.Name), True) + '>'
    else
    if VarType is TPasPointerType then
      Result := 'Object'
    else
      Result := ConvertType(VarType.Name);
end;

procedure WriteVar(Variavel: TPasVariable; Indent: String);
begin
  if Assigned(Variavel) then
    with Variavel do
    begin
      Write(G, Indent);
      if Assigned(VarType) and not(Variavel is TPasConst) then
      begin
        if VarType is TPasArrayType then
        begin
          if Assigned(Expr) then
          begin
            WriteArrayTypePre(TPasArrayType(VarType));
            Write(G, ConvertMember(Name), ' = ', WriteExpr(Expr));
          end
          else
            Write(G, 'var ', ConvertMember(Name), ' = ', GetArrayTypePos(TPasArrayType(VarType)), '()');
          Write(G, ';');
          Exit;
        end
        else
        if VarType is TPasSetType then
          Write(G, 'Set<', ConvertType(ConvertClassName(TPasSetType(Variavel.VarType).EnumType.Name), True) + '>')
        else
        if VarType is TPasPointerType then
          Write(G, 'Object')
        else
        if VarType is TPasRecordType then
          WriteRecord(TPasRecordType(VarType), Indent)
        else
          Write(G, ConvertType(TPasType(Variavel.VarType).Name));
        Write(G, ' ', ConvertMember(Name));
      end
      else
        Write(G, 'const ', ConvertMember(Name));  // const
      if Assigned(Expr) then // const AnArrayConst : Array[1..3] of Integer = (1,2,3);
        Write(G, ' = ', WriteExpr(Expr));
      Write(G, ';');
  end;
end;
  
procedure WriteTypes(Elemento: TPasElement; Indent: String);
var
  I: Integer;
  EnumType, EnumName: String;
begin
  if Elemento is TPasArrayType then
    AliasTypes.Add(Elemento.Name + '=' + GetArrayTypePos(TPasArrayType(Elemento)))
  else
  if Elemento is TPasEnumType then
  begin
    EnumType := ConvertClassName(Elemento.Name);
    Writeln(G, Indent, 'enum ' + EnumType + ' {');
    with TPasEnumType(Elemento) do
      for I := 0 to Values.Count - 1 do
      begin
        EnumName := ConvertMember(TPasEnumValue(Values[I]).Name);
        EnumTypes.Add(EnumName + '=' + EnumType);
        Writeln(G, Indent, TAB, EnumName, IfThen(I < (Values.Count - 1), ', '));
      end;
    Writeln(G, Indent + '}', LF);
  end
  else
  if Elemento is TPasFileType then
    AliasTypes.Add(Elemento.Name + '=File')
  else
  if Elemento is TPasProcedureType then
    AliasTypes.Add(Elemento.Name + '=Function')
  else
  if Elemento is TPasPointerType then
    AliasTypes.Add(Elemento.Name + '=Object')
  else
  if Elemento is TPasRangeType then
    with TPasRangeType(Elemento) do
      AliasTypes.Add(Name + '=int')
  else
  if Elemento is TPasRecordType then
    WriteRecord(TPasRecordType(Elemento), Indent)
  else
  if Elemento is TPasSetType then
    AliasTypes.Add(Elemento.Name + '=Set<' + ConvertType(ConvertClassName(TPasSetType(Elemento).EnumType.Name), True) + '>')
  else
  if Elemento is TPasClassOfType then
    AliasTypes.Add(Elemento.Name + '=class')
  else
  if Elemento is TPasAliasType then
    AliasTypes.Add(Elemento.Name + '=' + ConvertType(TPasAliasType(Elemento).DestType.Name))
  else
    Writeln(G, Indent, 'Unknown type: ', Elemento.Name, ' ', Elemento.Classname);
end;

function FindProcBody(Proc: TPasProcedure): TProcedureBody;
var
  Secao, Elemento: TPasElement;
  I: Integer;
begin
  Result := Proc.Body;
  if Assigned(Proc.Body) then Exit;
  Secao := Proc.Parent.Parent;
  if not(Secao is TImplementationSection) then
    if TPasModule(Secao.Parent) <> nil then
      Secao := TPasModule(Secao.Parent).ImplementationSection;
  if Secao is TImplementationSection then
    with TPasDeclarations(Secao) do
      for I := 0 to Declarations.Count - 1 do
      begin
        Elemento := TPasElement(Declarations[I]);
        if Elemento is TPasProcedure then
          with TPasProcedure(Elemento) do
            if (Name + ProcType.GetDeclaration(True)) = (Proc.Parent.Name + '.' + Proc.Name + Proc.Proctype.GetDeclaration(True)) then
            begin
              Result := Body;
              Exit;
            end;
      end;
end;

procedure WriteProcParams(Args: TFPList; IsClosure: Boolean = False);
var
  I : Integer;
  Optional: Boolean = false;
begin
  if not IsClosure then
    Write(G, '(');
  if Assigned(Args) then
    for I := 0 to Args.Count - 1 do
      with TPasArgument(Args[I]) do
      begin
        if (Value <> '') and not Optional then
        begin
          Optional := true;
          Write(G, '[');
        end;
        if ArgType is TPasArrayType then
          WriteArrayTypePre(TPasArrayType(ArgType))
        else
          Write(G, ConvertType(ArgType.Name) + ' ');
        Write(G, ConvertMember(Name));
        Write(G, IfThen(Value <> '', ' = ' + Value));
        Write(G, IfThen(I < (Args.Count - 1), ', '));
      end;
  Write(G, IfThen(Optional, ']'), IfThen(IsClosure, ' =>', ')'));
end;

function GetByRefArgs(Args: TFPList; InFunction: Boolean): TStringList;
var
  I: PtrInt;
begin
  Result := TStringList.Create;
  for I := 0 to Args.Count - 1 do
    with TPasArgument(Args[I]) do
      if (Access in [argVar, argOut]) and TypeIsPrimitive(ArgType.Name) then
        Result.AddObject(ConvertMember(Name), TObject(I));
  if (Result.Count <> 0) and InFunction then
    Result.InsertObject(0, 'result', TObject(Pointer(-1)));
end;

procedure WriteProcBody(Proc: TPasProcedure; Indent: String; FuncType: String; IsClosure: Boolean);
var
  Returns: String;
begin
  Proc.Body := FindProcBody(Proc);
  if ByRefArgs.Count > 0 then
  begin
    Proc.CustomData := ByRefArgs;
    FuncWithByRefs.AddObject(Proc.Name + IntToStr(Proc.ProcType.Args.Count), ByRefArgs);
  end;
  if Assigned(Proc.Body) then
  begin
    Writeln(G, IfThen(IsClosure, '', ' {'));
    if InFunction then
      Writeln(G, Indent + TAB, FuncType, ' result;');
    if not WriteDecls(Proc.Body, Indent + TAB, True) and InFunction then
      Writeln(G);
    Returns := '';
    if InFunction or (ByRefArgs.Count > 0) then
      Returns := 'return ' + IfThen(ByRefArgs.Count > 0, '[' + ListToStr(ByRefArgs) + ']', 'result') + ';';
    WriteBlock(TPasImplBlock(Proc.Body.Body), Indent, Returns, SoFinal);
  end
  else
    Writeln(G, ';');

end;

procedure WriteClosure(Proc: TPasProcedure; Indent: String);
var
  FuncType: String;
begin
  if Assigned(Proc) then
  begin
    Write(G, Indent, 'Function');
    InFunction := Proc.ProcType is TPasFunctionType;
    ByRefArgs := GetByRefArgs(Proc.ProcType.Args, InFunction);
    if InFunction then
      FuncType := '<' + ConvertType(TPasFunctionType(Proc.ProcType).ResultEl.ResultType.Name) + '>'
    else
      FuncType := '';
    Write(G, IfThen(ByRefArgs.Count > 0, '<List>', FuncType) + ' ' + ConvertMember(Proc.Name), ' = {');
    WriteProcParams(Proc.ProcType.Args, True);
    WriteProcBody(Proc, Indent, FuncType, True);
  end;
  InFunction := False;
end;

function WriteProcedure(Proc: TPasProcedure; Indent: String; Visibility: String = ''): Boolean;
var
  FuncType: String;
begin
  if Assigned(Proc) and not Proc.IsForward then
  begin
    Result := true;
    if Proc.IsOverride then
      Write(G, Indent, '@override', LF);
    Write(G, Indent, IfThen((Proc is TPasClassProcedure) or (Proc is TPasClassFunction), 'static '));
    InFunction := Proc.ProcType is TPasFunctionType;
    ByRefArgs := GetByRefArgs(Proc.ProcType.Args, InFunction);
    if InFunction then
      FuncType := ConvertType(TPasFunctionType(Proc.ProcType).ResultEl.ResultType.Name)
    else
      FuncType := 'void';
    Write(G, IfThen((Proc is TPasConstructor) or (TPasElement(Proc) is TPasConstructorImpl), ConvertClassName(Proc.Parent.Name),
      IfThen(ByRefArgs.Count > 0, '', FuncType + ' ') + Visibility + ConvertMember(Proc.Name)));
    WriteProcParams(Proc.ProcType.Args);
    WriteProcBody(Proc, Indent, FuncType, False);
  end
  else
    Result := false;
  InFunction := False;
end;

procedure GetFuncsWithoutParams(Members: TFPList);
var
  I: Integer;
  Elemento: TPasElement;
begin
  for I := 0 to Members.Count - 1 do
  begin
    Elemento := TPasElement(Members[I]);
    if Elemento is TPasFunction then
      with TPasFunction(Elemento) do
        if not Assigned(ProcType.Args) or (ProcType.Args.Count = 0) then
          FuncsWithoutParams := FuncsWithoutParams + UpperCase(Name) + '"';
  end;
end;

function HasAbstractMethod(Class_: TPasClassType): Boolean;
var
  I: Integer;
  Elemento: TPasElement;
begin
  Result := false;
  with Class_ do
    for I := 0 to Members.Count - 1 do
    begin
      Elemento := TPasElement(Members[I]);
      if (Elemento is TPasProcedure) and TPasProcedure(Elemento).IsAbstract then
      begin
        Result := true;
        Exit;
      end;
    end;
end;

procedure WriteClass(Class_: TPasClassType; Indent: String);
var
  GetVisibility: array[TPasMemberVisibility] of String = ('', '_', '', '', '', '', '_', '');
  I: Integer;
  Elemento: TPasElement;
  Prefix: String;
begin
  if Assigned(Class_) and not Class_.IsForward then
    with Class_ do
    begin
      Write(G, DocComment);
      Write(G, Indent, IfThen((ObjKind = okInterface) or IsAbstract or HasAbstractMethod(Class_), 'abstract class ', 'class '), ConvertClassName(Name));
    if Assigned(AncestorType) and (AncestorType.ElementTypeName <> '') then
      Write(G, ' extends ', ConvertClassName(AncestorType.Name));
    if Assigned(Interfaces) and (Interfaces.Count > 0) then
    begin
      Write(G, ' implements ', ConvertClassName(AncestorType.Name));
      for I := 0 to Interfaces.Count - 1 do
      begin
        Write(G, TPasElement(Interfaces[I]).Name);
        Write(G, IfThen(I <> (Interfaces.Count - 1), ','))
      end;
    end;
    Writeln(G, ' {');
    GetFuncsWithoutParams(Members);
    for I := 0 to Members.Count - 1 do
    begin
      Elemento := TPasElement(Members[I]);
      if Elemento.Name = '' then Continue;
      if (I <> 0) and (TPasElement(Members[I - 1]) is TPasVariable) and not (Elemento is TPasVariable) then
        Writeln(G);
      Write(G, Elemento.DocComment);
      Prefix := Indent + TAB;
      if Elemento is TPasProcedure then
        WriteProcedure(TPasProcedure(Elemento), Indent + TAB, GetVisibility[Elemento.Visibility])
      else
      begin
        Elemento.Name := ConvertMember(Elemento.Name);
        if Elemento is TPasProperty then
          with TPasProperty(Elemento) do
          begin
            if ReadAccessor <> nil then
              Write(G, Prefix, WritePropertyType(VarType), ' get ', CamelCase(Name), ' => ', WriteExpr(ReadAccessor), ';');
            if WriteAccessor <> nil then
            begin
              if ReadAccessor <> nil then Writeln(G);
              Write(G, Prefix, 'set ', CamelCase(Name), '(', WritePropertyType(VarType), ' value) => ', ConvertMember(WriteAccessorName), ' = value;');
            end;
          end
        else
        if Elemento is TPasVariable then
          WriteVar(TPasVariable(Elemento), Prefix)
        else
          Writeln(G, 'Unknown declaration in class/interface: ', Elemento.Name);
      end;
      Writeln(G);
    end;
    Writeln(G, Indent, '}');
  end;
end;

procedure WriteResString(Variavel: TPasResString);
begin
  if Assigned(Variavel) then
    with Variavel do
      Write(G, 'const ', ConvertMember(Name), ' = ', WriteExpr(Expr), ';');
end;

function WriteDecls(Decl: TPasDeclarations; Indent: String; IsClosure: Boolean = False): Boolean;
var
  I: Integer;
  Elemento, ElementoProx: TPasElement;
begin
  Result := False;
  if Assigned(Decl) then
  begin
    Elemento := TPasElement(Decl);
    if Elemento is TPasSection then // TInterfaceSection, TImplementationSection or TProgramSection
      with TPasSection(Elemento) do
      begin
        for I := 0 to UsesList.Count - 1 do
          case UpCase(TPasElement(UsesList[I]).Name) of
            'SYSTEM', 'STRUTILS', 'CLASSES', 'LCLTYPE' : ;
            'SYSUTILS': Writeln(G, 'import ''dart:io'';');
            'CONTNRS' : Writeln(G, 'import ''dart:collection'';');
            'MATH'    : Writeln(G, 'import ''dart:math'';');
          else
            Writeln(G, 'import ''', TPasElement(UsesList[I]).Name, ''';');
          end;
        if UsesList.Count <> 0 then
           Writeln(G);
      end;
    if Assigned(Decl.Declarations) then
    begin
      for I := 0 to Decl.Declarations.Count - 1 do
      begin
        Flush(G);
        Elemento := TPasElement(Decl.Declarations[I]);
        if Elemento.DocComment <> '' then
          Writeln(G, '//', Elemento.DocComment);
        if Elemento is TPasConst then
          WriteVar(TPasConst(Elemento), Indent) // static final <tipo> <const> = <value>
        else
        if Elemento is TPasResString then
          WriteResString(TPasResString(Elemento))
        else
        if Elemento is TPasVariable then
          WriteVar(TPasVariable(Elemento), Indent) // <tipo> <const> = <value>
        else
        if Elemento is TPasClassType then
          WriteClass(TPasClassType(Elemento), Indent)
        else
        if Elemento is TPasType then
        begin
          WriteTypes(TPasElement(Elemento), Indent); // def TAtribute = [1..7]; def Range = 1..7; e enums
          Continue;
        end
        else
        if Elemento is TPasProcedureBase then
        begin
          if Pos('.', TPasProcedureBase(Elemento).Name) <> 0 then
            Continue
          else
            if IsClosure then
              WriteClosure(TPasProcedure(Elemento), Indent)
            else
              if not WriteProcedure(TPasProcedure(Elemento), Indent) then
                 Continue;
        end
        else
          Writeln(G, 'Unknown declaration: ', Elemento.Name);
        Writeln(G);
        if I < (Decl.Declarations.Count - 1) then
        begin
          ElementoProx := TPasElement(Decl.Declarations[I + 1]);
          if ((Elemento is TPasVariable) or (Elemento is TPasResString)) and
             not((ElementoProx is TPasVariable) or (ElementoProx is TPasResString)) then
            Writeln(G);
        end;
      end;
      Result := Decl.Declarations.Count <> 0;
      if Result then
        Writeln(G);
    end;
  end;
end;

var
  Modulo: TPasModule;
  Tree: TPasTree;

begin
  AliasTypes := TStringList.Create;
  EnumTypes := TStringList.Create;
  FuncWithByRefs := TStringList.Create;
  Tree := TPasTree.Create;
  Tree.NeedComments := True;
  try
    Modulo := ParseSource(Tree, ParamStr(1) + ' -Sdelphi', 'WINDOWS', 'i386');
  except
    on E: EParserError do
    begin
      Writeln(G, E.Message, ' line:', E.Row, ' column:', E.Column, ' file:', E.Filename);
      Halt;
    end;
  end;
  AssignFile(G, 'C:\trabalho\pas2dart\' + Modulo.Name + '.dart');
  Rewrite(G);
  if Modulo is TPasProgram then
  begin
    WriteDecls(TPasProgram(Modulo).ProgramSection, '');
    WriteCommandBlock(Modulo.InitializationSection as TPasImplBlock, '', 'void main(List<String> args)', '', ComChaves);
  end
  else
  begin
    Writeln(G, 'library ', Modulo.Name, ';', LF);
    WriteDecls(Modulo.InterfaceSection as TPasDeclarations, '');
    WriteDecls(Modulo.ImplementationSection as TPasDeclarations, '');
    WriteCommandBlock(Modulo.InitializationSection as TPasImplBlock, '', 'void initialization()');
    WriteCommandBlock(Modulo.FinalizationSection as TPasImplBlock, '', 'void finalization()');
  end;
  AliasTypes.Free;
  FuncWithByRefs.Free;
  Close(G);
end.
