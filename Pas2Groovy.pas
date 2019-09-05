program Pas2Groovy;

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
  AliasTypes: TStringList;
  G: Text;
  InFunction: Boolean = False;
  ByRefArgs: TStringList;
  FuncWithByRefs: TStringList;
  LastParams: String;
  FuncsWithoutParams: String = '"';

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

function ConvertType(PasType: String; Wrapper: Boolean = False; ConvertClass: Boolean = True): String;
var
  I: Integer;
const
  PascalTypes: array[0..26]  of String =     ('string', 'boolean', 'char', 'integer','byte', 'single', 'real', 'double', 'extended', 'word',
    'longint', 'cardinal', 'smallint', 'int64', 'comp',       'variant', 'const', 'TDate', 'TList', 'TStringList', 'Text', 'procedure', 'pointer',
    'TObject', 'TFPList', 'TObjectList', 'currency');
  GroovyTypes: array[-1..26] of String = ('', 'String', 'boolean', 'char', 'int',   'byte',   'float', 'float', 'double', 'BigDecimal', 'short',
    'int',     'int',      'short',    'long',  'BigInteger', 'Object', 'Object', 'Date', 'List',   'List<String>', 'File', 'Closure', 'Object',
    'Object',  'List',  'List',  'BigDecimal');
  WrapperTypes: array[-1..26] of String = ('', 'String', 'Boolean','Char', 'Integer','Byte',  'Float', 'Float', 'Double', 'BigDecimal', 'Short',
    'Integer', 'Integer',  'Short',    'Long',  'BigInteger', 'Object', 'Object', 'Date', 'List',   'List<String>', 'File', 'Closure', 'Object',
    'Object',  'List',  'List',   'BigDecimal');
begin
  if Pos('File ', PasType) <> 0 then
    Result := 'File'
  else
  begin
    I := AnsiIndexText(PasType, PascalTypes);
    Result := IfThen(I >= 0, IfThen(Wrapper, WrapperTypes[I], GroovyTypes[I]), AliasTypes.Values[PasType]);
    Result := IfThen(Result = '', IfThen(ConvertClass, ConvertClassName(PasType), PasType), Result);
  end;
end;

function ConvertMember(PasMember: String): String;
var
  I: Integer;
const
  PascalMembers: array[0..6] of String = ('write', 'writeln', 'exit',   'inc', 'dec', 'halt',           'destroy');
  GroovyMembers: array[0..6] of String = ('print', 'println', 'return', '++',  '--',  'System.exit(0)', 'free');
begin
  Result := PasMember;
  I := Pos('.', Result);
  if I <> 0 then
    Result := Copy(Result, I + 1, MAXINT);
  if (Length(Result) > 1) and (Result[Length(Result)] in ['A'..'Z']) then Exit;
  Result := LowerCase(Result[1]) + Copy(Result, 2, Length(Result));
  I := AnsiIndexText(Result, PascalMembers);
  if I >= 0 then
  begin
    Result := GroovyMembers[I];
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
begin
  Result := '';
  Write(G, Left);
  for I := 0 to High(Lista) do
    if (Lista[I] is TBinaryExpr) and (Lista[I].Kind in [pekRange, pekSet]) then
      Write(G, '*(', WriteExpr(Lista[I]), ')', IfThen(I <> High(Lista), ', '))
    else
      Write(G, WriteExpr(Lista[I]), IfThen(I <> High(Lista), ', '));
  Write(G, Right);
end;

function GetInherited(Expr: TPasExpr): String;
var
  Elemento: TPasElement;
begin
  Result := '()';
  Elemento := Expr.Parent;
  while Elemento <> nil do
  begin
    if Elemento is TPasConstructor then
      Exit;
    if Elemento is TPasProcedure then
    begin
      Result := '.' + ConvertMember(Elemento.Name) + '(' + LastParams + ')';
      Exit;
    end;
    Elemento := Elemento.Parent;
  end;
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

function WriteExpr(Expr: TPasExpr; RemoveCreate: Boolean = False): String;
var
  I: Integer;
  GetOp: array[TExprOpCode] of String = ('..', ' + ', ' - ', ' * ', ' / ', ' / ', ' % ', ' ** ', ' >> ', ' << ', '!', ' && ', ' || ',
    ' ^ ', ' == ', ' != ', ' < ', ' > ', ' <= ', ' >= ', ' in ', ' instanceof ', ' as ', ' >< ', '', '', '', '.');
  GroovyBool: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  if not Assigned(Expr) then Exit;
  if Expr is TBinaryExpr then
    with TBinaryExpr(Expr) do
      if (OpCode = eopSubIdent) and (Right is TParamsExpr) and (Upcase(TPrimitiveExpr(TParamsExpr(Right).Value).Value) = 'CREATE') then
        Write(G, 'new ', WriteExpr(Left), WriteExpr(Right, True))
      else
        if OpCode = eopIs then
          Write(G, WriteExpr(Left), GetOp[OpCode], ConvertClassName(TPrimitiveExpr(Right).Value))
        else
          if Left is TInheritedExpr then
            Write(G, 'super' + GetInherited(Expr))
          else
          begin
            if (Left is TBinaryExpr) and (TBinaryExpr(Left).OpCode in ([eopAdd..eopAs] - [OpCode])) then
              Write(G, '(', WriteExpr(Left), ')')
            else
              Write(G, WriteExpr(Left));
            Write(G, GetOp[OpCode]);
            if (Right is TBinaryExpr) and (TBinaryExpr(Right).OpCode in ([eopAdd..eopAs] - [OpCode])) then
              Write(G, '(', WriteExpr(Right), ')')
            else
              Write(G, WriteExpr(Right));
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
      case Kind of
        pekNumber: Value := ReplaceText(Value, '$', '0x');
        pekString:
          case Value[1] of
            '#': Value := ConvertCharLiteral(Value);
            '''':
              if Value <> '''''' then
                Value := '''' + ReplaceText(Copy(Value, 2, Length(Value) - 2), '''', '\''') + '''';
          end;
      end;
      Write(G, ConvertType(ConvertMember(Value), False, False), IsFuncsWithoutParams(Value))
    end
  else
  if Expr is TBoolConstExpr then
    Write(G, GroovyBool[TBoolConstExpr(Expr).Value])
  else
  if Expr is TNilExpr then
    Write(G, 'null')
  else
  if Expr is TInheritedExpr then
    Write(G, 'super' + GetInherited(Expr))
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
                WriteList('(' + ConvertClassName(TPrimitiveExpr(Value).Value) + ')', Params, '')
              else
                if ConvertMember(TPrimitiveExpr(Value).Value)[1] in ['-', '+'] then
                  Write(G, WriteExpr(Params[0]), ConvertMember(TPrimitiveExpr(Value).Value))
                else
                  WriteList(WriteExpr(Value) + '(', Params, ')')
            else
              WriteList(WriteExpr(Value) + '(', Params, ')');
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
      Write(G, '[');
      for I := 0 to High(Fields) do
        with TRecordValuesItem(Fields[I]) do
          Write(G, Name, ': ', WriteExpr(ValueExp), IfThen(I <> High(Fields), ', '));
      Write(G, ']');
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
          if (P < FuncParams.Count) and IsValidIdent(FuncParams[P]) then
            Atrib := Atrib + ConvertMember(FuncParams[P]) + IfThen(I <> Args.Count - 1, ', ')
          else
            Exit;
        end;
        IfSmt := Pos('return', Variable) = 1;
        if IfSmt then
          Writeln(G, Indent, 'boolean ', Variable, LF);
        Writeln(G, Indent, '(', Atrib, ') = ', WriteExpr(Func));
        if IfSmt then
          Write(G, Indent, 'if (', Variable, ')');
        Result := True;
      end;
    end;
end;

procedure WriteSmt(Smt: TPasImplStatement; Indent: String);
var
  I: Integer;
begin
  if Smt is TPasImplSimple then
    with TPasImplSimple(Smt) do
    begin
      if not WriteByRefFunction(Expr, '', Indent) then
        Writeln(G, Indent, WriteExpr(Expr),
          IfThen((Expr is TPrimitiveExpr) and (Pos('"' + UpperCase(TPrimitiveExpr(Expr).Value) + '"', '"EXIT"BREAK"CONTINUE"') = 0) , '()'))
    end
  else
  if Smt is TPasImplAssign then
    with TPasImplAssign(Smt) do
    begin
      if ((Left is TPrimitiveExpr) and not WriteByRefFunction(Right, ConvertMember(TPrimitiveExpr(Left).Value), Indent)) or
        not(Left is TPrimitiveExpr) then
        Writeln(G, Indent, WriteExpr(Left), ' = ', WriteExpr(Right));
    end
  else
  if Smt is TPasImplCaseStatement then
    with TPasImplCaseStatement(Smt) do
    begin
      Write(G, Indent);
      for I := 0 to Expressions.Count - 1 do
        Write(G, 'case ', WriteExpr(TPasExpr(Expressions[I])), ': ');
      WriteImplElement(Body, Indent + TAB, LF, SemChaves);
      Writeln(G, Indent + TAB, 'break');
    end
  else
  if Smt is TPasImplWithDo then
    with TPasImplWithDo(Smt) do
    begin
      Write(G, Indent);
      for I := 0 to Expressions.Count - 1 do
        Write(G, WriteExpr(TPasExpr(Expressions[I])), '.with', IfThen(I < Expressions.Count - 1, ' { '));
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
      Write(G, Indent, 'catch (', ConvertType(TypeName), ' ', ConvertMember(VariableName), ')');
      WriteImplElement(Body, Indent, IfThen(TPasImplElement(Body) is TPasImplRaise, 'throw '), ComChaves)
    end
  else
  if Smt is TPasImplForLoop then
    with TPasImplForLoop(Smt) do
    begin
      if LoopType = ltIn then
        Write(G, Indent, 'for (', WriteExpr(VariableName), ' in ', WriteExpr(StartExpr), ')')
      else
      begin
        Write(G, Indent, 'for (', WriteExpr(VariableName), ' = ');
        WriteExpr(StartExpr);
        Write(G, '; ', WriteExpr(VariableName), IfThen(Down, ' >= ', ' <= '));
        WriteExpr(EndExpr);
        Write(G, '; ', WriteExpr(VariableName), IfThen(Down, '--', '++'), ')');
      end;
      WriteImplElement(Body, Indent, '', ComChaves);
    end
  else
  if Smt is TPasImplRaise then
    WriteImplElement(TPasImplElement(Smt), Indent, 'throw ')
  else
    WriteBlock(Smt, Indent + TAB);
end;

procedure WriteImplElement(Comando: TPasImplElement; Indent: String; Aditional: String = ''; Fechamento: TFechamento = SemChaves);
begin
  if not Assigned(Comando) then Exit;
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
      WriteImplElement(IfBranch, Indent, '', ComChaves);
      WriteImplElement(ElseBranch, Indent, Indent + 'else', ComChaves);
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
    WriteCommandBlock(TPasImplBlock(Comando), Indent, 'while (true) {' + LF, '', SemChaves);
    Writeln(G, Indent + TAB, 'if (', WriteExpr(TPasImplRepeatUntil(Comando).ConditionExpr), ') break', LF, Indent, '}')
  end
  else
  if Comando is TPasImplTry then
    with TPasImplTry(Comando) do
    begin
      Write(G, Indent, 'try');
      WriteBlock(TPasImplBlock(Comando), Indent);
      WriteImplElement(TPasImplElement(FinallyExcept), Indent);
      WriteImplElement(TPasImplElement(ElseBranch), Indent);
    end
  else
  if Comando is TPasImplTryFinally then
    WriteCommandBlock(TPasImplBlock(Comando), Indent, 'finally')
  else
  if Comando is TPasImplTryExcept then
    WriteBlock(TPasImplBlock(Comando), Indent, '', SemChaves)
  else
  if Comando is TPasImplTryExceptElse then
    WriteCommandBlock(TPasImplBlock(Comando), Indent, 'catch (all)')
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
    ComChaves, SoInicio: Writeln(G, ' {');
    SemChaves: Indent := Copy(Indent, 1, Length(Indent) - Length(TAB));
  end;
  with Block do
    if Assigned(Elements) then
      if Block is TPasImplBlock then
        for I := 0 to Elements.Count - 1 do
          WriteImplElement(TPasImplElement(Elements[I]), Indent + TAB);
  if Aditional <> '' then
    Writeln(G, Indent + TAB, Aditional);
  if Fechamento in [ComChaves, SoFinal] then Writeln(G, Indent, '}')
end;

procedure WriteArrayTypePre(ArrayType: TPasArrayType);
var
  I: Integer;
begin
  with ArrayType do
  begin
    Write(G, ConvertType(ElType.Name));
    for I := 0 to High(Ranges) do
      Write(G, '[]')
  end;
  Write(G, ' ');
end;

function GetArrayTypePos(ArrayType: TPasArrayType): String;
var
  I: Integer;
begin
  with ArrayType do
  begin
    Result := ' new ' + ConvertType(ElType.Name);
    for I := 0 to High(Ranges) do
      if Ranges[I] is TBinaryExpr then
        with TBinaryExpr(Ranges[I]) do
          Result := Result + '[' + TPrimitiveExpr(Right).Value + ' + 1' + IfThen(TPrimitiveExpr(Left).Value <> '0', ' - ' + TPrimitiveExpr(Left).Value) + ']'
      else
        with TPrimitiveExpr(Ranges[I]) do
          Result := Result + '[' + IfThen(Pos(UpperCase(Value), 'CHAR"BYTE') <> 0, '256', Value) + ']';
  end;
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
            Write(G, 'def ', ConvertMember(Name), ' =', GetArrayTypePos(TPasArrayType(VarType)));
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
        Write(G, 'static final ', ConvertMember(Name));  // const
      if Assigned(Expr) then // const AnArrayConst : Array[1..3] of Integer = (1,2,3);
        Write(G, ' = ', WriteExpr(Expr));
  end;
end;
  
procedure WriteTypes(Elemento: TPasElement; Indent: String);
var
  I: Integer;
begin
  if Elemento is TPasArrayType then
    AliasTypes.Add(Elemento.Name + '=' + GetArrayTypePos(TPasArrayType(Elemento)))
  else
  if Elemento is TPasEnumType then
  begin
    Write(G, Indent, 'enum ' + ConvertClassName(Elemento.Name) + ' {');
    with TPasEnumType(Elemento) do
      for I := 0 to Values.Count - 1 do
        Write(G, ConvertMember(TPasEnumValue(Values[I]).Name), IfThen(I < (Values.Count - 1), ', '));
    Writeln(G, '}');
  end
  else
  if Elemento is TPasFileType then
    AliasTypes.Add(Elemento.Name + '=File')
  else
  if Elemento is TPasProcedureType then
    AliasTypes.Add(Elemento.Name + '=Closure')
  else
  if Elemento is TPasPointerType then
    AliasTypes.Add(Elemento.Name + '=Object')
  else
  if Elemento is TPasRangeType then
    with TPasRangeType(Elemento) do
      AliasTypes.Add(Name + '=' + IfThen(RangeStart[1] in ['0'..'9'], 'Byte', '(' + RangeStart + '..' + RangeEnd + ')'))
  else
  if Elemento is TPasRecordType then
    WriteRecord(TPasRecordType(Elemento), Indent)
  else
  if Elemento is TPasSetType then
    AliasTypes.Add(Elemento.Name + '=Set<' + ConvertType(ConvertClassName(TPasSetType(Elemento).EnumType.Name), True) + '>')
  else
  if Elemento is TPasClassOfType then
    AliasTypes.Add(Elemento.Name + '=MetaClass')
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
begin
  if not IsClosure then
  begin
    Write(G, '(');
    LastParams := '';
  end;
  if Assigned(Args) then
    for I := 0 to Args.Count - 1 do
      with TPasArgument(Args[I]) do
      begin
        Write(G, IfThen(Access in [argConst, argConstRef], 'final '));
        if ArgType is TPasArrayType then
          WriteArrayTypePre(TPasArrayType(ArgType))
        else
          Write(G, ConvertType(ArgType.Name) + ' ');
        Write(G, ConvertMember(Name));
        Write(G, IfThen(Value <> '', ' = ' + Value));
        Write(G, IfThen(I < (Args.Count - 1), ', '));
        if not IsClosure then
          LastParams := ConvertMember(Name) + IfThen(I < (Args.Count - 1), ', ');
      end;
  Write(G, IfThen(IsClosure, ' ->', ')'));
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
      Writeln(G, Indent + TAB, FuncType, ' result');
    if not WriteDecls(Proc.Body, Indent + TAB, True) and InFunction then
      Writeln(G);
    Returns := '';
    if InFunction or (ByRefArgs.Count > 0) then
      Returns := 'return ' + IfThen(ByRefArgs.Count > 0, '[' + ListToStr(ByRefArgs) + ']', 'result');
    WriteBlock(TPasImplBlock(Proc.Body.Body), Indent, Returns, SoFinal);
  end;
end;

procedure WriteClosure(Proc: TPasProcedure; Indent: String);
var
  FuncType: String;
begin
  if Assigned(Proc) then
  begin
    Write(G, Indent, 'Closure');
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

procedure WriteProcedure(Proc: TPasProcedure; Indent: String; Visibility: String = '');
var
  FuncType: String;
begin
  if Assigned(Proc) and not Proc.IsForward then
  begin
    if Proc.IsOverride then
      Write(G, Indent, '@Override', LF);
    Write(G, Indent, Visibility, IfThen((Proc is TPasClassProcedure) or (Proc is TPasClassFunction), 'static '));
    InFunction := Proc.ProcType is TPasFunctionType;
    ByRefArgs := GetByRefArgs(Proc.ProcType.Args, InFunction);
    if InFunction then
      FuncType := ConvertType(TPasFunctionType(Proc.ProcType).ResultEl.ResultType.Name)
    else
      FuncType := 'void';
    Write(G, IfThen((Proc is TPasConstructor) or (TPasElement(Proc) is TPasConstructorImpl), ConvertClassName(Proc.Parent.Name),
      IfThen(ByRefArgs.Count > 0, 'def', FuncType) + ' ' + ConvertMember(Proc.Name)));
    WriteProcParams(Proc.ProcType.Args);
    WriteProcBody(Proc, Indent, FuncType, False);
  end;
  InFunction := False;
end;

procedure RemoveReadPrivates(ClassType: TPasClassType);
var
  I: Integer;
  Member: TPasElement;
begin
  with ClassType do
    for I := 0 to Members.Count - 1 do
    begin
      Member := TPasElement(Members[I]);
      if Member is TPasProperty then
        with TPasProperty(Member) do
          if ReadAccessorName <> '' then
          begin
            Member := FindMember(TPasVariable, ReadAccessorName);
            if Member <> nil then
              Member.Name := '';
          end;
    end;
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

procedure WriteClass(Class_: TPasClassType; Indent: String);
var
  GetVisibility: array[TPasMemberVisibility] of String = ('', 'private ', 'protected ', '', '', '', 'private ', 'protected ');
  I: Integer;
  Elemento: TPasElement;
  Prefix, Visib: String;
begin
  if Assigned(Class_) and not Class_.IsForward then
    with Class_ do
    begin
      Write(G, DocComment);
      Write(G, Indent, IfThen(ObjKind = okInterface, 'interface ', 'class '), ConvertClassName(Name));
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
    RemoveReadPrivates(Class_);
    GetFuncsWithoutParams(Members);
    for I := 0 to Members.Count - 1 do
    begin
      Elemento := TPasElement(Members[I]);
      if Elemento.Name = '' then Continue;
      Write(G, Elemento.DocComment);
      Visib := GetVisibility[Elemento.Visibility];
      Prefix := Indent + TAB + Visib;
      if Elemento is TPasVariable then
        WriteVar(TPasVariable(Elemento), Prefix)
      else
      if Elemento is TPasProcedure then
        WriteProcedure(TPasProcedure(Elemento), Indent + TAB, Visib)
      else
      if Elemento is TPasProperty then
        with TPasProperty(Elemento) do
          Writeln(G, Prefix, IfThen(WriteAccessorName = '', 'final '), ConvertType(TPasType(VarType).Name), Name)
      else
        Writeln(G, 'Unknown declaration in class/interface: ', Elemento.Name);
      Writeln(G);
    end;
    Writeln(G, Indent, '}');
  end;
end;

function WriteDecls(Decl: TPasDeclarations; Indent: String; IsClosure: Boolean = False): Boolean;
var
  I: Integer;
  Elemento: TPasElement;
begin
  Result := False;
  if Assigned(Decl) then
  begin
    Elemento := TPasElement(Decl);
    if Elemento is TPasSection then // TInterfaceSection, TImplementationSection or TProgramSection
      with TPasSection(Elemento) do
        for I := 0 to UsesList.Count - 1 do
          if Pos('"' + UpCase(TPasElement(UsesList[I]).Name) + '"', '"SYSTEM"SYSUTILS""STRUTILS"CLASSES"MATH"LCLTYPE"CONTNRS"') = 0 then
            Writeln(G, 'import ', TPasElement(UsesList[I]).Name, IfThen(I = UsesList.Count - 1, LF));
    if Assigned(Decl.Declarations) then
    begin
      for I := 0 to Decl.Declarations.Count - 1 do
      begin
        Elemento := TPasElement(Decl.Declarations[I]);
        if Elemento.DocComment <> '' then
          Writeln(G, '//', Elemento.DocComment);
        if Elemento is TPasConst then
          WriteVar(TPasConst(Elemento), Indent) // static final <tipo> <const> = <value>
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
          if Pos('.', TPasProcedureBase(Elemento).Name) <> 0 then
            Continue
          else
            if IsClosure then
              WriteClosure(TPasProcedure(Elemento), Indent)
            else
              WriteProcedure(TPasProcedure(Elemento), Indent)
        else
          Writeln(G, 'Unknown declaration: ', Elemento.Name);
        Writeln(G);
        if (Elemento is TPasVariable) and
           ((I < (Decl.Declarations.Count - 1)) and not(TPasElement(Decl.Declarations[I + 1]) is TPasVariable)) then
          Writeln(G);
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
  FuncWithByRefs := TStringList.Create;
  Tree := TPasTree.Create;
  Tree.NeedComments := True;
  try
    Modulo := ParseSource(Tree, (*'/Users/barbara/Downloads/Pas2Groovy-2.pas'*)ParamStr(1) + ' -Sdelphi', 'WINDOWS', 'i386');
  except
    on E: EParserError do
    begin
      Writeln(G, E.Message, ' line:', E.Row, ' column:', E.Column, ' file:', E.Filename);
      Halt;
    end;
  end;
  AssignFile(G, (*'/Users/barbara/Downloads/Pas2Groovy-2.groovy'*)Modulo.Name + '.groovy');
  Rewrite(G);
  Writeln(G, 'package ', Modulo.Name, LF);
  if Modulo is TPasProgram then
  begin
    WriteDecls(TPasProgram(Modulo).ProgramSection, '');
    WriteCommandBlock(Modulo.InitializationSection as TPasImplBlock, '', '// Main' + LF + LF, '', SemChaves);
  end
  else
  begin
    WriteDecls(Modulo.InterfaceSection as TPasDeclarations, '');
    WriteDecls(Modulo.ImplementationSection as TPasDeclarations, '');
    WriteCommandBlock(Modulo.InitializationSection as TPasImplBlock, '', 'void initialization()');
    WriteCommandBlock(Modulo.FinalizationSection as TPasImplBlock, '', 'void finalization()');
  end;
  AliasTypes.Free;
  FuncWithByRefs.Free;
  Close(G);
end.
(*          if (I = 0) or not(TPasElement(Decl.Declarations[I - 1]) is TPasConst) then
          begin
            Writeln(G, LF, Indent, 'import static Consts.*', LF);
            Writeln(G, Indent, 'class Consts {');
          end;
          repeat
            WriteVar(TPasConst(Elemento), Indent + TAB); // static final <tipo> <const> = <value>
            Writeln(G);
            Inc(I);
            if I >= Decl.Declarations.Count then Break;
            Elemento := TPasElement(Decl.Declarations[I]);
          until not(Elemento is TPasConst);
          Writeln(G, Indent, '}');*)
