Unit ITHelper.PascalParsing;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  Classes;

  Function Tokenize(Const strText: String): TStringList;
  
Implementation

Uses
  SysUtils,
  StrUtils;

(**

  This function returns a string list contains the tokenized representation of the passed string with 
  respect to some basic object pascal grammer.

  @precon  strText si the line of text to be tokenised
  @postcon Returns a new string list of the tokenized string

  @note    The string list returned must be destroyed be the calling method.

  @param   strText as a String as a constant
  @return  a TStringList

**)
Function Tokenize(Const strText: String): TStringList;

Type
  TBADITokenType = (ttUnknown, ttWhiteSpace, ttNumber, ttIdentifier, ttLineEnd,
    ttStringLiteral, ttSymbol);
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken: String;
  CurToken: TBADITokenType;
  LastToken: TBADITokenType;
  BlockType: TBlockType;
  (** Token size **)
  iTokenLen: Integer;
  i: Integer;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      {$IFNDEF D2009}
      If strText[i] In [#32, #9] Then
      {$ELSE}
      If CharInSet(strText[i], [#32, #9]) Then
        {$ENDIF}
        CurToken := ttWhiteSpace
        {$IFNDEF D2009}
      Else If strText[i] In ['#', '_', 'a' .. 'z', 'A' .. 'Z', '.', '*'] Then
        {$ELSE}
      Else If CharInSet(strText[i], ['#', '_', 'a' .. 'z', 'A' .. 'Z', '.', '*']) Then
        {$ENDIF}
        Begin
          {$IFNDEF D2009}
          If (LastToken = ttNumber) And (strText[i] In ['A' .. 'F', 'a' .. 'f']) Then
          {$ELSE}
          If (LastToken = ttNumber) And
            (CharInSet(strText[i], ['A' .. 'F', 'a' .. 'f'])) Then
            {$ENDIF}
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
        {$IFNDEF D2009}
      Else If strText[i] In ['$', '0' .. '9'] Then
        {$ELSE}
      Else If CharInSet(strText[i], ['$', '0' .. '9']) Then
        {$ENDIF}
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
        {$IFNDEF D2009}
      Else If strText[i] In [#10, #13] Then
        {$ELSE}
      Else If CharInSet(strText[i], [#10, #13]) Then
        {$ENDIF}
        CurToken := ttLineEnd
        {$IFNDEF D2009}
      Else If strText[i] In ['''', '"'] Then
        {$ELSE}
      Else If CharInSet(strText[i], ['''', '"']) Then
        {$ENDIF}
        CurToken := ttStringLiteral
        {$IFNDEF D2009}
      Else If strText[i] In [#0 .. #255] - ['#', '_', 'a' .. 'z', 'A' .. 'Z', '$',
        '0' .. '9'] Then
        {$ELSE}
      Else If CharInSet(strText[i], [#0 .. #255] - ['#', '_', 'a' .. 'z', 'A' .. 'Z', '$',
        '0' .. '9']) Then
        {$ENDIF}
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;
      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurToken <> ttLineEnd)) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := strText[i];
            End
          Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not(LastToken In [ttLineEnd, ttWhiteSpace]) Then
                  Result.AddObject(strToken, TObject(LastToken));
              BlockType := btNoBlock;
              iTokenLen := 1;
              SetLength(strToken, iTokenCapacity);
              strToken[iTokenLen] := strText[i];
            End;
        End
      Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strText[i];
        End;

      // Check for string literals
      If CurToken = ttStringLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If Not(CurToken In [ttLineEnd, ttWhiteSpace]) Then
        Result.AddObject(strToken, TObject(CurToken));
    End;
End;

End.
