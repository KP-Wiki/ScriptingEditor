unit SynEditUtils;

interface
uses
  Windows, SysUtils, Classes, Variants, Graphics,
  SynEdit, SynCompletionProposal, SynEditTypes;

type
  { The TdSyn_xxx things are using SynEdit specific formating strings }
  TdSyn_DisplayOption = (doSynStyle,        // uses synEdit markup tags for display
                         doIncludeImage,    // doIncludeImage requires doSynStyle
                         doShortText);      // abbreviate the text where applicable
  TdSyn_DisplayOptions = set of TdSyn_DisplayOption;

  { Content specific options. Separate from SynEdit styling }
  TContentOption = (coIncludeContext,       // list as "record.member", "class.member"
                    coIncludeUnit,          // include unit where declared
                    coMultiLine,            // list members for classes/records
                    coFunctionAsForward,    // create forwards for function symbols
                    coSimpleFunctionImpl    // create function implementations in "simple" format (no params or return type)
                  {, coDebugValue});
  TContentOptions = set of TContentOption;

{ Core functions that are used for Code completion, Parameter display and Symbol hints. }
function PerformCodeCompletion(Editor: TCustomSynEdit; Prg: TProgram;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings; var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;
function PerformParamProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean; overload;
function PerformHintProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             HintProposal: TSynCompletionProposal; var x, y: Integer;
                             IncludeImages: Boolean): Boolean; overload;

{ Core functions that are wrapped for convenience }
{function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings; var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;}
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal; var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;
function PerformParamProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean; overload;
function PerformHintProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer; IncludeImages: Boolean): Boolean; overload;

{ Navigation function }
procedure ToggleFromDecl2Impl(Editor: TCustomSynEdit; AProgram: TProgram);

// if a point is in a comment or a string (as defined by the highlighter)
function IsInCommentOrString(Editor: TCustomSynEdit; APoint: TPoint): Boolean;

{ Helper functions }
function GetImageIndex(ImageIndex: Integer; Include: Boolean): string;
function GetNameWithStyle(AName: string; UseStyle: Boolean = True): string;
// return the property that symbol is used as either a read or write access method
function GetPropertyForSymbol(ASym: TSymbol): TPropertySymbol;
function SymbolIsPropertyRead(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
function SymbolIsPropertyWrite(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;

// caller is responsible for freeing the TProgram result
function CompileWithSymbolsAndMap(Script: TDelphiWebScriptII; ScriptText: string): TProgram;

// Complete the class defined in the current 'Context'
function CompleteClassAtCursor(Compiler: TDelphiWebScriptII; AProgram: TProgram;
                               CurrentLine, CurrentRow: Integer;
                               ScriptText: TStrings; BareBonesImpl: Boolean = False): Boolean;
// Complete all incomplete classes
function CompleteAllClasses(Compiler: TDelphiWebScriptII; ScriptText: TStrings;
                            BareBonesImpl: Boolean=False): Boolean;
// Complete the passed in class
function CompleteClass(Compiler: TDelphiWebScriptII; AClassName: string; ScriptText: TStrings;
                       BareBonesImpl: Boolean = False): Boolean;

// Create a method implementation. If Barebones, no function result or params are included (still valid)
procedure CreateMethodImpl(Method: TMethodSymbol; InStrings: TStrings; BareBones: Boolean);

// given a line, find the function symbol that is declared or implemented, return symbol
function FindFuncDeclImplOnLine(ALine: Integer; Dictionary: TSymbolDictionary): TSymbolPosition;

// given a symbol's position, find the toggled Decl/Impl position
function FindFuncTogglePosition(Symbol: TSymbol; FromUsages: TSymbolUsages;
                                Dictionary: TSymbolDictionary): TSymbolPosition;

// given a position, return the Function context it is within. If none, returns nil.
function FindFuncContext(ACol, ALine: Integer; ContextMap: TContextMap): TFuncSymbol;

// Return the position in the script for the alternate method declaration or implementation (opposite of current)
function FindToggledFuncDeclImplPos(ACol, ALine: Integer; AProgram: TProgram): TScriptPos;

// Return if a symbol's declaration point is prior to the given positon.
function SymbolDeclBeforePos(ACol, ALine: Integer; AProgram: TProgram; ASymbol: TSymbol): Boolean;

// Return the value of the value symbol for display in an IDE-type environment
function ValueSymbolAsString(ValueSym: TValueSymbol; Stack: TStack): string;

{ Minor 'helper' routines }

{ Convert boolean value to str (needed for multiple Delphi versions }
function BoolToStr(Value: Boolean): string;
{ Convert a variant to a string. Puts quotes around a string type. }
function VariantToStr(Value: Variant): string;

implementation
uses
  SynEditHighlighter;

{
  Return if the CodeProposal should be executed. Fill the properties of the
  component based on the current position in the editor.
}
function PerformCodeCompletion(Editor: TCustomSynEdit; Prg: TProgram;
                               CodeProposal: TSynCompletionProposal;
                               UnitItemsList, UnitInserts: TStrings;
                               var x, y: Integer;
                               IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
var
  i,
  SearchStart:      Integer;
  SearchSymbol:     TSymbol;
  dispOpts:         TdSyn_DisplayOptions;
  Context:          TContext;
  ThisWordStart,            // position where this word begins
  PrevWordStart,            // position where previous word start
  PrevWordStop:     TPoint; // position where previous word stops
  tmpLineText:      string;
  includeFuncSelf,          // if the function's 'self' member should be included
  includeClassSelf,         // if the class' "Self" should be included
                            // (used for class methods - "Self" is an alias to the class type)
  showAll:          Boolean;
begin
  Assert(Editor <> nil, 'CodeProposalExecute - The Editor parameter must be valued.');
  Assert(Prg <> nil, 'CodeProposalExecute - The Program parameter must be valued.');
  Assert(CodeProposal <> nil, 'CodeProposalExecute - The CodeProposal parameter must be valued.');
  Result := not IsInCommentOrString(Editor, Editor.CaretXY); // don't display if inside a comment or a string

  if not Result then    // if not allowed to popup,
    Exit;               // don't continue

  showAll := True;      // default to show full list
  { Set display options }
  dispOpts := [doSynStyle, doShortText];

  if IncludeImages then                 // if graphics used,
    Include(dispOpts, doIncludeImage);  // add the graphics

  CodeProposal.ClearList;
  // get the previous symbol
  ThisWordStart := Editor.WordStart;

  // This is the first word, previous is the same
  if (ThisWordStart.X = 1) and (ThisWordStart.Y = 1) then
    PrevWordStop := ThisWordStart
  // There are other words before this one
  else
  begin
    PrevWordStart := Editor.PrevWordPosEx(Point(ThisWordStart.X-1, ThisWordStart.Y));   // find previous word's starting point
    PrevWordStop  := Editor.WordEndEx(PrevWordStart);
  end;

  SearchSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(ThisWordStart.X, ThisWordStart.Y);

  // if no Symbol was found, search to previous symbol for context (MyClass.Somethi - Somethi won't be found, go back to class for context)
  if (SearchSymbol = nil) and (ThisWordStart.Y = PrevWordStop.Y) then  // this word and previous on same line
  begin
    tmpLineText := Editor.LineText;
    SearchStart := Length(tmpLineText) + 1;  // +1 because the .X & .Y positions are 1 based.
                                             // An empty line needs to start at 1
    if ThisWordStart.X < SearchStart then
      SearchStart := ThisWordStart.X;

    // search from current word start to previous word stop (what is in between?)
    if SearchStart > PrevWordStop.X then    // delphi will loop 1 downto 1
      for i := SearchStart downto PrevWordStop.X do
      begin
        // if a '.' separates the words, use the previous word as a context
        if tmpLineText[i] = '.' then
        begin
          showAll := False;    // do NOT show full list. Should be a reduced list
          SearchSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(PrevWordStart.X, PrevWordStart.Y);
          Break;
        end;
      end;
  end;

  // if a function, look at return type for base symbol
  if SearchSymbol is TFuncSymbol then
  begin
    if TFuncSymbol(SearchSymbol).Kind = fkFunction then
    begin
      if SearchSymbol is TMethodSymbol then
        SearchSymbol := TMethodSymbol(SearchSymbol).Result
      else
        SearchSymbol := TFuncSymbol(SearchSymbol).Result;
    end;
  end;

  // if Symbol found is a value symbol then get the variable type
  if SearchSymbol is TValueSymbol then
    SearchSymbol := SearchSymbol.Typ;

  // if TClassSymbol
  if (SearchSymbol is TClassSymbol) then
    LoadClassSymbolToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TClassSymbol(SearchSymbol), True, IncludePropertyAccessors,
                             False, False, dispOpts)
  // if TRecordSymbol
  else if (SearchSymbol is TRecordSymbol) then
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         TRecordSymbol(SearchSymbol).Members, False, True, False, False, dispOpts)
  // if TUnitSymbol
  else if (SearchSymbol is TUnitSymbol) then
  begin
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         TUnitSymbol(SearchSymbol).Table, False, True, False, False, dispOpts);

    if TUnitSymbol(SearchSymbol).Table is TLinkedSymbolTable then
      LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                           TLinkedSymbolTable(TUnitSymbol(SearchSymbol).Table).Parent,
                           False, True, False, False, dispOpts);
  end else if (SearchSymbol = nil) and showAll then
  begin { If there is no symbol found, load the full global set including context based ones }
    { Start by examining context, if within a procedure, list those first? }
    // Check ContextMap for symbols local to the context
    Context := Prg.ContextMap.FindContext(Editor.CaretX, Editor.CaretY);

    while Assigned(Context) do  // don't stop until no contexts to check
    begin
      // add symbols local to a context
      if Assigned(Context.LocalTable) then
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             Context.LocalTable, False, False, False, False, dispOpts,
                             Prg, Editor.CaretX, Editor.CaretY);

      // add symbols specific to a function
      if Context.ParentSym is TFuncSymbol then
      begin
        // Determine if the "Self" variable should be added.
        // If function is a 'class method' then the "classSelf" should be included
        // as it is an alias to the class type. If the function is just a normal
        // method, then the 'self' is a data variable pointing to the class instance.
        includeFuncSelf  := True;
        includeClassSelf := False;

        if Context.ParentSym is TMethodSymbol then
          if TMethodSymbol(Context.ParentSym).IsClassMethod then
          begin
            includeClassSelf := True;    // include alias to class type
            includeFuncSelf := False;    // don't include function's "self" variable to class instance
          end;

        // add function parameters to the list
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TFuncSymbol(Context.ParentSym).Params,
                             False, False, False, False, dispOpts);

        //  add internal variables (result, self, etc) to the list
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TFuncSymbol(Context.ParentSym).InternalParams,
                             False, False, False, includeFuncSelf, dispOpts, // include variable 'Self'
                             Prg, Editor.CaretX, Editor.CaretY);

        // if a method, add the object's members to the list
        if Context.ParentSym is TMethodSymbol then
          // load this class' members to the lists
          LoadClassSymbolToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                                   TMethodSymbol(Context.ParentSym).ClassSymbol,
                                   True, IncludePropertyAccessors, includeClassSelf, False, dispOpts);
      end;

      // pop up to the next level context and continue checking
      Context := Context.Parent;
    end;

    // add script specific items (shown at top)
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         Prg.Table, False, False, IncludePropertyAccessors, False, dispOpts,
                         Prg, Editor.CaretX, Editor.CaretY);  // limit to symbols declared before position

    {
      Add unit global items (shown after script-based items)
      if not provided pre-built lists, then fill lists here.
    }
    if (UnitItemsList = nil) or (UnitInserts = nil) then begin
      LoadUnitDeclaredSymbols(Prg, CodeProposal.ItemList, CodeProposal.InsertList, dispOpts, [], False);
    end else
    begin { Provided pre-built lists of unit declared symbols, Append them now. }
      CodeProposal.ItemList.AddStrings(UnitItemsList);
      CodeProposal.InsertList.AddStrings(UnitInserts);
    end;
  end;
end;

{
  Will show the Param Proposal dialog filled with the data for the appropriate symbol.
}
function PerformParamProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             ParamProposal: TSynCompletionProposal; var x, y: Integer): Boolean;
var
  TmpX,
  TmpY,
  StartX,
  StartY,
  TmpLocation,
  p,
  NumBrackets:       Integer;
  FoundSymbol:       TSymbol;
  tmpPoint:          TPoint;
  locLine,
  ParamText,
  ParamDelim,
  thisParam:         string;
  { Related to paren matching (taken from SynEdit.pas GetMatchingBracketEx() }
  Test:              Char;
  isCommentOrString: Boolean;
begin
  Assert(Assigned(Editor.Highlighter), 'A highligher must be assigned to the editor.');

  with Editor do
  begin
    { We start by decrementing X, by adding a ' ' to then end of the line it makes it so
      we don't lose any meaningful data when a line reads like this "thing," because
      the last character is a ',' (it would have been skipped by first dec(X). }
    locLine := LineText + ' ';
    //go back from the cursor and find the first open paren
    TmpX    := CaretX;
    TmpY    := CaretY;

    if TmpX > Length(locLine) then
      TmpX := Length(locLine)
    else if TmpX = 1 then
      TmpX := 2;     // immediately gets decremented (allows it to work when at beginning of line)

    NumBrackets := 0;
    FoundSymbol := nil;
    TmpLocation := 0;
    dec(TmpX);

    while (TmpX > 0) and (TmpY > 0) and (FoundSymbol = nil) do
    begin
      isCommentOrString := IsInCommentOrString(Editor, Point(TmpX, TmpY));

      if ((LocLine[TmpX] = ')') or (NumBrackets > 0)) and (not isCommentOrString) then
      begin
        // search until start of line
        Inc(NumBrackets);// := 1;
        while (TmpX > 1) and (NumBrackets > 0) do begin
          Dec(TmpX);
          Test := LocLine[TmpX];

          if (Test = ')') or (Test = '(') then
          begin
            isCommentOrString := IsInCommentOrString(Editor, Point(TmpX, TmpY));

            if (Test = ')') and (not isCommentOrString) then
              Inc(NumBrackets)
            else if (Test = '(') and (not isCommentOrString) then
              Dec(NumBrackets);
          end;
        end;
      end else if (locLine[TmpX] = '(') and (not isCommentOrString) then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        StartY := TmpY;
        { Set the Popup window to start after the main word on the line after
          the current line.
          NOTE: The default behavior would be to move the popup window along with
                the cursor as you type. This makes it more like Delphi. }
        // use tmpPoint to get the screen coordinates for the popup
        tmpPoint := ClientToScreen(RowColumnToPixels(Point(StartX+1, CaretY+1)));
        x        := tmpPoint.X;
        y        := tmpPoint.Y;
        // re-use tmpPoint to get the position of the previous word
        tmpPoint    := PrevWordPosEx(Point(StartX, StartY));
        FoundSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(tmpPoint.X, tmpPoint.Y);

        if FoundSymbol = nil then
          dec(TmpX);
      end else if (LocLine[TmpX] = ',') and (not isCommentOrString) then
        inc(TmpLocation);

      // if reached the beginning of line, go up a line and begin searching from the end
      if TmpX <= 1 then
      begin
        dec(TmpY);
        locLine := Lines[TmpY - 1] + ' ';  //-1 because Lines is 0 based
        TmpX := Length(locLine);
      end;

      dec(TmpX);
    end;
  end;

  Result := False;   // default to NOT execute

  if FoundSymbol <> nil then
  begin
    ParamProposal.ItemList.Clear;

    // needs to keep searching the previous word until reached a TFuncSymbol word, position CurrentIndex at that point
    if FoundSymbol is TFuncSymbol then
    begin
      ParamDelim := ';';    // separate parameters in the list with ';'
      // set current index to highlight. Total param count - commas encountered
      ParamProposal.Form.CurrentIndex := TmpLocation;

      for p := 0 to TFuncSymbol(FoundSymbol).Params.Count - 1 do
      begin
        thisParam := TFuncSymbol(FoundSymbol).Params[p].Description;

        if Length(TParamSymbol(TFuncSymbol(FoundSymbol).Params[p]).DefaultValue) > 0 then
          thisParam := '[' + thisParam + ']';    // has a default value, display appropriately

        ParamText := ParamText + '"' + thisParam + ParamDelim + '"';

        if p < TFuncSymbol(FoundSymbol).Params.Count - 1 then  // if not the last param
          ParamText := ParamText + ', '                        //   separate with a comma
        else                                                   // if the last one
          ParamDelim := '';                                    //   clear the delimeter
      end;

      // if something to add, add to list
      if ParamText <> '' then
        ParamProposal.ItemList.Add(ParamText);

      Result := True;
    end;
  end;
end;

{
  Shows the pop-up hint information for the focused symbol.
}
function PerformHintProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer; IncludeImages: Boolean): Boolean;
var
  FoundSymbol: TSymbol;
  SymText:     string;    // text to display to represent the symbol
  DispOpts:    TdSyn_DisplayOptions;
  MousePos,
  WordStart:   TPoint;
  SymPos:      TSymbolPosition;      // position in script where symbol is defined
begin
  Result := False;              // default to NOT show it.

  if not Editor.GetPositionOfMouse(MousePos) then
    Exit;

  HintProposal.ItemList.Clear;
  HintProposal.ClBackground := clInfoBk;
  HintProposal.Font.Color   := clInfoText;
  DispOpts                  := [doSynStyle];

  if IncludeImages then
    Include(DispOpts, doIncludeImage);

  try
    WordStart   := Editor.WordStartEx(MousePos);
    FoundSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(WordStart.X, WordStart.Y);

    if FoundSymbol <> nil then
    begin
      { If program is running, make the hint display the current value }
      if Prg.IsDebugging and (Prg.ProgramState in [psRunning, psRunningStopped]) then
      begin
        if FoundSymbol is TValueSymbol then
          Result := True;
      end else if Prg.ProgramState in [psReadyToRun, psTerminated] then { If not running and okay to display descriptive text }
      begin
        SymText := SymbolToText(Prg.Table, FoundSymbol, DispOpts,  [coIncludeContext, coIncludeUnit], nil);

        if SymText <> '' then // if something to display, show it. ('Integer' is a symbol but is not represented with anything)
        begin
          // find the line where symbol is declared, add to end if found
          SymPos := Prg.SymbolDictionary.FindSymbolUsage(FoundSymbol, suDeclaration);

          if Assigned(SymPos) then
            SymText := SymText + Format(' (%d)', [SymPos.ScriptPos.Line]);

          HintProposal.ItemList.Add(SymText);
          Result := True;   // OK to show it
        end;
      end;
    end;

    // position the Hint window to be at the start of the word on the following line
    y := Editor.ClientToScreen(Editor.RowColumnToPixels(Point(WordStart.X, WordStart.Y + 1))).Y; // position under the current item
    x := Editor.ClientToScreen(Editor.RowColumnToPixels(WordStart)).X;
  finally
    // if we can't show it, if it IS showing, turn it off (may be from previous time)
    if (not Result) and HintProposal.Form.Visible then
      HintProposal.CancelCompletion;
  end;
end;

{
  Simple wrapper for easier use.
}
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings; var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);

  try
    Result := PerformCodeCompletion(Editor, Prg, CodeProposal, UnitItemsList,
                                    UnitInserts, x, y, IncludeImages,
                                    IncludePropertyAccessors);
  finally
    Prg.Free;
  end;
end;

{
  Simple wrapper for easier use.
}
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal; var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
begin
  // Pass nil string lists. Force it to fill them internally.
  Result := PerformCodeCompletion(Editor, DWScript, CodeProposal, nil, nil, x, y,
                                  IncludeImages, IncludePropertyAccessors);
end;

{
  Simple wrapper for easier use.
}
function PerformParamProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             ParamProposal: TSynCompletionProposal; var x, y: Integer): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);

  try
    Result := PerformParamProposal(Editor, Prg, ParamProposal, x, y);
  finally
    Prg.Free;
  end;
end;

{
  Simple wrapper for easier use.
}
function PerformHintProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             HintProposal: TSynCompletionProposal; var x, y: Integer;
                             IncludeImages: Boolean): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);

  try
    Result := PerformHintProposal(Editor, Prg, HintProposal, x, y, IncludeImages);
  finally
    Prg.Free;
  end;
end;

{
  Toggle position between Implementation and Declaration (procedures/methods)
}
procedure ToggleFromDecl2Impl(Editor: TCustomSynEdit; AProgram: TProgram);
var
  NewPos: TScriptPos;
begin
  NewPos := FindToggledFuncDeclImplPos(Editor.CaretX, Editor.CaretY, AProgram);

  if NewPos.Pos > -1 then    // -1 if NullPos was returned
  begin
    { Place cursor on correct location }
    Editor.CaretX  := NewPos.Col;
    Editor.CaretY  := NewPos.Line;
    { Try to roughly center the new position in the window }
    Editor.TopLine := Editor.CaretY - (Editor.LinesInWindow div 2);
  end;
end;

{
  Return if the specified position (APoint) is in a comment or a string as
  defined by the highlighter.
}
function IsInCommentOrString(Editor: TCustomSynEdit; APoint: TPoint): Boolean;
var
  dumbstr: string;
  attr:    TSynHighlighterAttributes;
begin
  Result := False;

  if not Assigned(Editor) then
    Exit;

  if Editor.GetHighlighterAttriAtRowCol(APoint, dumbstr, attr) then
    Result := (attr = Editor.Highlighter.StringAttribute) or
              (attr = Editor.Highlighter.CommentAttribute)
  else
    Result := False;
end;

{
  Give easy ability independent of Delphi version to convert a boolean value
  into a string.
}
function BoolToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

{
  Convert a variant value to a string. Will add the quotes to string types so it
  is compatible with Delphi and DWS code.
}
function VariantToStr(Value: Variant): string;
begin
  if VarIsNull(Value) then
    Result := '<NULL>'
  else if VarIsEmpty(Value) then
    Result := '<Unassigned>'
  else if VarType(Value) = varString then   // NOTE: this is not OLE compatible
    Result := '''' + VarToStr(Value) + ''''
  else if VarType(Value) in [varSmallint..varDate, varBoolean] then
    Result := VarToStr(Value)
  else
    Result := '[other]';
end;


{
  Return the SynCompletionProposal image index for an attached image list.
}
function GetImageIndex(ImageIndex: Integer; Include: Boolean): string;
begin
  if Include then
    Result := Format('\image{%d}\column{}', [ImageIndex])  // done as a column
  else
    Result := '';
end;


{
  Return the provided name as a column that is 'bold toggled'
}
function GetNameWithStyle(AName: string; UseStyle: Boolean): string;
begin
  if UseStyle then
    Result := Format('\column{}\style{~B}%s\column{}\style{~B}', [AName])
  else
    Result := AName;
end;


{
  Convenience wrapper that lets you easily attempt to complete any incomplete
  class that the cursor is inside of. (inside the context)
}
function CompleteClassAtCursor(Compiler: TDelphiWebScriptII; AProgram: TProgram;
                               CurrentLine, CurrentRow: Integer;
                               ScriptText: TStrings; BareBonesImpl: Boolean): Boolean;
var
  Context: TContext;
begin
  Result := False;

  if Assigned(AProgram) then
  begin
    { Find the class that is to be completed }
    Context := AProgram.ContextMap.FindContext(CurrentRow, CurrentLine);

    if Assigned(Context) then
      if Context.ParentSym is TClassSymbol then
        Result := CompleteClass(Compiler, Context.ParentSym.Name, ScriptText, BareBonesImpl);
  end;
end;

{
  Creates an implementation stub for a method. The BareBones flag when true will
  supress all parameters and return types.
}
procedure CreateMethodImpl(Method: TMethodSymbol; InStrings: TStrings; BareBones: Boolean);
var
  p:              Integer;
  ParamText,
  FuncReturnType,
  FuncType:       string;
begin
  Assert(Assigned(Method));
  Assert(Assigned(InStrings));
  { Assemble the method text piece-wise. }
  FuncReturnType := '';

  { Get string description of function type }
  case Method.Kind of
    fkFunction:
    begin
      FuncType := 'function';
      if (not BareBones) and Assigned(Method.Result) then
        FuncReturnType := ': ' + Method.Typ.Caption;
    end;
    fkProcedure:
    begin
      FuncType := 'procedure';
    end;
    fkConstructor:
    begin
      FuncType := 'constructor';
    end;
    fkDestructor:
    begin
      FuncType := 'destructor';
    end;
    else
      FuncType := '<unknown>';
  end;

  // Add 'class' to class methods
  if Method.IsClassMethod then
    FuncType := 'class ' + FuncType;

  ParamText := '';

  // load params for function
  if not BareBones then
  begin
    for p := 0 to Method.Params.Count - 1 do
    begin
      ParamText := ParamText + Method.Params[p].Description;

      if p < Method.Params.Count - 1 then
        ParamText := ParamText + '; ';
    end;

    // Add parenthesis if there are params
    if ParamText <> '' then
      ParamText := '('+ParamText+')';
  end;

  InStrings.Add('');
  InStrings.Add(Format('%s %s.%s%s%s;', [FuncType, Method.ClassSymbol.Name,
                                         Method.Name, ParamText, FuncReturnType]));
  InStrings.Add('begin');
  InStrings.Add(''); // auto-generated');
  InStrings.Add('end;');
end;

{
  All classes will be completed by default. Only options are for how much detail
  should be added to the implementation (parameters and return type -- BareBonesImpl)
}
function CompleteAllClasses(Compiler: TDelphiWebScriptII; ScriptText: TStrings;
                            BareBonesImpl: Boolean = False): Boolean;
begin
  // if one or more was built, will return true
  Result := CompleteClass(Compiler, '', ScriptText, BareBonesImpl);
end;

{
  If AClassName has a value then just that class will be completed.
  If AClassName is blank, then any incomplete class will be changed.
  BareBonesImpl means no parameters or return types will be added to the implementation.
}
function CompleteClass(Compiler: TDelphiWebScriptII; AClassName: string;
                       ScriptText: TStrings; BareBonesImpl: Boolean = False): Boolean;
var
  i, x,
  insertDeclLine,
  insertDeclCol:  Integer;
  method:         TMethodSymbol;
  ErrorClassName: string;
  fixClassPos:    TSymbolPosition;
  fixClass:       TClassSymbol;   // class to complete
  locProgram:     TProgram;       // local program used for recompiles and completion help
  ChangedInLoop:  Boolean;        // flag to denote if a change was made this time through the loop
begin
  Result := False;

  if (ScriptText = nil) then
    Exit;

  locProgram := CompileWithSymbolsAndMap(Compiler, ScriptText.Text);

  try
    { Take multiple passes. Recompile as we cycle and keep completing until nothing
      left to complete. Recompiles are needed because we stop compiling on
      error conditions. As the condition is satisfied, need to recompile to get
      further class members checked. }
    repeat
      ChangedInLoop := False;

      // needs to continously recompile the script until all possible changes have been made (re-collect pointers each time)
      for i := 0 to Length(locProgram.ClassCompleteNeeds) - 1 do
      begin
        Assert(Assigned(locProgram.ClassCompleteNeeds[i].ErrorClass));
        ErrorClassName := locProgram.ClassCompleteNeeds[i].ErrorClass.Name;

        { If no specific class is specified, fix errors with any class that has problems. }
        if AClassName = '' then
          fixClassPos := locProgram.SymbolDictionary.FindSymbolUsage(TSymbol(locProgram.ClassCompleteNeeds[i].ErrorClass), suDeclaration)
        else
          fixClassPos := locProgram.SymbolDictionary.FindSymbolUsageOfType(AClassName, TClassSymbol, suDeclaration);

        if not Assigned(fixClassPos) then
          raise Exception.CreateFmt('Class completion failed. Cannot find class "%s".', [ErrorClassName]);

        fixClass := TClassSymbol(fixClassPos.Symbol);        // get a shortcut for easer operation

        { If not completing all and the classes don't match, move to next item.
          The repeat..until will stop if no changes could be made }
        if (AClassName<>'') and (not SameText(AClassName, ErrorClassName)) then
          Continue;

        // property completion needed
        if locProgram.ClassCompleteNeeds[i].ErrorType = ccePropAccessDeclMissing then
        begin
          // default insert position to first line after class declaration
          insertDeclCol  := fixClassPos.ScriptPos.Col + 1;
          insertDeclLine := fixClassPos.ScriptPos.Line + 1;

          // cycle the members, find first place fit for new declaration
          for x := 0 to fixClass.Members.Count - 1 do
          begin
            // if a Field, default new position to be AFTER the field.
            if fixClass.Members[x] is TFieldSymbol then
              insertDeclLine := locProgram.SymbolDictionary.FindSymbolUsage(fixClass.Members[x],
                                                                            suDeclaration).ScriptPos.Line + 1;
          end;

          // insert the completed property information
          ScriptText.Insert(insertDeclLine - 1, StringOfChar(' ', insertDeclCol) +
                                                locProgram.ClassCompleteNeeds[i].SuggestedFix);
          Result        := True; // a change was successfully made
          ChangedInLoop := True;
        end;

        { Method implementation completion }
        if locProgram.ClassCompleteNeeds[i].ErrorType = cceMethodImplMissing then
        begin
          for x := 0 to fixClass.Members.Count - 1 do
            if fixClass.Members[x] is TMethodSymbol then
            begin
              method := fixClass.Members[x] as TMethodSymbol;

              { If the method is not abstract and is declared here but not implemented }
              if not method.IsAbstract and
                 Assigned(locProgram.SymbolDictionary.FindSymbolUsage(method, suDeclaration)) and
                 (locProgram.SymbolDictionary.FindSymbolUsage(method, suImplementation) = nil) then
              begin
                CreateMethodImpl(method, ScriptText, BareBonesImpl);
                Result        := True; // at least one was built
                ChangedInLoop := True;
              end;
            end;
        end;
      end;

      { Recompile - free program, compile new one }
      locProgram.Free;
      locProgram := CompileWithSymbolsAndMap(Compiler, ScriptText.Text);

      // if nothing was changed and there are still errors, show message and abort.
      if (not ChangedInLoop) and (Length(locProgram.ClassCompleteNeeds) > 0) then
        if (AClassName = '') or SameText(AClassName, ErrorClassName) then
          raise Exception.CreateFmt('Unable to complete class "%s". Correct other script errors first.', [ErrorClassName]);
    until (Length(locProgram.ClassCompleteNeeds) = 0) or (not ChangedInLoop);
  finally
    locProgram.Free;
  end;
end;

{
  Return the function symbol that is either declared or implemented on the
  specified line. Returns nil if nothing fits it.
}
function FindFuncDeclImplOnLine(ALine: Integer; Dictionary: TSymbolDictionary): TSymbolPosition;
var
  i:         Integer;
  Declared,
  Implement: TSymbolPosition;
begin
  Result := nil;

  for i := 0 to Dictionary.Count - 1 do
  begin
    { If a method, look for Declaration, Implementation }
    if Dictionary[i].Symbol is TMethodSymbol then
    begin
      Declared := Dictionary[i].FindUsage(suDeclaration);

      if Assigned(Declared) then
        if Declared.ScriptPos.Line = ALine then
          Result := Declared;

      Implement:= Dictionary[i].FindUsage(suImplementation);

      if Assigned(Implement) then
        if Implement.ScriptPos.Line = ALine then
          Result := Implement;
    end else if Dictionary[i].Symbol is TFuncSymbol then { If a function (not method), look for Forward, Declaration }
    begin
      // forwards are the early declarations
      Declared := Dictionary[i].FindUsage(suForward);

      if Assigned(Declared) then
        if Declared.ScriptPos.Line = ALine then
          Result := Declared;

      // declaration is the implementation for functions
      Implement:= Dictionary[i].FindUsage(suDeclaration);

      if Assigned(Implement) then
        if Implement.ScriptPos.Line = ALine then
          Result := Implement;
    end;

    if Assigned(Result) then
      Break;
  end;
end;

{
  Return the symbol position that corresponds to the current function or method.
  This is used to jump from a method declaration to the interface and back.
  A function (non-method) can have a 'forward' and this can be used as a jump point.
}
function FindFuncTogglePosition(Symbol: TSymbol; FromUsages: TSymbolUsages;
                                Dictionary: TSymbolDictionary): TSymbolPosition;
begin
  Result := nil;

  if not Assigned(Symbol) then
    Exit;

  { If a method, determine toggled usage }
  if Symbol is TMethodSymbol then
  begin
    if suDeclaration in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suImplementation)
    else if suImplementation in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suDeclaration);
  end else if Symbol is TFuncSymbol then { If a function, determine toggled usage }
  begin
    if suForward in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suImplementation)
    else if (suDeclaration in FromUsages) or (suImplementation in FromUsages) then
      Result := Dictionary.FindSymbolUsage(Symbol, suForward);
  end;
end;

{
  Find the Function symbol associated with the current location in the script.
  Nil is returned if none found.
}
function FindFuncContext(ACol, ALine: Integer; ContextMap: TContextMap): TFuncSymbol;
var
  Context: TContext;
begin
  Result  := nil;
  Context := ContextMap.FindContext(ACol, ALine);

  if Assigned(Context) then
  begin
    { Determine if context is a function. Go up parent tree until reach top. }
    repeat
      if Context.ParentSym is TFuncSymbol then
        Result := TFuncSymbol(Context.ParentSym)
      else
        Context := Context.Parent;
    until Assigned(Result) or (Context = nil);
  end;
end;

{
  Given a symbol, return the property for which it is either a read or write
  access method. Currently only applies to TMethodSymbols and TFieldSymbols.
}
function GetPropertyForSymbol(ASym: TSymbol): TPropertySymbol;
var
  i:        Integer;
  ClassSym: TClassSymbol;
begin
  Result := nil;

  if ASym is TMethodSymbol then
    ClassSym := TMethodSymbol(ASym).ClassSymbol
  else if ASym is TFieldSymbol then
    ClassSym := TFieldSymbol(ASym).ClassSymbol
  else
    ClassSym := nil;

  if Assigned(ClassSym) and Assigned(ASym) then
    for i := 0 to ClassSym.Members.Count - 1 do
    begin
      if ClassSym.Members[i] is TPropertySymbol then
        if (TPropertySymbol(ClassSym.Members[i]).ReadSym = ASym) or
           (TPropertySymbol(ClassSym.Members[i]).WriteSym = ASym)
        then
        begin
          // Return pointer to the property symbol
          Result := TPropertySymbol(ClassSym.Members[i]);
          Break;
        end;
    end;
end;

{
  Return if a symbol is used in the 'read' portion of an object property.
}
function SymbolIsPropertyRead(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
begin
  // Pointer match is the answer.
  Result := False;

  if Assigned(AProperty) and Assigned(ASym) then
    Result := AProperty.ReadSym = ASym;
end;

{
  Return if a method is used in the 'write' portion of an object property.
}
function SymbolIsPropertyWrite(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
begin
  // Pointer match is the answer.
  Result := False;

  if Assigned(AProperty) and Assigned(ASym) then
    Result := AProperty.WriteSym = ASym;
end;

{
  Compile a script using SymbolDictionary and ContentMap.
  Restores settings immediately.
}
function CompileWithSymbolsAndMap(Script: TDelphiWebScriptII; ScriptText: string): TProgram;
var
  OldOpts: TCompilerOptions;
begin
  // compile the script (store original options, turn on needed options)
  OldOpts := Script.Config.CompilerOptions;

  try
    Script.Config.CompilerOptions := Script.Config.CompilerOptions +
                                     [coSymbolDictionary, coContextMap];
    Result                        := Script.Compile(ScriptText);
  finally
    Script.Config.CompilerOptions := OldOpts;
  end;
end;


{
  Return toggled position between Implementation and Declaration (procedures/methods)
}
function FindToggledFuncDeclImplPos(ACol, ALine: Integer; AProgram: TProgram): TScriptPos;
var
  SymPos,
  TogglePos:  TSymbolPosition;
  FuncSym:    TFuncSymbol;
  FromUsages: TSymbolUsages;
begin
  SymPos := FindFuncDeclImplOnLine(ALine, AProgram.SymbolDictionary);

  { If assigned, use the function symbol }
  if Assigned(SymPos) then
  begin
    FuncSym    := TFuncSymbol(SymPos.Symbol);
    FromUsages := SymPos.SymbolUsages;
  end else { If nothing found, check context  }
  begin
    FuncSym    := FindFuncContext(ACol, ALine, AProgram.ContextMap);
    FromUsages := [suImplementation];
  end;

  { If we found the function symbol in question, find where to jump to. }
  if Assigned(FuncSym) then
  begin
    TogglePos := FindFuncTogglePosition(FuncSym, FromUsages, AProgram.SymbolDictionary);

    if Assigned(TogglePos) then
      Result := TogglePos.ScriptPos  // Return position
    else
      Result := NullPos;             // return invalid position
  end;
end;

{
  Determine if a position (ACol, ALine) is before the declaration of a symbol.
}
function SymbolDeclBeforePos(ACol, ALine: Integer; AProgram: TProgram; ASymbol: TSymbol): Boolean;
var
  SymPos: TSymbolPosition;
begin
  Assert(AProgram <> nil);
  Assert(ASymbol <> nil);
  // Check if position is before declaration position
  SymPos := AProgram.SymbolDictionary.FindSymbolUsage(ASymbol, suDeclaration);

  if Assigned(SymPos) then
    // if same line and before column position or on a previous line
    Result := ((ALine = SymPos.ScriptPos.Line) and (SymPos.ScriptPos.Col <= ACol )) or
              (SymPos.ScriptPos.Line < ALine)
  else  // if declaration of symbol not found, then assumed prior to position
    Result := True;
end;

{
  Return the string representation of the value of the value symbol.
  This will format it as suitable for display in an IDE. Ex: a class will
  display as "()" while a string will be "'my string'"
}
function ValueSymbolAsString(ValueSym: TValueSymbol; Stack: TStack): string;
var
  typ:   TTypeSymbol;
  Value: Variant;
begin
  Result := '';

  if not Assigned(ValueSym.Typ) then
    Exit;

  if typ is TClassSymbol then
  begin
    if Value = Unassigned then
      Result := 'nil'
    else
      Result := '()';
  end else if VarType(Value) = varString then
    Result := '''' + Value + ''''
  else
    Result := Value;
end;

end.
