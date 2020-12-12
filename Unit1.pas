unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Menus,
  FMX.ListBox;

type
  ESudokuRule = class(Exception);

  TDigit = 1 .. 9;
  TDigitWithSentinel = 0 .. 9;
  TDigitSet = set of TDigit;
  TSourceOrigin = (soInitial, soSet, soCalculated, soSolver, soError);

  TNumber = record
  private
    fSet: TDigitSet; {Digits that are still available for selection}
    fCalcSet: TDigitSet; {Reduced set of digits after calculation}
    fFinale: TDigitWithSentinel; {If the value is greater than 0 it contains the final selected digit}
    fBack: TDigitWithSentinel;
    fSourceOrigin: TSourceOrigin; {Who set the digit?}
    fSolverPos: TDigitWithSentinel; {Position in value set during auto solving}
    function SearchNextSolverPos: boolean;
    procedure SetDigitSet(Digits: TDigitSet);
    procedure SetCalcSet(Digits: TDigitSet);
  public
    procedure Init;
    procedure StartCalc; {Initialization of CalcSet}
    function EndCalc: boolean;
    procedure SaveBack; {Stash last number}
    procedure RollBack; {Get the last number out of the stash}
    procedure SetDefFinal(Digit: TDigit); {Set a final digit}
    procedure TakeSolverPos; {Select the next possible digit in the solver}
    procedure SetRuleError; {There was rule violation when using this digit}
    function IsFinale: boolean; {Was the digit set?}
    function GetCount: integer; {Counting the digits that are still available}
    function GetAsStr: string; {Get possible digits}
    property DigitSet: TDigitSet read fSet write SetDigitSet;
    property CalcSet: TDigitSet read fCalcSet write SetCalcSet;
    property SourceOrigin: TSourceOrigin read fSourceOrigin;
    property Finale: TDigitWithSentinel read fFinale;
    property SolverPos: TDigitWithSentinel read fSolverPos write fSolverPos;
  end;

  TNumberPos = 0..8;
  TNumbers = array [TNumberPos, TNumberPos] of TNumber;

  TfmxMain = class(TForm)
    PaintBox: TPaintBox;
    ScaledLayout: TScaledLayout;
    Popup: TPopup;
    CalloutPanel1: TCalloutPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button5: TButton;
    Button7: TButton;
    Button9: TButton;
    Button8: TButton;
    btnClear: TButton;
    pnlToolbar: TPanel;
    btnSave: TButton;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnInit: TButton;
    btnUndo: TButton;
    btnSolve: TButton;
    tmrAutoSolver: TTimer;
    btnAutoSolver: TButton;
    btnStopAutoSolver: TButton;
    rctMessage: TRectangle;
    txtMessage: TText;
    lstLog: TListBox;
    btnShowLog: TButton;
    laySolverTools: TLayout;
    trbSpeed: TTrackBar;
    Text1: TText;
    Text2: TText;
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure CalloutPanel1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject); {Saves the current configuration}
    procedure btnLoadClick(Sender: TObject); {Loads a saved configuration}
    procedure btnInitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnUndoClick(Sender: TObject); {Reverse last step}
    procedure FormDestroy(Sender: TObject);
    procedure btnSolveClick(Sender: TObject); {Next solve step}
    procedure tmrAutoSolverTimer(Sender: TObject); {Timer for AutoSolver step}
    procedure btnAutoSolverClick(Sender: TObject); {Starts the AutoSolver process}
    procedure btnStopAutoSolverClick(Sender: TObject); {Stops the AutoSolver process}
    procedure btnShowLogClick(Sender: TObject);
    procedure trbSpeedChange(Sender: TObject);
  private
    fSelectedBox: TPoint;
    fNumbers: TNumbers;
    fSolverSteps: integer;
    fButton: array [1 .. 9] of TButton;
    fStack: TStack<TNumbers>; {History of the manipulated numbers as a stack}
    procedure CalcVertical; {Checks that all digits do not appear in the same column again}
    procedure CalcHorizontal; {Checks that all digits do not appear in the same line again}
    procedure CalcBox; {Checks that all digits do not appear in the same box again}
    procedure Push; {Pushes the present state on to the stack}
    procedure Pop; {Removes the last added state from the stack}
    procedure StackInit;
    function GetFileName(Load: boolean): string;
    function GetNextSolverPos: boolean; {Find the number position with the least variability}
  public
    procedure Init;
    function CalcAll(var Error: string): boolean;
    procedure ShowError(const Error: string);
    procedure ClearError;
    procedure LogMsg(const Msg: string);
  end;

var
  fmxMain: TfmxMain;

implementation

uses
  System.IOUtils;

{$R *.fmx}

{$REGION 'Form contruction/destruction'}
procedure TfmxMain.FormCreate(Sender: TObject);
begin
  fButton[1] := Button1;
  fButton[2] := Button2;
  fButton[3] := Button3;
  fButton[4] := Button4;
  fButton[5] := Button5;
  fButton[6] := Button6;
  fButton[7] := Button7;
  fButton[8] := Button8;
  fButton[9] := Button9;
  fStack := TStack<TNumbers>.Create;
  Init;
end;

procedure TfmxMain.FormDestroy(Sender: TObject);
begin
  fStack.Free;
end;
{$ENDREGION}

{$REGION 'Popup Panel'}
procedure TfmxMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  p: TPointF;
  c: integer;
begin
  fSelectedBox.X := trunc(X / paintbox.Width * 9);
  fSelectedBox.Y := trunc(Y / paintbox.Height * 9);
  PaintBox.Repaint;
  p := ClientToScreen(pointF((fSelectedBox.X - 0.75) * 50 * ScaledLayout.Width /
    ScaledLayout.OriginalWidth, (fSelectedBox.Y + 1.2) * 50 *
    ScaledLayout.height / ScaledLayout.OriginalHeight));
  Popup.PlacementRectangle.Left := p.X;
  Popup.PlacementRectangle.Top := p.Y + pnlToolbar.height;
  for c := 1 to 9 do
    fButton[c].Visible := c in fNumbers[fSelectedBox.X, fSelectedBox.Y].DigitSet;
  btnClear.SetFocus;
  Popup.IsOpen := true;
  ClearError;
end;

procedure TfmxMain.btnClearClick(Sender: TObject);
var
  oldValS: TDigitSet;
  Error: string;
begin
  if (fSelectedBox.X >= 0) and (fSelectedBox.Y >= 0) then
  begin
    oldValS := fNumbers[fSelectedBox.X, fSelectedBox.Y].DigitSet;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].Init;
    if not CalcAll(Error) then
    begin
      fNumbers[fSelectedBox.X, fSelectedBox.Y].SetRuleError;
      fNumbers[fSelectedBox.X, fSelectedBox.Y].DigitSet := oldValS;
      ShowError(Error);
      PaintBox.Repaint;
    end else begin
      PaintBox.Repaint;
      Popup.IsOpen := false;
    end;
  end;
end;

procedure TfmxMain.ButtonClick(Sender: TObject);
var
  Val: integer;
  oldValS: TDigitSet;
  Error: string;
begin
  Push;
  Val := TButton(Sender).Tag;
  if (fSelectedBox.X >= 0) and (fSelectedBox.Y >= 0) then
  begin
    oldValS := fNumbers[fSelectedBox.X, fSelectedBox.Y].DigitSet;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].SetDefFinal(Val);
    if not CalcAll(Error) then
    begin
      ShowError(Error);
      fNumbers[fSelectedBox.X, fSelectedBox.Y].SetRuleError;
      fNumbers[fSelectedBox.X, fSelectedBox.Y].DigitSet := oldValS;
      PaintBox.Repaint;
    end else begin
      ClearError;
      PaintBox.Repaint;
      Popup.IsOpen := false;
    end;
  end;
end;

procedure TfmxMain.CalloutPanel1Click(Sender: TObject);
begin
  ClearError;
  Popup.IsOpen := false;
  fSelectedBox := Point(-1, -1);
end;
{$ENDREGION}

{$REGION 'Save/Load File'}
procedure TfmxMain.btnInitClick(Sender: TObject);
begin
  Init;
  StackInit;
  PaintBox.Repaint;
end;

function TfmxMain.GetFileName(Load: boolean): string;
begin
  result := '';
{$IFDEF MSWINDOWS}
  if Load then
  begin
    if OpenDialog.Execute then
      result := OpenDialog.FileName;
  end
  else
  begin
    if SaveDialog.Execute then
      result := SaveDialog.FileName;
  end;
{$ENDIF}
{$IFDEF MACOS}
  if Load then
  begin
    if OpenDialog.Execute then
      result := OpenDialog.FileName;
  end
  else
  begin
    if SaveDialog.Execute then
      result := SaveDialog.FileName;
  end;
{$ENDIF}
{$IFDEF IOS}
  result := TPath.GetDocumentsPath + PathDelim + 'Sudoku.txt';
{$ENDIF}
{$IFDEF ANDROID}
  result := TPath.GetDocumentsPath + PathDelim + 'Sudoku.txt';
{$ENDIF}
end;

procedure TfmxMain.btnLoadClick(Sender: TObject);
var
  sl: TStringList;
  X, Y, f: integer;
  Error: string;
  fn: string;
begin
  fn := GetFileName(true);
  if fn.IsEmpty then
    exit;
  Init;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fn);
    for X := 0 to 8 do
      for Y := 0 to 8 do
      begin
        f := StrToInt(sl.Values[IntToStr(X) + '.' + IntToStr(Y)]);
        if f > 0 then
          fNumbers[X, Y].SetDefFinal(f)
        else
          fNumbers[X, Y].Init;
      end;
  finally
    sl.Free;
  end;

  if not CalcAll(Error) then
  begin
    ShowError(Error);
    Init;
  end;
  PaintBox.Repaint;
  StackInit;
end;

procedure TfmxMain.btnSaveClick(Sender: TObject);
var
  sl: TStringList;
  X, Y: TNumberPos;
  fn: string;
begin
  fn := GetFileName(false);
  if fn.IsEmpty then
    exit;
  sl := TStringList.Create;
  try
    for X := 0 to 8 do
      for Y := 0 to 8 do
        if fNumbers[X, Y].SourceOrigin = soSet then
          sl.AddPair(IntToStr(X) + '.' + IntToStr(Y),
            IntToStr(fNumbers[X, Y].Finale))
        else
          sl.AddPair(IntToStr(X) + '.' + IntToStr(Y), IntToStr(0));
    sl.SaveToFile(fn);
  finally
    sl.Free;
  end;
end;
{$ENDREGION}

{$REGION 'Init File'}
procedure TfmxMain.Init;
var
  X, Y: TNumberPos;
begin
  {All boxes are initialized and buttons get reset}
  btnSolve.Enabled:= true;
  btnSolve.Text := 'Solve';
  fSolverSteps := 0;
  btnAutoSolver.Visible := true;
  btnAutoSolver.Enabled := true;
  btnStopAutoSolver.Visible := false;
  fSelectedBox := Point(-1, -1);
  for X := 0 to 8 do
    for Y := 0 to 8 do
      fNumbers[X, Y].Init;
  ClearError;
  lstLog.Clear;
end;
{$ENDREGION}

{$REGION 'Paint the matrix'}
procedure TfmxMain.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  X, Y: TNumberPos;
  r: TRectF;
begin
  Canvas.BeginScene;

  Canvas.Fill.Color := TAlphaColors.Lightblue;
  Canvas.Fill.Kind := TBrushKind.Solid;

  Canvas.Stroke.Color := TAlphaColors.Black;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Thickness := 3;
  for X := 0 to 2 do
    for Y := 0 to 2 do
    begin
      r := rectF(X * 150, Y * 150, (X + 1) * 150 - 1, (Y + 1) * 150 - 1);
      Canvas.DrawRect(r, 0, 0, AllCorners, 1);
    end;

  for X := 0 to 8 do
    for Y := 0 to 8 do
    begin
      if (fSelectedBox.X = X) and (fSelectedBox.Y = Y) then
      begin
        Canvas.Stroke.Thickness := 4;
        Canvas.Stroke.Color := TAlphaColors.Red
      end else begin
        Canvas.Stroke.Thickness := 1;
        Canvas.Stroke.Color := TAlphaColors.Black;
      end;
      case fNumbers[X, Y].SourceOrigin of
        soInitial:
          Canvas.Fill.Color := TAlphaColors.Lightgray;
        soSet:
          Canvas.Fill.Color := TAlphaColors.White;
        soCalculated:
          Canvas.Fill.Color := TAlphaColors.Lime;
        soSolver:
          Canvas.Fill.Color := TAlphaColors.Lightblue;
        else
          Canvas.Fill.Color := TAlphaColors.Pink
      end;
      r := rectF(X * 50, Y * 50, (X + 1) * 50 - 1, (Y + 1) * 50 - 1);
      Canvas.FillRect(r, 0, 0, AllCorners, 1);
      Canvas.DrawRect(r, 0, 0, AllCorners, 1);

      Canvas.Font.Size := 20;
      if fNumbers[X, Y].IsFinale then
      begin
        Canvas.Fill.Color := TAlphaColors.Black;
        Canvas.FillText(r, IntToStr(fNumbers[X, Y].Finale), false, 1, [],
          TTextAlign.Center, TTextAlign.Center);
      end
      else if fNumbers[X, Y].GetCount = 2 then
      begin
        Canvas.Fill.Color := TAlphaColors.Black;
        Canvas.FillText(r, '{' + fNumbers[X, Y].GetAsStr + '}', false, 1, [],
          TTextAlign.Center, TTextAlign.Center);
      end else begin
        Canvas.Fill.Color := TAlphaColors.White;
        Canvas.FillText(r, '(' + IntToStr(fNumbers[X, Y].GetCount) + ')', false,
          1, [], TTextAlign.Center, TTextAlign.Center);
      end;
    end;

  Canvas.EndScene;
end;
{$ENDREGION}

{$REGION 'Half-Solver: Calculate'}
function TfmxMain.CalcAll(var Error: string): boolean;
var
  X, Y: TNumberPos;
  EndCalc: boolean;
begin
  result := false;
  for X := 0 to 8 do
    for Y := 0 to 8 do
      fNumbers[X, Y].SaveBack;
  try
    repeat
      for X := 0 to 8 do
        for Y := 0 to 8 do
          fNumbers[X, Y].StartCalc;
      CalcHorizontal;
      CalcVertical;
      CalcBox;
      EndCalc := false;
      for X := 0 to 8 do
        for Y := 0 to 8 do
          if not fNumbers[X, Y].IsFinale then
            if fNumbers[X, Y].EndCalc then
            begin
              Log.d('End calculated of : ' + IntToStr(X + 1) + ',' +
                IntToStr(Y + 1));
              EndCalc := true;
            end;
    until not EndCalc;
    result := true;
  except
    on e: ESudokuRule do
    begin
      Error := e.Message;
      for X := 0 to 8 do
        for Y := 0 to 8 do
          fNumbers[X, Y].RollBack;
    end;
  end;
end;

procedure TfmxMain.CalcVertical;
var
  X, Y: TNumberPos;
  ValSet: TDigitSet;
begin
  for X := 0 to 8 do
  begin
    ValSet := [];
    for Y := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ESudokuRule.CreateFmt
            ('Vertical rule violated in row %d', [X + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for Y := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ESudokuRule.CreateFmt
            ('Vertical rule violated in row %d', [X + 1]);
      end;
  end;
end;

procedure TfmxMain.CalcHorizontal;
var
  X, Y: TNumberPos;
  ValSet: TDigitSet;
begin
  for Y := 0 to 8 do
  begin
    ValSet := [];
    for X := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ESudokuRule.CreateFmt
            ('Horizontal rule violated in column %d', [Y + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for X := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ESudokuRule.CreateFmt
            ('Horizontal rule violated in column %d', [Y + 1]);
      end;
  end;
end;

procedure TfmxMain.CalcBox;
var
  xB, yB: integer;
  X, Y: integer;
  ValSet: TDigitSet;
begin
  for xB := 0 to 2 do
    for yB := 0 to 2 do
    begin
      ValSet := [];
      for X := xB * 3 to xB * 3 + 2 do
        for Y := yB * 3 to yB * 3 + 2 do
          if fNumbers[X, Y].IsFinale then
            if fNumbers[X, Y].Finale in ValSet then
              raise ESudokuRule.CreateFmt('Rule violated in in box %d, %d',
                [xB + 1, yB + 1])
            else
              ValSet := ValSet + [fNumbers[X, Y].Finale];
      for X := xB * 3 to xB * 3 + 2 do
        for Y := yB * 3 to yB * 3 + 2 do
          if not fNumbers[X, Y].IsFinale then
          begin
            fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
            if fNumbers[X, Y].CalcSet = [] then
              raise ESudokuRule.CreateFmt(
                'Rule violated in in box %d, %d', [xB + 1, yB + 1]);
          end;
    end;
end;

procedure TfmxMain.ShowError(const Error: string);
begin
  rctMessage.Visible := true;
  txtMessage.Text := Error;
  laySolverTools.Visible := false;
  LogMsg(Error);
end;

procedure TfmxMain.ClearError;
begin
  rctMessage.Visible := false;
  laySolverTools.Visible := true;
end;
{$ENDREGION}

{$REGION 'Stack handling'}
procedure TfmxMain.Pop;
begin
  fNumbers := fStack.Pop;
  btnUndo.Enabled := fStack.Count > 0;
  if btnUndo.Enabled then
    btnUndo.Text := Format('Undo(%d)', [fStack.Count])
  else
    btnUndo.Text := 'Undo';
end;

procedure TfmxMain.Push;
begin
  fStack.Push(fNumbers);
  btnUndo.Text := Format('Undo(%d)', [fStack.Count]);
  btnUndo.Enabled := true;
end;

procedure TfmxMain.StackInit;
begin
  fStack.Clear;
  btnUndo.Text := 'Undo';
  btnUndo.Enabled := false;
end;

procedure TfmxMain.btnUndoClick(Sender: TObject);
begin
  Pop;
  PaintBox.Repaint;
end;
{$ENDREGION}

{$REGION 'Solver'}
function TfmxMain.GetNextSolverPos: boolean; {Find the number position with the least variability}
var
  X, Y: TNumberPos;
  LastFound: TDigitWithSentinel;
begin
  LastFound:= 0;
  for Y := 0 to 8 do
    for X := 0 to 8 do
      begin
        if not (fNumbers[X, Y].IsFinale) and ((LastFound = 0) or
          (LastFound > fNumbers[X, Y].GetCount)) then
        begin
          fSelectedBox.X := X;
          fSelectedBox.Y := Y;
          LastFound := fNumbers[X, Y].GetCount;
          if LastFound = 2 then
            exit(true);
        end;
      end;
  result := LastFound <> 0;
end;

procedure TfmxMain.btnSolveClick(Sender: TObject);

  function SelectNewSolverPos: boolean;
  var
    Error: string;
    ValSet: string;
  begin
    Push;
    ValSet := fNumbers[fSelectedBox.X, fSelectedBox.Y].GetAsStr;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].TakeSolverPos;
    LogMsg(Format('%d) [%d,%d] Select %d from {%s}: %d',
      [fSolverSteps, fSelectedBox.X + 1, fSelectedBox.Y + 1,
       fNumbers[fSelectedBox.X, fSelectedBox.Y].fFinale, ValSet, fStack.Count]));
    if not CalcAll(Error) then
    begin
      ShowError(Error);
      Pop;
      fNumbers[fSelectedBox.X, fSelectedBox.Y].SetRuleError;
      result := false;
    end else
      result := true;
  end;

  function FinishedCalced: boolean;     {True when Soduko is solved}
  var
    x, y: TNumberPos;
  begin
    for Y := 0 to 8 do
      for X := 0 to 8 do
        if not (fNumbers[X, Y].IsFinale) then
          exit(false);
    btnStopAutoSolver.Visible := false;
    btnAutoSolver.Visible := true;
    btnAutoSolver.Enabled := false;
    tmrAutoSolver.Enabled := false;
    btnSolve.Enabled := false;
    btnUndo.Enabled := false;
    LogMsg(Format('Sudoku solved in %d steps', [fSolverSteps]));
    result := true;
  end;

  procedure NoSolution;
  begin
    btnSolve.Enabled := false;
    btnStopAutoSolver.Visible := false;
    btnAutoSolver.Visible := true;
    ShowError('No solution found');
  end;

begin
  ClearError;
  if GetNextSolverPos then
  begin
    inc(fSolverSteps);
    btnSolve.Text := Format('Solve (%d)', [fSolverSteps]);
    if fNumbers[fSelectedBox.X, fSelectedBox.Y].SearchNextSolverPos then
    begin
      SelectNewSolverPos;
      btnSolve.Enabled := not FinishedCalced;
    end
    else if fStack.Count > 0 then
    begin
      Pop;
      LogMsg(Format('%d) Revert one step: %d', [fSolverSteps, fStack.Count]));
      GetNextSolverPos;
    end else
      NoSolution;
  end
  else if not FinishedCalced then
    NoSolution;
  PaintBox.Repaint;
end;

procedure TfmxMain.btnAutoSolverClick(Sender: TObject);
begin
  btnAutoSolver.Visible := false;
  tmrAutoSolver.Enabled := true;
  btnStopAutoSolver.Visible := true;
  StackInit;
end;

procedure TfmxMain.btnStopAutoSolverClick(Sender: TObject);   {Pause the timer}
begin
  tmrAutoSolver.Enabled := false;
  btnAutoSolver.Visible := true;
  btnStopAutoSolver.Visible := false;
end;

procedure TfmxMain.btnShowLogClick(Sender: TObject);
begin
  lstLog.Visible := not lstLog.Visible;
  if lstLog.Visible then
    TButton(Sender).Text := 'Hide Log'
  else
    TButton(Sender).Text := 'Show Log';
end;

procedure TfmxMain.tmrAutoSolverTimer(Sender: TObject);
begin
  btnSolveClick(Sender);
  tmrAutoSolver.Enabled := btnSolve.Enabled;
end;

procedure TfmxMain.trbSpeedChange(Sender: TObject);
begin
  tmrAutoSolver.Interval := trunc(trbSpeed.Value);
end;

procedure TfmxMain.LogMsg(const Msg: string);
begin
  lstLog.ItemIndex := lstLog.Items.Add(Msg);
end;
{$ENDREGION}

{$REGION 'TNumber'}
procedure TNumber.Init;
begin
  fSolverPos := 0;
  fSet := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  fFinale := 0;
  fSourceOrigin := soInitial;
end;

function TNumber.SearchNextSolverPos: boolean;
var
  c: TDigitWithSentinel;
begin
  c := 1;
  while not (fSolverPos + c in DigitSet) and (fSolverPos + c < 9) do
    inc(c);
  result := fSolverPos + c in DigitSet;
  if result then
    fSolverPos := fSolverPos + c;
end;

procedure TNumber.TakeSolverPos;
begin
  fSet := [fSolverPos];
  fFinale := fSolverPos;
  fSourceOrigin := soSolver;
end;

procedure TNumber.SetDefFinal(Digit: TDigit);
begin
  fSet := [Digit];
  fFinale := Digit;
  fSourceOrigin := soSet;
end;

procedure TNumber.SetDigitSet(Digits: TDigitSet);
var
  v: TDigit;
  Count: integer;
begin
  if Digits = [] then
    raise ESudokuRule.Create('Empty set violates the rules');
  fSet := Digits;
  Count := 0;
  for v in DigitSet do
  begin
    inc(Count);
    if Count = 1 then
      fFinale := v
    else
      fFinale := 0;
  end;
end;

procedure TNumber.SetCalcSet(Digits: TDigitSet);
begin
  if Digits = [] then
    raise ESudokuRule.Create('Empty set violates the rules');
  fCalcSet := Digits;
end;

procedure TNumber.StartCalc;
begin
  fCalcSet := [1, 2, 3, 4, 5, 6, 7, 8, 9];
end;

function TNumber.EndCalc: boolean;
begin
  result := false;
  if not IsFinale then
  begin
    SetDigitSet(CalcSet);
    result := IsFinale;
    if result then
      fSourceOrigin := soCalculated;
  end;
end;

function TNumber.GetAsStr: string;
var
  v: TDigit;
begin
  result := '';
  for v in fSet do
    result := result + IntToStr(v);
end;

function TNumber.GetCount: integer;
var
  v: TDigit;
begin
  result := 0;
  for v in fSet do
    inc(result);
end;

function TNumber.IsFinale: boolean;
begin
  result := fFinale > 0;
end;

procedure TNumber.SaveBack;
begin
  fBack := fFinale;
end;

procedure TNumber.RollBack;
begin
  fFinale := fBack;
end;

procedure TNumber.SetRuleError;
begin
  fSourceOrigin := soError;
end;
{$ENDREGION}

end.
