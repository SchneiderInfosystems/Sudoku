unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Menus;

type
  ESudokuRule = class(Exception);

  TValue = 1 .. 9;
  TValueWithSentinel = 0 .. 9;
  TValSet = set of TValue;

  TNumber = record
  private
    fValSet, fCalcSet: TValSet;          {fValSet= Numbers that are possible for selection}
    fFinale, fBack: TValueWithSentinel;  {fFinale= Numbers that are final and cannot be changed}
    fCalculated: boolean;
    procedure SetValSet(Val: TValSet);
    procedure SetCalcSet(Val: TValSet);
  public
    procedure Init;                      {Initialization of TNumbers}
    procedure StartCalc;                 {Initialization of fCalcSet}
    function EndCalc: boolean;
    procedure SaveBack;                  {Save last Numbers}
    procedure RollBack;                  {Roll back the last Numbers}
    procedure SetDefFinal(Val: TValue);
    function IsFinale: boolean;          {Validation if the Number is already set}
    function GetCount: integer;          {Counting the amount of numbers that are available in the ValSet}
    function GetAsStr: string;
    property ValSet: TValSet read fValSet write SetValSet;
    property CalcSet: TValSet read fCalcSet write SetCalcSet;
    property Calculated: boolean read fCalculated;
    property Finale: TValueWithSentinel read fFinale;
  end;

  TNumberPos = 0..8;
  TNumbers = array [TNumberPos,TNumberPos] of TNumber;
  TSolverPos = array [TNumberPos, TnumberPos] of TValueWithSentinel;

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
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure CalloutPanel1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);           {Saves the present sudoku}
    procedure btnLoadClick(Sender: TObject);           {Loads a saved sudoku}
    procedure btnInitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnUndoClick(Sender: TObject);           {Reverse last step}
    procedure FormDestroy(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);          {Next solve step}
    procedure tmrAutoSolverTimer(Sender: TObject);     {Timer for AutoSolver step}
    procedure btnAutoSolverClick(Sender: TObject);     {Starts the AutoSolver process}
    procedure btnStopAutoSolverClick(Sender: TObject); {Stops the AutoSolver process}
  private
    fSelectedBox: TPoint;
    fNumbers: TNumbers;
    fSolverpos: TSolverPos;
    fButton: array [1 .. 9] of TButton;
    fStack: TStack<TNumbers>;
    procedure CalcVertical;   {Validates that the number isnt reoccuring in the same vertical row}
    procedure CalcHorizontal; {Validates that the number isnt reoccuring in the same horizontal row}
    procedure CalcBox;
    procedure Push;           {Pushes the present state on to the stack}
    procedure Pop;            {Removes the last added state from the stack}
    procedure StackInit;
    function GetFileName(Load: boolean): string;
    function NewSolverPos: boolean;      {Searching for the Box with the lowest possibilities}
    function SearchNextSolverPos(x, y: TNumberPos): boolean; {Selects a new Number that wasnt used in this box}
  public
    procedure Init;
    function CalcAll(var Error: string): boolean;
  end;

var
  fmxMain: TfmxMain;

implementation

uses
  System.IOUtils;

{$R *.fmx}

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
  Init;
  fStack := TStack<TNumbers>.Create;
end;

procedure TfmxMain.FormDestroy(Sender: TObject);
begin
  fStack.Free;
end;

procedure TfmxMain.Init;        {All boxes are set to 0 and buttons get reset}
var
  X, Y: TNumberPos;
begin
  btnSolve.Enabled:= true;
  btnAutoSolver.Visible := true;
  btnAutoSolver.Enabled := true;
  btnStopAutoSolver.Visible := false;
  fSelectedBox := Point(-1, -1);
  for X := 0 to 8 do
    for Y := 0 to 8 do
    begin
      fNumbers[X, Y].Init;
      fSolverPos[X, Y]:= 0;
    end;
end;

function TfmxMain.NewSolverPos: boolean;  {Searching for the Box with the lowest possibilities}
var
  X, Y: TNumberPos;
  LastFound: TValueWithSentinel;
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

{$REGION 'Popup Panel'}

procedure TfmxMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  p: TPointF;
  c: integer;
begin
  fSelectedBox.X := trunc(X / paintbox.Width*9);
  fSelectedBox.Y := trunc(Y / paintbox.Height*9);
  PaintBox.Repaint;
  p := ClientToScreen(pointF((fSelectedBox.X - 0.75) * 50 * ScaledLayout.Width /
    ScaledLayout.OriginalWidth, (fSelectedBox.Y + 1.2) * 50 *
    ScaledLayout.height / ScaledLayout.OriginalHeight));
  Popup.PlacementRectangle.Left := p.X;
  Popup.PlacementRectangle.Top := p.Y + pnlToolbar.height;
  for c := 1 to 9 do
    fButton[c].Visible := c in fNumbers[fSelectedBox.X, fSelectedBox.Y].ValSet;
  btnClear.SetFocus;
  Popup.IsOpen := true;
end;

procedure TfmxMain.btnClearClick(Sender: TObject);
var
  oldValS: TValSet;
  Error: string;
begin
  if (fSelectedBox.X >= 0) and (fSelectedBox.Y >= 0) then
  begin
    oldValS := fNumbers[fSelectedBox.X, fSelectedBox.Y].ValSet;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].Init;
    if not CalcAll(Error) then
    begin
      ShowMessage(Error);
      fNumbers[fSelectedBox.X, fSelectedBox.Y].ValSet := oldValS;
      PaintBox.Repaint;
    end
    else
    begin
      PaintBox.Repaint;
      Popup.IsOpen := false;
    end;
  end;
end;

procedure TfmxMain.btnAutoSolverClick(Sender: TObject);
begin
  btnAutoSolver.Visible := false;
  tmrAutoSolver.Enabled := true;
  btnStopAutoSolver.Visible := true;
end;

procedure TfmxMain.ButtonClick(Sender: TObject);
var
  Val: integer;
  oldValS: TValSet;
  Error: string;
begin
  Push;
  Val := TButton(Sender).Tag;
  if (fSelectedBox.X >= 0) and (fSelectedBox.Y >= 0) then
  begin
    oldValS := fNumbers[fSelectedBox.X, fSelectedBox.Y].ValSet;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].SetDefFinal(Val);
    if not CalcAll(Error) then
    begin
      ShowMessage(Error);
      fNumbers[fSelectedBox.X, fSelectedBox.Y].ValSet := oldValS;
      PaintBox.Repaint;
    end
    else
    begin
      PaintBox.Repaint;
      Popup.IsOpen := false;
    end;
  end;
end;

procedure TfmxMain.CalloutPanel1Click(Sender: TObject);
begin
  Popup.IsOpen := false;
  fSelectedBox := Point(-1, -1);
end;
{$ENDREGION}
{$REGION 'Save/Load File'}

procedure TfmxMain.btnInitClick(Sender: TObject);
begin
  Init;
  PaintBox.Repaint;
  StackInit;
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
  init;
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
    ShowMessage(Error);
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
        if not fNumbers[X, Y].Calculated then
          sl.AddPair(IntToStr(X) + '.' + IntToStr(Y),
            IntToStr(fNumbers[X, Y].Finale))
        else
          sl.AddPair(IntToStr(X) + '.' + IntToStr(Y), IntToStr(0));
    sl.SaveToFile(fn);
  finally
    sl.Free;
  end;
end;

procedure TfmxMain.btnSolveClick(Sender: TObject);

  function SelectNewSolverPos: boolean;
  var
    Error: string;
  begin
    Push;
    fNumbers[fSelectedBox.X, fSelectedBox.Y].SetDefFinal
      (fSolverPos[fSelectedBox.X, fSelectedBox.Y]);
    if not CalcAll(Error) then
    begin
      if not tmrAutoSolver.Enabled then
        ShowMessage(Error);
      Pop;
      result := false;
    end
    else
      result := true;
  end;

  function FinishedCalced: boolean;     {True when Soduko solved}
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
    btnSolve.Enabled:= false;
    btnUndo.Enabled:= false;
    result:= true;
  end;

begin
  if NewSolverPos then
  begin
    if SearchNextSolverPos(fSelectedBox.X , fSelectedBox.Y) then
    begin
      if SelectNewSolverPos then
        PaintBox.Repaint;
      btnSolve.Enabled := not FinishedCalced
    end
    else
      Pop;
  end
  else
    if not FinishedCalced then
      ShowMessage('No SolverPos found')
    else
      exit;
end;

procedure TfmxMain.btnStopAutoSolverClick(Sender: TObject);   {Pause the timer}
begin
  tmrAutoSolver.Enabled := false;
  btnAutoSolver.Visible := true;
  btnStopAutoSolver.Visible := false;
end;

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
  btnUndo.Text := 'Undo';
  btnUndo.Enabled := false;
end;

procedure TfmxMain.tmrAutoSolverTimer(Sender: TObject); {Timer}
begin
  btnSolveClick(Sender);
  tmrAutoSolver.Enabled := btnSolve.Enabled;
end;

procedure TfmxMain.btnUndoClick(Sender: TObject);
begin
  Pop;
  PaintBox.Repaint;
end;
{$ENDREGION}

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
  Canvas.Stroke.Thickness := 1;

  for X := 0 to 8 do
    for Y := 0 to 8 do
    begin
      if (fSelectedBox.X = X) and (fSelectedBox.Y = Y) then
        Canvas.Fill.Color := TAlphaColors.Red
      else if fNumbers[X, Y].IsFinale and not fNumbers[X, Y].Calculated then
        Canvas.Fill.Color := TAlphaColors.White
      else if fNumbers[X, Y].IsFinale and fNumbers[X, Y].Calculated then
        Canvas.Fill.Color := TAlphaColors.Lime
      else if not fNumbers[X, Y].IsFinale and fNumbers[X, Y].Calculated then
        Canvas.Fill.Color := TAlphaColors.Pink
      else
        Canvas.Fill.Color := TAlphaColors.Lightblue;

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
      end
      else
      begin
        Canvas.Fill.Color := TAlphaColors.White;
        Canvas.FillText(r, '(' + IntToStr(fNumbers[X, Y].GetCount) + ')', false,
          1, [], TTextAlign.Center, TTextAlign.Center);
      end;
    end;

  Canvas.Stroke.Thickness := 3;
  for X := 0 to 2 do
    for Y := 0 to 2 do
    begin
      r := rectF(X * 150, Y * 150, (X + 1) * 150 - 1, (Y + 1) * 150 - 1);
      Canvas.DrawRect(r, 0, 0, AllCorners, 1);
    end;

  Canvas.EndScene;
end;

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
  ValSet: TValSet;
begin
  for X := 0 to 8 do
  begin
    ValSet := [];
    for Y := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ESudokuRule.CreateFmt
            ('Vertical rule violated in column %d', [X + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for Y := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ESudokuRule.CreateFmt
            ('Vertical rule violated in column% d', [X + 1]);
      end;
  end;
end;

procedure TfmxMain.CalcHorizontal;
var
  X, Y: TNumberPos;
  ValSet: TValSet;
begin
  for Y := 0 to 8 do
  begin
    ValSet := [];
    for X := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ESudokuRule.CreateFmt
            ('Horizontal rule violated in column% d', [Y + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for X := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ESudokuRule.CreateFmt
            ('Horizontal rule violated in column% d', [Y + 1]);
      end;
  end;
end;

procedure TfmxMain.CalcBox;
var
  xB, yB: integer;
  X, Y: integer;
  ValSet: TValSet;
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

function TfmxMain.SearchNextSolverPos(x, y :TNumberPos): boolean;
var
  c: TValueWithSentinel;
begin
  c:= 1;
  while not (fSolverPos[x, y]+c in fNumbers[x, y].Valset) and (fSolverPos[x, y]+ c < 9) do
    inc(c);
  if fSolverPos[x, y]+c in fNumbers[x, y].Valset then
  begin
    fSolverPos[x, y] := fSolverPos[x, y] + c;
    result:= true;
  end
  else
    result := false;
end;

{ TNumber }

procedure TNumber.Init;
begin
  fValSet := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  fFinale := 0;
  fCalculated := false;
end;

procedure TNumber.SetDefFinal(Val: TValue);
begin
  fValSet := [Val];
  fFinale := Val;
  fCalculated := false;
end;

procedure TNumber.SetValSet(Val: TValSet);
var
  v: TValue;
  Count: integer;
begin
  if Val = [] then
    raise ESudokuRule.Create('Empty set violates the rules');
  fValSet := Val;
  Count := 0;
  for v in ValSet do
  begin
    inc(Count);
    if Count = 1 then
      fFinale := v
    else
      fFinale := 0;
  end;
end;

procedure TNumber.SetCalcSet(Val: TValSet);
begin
  if Val = [] then
    raise ESudokuRule.Create('Empty set violates the rules');
  fCalcSet := Val;
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
    SetValSet(CalcSet);
    result := IsFinale;
    if result then
      fCalculated := true;
  end;
end;

function TNumber.GetAsStr: string;
var
  v: TValue;
begin
  result := '';
  for v in fValSet do
    result := result + IntToStr(v);
end;

function TNumber.GetCount: integer;
var
  v: TValue;
begin
  result := 0;
  for v in fValSet do
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

end.
