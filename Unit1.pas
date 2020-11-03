unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Menus;

type
  ERuleException = class(Exception);

  TValue = 1 .. 9;
  TValueWithFinale = 0 .. 9;
  TValSet = set of TValue;

  TNumber = record
    ValSet, CalcSet: TValSet;
    Finale, Back: TValueWithFinale;
    Calculated: boolean;
    SolverPos: TValueWithFinale;
    function SearchNextSolverPos: boolean;
    procedure Init;
    procedure StartCalc;
    function EndCalc: boolean;
    procedure SaveBack;
    procedure RollBack;
    procedure SetDefFinal(Val: TValue);
    procedure SetVals(ValS: TValSet);
    function IsFinale: boolean;
    function GetCount: integer;
    function GetAsStr: string;
  end;

  TNumbers = array [0 .. 8, 0 .. 8] of TNumber;

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
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure CalloutPanel1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnInitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnUndoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
  private
    fSelectedBox: TPoint;
    fNumbers: TNumbers;
    fButton: array [1 .. 9] of TButton;
    fStack: TStack<TNumbers>;
    procedure CalcVertical;
    procedure CalcHorizontal;
    procedure CalcBox;
    procedure Push;
    procedure Pop;
    procedure StackInit;
    function GetFileName(Load: boolean): string;
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

procedure TfmxMain.Init;
var
  X, Y: integer;
begin
  fSelectedBox := Point(-1, -1);
  for X := 0 to 8 do
    for Y := 0 to 8 do
      fNumbers[X, Y].Init;
end;

{$REGION 'Popup Panel'}

procedure TfmxMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  p: TPointF;
  c: integer;
begin
  fSelectedBox.X := trunc(X / 50);
  fSelectedBox.Y := trunc(Y / 50);
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
      fNumbers[fSelectedBox.X, fSelectedBox.Y].SetVals(oldValS);
      PaintBox.Repaint;
    end
    else
    begin
      PaintBox.Repaint;
      Popup.IsOpen := false;
    end;
  end;
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
      fNumbers[fSelectedBox.X, fSelectedBox.Y].SetVals(oldValS);
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
    if not CalcAll(Error) then
    begin
      ShowMessage(Error);
      Init;
    end;
    PaintBox.Repaint;
  finally
    sl.Free;
  end;
  StackInit;
end;

procedure TfmxMain.btnSaveClick(Sender: TObject);
var
  sl: TStringList;
  X, Y: integer;
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
begin
  fSelectedBox.X := 3;
  fSelectedBox.Y := 7;
  paintBox.Repaint;
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

procedure TfmxMain.btnUndoClick(Sender: TObject);
begin
  Pop;
  PaintBox.Repaint;
end;
{$ENDREGION}

procedure TfmxMain.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  X, Y: integer;
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
  X, Y: integer;
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
    on e: ERuleException do
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
  X, Y: integer;
  ValSet: TValSet;
begin
  for X := 0 to 8 do
  begin
    ValSet := [];
    for Y := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ERuleException.CreateFmt
            ('Vertical rule violated in column %d', [X + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for Y := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ERuleException.CreateFmt
            ('Vertical rule violated in column% d', [X + 1]);
      end;
  end;
end;

procedure TfmxMain.CalcHorizontal;
var
  X, Y: integer;
  ValSet: TValSet;
begin
  for Y := 0 to 8 do
  begin
    ValSet := [];
    for X := 0 to 8 do
      if fNumbers[X, Y].IsFinale then
        if fNumbers[X, Y].Finale in ValSet then
          raise ERuleException.CreateFmt
            ('Horizontal rule violated in column% d', [Y + 1])
        else
          ValSet := ValSet + [fNumbers[X, Y].Finale];
    for X := 0 to 8 do
      if not fNumbers[X, Y].IsFinale then
      begin
        fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
        if fNumbers[X, Y].CalcSet = [] then
          raise ERuleException.CreateFmt
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
              raise ERuleException.CreateFmt('Rule violated in in box %d, %d',
                [xB + 1, yB + 1])
            else
              ValSet := ValSet + [fNumbers[X, Y].Finale];
      for X := xB * 3 to xB * 3 + 2 do
        for Y := yB * 3 to yB * 3 + 2 do
          if not fNumbers[X, Y].IsFinale then
          begin
            fNumbers[X, Y].CalcSet := fNumbers[X, Y].CalcSet - ValSet;
            if fNumbers[X, Y].CalcSet = [] then
              raise ERuleException.CreateFmt(
                'Rule violated in in box %d, %d', [xB + 1, yB + 1]);
          end;
    end;
end;

{ TNumber }

procedure TNumber.Init;
begin
  SolverPos:= 0;
  ValSet := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  Finale := 0;
  Calculated := false;
end;

function TNumber.SearchNextSolverPos: boolean;
var
  c: TValueWithFinale;
begin
  c:= 1;
  while (SolverPos+c in ValSet) and (SolverPos + c < 9) do
    inc(c);
  if SolverPos+c in ValSet then
    Solverpos := Solverpos + c
  else
    Result := false;
  Result:= true;
end;

procedure TNumber.SetDefFinal(Val: TValue);
begin
  ValSet := [Val];
  Finale := Val;
  Calculated := false;
end;

procedure TNumber.SetVals(ValS: TValSet);
var
  v: TValue;
  Count: integer;
begin
  ValSet := ValS;
  Count := 0;
  for v in ValSet do
  begin
    inc(Count);
    if Count = 1 then
      Finale := v
    else
      Finale := 0;
  end;
end;

procedure TNumber.StartCalc;
begin
  CalcSet := [1, 2, 3, 4, 5, 6, 7, 8, 9];
end;

function TNumber.EndCalc: boolean;
begin
  result := false;
  if not IsFinale then
  begin
    SetVals(CalcSet);
    result := IsFinale;
    if result then
      Calculated := true;
  end;
end;

function TNumber.GetAsStr: string;
var
  v: TValue;
begin
  result := '';
  for v in ValSet do
    result := result + IntToStr(v);
end;

function TNumber.GetCount: integer;
var
  v: TValue;
begin
  result := 0;
  for v in ValSet do
    inc(result);
end;

function TNumber.IsFinale: boolean;
begin
  result := Finale > 0;
end;

procedure TNumber.SaveBack;
begin
  Back := Finale;
end;

procedure TNumber.RollBack;
begin
  Finale := Back;
end;

end.
