program Sudoku;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {fmxMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmxMain, fmxMain);
  Application.Run;
end.
