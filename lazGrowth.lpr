program lazGrowth;

{$mode objfpc}{$H+}
{$DEFINE debug}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, uPointStore, formAbout, formhelp, formLicence
  { you can add units after this }
  , SysUtils;

{$R *.res}

begin
  //if FileExists('heap.trc') then
  //  DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmGrowth, frmGrowth);
  Application.Run;
end.

