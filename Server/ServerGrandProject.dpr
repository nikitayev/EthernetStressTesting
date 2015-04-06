program ServerGrandProject;

uses
  FastMM4,
  Forms,
  ServerGrand in 'ServerGrand.pas' {ServerMainForm},
  ServerThreadUnit in 'ServerThreadUnit.pas',
  U_GlobalDataUnit in '..\U_GlobalDataUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerMainForm, ServerMainForm);
  Application.Run;
end.
