program ServerGrandProject;

uses
  //ScaleMM3,
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
