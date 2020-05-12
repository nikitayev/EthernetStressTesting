// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ServerGrandProject;

uses
  //FastMM4,
  Forms,
  ServerGrand in 'ServerGrand.pas' {ServerMainForm},
  ServerThreadUnit in 'ServerThreadUnit.pas',
  U_GlobalDataUnit in '..\U_GlobalDataUnit.pas',
  Unit_Indy_Functions in '..\Unit_Indy_Functions.pas',
  Unit_Indy_Classes in '..\Unit_Indy_Classes.pas',
  Web.Win.Sockets in '..\Web.Win.Sockets.pas',
  IOCPPool in 'IOCPPool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerMainForm, ServerMainForm);
  Application.Run;
end.
