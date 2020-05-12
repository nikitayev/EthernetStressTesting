// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program SPIGrandVirtualModules;

uses
  //FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  ClientThreadUnit in 'ClientThreadUnit.pas',
  U_GlobalDataUnit in '..\U_GlobalDataUnit.pas',
  U_StartClientsUnit in 'U_StartClientsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
