unit ServerGrand;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ServerThreadUnit, Vcl.StdCtrls, Vcl.ExtCtrls, U_GlobalDataUnit;

type
  TServerMainForm = class(TForm)
    ImageDevices: TImage;
    lePort: TLabeledEdit;
    btStart: TButton;
    CheckingTimer: TTimer;
    lbClientsCount: TLabel;
    procedure btStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckingTimerTimer(Sender: TObject);
  private
    { Private declarations }
    Server: TListenerThread;
    procedure TCPClientNotify (var Message: TMessage); message WM_TCPClientNotify;
  public
    { Public declarations }
  end;

var
  ServerMainForm: TServerMainForm;

implementation

{$R *.dfm}

procedure TServerMainForm.btStartClick(Sender: TObject);
begin
  if not Assigned(Server) then  
  begin
    Server:=TListenerThread.Create(lePort.Text);
    btStart.Caption := 'STOP';
    CheckingTimer.Enabled := True;
  end else
  begin
    CheckingTimer.Enabled := False;
    try
      Server.Free;
    finally
      Server := nil;
    end;
    btStart.Caption := 'START';
  end;
end;

procedure TServerMainForm.CheckingTimerTimer(Sender: TObject);
begin
  lbClientsCount.Caption := Format('Зафиксировано коннектов: %d', [Server.ConnectionsCount]);
  CheckingTimer.Enabled := False;
  DrawIOTransactStates(ImageDevices.Picture.Bitmap.Canvas);
  CheckingTimer.Enabled := True;
end;

procedure TServerMainForm.FormShow(Sender: TObject);
begin
  ImageDevices.Picture.Bitmap.SetSize(100, 100);
  ImageDevices.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  ImageDevices.Picture.Bitmap.Canvas.Rectangle(0,0,99,99);
end;

procedure TServerMainForm.TCPClientNotify(var Message: TMessage);
var
  zData: PClentInfo;
begin
  zData := PClentInfo(Message.WParam);
  IOTransactDone(zData);
end;

end.
