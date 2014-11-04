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
    Server.Free;
    btStart.Caption := 'START';
  end;
end;

procedure TServerMainForm.CheckingTimerTimer(Sender: TObject);
begin
  lbClientsCount.Caption := Format('Зафиксировано коннектов: %d', [Server.ConnectionsCount]);
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
  zX, zY: Integer;
begin
  zData := PClentInfo(Message.WParam);
  zY := zData.DeviceID div 100;
  zX := zData.DeviceID mod 100;
  //ImageDevices.Picture.Bitmap.Canvas.Lock;
  case zData.IOState of
    csWaiting: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(240, 240, 255);
    csReady: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(240, 240, 255);
    csTryToConnect: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := clYellow;
    csConnected: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(230, 255, 230);
    csInTransaction: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(0, 255, 0);
    csDone: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(0, 0, 0);
    csDataError: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(255, 0, 0);
    csConnectError: ImageDevices.Picture.Bitmap.Canvas.Pixels[zX, zY] := RGB(255, 170, 170);
  end;

  //ImageDevices.Picture.Bitmap.Canvas.Unlock;
  Dispose(zData);
end;

end.
