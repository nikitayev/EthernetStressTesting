unit ServerGrand;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ServerThreadUnit, Vcl.StdCtrls, Vcl.ExtCtrls, U_GlobalDataUnit,
  Winapi.WinSock, Web.Win.Sockets;

type
  TServerMainForm = class(TForm)
    ImageDevices: TImage;
    lePort: TLabeledEdit;
    btStartSynapse: TButton;
    CheckingTimer: TTimer;
    lbClientsCount: TLabel;
    btStartWinSock: TButton;
    procedure btStartSynapseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckingTimerTimer(Sender: TObject);
    procedure btStartWinSockClick(Sender: TObject);
  private
    { Private declarations }
    SynapseServer: TListenerThread;
    WinSockServer: TTcpServer;
    procedure TCPClientNotify(var Message: TMessage);
      message WM_TCPClientNotify;
    procedure TcpServerWinSockAccept(Sender: TObject; ClientSocket: TCustomIpClient);
  public
    { Public declarations }
  end;

var
  ServerMainForm: TServerMainForm;

implementation

{$R *.dfm}

procedure TServerMainForm.btStartSynapseClick(Sender: TObject);
begin
  if not Assigned(SynapseServer) then
  begin
    SynapseServer := TListenerThread.Create(lePort.Text);
    btStartSynapse.Caption := 'STOP';
    CheckingTimer.Enabled := True;
  end
  else
  begin
    CheckingTimer.Enabled := False;
    try
      SynapseServer.Free;
    finally
      SynapseServer := nil;
    end;
    btStartSynapse.Caption := 'START';
  end;
end;

procedure TServerMainForm.btStartWinSockClick(Sender: TObject);
begin
  // WinSockServer: TTcpServer
  if not Assigned(WinSockServer) then
  begin
    WinSockServer := TTcpServer.Create(nil);
    WinSockServer.LocalHost := '0.0.0.0';
    WinSockServer.LocalPort := lePort.Text;
    WinSockServer.OnAccept := TcpServerWinSockAccept;
    WinSockServer.Active := True;
    btStartWinSock.Caption := 'STOP';
    CheckingTimer.Enabled := True;
  end
  else
  begin
    CheckingTimer.Enabled := False;
    try
      WinSockServer.Free;
    finally
      WinSockServer := nil;
    end;
    btStartWinSock.Caption := 'START';
  end;
end;

procedure TServerMainForm.CheckingTimerTimer(Sender: TObject);
begin
 if Assigned(SynapseServer) then
 begin
  lbClientsCount.Caption := Format('Зафиксировано коннектов: %d',
    [SynapseServer.ConnectionsCount]);
 end;
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
  ImageDevices.Picture.Bitmap.Canvas.Rectangle(0, 0, 99, 99);
end;

procedure TServerMainForm.TCPClientNotify(var Message: TMessage);
var
  zData: PClentInfo;
begin
  zData := PClentInfo(Message.WParam);
  IOTransactDone(zData);
end;

procedure TServerMainForm.TcpServerWinSockAccept(Sender: TObject;
  ClientSocket: TCustomIpClient);

  function SendStream(aStream: TStream; aSocket: TCustomIpClient):boolean;
  var
    zBuffLen: Integer;
  begin
    zBuffLen := aStream.Size;
    aSocket.SendBuf(zBuffLen, SizeOf(zBuffLen));
    aSocket.SendStream(aStream);
  end;

  function RecvStream(aStream: TMemoryStream; aSocket: TCustomIpClient):boolean;
  var
    zBuffLen: Integer;
  begin
    Result := false;
    if aSocket.ReceiveBuf(zBuffLen, SizeOf(zBuffLen)) = SOCKET_ERROR then exit;
    aStream.Size := zBuffLen;
    aStream.Position := 0;
    if aSocket.ReceiveBuf(aStream.Memory^,zBuffLen) = SOCKET_ERROR then exit;
    Result := true;
  end;
  
var
  FDeviceID: Word;
  zMemStream: TStreamHelper;
  zClientResult: PClentInfo;
  //zMode: TClientMode;
begin
  //if data <> '' then
  // WriteLn(data+#32+'we get it from '+IntToStr(number)+' thread');
  FDeviceID := 0;
  zMemStream := TStreamHelper.Create;
  try
    try
      //procSock.RecvStream(zMemStream, cClientTimeout);
      if not RecvStream(zMemStream, ClientSocket) then exit;
      zMemStream.Position := 0;
      FDeviceID := zMemStream.ReadWord;
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csTryToConnect);
      //PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      //SendMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      IOTransactDone(zClientResult);
      // устанавливаем режим
      zMemStream.WriteByte(byte(cmDefaultMode));
      zMemStream.Position := 0;
      //procSock.SendStream(zMemStream);
      if not SendStream(zMemStream, ClientSocket) then exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csConnected);
      //SendMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      IOTransactDone(zClientResult);
      // прочитаем ответ
      //procSock.RecvStream(zMemStream, cClientTimeout);
      if not RecvStream(zMemStream, ClientSocket) then exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csDone);
      //SendMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      IOTransactDone(zClientResult);
    finally
      FreeAndNil(zMemStream);
    end; 
  except
    on E: Exception do
    begin
      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csConnectError);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
    end;
  end;
end;

end.
