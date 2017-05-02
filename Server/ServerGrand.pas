unit ServerGrand;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ServerThreadUnit, Vcl.StdCtrls, Vcl.ExtCtrls, U_GlobalDataUnit,
  Winapi.WinSock, Web.Win.Sockets,
  IdContext, IdSync, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer, IdGlobal, IdIOHandler, Unit_Indy_Functions;

type
  TServerMainForm = class(TForm)
    ImageDevices: TImage;
    lePort: TLabeledEdit;
    btStartSynapse: TButton;
    CheckingTimer: TTimer;
    lbClientsCount: TLabel;
    btStartWinSock: TButton;
    btStartIndy: TButton;
    procedure btStartSynapseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckingTimerTimer(Sender: TObject);
    procedure btStartWinSockClick(Sender: TObject);
    procedure btStartIndyClick(Sender: TObject);
  private
    { Private declarations }
    SynapseServer: TListenerThread;
    WinSockServer: TTcpServer;
    IdTCPServer: TIdTCPServer;
    procedure TCPClientNotify(var Message: TMessage);
      message WM_TCPClientNotify;
    procedure TcpServerWinSockAccept(Sender: TObject;
      ClientSocket: TCustomIpClient);
    procedure IdTCPServerExecute(AContext: TIdContext);
  public
    { Public declarations }
  end;

var
  ServerMainForm: TServerMainForm;

implementation

{$R *.dfm}


procedure TServerMainForm.btStartIndyClick(Sender: TObject);
begin
  if not Assigned(IdTCPServer) then
  begin
    IdTCPServer := TIdTCPServer.Create(nil);
    IdTCPServer.DefaultPort := StrToIntDef(lePort.Text, 5706);
    IdTCPServer.Bindings.Add.IP := '127.0.0.1';
    IdTCPServer.Bindings.Add.Port := IdTCPServer.DefaultPort;
    IdTCPServer.OnExecute := IdTCPServerExecute;
    IdTCPServer.Active := True;
    btStartIndy.Caption := 'STOP';
    CheckingTimer.Enabled := True;
  end
  else
  begin
    CheckingTimer.Enabled := False;
    try
      FreeAndNil(IdTCPServer);
    finally
      IdTCPServer := nil;
    end;
    btStartIndy.Caption := 'START Indy';
  end;
end;

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
      FreeAndNil(SynapseServer);
    finally
      SynapseServer := nil;
    end;
    btStartSynapse.Caption := 'START Synapse';
  end;
end;


procedure TServerMainForm.btStartWinSockClick(Sender: TObject);
begin
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
      FreeAndNil(WinSockServer);
    finally
      WinSockServer := nil;
    end;
    btStartWinSock.Caption := 'START WinSock';
  end;
end;

procedure TServerMainForm.CheckingTimerTimer(Sender: TObject);
begin
  if Assigned(SynapseServer) then
  begin
    lbClientsCount.Caption := Format('«афиксировано коннектов: %d',
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


procedure TServerMainForm.IdTCPServerExecute(AContext: TIdContext);
var
  FDeviceID: Word;
  zMemStream: TStreamHelper;
  zClientResult: PClentInfo;
begin
  FDeviceID := 0;
  zMemStream := TStreamHelper.Create;
  try
    try
      if not ReceiveStream2(AContext, zMemStream) then
        exit;
      zMemStream.Position := 0;
      FDeviceID := zMemStream.ReadWord;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0,
        csTryToConnect);
      IOTransactDone(zClientResult);
      // устанавливаем режим
      zMemStream.WriteByte(byte(cmDefaultMode));
      zMemStream.Position := 0;
      if not SendStream(AContext, zMemStream) then
        exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnected);
      IOTransactDone(zClientResult);
      // прочитаем ответ
      if not ReceiveStream2(AContext, zMemStream) then
        exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csDone);
      IOTransactDone(zClientResult);
    finally
      FreeAndNil(zMemStream);
      //«акрываем соединение с пользователем
      if Assigned(AContext.Connection) then
        AContext.Connection.Disconnect;
    end;
  except
    on E: Exception do
    begin
      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0,
        csConnectError);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify,
        Integer(zClientResult), 0);
    end;
  end;
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

  function SendStream(aStream: TStream; aSocket: TCustomIpClient): boolean;
  var
    zBuffLen: Integer;
  begin
    zBuffLen := aStream.Size;
    result := aSocket.SendBuf(zBuffLen, SizeOf(zBuffLen)) <> SOCKET_ERROR;
    if result then
      result := aSocket.SendStream(aStream) <> SOCKET_ERROR;
  end;

  function RecvStream(aStream: TMemoryStream; aSocket: TCustomIpClient)
    : boolean;
  var
    zBuffLen: Integer;
  begin
    Result := False;
    if aSocket.ReceiveBuf(zBuffLen, SizeOf(zBuffLen)) = SOCKET_ERROR then
      exit;
    aStream.Size := zBuffLen;
    aStream.Position := 0;
    if aSocket.ReceiveBuf(aStream.Memory^, zBuffLen) = SOCKET_ERROR then
      exit;
    Result := True;
  end;

var
  FDeviceID: Word;
  zMemStream: TStreamHelper;
  zClientResult: PClentInfo;
begin
  FDeviceID := 0;
  zMemStream := TStreamHelper.Create;
  try
    try
      if not RecvStream(zMemStream, ClientSocket) then
        exit;
      zMemStream.Position := 0;
      FDeviceID := zMemStream.ReadWord;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0,
        csTryToConnect);
      IOTransactDone(zClientResult);
      // устанавливаем режим
      zMemStream.WriteByte(byte(cmDefaultMode));
      zMemStream.Position := 0;
      if not SendStream(zMemStream, ClientSocket) then
        exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnected);
      IOTransactDone(zClientResult);
      // прочитаем ответ
      if not RecvStream(zMemStream, ClientSocket) then
        exit;
      zMemStream.Clear;

      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csDone);
      IOTransactDone(zClientResult);
    finally
      FreeAndNil(zMemStream);
    end;
  except
    on E: Exception do
    begin
      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0,
        csConnectError);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify,
        Integer(zClientResult), 0);
    end;
  end;
end;

end.
