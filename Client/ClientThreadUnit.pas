unit ClientThreadUnit;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF}, U_GlobalDataUnit,
  SysUtils, synsock, blcksock;

type
  TClientThread = class(TThread)
  private
    FIP: string;
    FPort: string;
    FDateTimeOnline: TDatetime;
    FMode: TClientMode;
    FDeviceID: Word;
    procedure SetName;
    procedure StrongAlgorithm;
    procedure Echo3;
    procedure Echo1(tcpSock: TTCPBlockSocket);
  protected
    procedure Execute; override;
  public
    constructor Create(const aDateTimeOnline: TDatetime; aMode: TClientMode; const aIP, aPort: string; aDeviceID: Word);
  end;

implementation

uses Forms;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

  Synchronize(UpdateCaption);

  and UpdateCaption could look like,

  procedure TClientThread.UpdateCaption;
  begin
  Form1.Caption := 'Updated in a thread';
  end; }

{$IFDEF MSWINDOWS}

type
  TThreadNameInfo = record
    FType: LongWord; // must be 0x1000
    FName: PChar; // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord; // reserved for future use, must be zero
  end;
{$ENDIF}
  { TClientThread }

procedure TClientThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar('ClientN' + IntToStr(FDeviceID));
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException($406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo);
  except
  end;
{$ENDIF}
end;

constructor TClientThread.Create(const aDateTimeOnline: TDatetime; aMode: TClientMode; const aIP, aPort: string;
  aDeviceID: Word);
begin
  FIP := aIP;
  FPort := aPort;
  FDateTimeOnline := aDateTimeOnline;
  FMode := aMode;
  FDeviceID := aDeviceID;
  FreeOnTerminate := true;
  inherited Create;
  // Priority := tpLower;
end;

procedure TClientThread.Echo1(tcpSock: TTCPBlockSocket);
var
  zAnswer: AnsiString;
begin
  // отправили пакет
  tcpSock.SendString('test echo string'+CRLF);
  IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csInTransaction));
  // получили ответ от сервера
  zAnswer := tcpSock.RecvString(cClientTimeout);
  if (zAnswer = 'test echo string') then
  begin
    IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csInTransaction));
  end
  else
    IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csDataError));
end;

procedure TClientThread.StrongAlgorithm;
var
  zClientResult: PClentInfo;
  zMemStream: TStreamHelper;
  tcpSock: TTCPBlockSocket;
  zAutoIncValue: Word;
  i: Integer;
  zCanWrite: Boolean;
begin
  SetName;
  { Place thread code here }
  zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csWaiting);
  // PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
  IOTransactDone(zClientResult);
  // ждём указанного времени
  while (not Terminated) and (Now < FDateTimeOnline) do
    Sleep(10);
  zMemStream := TStreamHelper.Create;
  tcpSock := TTCPBlockSocket.Create;
  tcpSock.ConnectionTimeout := cClientConnectionTimeout;
  tcpSock.SetTimeout(cSetTimeout);
  tcpSock.SocksTimeout := cSocketsTimeOut;
  tcpSock.SetLinger(false, cLinger);
  tcpSock.RaiseExcept := false;
  try
    try
      // составим основной пакет
      zMemStream.WriteWord(FDeviceID);
      zAutoIncValue := 1;
      for i := zMemStream.Size div 2 to cDefaultPacketSize div 2 do
      begin
        zMemStream.WriteWord(zAutoIncValue);
        Inc(zAutoIncValue);
      end;
      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csTryToConnect);
      // SendMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      IOTransactDone(zClientResult);
      zCanWrite := false;
      // подключились
      for i := 1 to 5 do
      begin
        tcpSock.Connect(FIP, FPort);
        // zCanWrite := tcpSock.CanWrite(cSocketsTimeOut);
        if (Terminated or (tcpSock.LastError = 0) or (zCanWrite)) then
          break;
        tcpSock.CloseSocket;
        tcpSock.ResetLastError;
        Sleep(cClientConnectionTimeout);
      end;
      if ((tcpSock.LastError = 0) or (zCanWrite)) then
      begin
        tcpSock.RaiseExcept := true;
        zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnected);
        IOTransactDone(zClientResult);
        // отправили пакет
        zMemStream.Position := 0;
        tcpSock.SendStream(zMemStream);
        zMemStream.Clear;
        zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csInTransaction);
        IOTransactDone(zClientResult);
        // получили ответ от сервера
        tcpSock.RecvStream(zMemStream, cClientTimeout);
        zMemStream.Position := 0;
        // расшифровали режим
        FMode := TClientMode(zMemStream.ReadByte);
        // пошлём ответ
        zMemStream.Clear;
        zMemStream.WriteWord(FDeviceID);
        zMemStream.WriteWord(255);
        zMemStream.Position := 0;
        case FMode of
          cmDefaultMode:
            begin
              tcpSock.SendStream(zMemStream);
            end;
          cmWithUpdateTransaction:
            begin
              tcpSock.SendStream(zMemStream);
            end;
        end;
        zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csDone);
        IOTransactDone(zClientResult);
      end
      else
      begin
        zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnectError);
        IOTransactDone(zClientResult);
      end;
    finally
      tcpSock.Free;
      zMemStream.Free;
    end;
  except
    on E: ESynapseError do
    begin
      zClientResult := GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnectError);
      IOTransactDone(zClientResult);
    end;
  end;
end;

procedure TClientThread.Echo3;
var
  tcpSock: TTCPBlockSocket;
  //zAutoIncValue: Word;
  //i: Integer;
  zCanWrite: Boolean;
begin
  SetName;
  { Place thread code here }
  // PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
  IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csWaiting));
  // ждём указанного времени
  while (not Terminated) and (Now < FDateTimeOnline) do
    Sleep(10);
  tcpSock := TTCPBlockSocket.Create;
  tcpSock.ConnectionTimeout := cClientConnectionTimeout;
  tcpSock.SetTimeout(cSetTimeout);
  tcpSock.SocksTimeout := cSocketsTimeOut;
  tcpSock.SetLinger(false, cLinger);
  tcpSock.RaiseExcept := false;
  try
    try
      // SendMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csTryToConnect));
      zCanWrite := false;
      {
      // подключились
      for i := 0 to 30 do
      begin
        tcpSock.Connect(FIP, FPort);
        // zCanWrite := tcpSock.CanWrite(cSocketsTimeOut);
        if (Terminated or (tcpSock.LastError = 0) or (zCanWrite)) then
          break;
        tcpSock.CloseSocket;
        tcpSock.ResetLastError;
        Sleep(cClientConnectionTimeout);
      end;
      }
      tcpSock.Connect(FIP, FPort);
      if ((tcpSock.LastError = 0) or (zCanWrite)) then
      begin
        tcpSock.RaiseExcept := true;
        IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnected));

        Echo1(tcpSock);
        Echo1(tcpSock);
        Echo1(tcpSock);

        IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csDone));
      end
      else
      begin
        IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnectError));
      end;
    finally
      tcpSock.Free;
    end;
  except
    on E: ESynapseError do
    begin
      IOTransactDone(GetPClentInfo(FDeviceID, cmDefaultMode, 0, csConnectError));
    end;
  end;
end;

procedure TClientThread.Execute;
begin
  StrongAlgorithm;
  //Echo3;
end;

end.
