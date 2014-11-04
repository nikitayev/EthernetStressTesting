unit ServerThreadUnit;

interface

uses
  {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
  Windows, Classes, Sysutils,  syncobjs, blcksock, synsock;

type

 TThreadManager = class;

 { TManagedThread }

 TManagedThread = class(TThread)
 public
   constructor Create(waiting : Boolean);
   function    isDone()     : Boolean;
   function    isErroneus() : Boolean;

 protected
   done_,
   erroneous_ : Boolean;
end;

  { TTCPThread }

TTCPThread = class(TManagedThread)
    private
     fSock: TTCPBlockSocket;
     fIP: string;
     FPort: integer;
     FNumber: integer;
     procedure SetSocket(aSock: TSocket);
    protected
     procedure Execute; override;
    public
     constructor Create();
     destructor Destroy; override;
     procedure ProcessingData(procSock: TTCPBlockSocket);
     Property Number: integer read Fnumber Write FNumber;
end;

 { TListenerThread }

  TListenerThread = class(TThread)
  private
    FPort: string;
    ListenerSocket: TTCPBlockSocket;
    FThreadManager: TThreadManager;
    FConnectionsCount: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const aPort: string);
    destructor Destroy; override;
    property ConnectionsCount: Integer read FConnectionsCount;
end;

 { TThreadManager }

 TThreadManager = class(TObject)
	private
		FAbort: Boolean;
		FThreadList: TList;
		FMaxThreadCount: Integer;
		procedure SetMaxThreadCount(Count: Integer);
	public
		constructor Create(MaxThreads: integer);
		destructor Destroy; override;
		function GetSuspendThread(aSock: TSocket): TTCPThread;
    procedure clearFinishedThreads;
		function GetActiveThreadCount: Integer;
		property MaxThreadCount: Integer read FMaxThreadCount write SetMaxThreadCount;
	end;

implementation
uses U_GlobalDataUnit, Forms;

{ TThreadManager }

procedure TThreadManager.SetMaxThreadCount(Count: Integer);
begin
  FMaxThreadCount := Count;
end;

constructor TThreadManager.Create(MaxThreads: integer);
begin
  inherited Create;
	FThreadList := TList.Create;
  FMaxThreadCount := MaxThreads;
end;

destructor TThreadManager.Destroy;
var
	i: Integer;
begin
  FThreadList.Pack;
  clearFinishedThreads;
	for i := FThreadList.Count - 1 downto 0 do 
  if Assigned(FThreadList[i]) then  
  begin
  	TTCPThread(FThreadList[i]).Free;
    FThreadList[i] := nil;
	end;
	FThreadList.Free;
	inherited;
end;

function TThreadManager.GetSuspendThread(aSock: TSocket): TTCPThread;
var
	i: Integer;
	TCPThread: TTCPThread;
begin
	Result := nil;
	if GetActiveThreadCount >= FMaxThreadCount then Exit;
	for i := 0 to FThreadList.Count - 1 do 
  begin
		if Assigned(FThreadList[i]) and TTCPThread(FThreadList[i]).Suspended then
    begin
			Result := TTCPThread(FThreadList[i]);
      Result.SetSocket(aSock);
      Result.Resume;
			Break;
		end;
	end;
	if (Result = nil) and (FMaxThreadCount > FThreadList.Count) then 
  begin
		TCPThread := TTCPThread.Create;
		TCPThread.FreeOnTerminate := False;
    TCPThread.SetSocket(aSock);
		TCPThread.Number := FThreadList.Count;
		FThreadList.Add(TCPThread);
		Result := TCPThread;
	end;
end;

procedure TThreadManager.clearFinishedThreads;
var
	i: Integer;
begin
	for i := 0 to FThreadList.Count - 1 do
  begin
    if (TTCPThread(FThreadList[i]) <> nil) and TTCPThread(FThreadList[i]).isDone() then
    begin
      TTCPThread(FThreadList[i]).WaitFor;
      TTCPThread(FThreadList[i]).Free;
      FThreadList[i] := nil;
    end;
  end;
end;

function TThreadManager.GetActiveThreadCount: Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to FThreadList.Count - 1 do 
  begin
    if (TTCPThread(FThreadList[i]) <> nil) then
      if not TTCPThread(FThreadList[i]).Suspended then
	      Inc(Result);
	end;
end;

{ TManagedThread }

constructor TManagedThread.Create(waiting : Boolean);
begin
 inherited Create(waiting);
 done_ := false;
 erroneous_ := false;
end;

function  TManagedThread.isDone()     : Boolean;
begin
 Result := done_;
end;


function  TManagedThread.isErroneus() : Boolean;
begin
 Result := erroneous_;
end;

{ TListenerThread }

procedure TListenerThread.Execute;
var
  ClientSock: TSocket;
begin
  with ListenerSocket do
  begin    
    RaiseExcept := true;
    CreateSocket;
    //if LastError = 0 then
    //  WriteLn('Socket successfully initialized')
    //else
    //  WriteLn('An error occurred while initializing the socket: '+GetErrorDescEx);
    Family := SF_IP4;
    setLinger(false, cLinger);
    bind('0.0.0.0', FPort);
    //if LastError = 0 then
    //  WriteLn('Bind on 5050')
    // else
    //  WriteLn('Bind error: '+GetErrorDescEx);
    listen;
    repeat
      if CanRead(cClientTimeout) then
      begin
        ClientSock := Accept;
        inc(FConnectionsCount);
        if LastError = 0 then
        begin
          //TTCPThread.Create()
          //ClientThread:=FThreadManager.GetSuspendThread(ClientSock);
          FThreadManager.GetSuspendThread(ClientSock);
          //    WriteLn('We have '+ IntToStr(FThreadManager.GetActiveThreadCount)+#32+'client threads!');
        end;
        //else
        //  WriteLn('TCP thread creation error: '+GetErrorDescEx);
      end;
      FThreadManager.clearFinishedThreads;
      sleep(1);
    until Terminated;
    FThreadManager.Free;
  end;
end;

constructor TListenerThread.Create(const aPort: string);
begin
  FreeOnTerminate := True;
  ListenerSocket := TTCPBlockSocket.Create;
  FThreadManager:=TThreadManager.Create(20000);
  {if ListenerSocket.LastError = 0
  then
    WriteLn('Listener has been created')
  else
    WriteLn('Listener creation error: '+ListenerSocket.GetErrorDescEx);
  }
  FPort := aPort;
  inherited Create(False);
end;

destructor TListenerThread.Destroy;
begin
  ListenerSocket.Free;
  {if ListenerSocket.LastError = 0 then
    WriteLn('Listener has been deleted')
  else
    WriteLn('Listener deleting error: '+ListenerSocket.GetErrorDescEx);
  }
  inherited;
end;

{ TTCPThread }

procedure TTCPThread.SetSocket(aSock: TSocket);
begin
  fSock.Socket := aSock;
  fSock.GetSins;
end;

procedure TTCPThread.Execute;
begin
  fIp:=fSock.GetRemoteSinIP;
  fPort:=fSock.GetRemoteSinPort;
  //WriteLn(format('Accepted connection from %s:%d',[fIp,fPort]));
  while (not Terminated) and (not isDone) do
  begin
    //if fSock.WaitingData > 0 then
    //begin
      //s:=fSock.RecvPacket(2000);
      //if fSock.LastError <> 0 then
      //  WriteLn(fSock.GetErrorDescEx);
      ProcessingData(fSock);
    //end;
    if (not Terminated) then    
      Suspend;
  end;
end;

constructor TTCPThread.Create();
begin
  FreeOnTerminate := False;
  fSock := TTCPBlockSocket.Create;
  fSock.SetTimeout(cSocketsTimeOut);
  fSock.SocksTimeout := cSocketsTimeOut;
  fSock.ConnectionTimeout := cSocketsTimeOut;
  inherited Create(false);
end;

destructor TTCPThread.Destroy;
begin
  //WriteLn(format('Disconnect from %s:%d',[fIp,fPort]));
  Terminate;
  Resume;
  WaitFor;
  fSock.Free;
  inherited;
end;

procedure TTCPThread.ProcessingData(procSock: TTCPBlockSocket);
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
      procSock.RecvStream(zMemStream, cClientTimeout);
      zMemStream.Position := 0;
      FDeviceID := zMemStream.ReadWord;
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csTryToConnect);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      // устанавливаем режим
      zMemStream.WriteByte(byte(cmDefaultMode));
      zMemStream.Position := 0;
      procSock.SendStream(zMemStream);
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csConnected);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
      // прочитаем ответ
      procSock.RecvStream(zMemStream, cClientTimeout);      
      zMemStream.Clear;

      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csDone);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
    finally
      zMemStream.Free;
    end; 
  except
    on E: ESynapseError do
    begin
      zClientResult := GetPClentInfo( FDeviceID, cmDefaultMode, 0, csConnectError);
      PostMessage(Application.MainFormHandle, WM_TCPClientNotify, Integer(zClientResult), 0);
    end;
  end;
end;

begin
end.
