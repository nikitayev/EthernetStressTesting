{*

Synapse IOCPPool implementation.

The TTCPDaemon class works like a web-server using a pool of TWorkerThreads waiting
for an IOCP handle. It reads a .config file to configurate the class.

The free XML parser component (no source code) can be found in
http://www.icom-dv.de

These class was inspired on the William Kennedy article at Continuum Technology
"IOCP Thread Pooling in C#" - http://www.theukwebdesigncompany.com/articles/article.php?id=82

Feel free to use it.
Any comments or improvements please tell me ! :)

Synapse rocks !

--
Andre Azevedo
midsilence@yahoo.com.br

*}

unit IOCPPool;

interface

uses
  ComObj, ActiveX, AdoDb, Windows, SyncObjs, Classes,
  BlckSock, WinSock, SysUtils, XMLIntf, XMLDoc;

type

  TUpdateEvent = procedure(nActThreads, nCurWorks, nCurThreads: integer) of object;
  TProcessEvent = procedure(ASock: TTCPBlockSocket) of object;
  TErrorEvent = procedure(AE: Exception) of object;

  TTcpDaemon = class;

  TWorkerThread = class(TThread)
  private
    FDBConnection: TAdoConnection;
    FConnectionString: string;
    FTcpDaemon: TTCPDaemon;
    FWorkerSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(ATcpDaemon: TTCPDaemon; AConnectionString: string);
    destructor Destroy; override;
  end;

  TAcceptThread = class(TThread)
  private
    FTcpDaemon: TTCPDaemon;
    FPort: integer;
    FSock: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(ATcpDaemon: TTcpDaemon; APort: integer);
    destructor Destroy; override;
  end;

  TTCPDaemon = class(TObject)
  private
    FAcceptthread: TAcceptThread;
    FPort: integer;
    FIOCPHandle: THandle;
    FMaxThreadsInPool: integer;
    FMinThreadsInPool: integer;
    FCurThreadsInPool: integer;
    FActThreadsInPool: integer;
    FIdleTimeOut: integer;
    FCurWorksInPool: integer;
    FCRTSection: TCriticalSection;
    FTerminateEvent: TSimpleEvent;
    FServiceTerminateEvent: TSimpleEvent;
    FWorkerThreads: TList;
    FOnUpdateEvent: TUpdateEvent;
    FOnProcessEvent: TProcessEvent;
    FOnErrorEvent: TErrorEvent;
    FErrorException: Exception;
    FConnectionString: string;
    FConnectionTimeOut: integer;
    FCommandTimeOut: integer;
    FActive: boolean;
    procedure IncActThreadsInPool;
    procedure DecActThreadsInPool;
    procedure IncCurThreadsInPool;
    procedure DecCurThreadsInPool;
    procedure IncCurWorksInPool;
    procedure DecCurWorksInPool;
    procedure SyncAtualiza;
    procedure SyncErros;
    procedure Config;
  public
    constructor Create;
    destructor Destroy; override;
    property Active: boolean read FActive write FActive;
    property Port: integer read FPort write FPort;
    property MaxThreadsInPool: integer read FMaxThreadsInPool write FMaxThreadsInPool;
    property MinThreadsInPool: integer read FMinThreadsInPool write FMinThreadsInPool;
    property IdleTimeOut: integer read FIdleTimeOut write FIdleTimeOut;
    property ConnectionString: string read FConnectionString write FConnectionString;
    property ConnectionTimeOut: integer read FConnectionTimeOut write FConnectionTimeOut;
    property CommandTimeOut: integer read FCommandTimeOut write FCommandTimeOut;
    property OnUpdate: TUpdateEvent read FOnUpdateEvent write FOnUpdateEvent;
    property OnProcess: TProcessEvent read FOnProcessEvent write FOnProcessEvent;
    property OnError: TErrorEvent read FOnErrorEvent write FOnErrorEvent;
    procedure Start;
    procedure Stop;
    procedure WaitForTerminate;
  end;

implementation

{ TTCPDaemon }
const
  KILL_THREAD = $7fffffff;

procedure TTCPDaemon.DecActThreadsInPool;
begin
  InterlockedDecrement(FActThreadsInPool);
end;

procedure TTCPDaemon.DecCurWorksInPool;
begin
  InterlockedDecrement(FCurWorksInPool);
end;

procedure TTCPDaemon.DecCurThreadsInPool;
begin
  InterlockedDecrement(FCurThreadsInPool);
end;

destructor TTCPDaemon.Destroy;
begin

  FreeAndNil(FWorkerThreads);
  FreeAndNil(FCRTSection);
  FreeAndNil(FTerminateEvent);
  FreeAndNil(FServiceTerminateEvent);

  inherited;

end;

procedure TTCPDaemon.IncActThreadsInPool;
begin
  InterlockedIncrement(FActThreadsInPool);
end;

procedure TTCPDaemon.IncCurWorksInPool;
begin
  InterlockedIncrement(FCurWorksInPool);
end;

procedure TTCPDaemon.IncCurThreadsInPool;
begin
  InterlockedIncrement(FCurThreadsInPool);
end;

procedure TTCPDaemon.SyncAtualiza;
begin

  if Assigned(OnUpdate) then
    OnUpdate(FActThreadsInPool, FCurWorksInPool, FCurThreadsInPool);

end;

procedure TTCPDaemon.Start;
var
  nContador: integer;
  Thread: TWorkerThread;
  SystemInfo: TSystemInfo;
begin

  try

    //Config;
    GetSystemInfo(SystemInfo);
    FIOCPHandle := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, SystemInfo.dwNumberOfProcessors * 2);

    if (FIOCPHandle = 0) then
      raise Exception.Create('Erro IOCP Creation');

    for nContador := 0 to FMinThreadsInPool - 1 do
    begin
      Thread := TWorkerThread.Create(Self, FConnectionString);
      FWorkerThreads.Add(Thread);
      IncCurThreadsInPool;
    end;

    FAcceptthread := TAcceptThread.Create(Self, FPort);

    Active := true;

  except

    Active := false;
    raise;

  end;

end;

procedure TTCPDaemon.Stop;
begin

  try

    FAcceptThread.FSock.CloseSocket;
    FAcceptthread.Terminate;
    FAcceptthread.WaitFor;

    CloseHandle(FIOCPHandle);

  finally

    Active := false;
    FServiceTerminateEvent.SetEvent;

  end;

end;

constructor TTCPDaemon.Create;
begin

  try

    FWorkerThreads := TList.Create;
    FCRTSection := TCriticalSection.Create;
    FTerminateEvent := TSimpleEvent.Create;
    FServiceTerminateEvent := TSimpleEvent.Create;

    FConnectionString := '';
    FConnectionTimeout := 0;
    FCommandTimeOut := 0;
    FIdleTimeOut := 0;
    FMinThreadsInPool := 0;
    FMaxThreadsInPool := 0;

    inherited;

  except

    raise

  end;

end;

procedure TTCPDaemon.SyncErros;
begin

  if Assigned(FOnErrorEvent) then
    FOnErrorEvent(FErrorException);

end;

procedure TTCPDaemon.WaitForTerminate;
begin

  FServiceTerminateEvent.WaitFor(INFINITE);

end;

procedure TTCPDaemon.Config;
var
  elRoot, elElement: IXMLNode;
//  elNode: IXMLNode;
  doc: IXMLDocument;
//  IcXMLParser: TIcXMLParser;
//  stmXML: TMemoryStream;
begin

  doc := TXMLDocument.Create(nil);

//doc.setEncoding('UTF-8');

  doc.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'main.config');


//----- Root !
  elRoot := doc.GetDocumentElement;

  if elRoot.NodeName = 'Configuration' then
  begin

  //----- Configuration !
    if elRoot.HasChildNodes then
    begin

      elElement := elRoot.ChildNodes[0];

      while Assigned(elElement) do
      begin

      //----- DbConnection !
        if elElement.NodeName = 'DbConnection' then
        begin

          if elElement.HasAttribute('ConnectionString') then
            FConnectionString := elElement.Attributes['ConnectionString'];

          if elElement.HasAttribute('ConnectionTimeOut') then
            FConnectionTimeOut := StrToInt(elElement.Attributes['ConnectionTimeOut']);

          if elElement.HasAttribute('CommandTimeOut') then
            FCommandTimeOut := StrToInt(elElement.Attributes['CommandTimeOut']);

        end;

      //----- SocketConnection !
        if elElement.NodeName = 'SocketConnection' then
        begin

          if elElement.HasAttribute('Port') then
            FPort := StrToInt(elElement.Attributes['Port']);

          if elElement.HasAttribute('MinThreads') then
            FMinThreadsInPool := StrToInt(elElement.Attributes['MinThreads']);

          if elElement.HasAttribute('MaxThreads') then
            FMaxThreadsInPool := StrToInt(elElement.Attributes['MaxThreads']);

          if elElement.HasAttribute('IdleTimeOut') then
            FIdleTimeOut := StrToInt(elElement.Attributes['IdleTimeOut']) * 1000;

        end;

        elElement := elElement.NextSibling;
      end;

    end;

  end;

  doc := nil;

end;

{ TAcceptThread }

constructor TAcceptThread.Create;
begin
  FTcpDaemon := ATcpDaemon;
  FPort := APort;
  FSock := TTCPBlockSocket.Create;
  //FreeOnTerminate := true;

  inherited Create(false);
end;

destructor TAcceptThread.Destroy;
begin
  inherited;
  FreeAndNil(FSock);
end;

procedure TAcceptThread.Execute;
var
  ClientSocket: TSocket;
  nContador: integer;
begin

  with FSock do
  begin

    CreateSocket;

    setLinger(true, 10000);

    bind('0.0.0.0', IntToStr(FPort));
    listen;

    FTcpDaemon.SyncAtualiza;

    repeat

      if CanRead(5000) then
      begin

        ClientSocket := Accept;

        if ClientSocket <> -1 then
        begin
          FTcpDaemon.IncCurWorksInPool;
          PostQueuedCompletionStatus(FTcpDaemon.FIOCPHandle, 0, cardinal(ClientSocket), nil);
        end;

      end

    until Terminated;

    CloseSocket;

    with FTcpDaemon do
    begin

      FCRTSection.Enter;

      try

        for nContador := FWorkerThreads.Count - 1 downto 0 do
          PostQueuedCompletionStatus(FIOCPHandle, 0, cardinal(KILL_THREAD), nil);

      finally
        FCRTSection.Leave;
      end;

      WaitForSingleObject(FTerminateEvent.Handle, INFINITE);

    end;

  end;

end;

{ TWorkerThread }

constructor TWorkerThread.Create(ATcpDaemon: TTCPDaemon; AConnectionString: string);
begin

  FTcpDaemon := ATcpDaemon;
  FConnectionString := AConnectionString;

  FreeOnTerminate := true;
  inherited Create(false);

end;

destructor TWorkerThread.Destroy;
begin
  inherited;
end;

procedure TWorkerThread.Execute;
var
  Ov: POverlapped;
  Transfered: dword;
  ClientSocket: TSocket;
  Thread: TThread;
  nContador: integer;
  zClientSocket: ULONG_PTR;
begin
  CoInitialize(nil);
  with FTcpDaemon do
  begin

    if Length(FConnectionString) > 0 then
    begin

      FDBConnection := TAdoConnection.Create(nil);

      FDBConnection.LoginPrompt := false;
      FDBConnection.ConnectionTimeout := FConnectionTimeOut;
      FDBConnection.CommandTimeout := FCommandTimeOut;
      FDBConnection.ConnectionString := FConnectionString;

      FDBConnection.Open;
      FDBConnection.Execute('set quoted_identifier off');

    end;

    while not Terminated do
    begin

      //----- Espera pelo evento !
      zClientSocket := ULONG_PTR(ClientSocket);
      if not GetQueuedCompletionStatus(FIOCPHandle, Transfered, zClientSocket, Ov, FIdleTimeOut + 500) then
      begin

        ClientSocket := TSocket(zClientSocket);
        //----- Verifica se o tempo expirou !
        if GetLastError = WAIT_TIMEOUT then
        begin
          FCRTSection.Enter;
          try
            if (FCurThreadsInPool > FMinThreadsInPool) then
              Terminate;
          finally
            FCRTSection.Leave;
          end;
        end
        else
        begin
          Terminate;
          Break;
        end;

      end
      else
      begin
        ClientSocket := TSocket(zClientSocket);

        //----- Verifica se o servidor esta sendo destruido !
        if (ClientSocket = KILL_THREAD) then
        begin
          Terminate;
          Break;
        end;

        //----- Atualiza Threads!
        IncActThreadsInPool;
        DecCurWorksInPool;
        Synchronize(SyncAtualiza);

        FCRTSection.Enter;

        try

          if (FTcpDaemon.FCurThreadsInPool < FTcpDaemon.FMaxThreadsInPool) then
          begin

            if (FTcpDaemon.FActThreadsInPool = FTcpDaemon.FCurThreadsInPool) then
            begin
              Thread := TWorkerThread.Create(FTcpDaemon, FTcpDaemon.ConnectionString);
              FWorkerThreads.Add(Thread);
              IncCurThreadsInPool;
              Synchronize(SyncAtualiza);
            end;

          end;

        finally
          FCRTSection.Leave;
        end;

        FWorkerSocket := TTCPBlockSocket.Create;

        with FWorkerSocket do
        begin

          //----- Processa Mensagens !
          try

            //--- Maldito try/except/finally !
            try
              Socket := ClientSocket;
              GetSins;
              setLinger(true, 10000);

              if Assigned(FOnProcessEvent) then
                FOnProcessEvent(FWorkerSocket);
            except
              on e: Exception do
              begin
                FCRTSection.Enter;
                try
                  FErrorException := e;
                finally
                  FCRTSection.Leave;
                end;
                Synchronize(SyncErros);
              end;
            end;

          finally
            DecActThreadsInPool;
            CloseSocket;
          end;

        end;

        FreeAndNil(FWorkerSocket);
        Synchronize(SyncAtualiza);

      end; //if
      ClientSocket := TSocket(zClientSocket);

    end; //while

    if Assigned(FDBConnection) then
    begin
      FDBConnection.Close;
      FreeAndNil(FDBConnection);
    end;

    FCRTSection.Enter;

    try
      nContador := FWorkerThreads.IndexOf(Self);

      if nContador >= 0 then
        FWorkerThreads.Delete(nContador);

      DecCurThreadsInPool;
      Synchronize(SyncAtualiza);

      if FCurThreadsInPool = 0 then
        FTerminateEvent.SetEvent;
    finally
      FCRTSection.Leave;
    end;

    CoUninitialize();

  end; //with

end;

end.
