unit U_GlobalDataUnit;

interface
uses SysUtils, Classes, Messages;

const
  cDefaultPacketSize = 2000;
  cUpdatePacketSize = 14000;
  // время таймаута ожидания при посылке Update
  cUpdateFirstSleep = 5000;
  // клиентский таймаут
  cClientTimeout = 50000;
  // таймаут для сокета
  //cSocketsTimeOut = 1000; - bad timeout for high load I/O
  cSocketsTimeOut = 6000; // - good timeout for high load I/O
  cLinger = 100;
  // нотификация от клиента
  WM_TCPClientNotify = WM_USER + 1;

 
type
  TClientMode = (cmDefaultMode, cmWithUpdateTransaction);
  TConnectionState = (csWaiting, csReady, csTryToConnect, csConnected, 
    csInTransaction, csDone, csDataError, csConnectError);

  TClentInfo = record
    DeviceID: word;
    IOMode: TClientMode;
    LastIOError: Integer;
    IOState: TConnectionState;
  end;
  PClentInfo = ^TClentInfo;

  TStreamHelper = class(TMemoryStream)
  public
    function ReadBoolean: Boolean;
    procedure WriteBoolean(v: Boolean);
    Function ReadByte : Byte;
    Procedure WriteByte(v : Byte);
    Function ReadWord : Word;
    Procedure WriteWord(v : Word);
    function ReadInteger: Integer;
    procedure WriteInteger(v: Integer);
    function ReadCardinal: Cardinal;
    procedure WriteCardinal(v: Cardinal);
    function ReadInt64: Int64;
    procedure WriteInt64(v: Int64);
    function ReadSingle: Single;
    procedure WriteSingle(v: Single);
    function ReadDouble: Double;
    procedure WriteDouble(v: Double);
    function ReadString: AnsiString;
    procedure WriteString(const v: AnsiString);
    function ReadChars(count: Integer): AnsiString;
    procedure WriteChars(const v: AnsiString; count: Integer);
    function ReadWideString: WideString;
    procedure WriteWideString(const v: WideString);
    function ReadWideChars(count: Integer): WideString;
    procedure WriteWideChars(const v: WideString; count: Integer);
    function StreamMove(Src, Dst, Count: Int64): Int64;
  end;

function GetPClentInfo(DeviceID: word; IOMode: TClientMode; LastIOError: Integer;
    IOState: TConnectionState):PClentInfo;

implementation

function GetPClentInfo(DeviceID: word; IOMode: TClientMode; LastIOError: Integer;
    IOState: TConnectionState):PClentInfo;
begin
  New(Result);
  Result.DeviceID := DeviceID;
  Result.IOMode := IOMode;
  Result.LastIOError := LastIOError;
  Result.IOState := IOState;
end;

function TStreamHelper.StreamMove(Src, Dst, Count: Int64): Int64;
var
  Buffer : Array [0..1024 * 32 - 1] of Byte;
  ByteCount : Integer;
  ReadBytes : Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    If Count > length(Buffer) then
      ByteCount := length(Buffer)
    else
      ByteCount := Count;

    Position := Src;
    ReadBytes := Read(Buffer, ByteCount);
    If ReadBytes <> ByteCount then
      raise Exception.Create('Abnormal exception. Could not read desired bytes from the file.');

    Position := Dst;
    ByteCount := Write(Buffer, ReadBytes);
    If ByteCount <> ReadBytes then
      raise Exception.Create('Abnormal exception. Could not write desired bytes to the file.');

    Count := Count - ByteCount;
    Result := Result + ByteCount;
    Src := Src + ByteCount;
    Dst := Dst + ByteCount;
  end;
End;

Function TStreamHelper.ReadBoolean : Boolean;
Begin
  Read(Result, 1);
End;

Procedure TStreamHelper.WriteBoolean(v : Boolean);
Begin
  Write(v, 1);
End;

Function TStreamHelper.ReadByte : Byte;
Begin
  Read(Result, 1);
End;

Procedure TStreamHelper.WriteByte(v : Byte);
Begin
  Write(v, 1);
End;

Function TStreamHelper.ReadWord : Word;
Begin
  Read(Result, 2);
End;

Procedure TStreamHelper.WriteWord(v : Word);
Begin
  Write(v, 2);
End;

Function TStreamHelper.ReadInteger : Integer;
Begin
  Read(Result, 4);
End;

Procedure TStreamHelper.WriteInteger(v : Integer);
Begin
  Write(v, 4);
End;

Function TStreamHelper.ReadCardinal : Cardinal;
Begin
  Read(Result, 4);
End;

Procedure TStreamHelper.WriteCardinal(v : Cardinal);
Begin
  Write(v, 4);
End;

function TStreamHelper.ReadInt64: Int64;
begin
  Read(Result, 8);
end;

procedure TStreamHelper.WriteInt64(v: Int64);
begin
  Write(v, 8);
end;

Function TStreamHelper.ReadSingle : Single;
Begin
  Read(Result, 4);
End;

Procedure TStreamHelper.WriteSingle(v : Single);
Begin
  Write(v, 4);
End;

function TStreamHelper.ReadDouble: Double;
begin
  Read(Result, 8);
end;

procedure TStreamHelper.WriteDouble(v: Double);
begin
  Write(v, 8);
end;

Procedure TStreamHelper.WriteString(Const v : AnsiString);
Var Len : Integer;
Begin
  Len := Length(v);
  Write(Len, SizeOf(Len));
  Write(PAnsiChar(v)^, Len);
End;

Function TStreamHelper.ReadString: AnsiString;
Var Len : Integer;
Begin
  Read(Len, SizeOf(Len));
  //If len > 20000 Then exit;
  SetLength(Result, Len);
  Read(PAnsiChar(Result)^, Len);
End;

Procedure TStreamHelper.WriteChars(Const v : AnsiString; count: Integer);
Begin
  Write(PAnsiChar(v)^, count);
End;

Function TStreamHelper.ReadChars(count: Integer) : AnsiString;
Begin
  SetLength(Result, count);
  Read(PAnsiChar(Result)^, count);
End;

Procedure TStreamHelper.WriteWideString(Const v : WideString);
Var Len : Integer;
Begin
  Len := Length(v);
  Write(Len, SizeOf(Len));
  Write(PWideChar(v)^, Len * 2);
End;

Function TStreamHelper.ReadWideString: WideString;
Var Len : Integer;
Begin
  Read(Len, SizeOf(Len));
  //If len > 20000 Then exit;
  SetLength(Result, Len);
  Read(PWideChar(Result)^, Len * 2);
End;

Procedure TStreamHelper.WriteWideChars(Const v : WideString; count: Integer);
Begin
  Write(PWideChar(v)^, count * 2);
End;

Function TStreamHelper.ReadWideChars(count: Integer) : WideString;
Begin
  SetLength(Result, count);
  Read(PWideChar(Result)^, count * 2);
End;

end.
