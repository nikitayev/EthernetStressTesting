unit U_StartClientsUnit;

interface

uses
  System.Classes, U_GlobalDataUnit, ClientThreadUnit;

type
  TCreateClientsThread = class(TThread)
  private
    { Private declarations }
    FBeginIDX, FCount:Integer;
    FBeginTime: TDateTime;
    FIP, FPort: string;
  protected
    procedure Execute; override;
  public
    constructor Create(aBeginIDX, aCount:Integer; aBeginTime: TDateTime;
      const aIP, aPort: string);
  end;

implementation

{ 
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);  

  and UpdateCaption could look like,

    procedure TCreateClientsThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; 
    
    or 
    
    Synchronize( 
      procedure 
      begin
        Form1.Caption := 'Updated in thread via an anonymous method' 
      end
      )
    );
    
  where an anonymous method is passed.
  
  Similarly, the developer can call the Queue method with similar parameters as 
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.
    
}

{ TCreateClientsThread }

constructor TCreateClientsThread.Create(aBeginIDX, aCount: Integer; 
  aBeginTime: TDateTime; const aIP, aPort: string);
begin
  FreeOnTerminate := false;
  FBeginIDX := aBeginIDX;
  FCount := aCount;
  FBeginTime := aBeginTime;
  FIP := aIP;
  FPort := aPort;
  inherited Create;
end;

procedure TCreateClientsThread.Execute;
var
  I:Integer;
begin
  for I := FBeginIDX to FBeginIDX + FCount - 1 do
  begin
    TClientThread.Create(FBeginTime, cmDefaultMode, FIP, FPort, I);
  end;
end;

end.
