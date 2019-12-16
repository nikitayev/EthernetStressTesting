unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, U_GlobalDataUnit, ComCtrls, StdCtrls, ExtCtrls, DateUtils,
  ClientThreadUnit, U_StartClientsUnit, INIFiles;

type
  TForm1 = class(TForm)
    leStartID: TLabeledEdit;
    leDeviceCount: TLabeledEdit;
    dtpStartTime: TDateTimePicker;
    Label1: TLabel;
    btStart: TButton;
    leAddress: TLabeledEdit;
    lePort: TLabeledEdit;
    ImageDevices: TImage;
    UpdateTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FClientList: TCreateClientsThread;
  public
    { Public declarations }
    procedure TCPClientNotify(var Message: TMessage); message WM_TCPClientNotify;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.btStartClick(Sender: TObject);
begin
  dtpStartTime.DateTime := IncSecond(Now, 15);
  if (Assigned(FClientList)) then
  begin
    FreeAndNil(FClientList);
  end;
  FClientList := TCreateClientsThread.Create(StrToInt(leStartID.Text),
    StrToInt(leDeviceCount.Text), dtpStartTime.DateTime,
    leAddress.Text, lePort.Text);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  zINI: TINIFile;
begin
  zINI := TINIFile.Create(ExtractFilePath(ParamStr(0)) + 'parameters.ini');
  try
    zINI.WriteString('net', 'ip', leAddress.Text);
    zINI.WriteString('net', 'port', lePort.Text);
  finally
    FreeAndNil(zINI);
  end;
  if (Assigned(FClientList)) then
  begin
    FreeAndNil(FClientList);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  zINI: TINIFile;
begin
  dtpStartTime.DateTime := IncSecond(Now, 30);
  ImageDevices.Picture.Bitmap.SetSize(100, 100);
  ImageDevices.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  ImageDevices.Picture.Bitmap.Canvas.Rectangle(0, 0, 99, 99);

  zINI := TINIFile.Create(ExtractFilePath(ParamStr(0)) + 'parameters.ini');
  try
    leAddress.Text := zINI.ReadString('net', 'ip', '127.0.0.1');
    lePort.Text := zINI.ReadString('net', 'port', '8081');
  finally
    FreeAndNil(zINI);
  end;
end;

procedure TForm1.TCPClientNotify(var Message: TMessage);
var
  zData: PClentInfo;
begin
  zData := PClentInfo(Message.WParam);
  IOTransactDone(zData);
end;

procedure TForm1.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled := false;
  DrawIOTransactStates(ImageDevices.Picture.Bitmap.Canvas);
  UpdateTimer.Enabled := true;
end;

end.
