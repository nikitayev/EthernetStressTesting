unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, U_GlobalDataUnit, ComCtrls, StdCtrls, ExtCtrls, DateUtils,
  ClientThreadUnit, U_StartClientsUnit;

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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure TCPClientNotify (var Message: TMessage); message WM_TCPClientNotify;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.btStartClick(Sender: TObject);
begin
  TCreateClientsThread.Create(StrToInt(leStartID.Text),
    StrToInt(leDeviceCount.Text), dtpStartTime.DateTime, 
    leAddress.Text, lePort.Text);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  dtpStartTime.DateTime := IncSecond(Now, 30);
  ImageDevices.Picture.Bitmap.SetSize(100, 100);
  ImageDevices.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  ImageDevices.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  ImageDevices.Picture.Bitmap.Canvas.Rectangle(0,0,99,99);
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
  UpdateTimer.Enabled := False;
  DrawIOTransactStates(ImageDevices.Picture.Bitmap.Canvas);
  UpdateTimer.Enabled := True;
end;

end.
