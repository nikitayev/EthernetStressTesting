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
    procedure FormShow(Sender: TObject);
    procedure btStartClick(Sender: TObject);
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
