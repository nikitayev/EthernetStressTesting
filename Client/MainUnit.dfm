object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1050#1083#1080#1077#1085#1090#1099
  ClientHeight = 526
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 200
    Top = 16
    Width = 44
    Height = 13
    Caption = #1057#1090#1072#1088#1090' '#1074':'
  end
  object ImageDevices: TImage
    Left = 16
    Top = 112
    Width = 400
    Height = 400
    Stretch = True
  end
  object leStartID: TLabeledEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 145
    EditLabel.Height = 13
    EditLabel.Caption = #1057#1090#1072#1088#1090#1086#1074#1099#1081' '#1085#1086#1084#1077#1088' '#1091#1089#1090#1086#1081#1089#1090#1074#1072
    TabOrder = 0
    Text = '0'
  end
  object leDeviceCount: TLabeledEdit
    Left = 8
    Top = 68
    Width = 121
    Height = 21
    EditLabel.Width = 115
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1091#1089#1090#1088#1086#1081#1089#1090#1074
    TabOrder = 1
    Text = '10000'
  end
  object dtpStartTime: TDateTimePicker
    Left = 200
    Top = 32
    Width = 186
    Height = 21
    Date = 41916.587719791660000000
    Format = 'yyyy-MM-dd hh:mm:ss'
    Time = 41916.587719791660000000
    DateFormat = dfLong
    DateMode = dmUpDown
    TabOrder = 2
  end
  object btStart: TButton
    Left = 200
    Top = 64
    Width = 75
    Height = 25
    Caption = #1057#1058#1040#1056#1058'!'
    TabOrder = 3
    OnClick = btStartClick
  end
  object leAddress: TLabeledEdit
    Left = 408
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = #1040#1076#1088#1077#1089
    TabOrder = 4
    Text = '127.0.0.1'
  end
  object lePort: TLabeledEdit
    Left = 535
    Top = 32
    Width = 58
    Height = 21
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1086#1088#1090
    TabOrder = 5
    Text = '5706'
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerTimer
    Left = 472
    Top = 96
  end
end
