object ServerMainForm: TServerMainForm
  Left = 0
  Top = 0
  Caption = #1057#1077#1088#1074#1077#1088
  ClientHeight = 466
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ImageDevices: TImage
    Left = 0
    Top = 3
    Width = 400
    Height = 400
    Stretch = True
  end
  object lbClientsCount: TLabel
    Left = 488
    Top = 27
    Width = 52
    Height = 13
    Caption = '-------------'
  end
  object lePort: TLabeledEdit
    Left = 406
    Top = 24
    Width = 58
    Height = 21
    EditLabel.Width = 25
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1086#1088#1090
    TabOrder = 0
    Text = '5706'
  end
  object btStartSynapse: TButton
    Left = 406
    Top = 51
    Width = 147
    Height = 25
    Caption = #1057#1058#1040#1056#1058' Synapse'
    TabOrder = 1
    OnClick = btStartSynapseClick
  end
  object btStartWinSock: TButton
    Left = 406
    Top = 82
    Width = 147
    Height = 25
    Caption = #1057#1058#1040#1056#1058' WinSock'
    TabOrder = 2
    OnClick = btStartWinSockClick
  end
  object btStartIndy: TButton
    Left = 406
    Top = 113
    Width = 147
    Height = 25
    Caption = #1057#1058#1040#1056#1058' Indy'
    TabOrder = 3
    OnClick = btStartIndyClick
  end
  object CheckingTimer: TTimer
    Enabled = False
    OnTimer = CheckingTimerTimer
    Left = 424
    Top = 104
  end
end
