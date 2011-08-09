object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1054' '#1087#1083#1072#1075#1080#1085#1077
  ClientHeight = 418
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelInfo: TLabel
    Left = 8
    Top = 8
    Width = 273
    Height = 89
    AutoSize = False
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 120
    Width = 273
    Height = 17
    AutoSize = False
    Caption = #1055#1091#1090#1100' '#1082' '#1082#1072#1090#1072#1083#1086#1075#1091' '#1089' '#1092#1072#1081#1083#1072#1084#1080' '#1085#1072#1089#1090#1088#1086#1077#1082':'
  end
  object EditPath: TEdit
    Left = 8
    Top = 144
    Width = 249
    Height = 21
    ReadOnly = True
    TabOrder = 0
    Text = #1055#1091#1090#1100
  end
  object ButtonOpenPath: TButton
    Left = 263
    Top = 143
    Width = 18
    Height = 22
    Caption = '...'
    TabOrder = 1
    OnClick = ButtonOpenPathClick
  end
end
