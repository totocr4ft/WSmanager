object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Webshop Manager | V 1.0'
  ClientHeight = 465
  ClientWidth = 797
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    797
    465)
  PixelsPerInch = 96
  TextHeight = 12
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 89
    Top = 8
    Width = 700
    Height = 71
    TabOrder = 1
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 85
    Width = 781
    Height = 372
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    FixedCols = 0
    TabOrder = 2
    ColWidths = (
      98)
  end
end
