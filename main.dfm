object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Webshop Manager | V 1.0'
  ClientHeight = 738
  ClientWidth = 1031
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1031
    738)
  PixelsPerInch = 96
  TextHeight = 12
  object Image1: TImage
    Left = 8
    Top = 118
    Width = 217
    Height = 210
    Stretch = True
  end
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
    Left = 231
    Top = 118
    Width = 792
    Height = 612
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 334
    Width = 217
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    FixedCols = 0
    TabOrder = 2
    ColWidths = (
      98)
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test Spec'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 92
    Width = 217
    Height = 20
    TabOrder = 4
  end
  object Button3: TButton
    Left = 8
    Top = 61
    Width = 89
    Height = 25
    Caption = 'Download Image'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Posttest'
    TabOrder = 6
    OnClick = Button4Click
  end
  object e_cons_key: TEdit
    Left = 231
    Top = 8
    Width = 242
    Height = 20
    TabOrder = 7
    Text = 'ck_8fc193b5bd5fde09d466d6d0c5afe44394d4110e'
  end
  object e_secret: TEdit
    Left = 231
    Top = 34
    Width = 242
    Height = 20
    TabOrder = 8
    Text = 'cs_9b7a46cbecb16454cabbd5dad8e0681b94c0d4ee'
  end
  object e_base_url: TEdit
    Left = 231
    Top = 66
    Width = 242
    Height = 20
    TabOrder = 9
    Text = 'http://192.168.1.7/wordpress'
  end
  object Button5: TButton
    Left = 231
    Top = 92
    Width = 75
    Height = 25
    Caption = 'Test RESTAPI'
    TabOrder = 10
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 488
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 11
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 664
    Top = 8
    Width = 177
    Height = 25
    Caption = 'Test MAIN Download 1'
    TabOrder = 12
    OnClick = Button7Click
  end
end
