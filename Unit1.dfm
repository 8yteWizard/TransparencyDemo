object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Transparency Demo'
  ClientHeight = 300
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = -8
    Top = -7
    Width = 505
    Height = 312
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 24
      Width = 151
      Height = 15
      Caption = 'Select Transparency Method:'
      Color = clBlack
      ParentColor = False
    end
    object ComboBox1: TComboBox
      Left = 24
      Top = 48
      Width = 350
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox1Change
    end
  end
end
