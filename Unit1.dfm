object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtPerro: TButton
    Left = 149
    Top = 21
    Width = 75
    Height = 25
    Caption = 'Perro'
    TabOrder = 0
    OnClick = BtPerroClick
  end
  object btnGato: TButton
    Left = 29
    Top = 21
    Width = 75
    Height = 25
    Caption = 'Gato'
    TabOrder = 1
    OnClick = btnGatoClick
  end
  inline FrInspector1: TFrInspector
    Left = 315
    Top = 0
    Width = 320
    Height = 299
    Align = alRight
    TabOrder = 2
    ExplicitLeft = 315
    ExplicitHeight = 299
    inherited Panel3: TPanel
      Height = 278
      ExplicitHeight = 278
      inherited Panel2: TPanel
        Height = 240
        ExplicitHeight = 240
        inherited Inspector: TRTTIInspectorBar
          Height = 238
          ExplicitHeight = 238
        end
      end
      inherited BtnAyuda: TButton
        Top = 241
        ExplicitTop = 241
      end
    end
  end
end
