object FrInspector: TFrInspector
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object cmbObjetos: TComboBox
    Left = 0
    Top = 0
    Width = 320
    Height = 21
    Align = alTop
    Sorted = True
    TabOrder = 0
    Text = 'cmbObjetos'
    OnSelect = cmbObjetosSelect
  end
  object Panel3: TPanel
    Left = 0
    Top = 21
    Width = 320
    Height = 219
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 318
      Height = 181
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      object Inspector: TRTTIInspectorBar
        Left = 1
        Top = 1
        Width = 316
        Height = 179
        AcceptFiles = False
        AutoAdvance = True
        Align = alClient
        CheckTrue = 'Verdadero'
        CheckFalse = 'Falso'
        Color = clWhite
        Ellipsis = True
        Flat = False
        HelpPanel.BevelInner = bvLowered
        HelpPanel.BevelOuter = bvNone
        HelpPanel.BevelWidth = 0
        HelpPanel.Color = 15590880
        HelpPanel.ColorTo = clWhite
        HelpPanel.Font.Charset = DEFAULT_CHARSET
        HelpPanel.Font.Color = clWindowText
        HelpPanel.Font.Height = -11
        HelpPanel.Font.Name = 'Tahoma'
        HelpPanel.Font.Style = []
        HelpPanel.Height = 0
        HelpPanel.Visible = False
        Mode = imMultiPanelActive
        PanelCaption.ActiveFont.Charset = DEFAULT_CHARSET
        PanelCaption.ActiveFont.Color = clBlack
        PanelCaption.ActiveFont.Height = -11
        PanelCaption.ActiveFont.Name = 'Tahoma'
        PanelCaption.ActiveFont.Style = []
        PanelCaption.Button = True
        PanelCaption.Color = clWhite
        PanelCaption.ColorTo = 15590880
        PanelCaption.Font.Charset = DEFAULT_CHARSET
        PanelCaption.Font.Color = clWindowText
        PanelCaption.Font.Height = -11
        PanelCaption.Font.Name = 'Tahoma'
        PanelCaption.Font.Style = []
        PanelCaption.SideDisplay = False
        PanelCaption.SideWidth = 20
        PanelCaption.OpenClosePosition = ocpLeft
        PanelCaption.OpenCloseGraphic = ocgNone
        PanelCaption.ShadeGrain = 32
        PanelCaption.ShadeType = stVBump
        PanelCaption.Shape = csSemiRounded
        PanelCaption.VAlignment = vaCenter
        Panels = <>
        ShowEditorAlways = False
        ShowHint = True
        TabStop = True
        TabOrder = 0
        TopPanel = 0
        OnEditStart = InspectorEditStart
        OnEditStop = InspectorEditStop
        Version = '1.8.8.0'
        Style = esOffice2010Silver
        DefaultGradientDirection = gdVertical
      end
    end
    object BtnAyuda: TButton
      Left = 1
      Top = 182
      Width = 318
      Height = 36
      Align = alBottom
      Caption = 'Ayuda'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BtnAyudaClick
    end
  end
end
