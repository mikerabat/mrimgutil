object frmAAMBuilder: TfrmAAMBuilder
  Left = 0
  Top = 0
  Caption = 'AAM Building Program'
  ClientHeight = 618
  ClientWidth = 694
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object pnlOptions: TPanel
    Left = 496
    Top = 41
    Width = 198
    Height = 577
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      198
      577)
    object Label2: TLabel
      Left = 16
      Top = 26
      Width = 137
      Height = 16
      Caption = 'AAM Building Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblLevels: TLabel
      Left = 72
      Top = 90
      Width = 65
      Height = 16
      Caption = 'Num Levels'
    end
    object lblModelEnergy: TLabel
      Left = 16
      Top = 175
      Width = 77
      Height = 16
      Caption = 'Model Energy'
    end
    object Label3: TLabel
      Left = 90
      Top = 200
      Width = 52
      Height = 16
      Caption = '% Shape'
    end
    object Label4: TLabel
      Left = 90
      Top = 227
      Width = 60
      Height = 16
      Caption = '% Texture'
    end
    object Label5: TLabel
      Left = 90
      Top = 254
      Width = 73
      Height = 16
      Caption = '% Combined'
    end
    object Label6: TLabel
      Left = 16
      Top = 322
      Width = 77
      Height = 16
      Caption = 'Learn options'
    end
    object Label7: TLabel
      Left = 90
      Top = 347
      Width = 89
      Height = 16
      Caption = 'Learn iterations'
    end
    object Label8: TLabel
      Left = 90
      Top = 379
      Width = 89
      Height = 16
      Caption = 'Max Model Size'
    end
    object lblStep: TLabel
      Left = 119
      Top = 524
      Width = 15
      Height = 16
      Anchors = [akRight, akBottom]
      Caption = '---'
    end
    object chkBuildColorAAM: TCheckBox
      Left = 16
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Colored Model'
      TabOrder = 0
    end
    object pbOverall: TProgressBar
      Left = 16
      Top = 548
      Width = 150
      Height = 17
      Anchors = [akLeft, akBottom]
      TabOrder = 1
    end
    object btnBuild: TButton
      Left = 16
      Top = 493
      Width = 153
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Build Model'
      TabOrder = 2
      OnClick = btnBuildClick
    end
    object cboWarpClass: TComboBox
      Left = 16
      Top = 119
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Linear Warp'
      Items.Strings = (
        'Linear Warp'
        'Thin Plate Splines Warp')
    end
    object pbStep: TProgressBar
      Left = 16
      Top = 524
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      TabOrder = 10
    end
    object edLevels: TEdit
      Left = 16
      Top = 87
      Width = 41
      Height = 26
      TabOrder = 3
      Text = '4'
    end
    object edShapeEnergy: TEdit
      Left = 32
      Top = 197
      Width = 49
      Height = 26
      TabOrder = 5
      Text = '98'
    end
    object edTextureEnergy: TEdit
      Left = 32
      Top = 224
      Width = 49
      Height = 26
      TabOrder = 6
      Text = '92'
    end
    object edCombinedEnergy: TEdit
      Left = 32
      Top = 251
      Width = 49
      Height = 26
      TabOrder = 7
      Text = '95'
    end
    object edLearnSteps: TEdit
      Left = 32
      Top = 344
      Width = 49
      Height = 26
      TabOrder = 8
      Text = '10'
    end
    object edScale: TEdit
      Left = 32
      Top = 376
      Width = 57
      Height = 26
      TabOrder = 9
      Text = '180'
      TextHint = 'In Pixels'
    end
  end
  object pnlDir: TPanel
    Left = 0
    Top = 0
    Width = 694
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 12
      Width = 85
      Height = 16
      Caption = 'AAM Image DB'
    end
    object lblAAMDB: TLabel
      Left = 615
      Top = 12
      Width = 15
      Height = 16
      Caption = '---'
    end
    object btnLoad: TButton
      Left = 568
      Top = 8
      Width = 41
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object edAAMDir: TEdit
      Left = 117
      Top = 9
      Width = 445
      Height = 24
      TabOrder = 1
      Text = '..\AAMTests\'
    end
  end
  object tbAAMModel: TTabControl
    Left = 0
    Top = 41
    Width = 496
    Height = 577
    Align = alClient
    TabOrder = 2
    Tabs.Strings = (
      'Base Model Params'
      'Test Model fitting')
    TabIndex = 0
    OnChange = tbAAMModelChange
  end
  object svModel: TSaveDialog
    DefaultExt = '*.aam'
    Filter = 'Active Appearance Models (*.aam)|*.aam'
    Title = 'Save Active Appearance Model'
    Left = 8
    Top = 8
  end
  object deOpenAAM: TOpenDialog
    DefaultExt = '*.aam'
    Filter = 'Active Appearance Models (*.aam)|*.aam'
    Title = 'Load Active Appearance Model'
    Left = 40
    Top = 8
  end
end
