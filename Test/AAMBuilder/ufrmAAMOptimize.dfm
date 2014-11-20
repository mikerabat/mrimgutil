object frmAAMOptimize: TfrmAAMOptimize
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  ClientHeight = 532
  ClientWidth = 679
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object pbTestImage: TPaintBox
    Left = 0
    Top = 49
    Width = 321
    Height = 483
    Hint = 'Click into the image to start an AAM iteration.'
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    OnMouseMove = pbTestImageMouseMove
    OnMouseUp = pbTestImageMouseUp
    OnPaint = pbTestImagePaint
  end
  object pbOptimized: TPaintBox
    Left = 336
    Top = 49
    Width = 343
    Height = 483
    Align = alRight
    OnPaint = pbOptimizedPaint
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 679
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      679
      49)
    object Label1: TLabel
      Left = 200
      Top = 16
      Width = 66
      Height = 16
      Caption = 'Initial Scale'
    end
    object lblOptTime: TLabel
      Left = 560
      Top = 15
      Width = 113
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '---'
    end
    object btnLoadTestImage: TButton
      Left = 16
      Top = 9
      Width = 113
      Height = 30
      Hint = 
        'Load Image - a good initial check can be done with an '#13#10'image fr' +
        'om the training set'
      Caption = 'Load Test Image'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnLoadTestImageClick
    end
    object edScale: TEdit
      Left = 280
      Top = 12
      Width = 57
      Height = 26
      TabOrder = 1
      Text = '180'
    end
  end
  object dlTestImage: TOpenPictureDialog
    DefaultExt = '*.jpg'
    Filter = 
      'Alle (*.jpg;*.jpeg;*.jpg;*.jpeg;*.bmp)|*.jpg;*.jpeg;*.jpg;*.jpeg' +
      ';*.bmp|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.' +
      'jpeg|JPEG-Grafikdatei (*.jpg)|*.jpg|JPEG-Grafikdatei (*.jpeg)|*.' +
      'jpeg|Bitmaps (*.bmp)|*.bmp'
    InitialDir = '..\AAMTests'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open Test Image'
    Left = 152
    Top = 8
  end
end
