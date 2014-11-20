object frmAAMAnnotator: TfrmAAMAnnotator
  Left = 0
  Top = 0
  Caption = 'AAM Annotations'
  ClientHeight = 680
  ClientWidth = 821
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object pbAAMAnnot: TPaintBox
    Left = 0
    Top = 89
    Width = 821
    Height = 591
    Align = alClient
    OnMouseDown = pbAAMAnnotMouseDown
    OnMouseMove = pbAAMAnnotMouseMove
    OnMouseUp = pbAAMAnnotMouseUp
    OnPaint = pbAAMAnnotPaint
    ExplicitLeft = 368
    ExplicitTop = 304
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 821
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 464
      Top = 31
      Width = 32
      Height = 16
      Caption = 'Zoom'
    end
    object Label2: TLabel
      Left = 632
      Top = 28
      Width = 69
      Height = 16
      Caption = 'Num Points:'
    end
    object lblNumPoints: TLabel
      Left = 707
      Top = 28
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Button1: TButton
      Left = 24
      Top = 25
      Width = 137
      Height = 48
      Caption = 'Load Image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 176
      Top = 25
      Width = 137
      Height = 48
      Caption = 'Save Annotations'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 344
      Top = 25
      Width = 97
      Height = 25
      Caption = 'Clear all'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 344
      Top = 48
      Width = 97
      Height = 25
      Caption = 'Clear last'
      TabOrder = 3
      OnClick = Button4Click
    end
    object cboZoom: TComboBox
      Left = 464
      Top = 50
      Width = 145
      Height = 24
      ItemHeight = 16
      TabOrder = 4
      Text = 'cboZoom'
      OnExit = cboZoomExit
      OnSelect = cboZoomSelect
      Items.Strings = (
        '50'
        '75'
        '100'
        '125'
        '200'
        '300'
        '500')
    end
  end
  object openImgDialog: TOpenPictureDialog
    Filter = 
      'Alle (*.jpg;*.jpeg;*.jpg;*.jpeg;*.bmp)|*.jpg;*.jpeg;*.jpg;*.jpeg' +
      ';*.bmp|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.' +
      'jpeg|JPEG-Grafikdatei (*.jpg)|*.jpg|JPEG-Grafikdatei (*.jpeg)|*.' +
      'jpeg|Bitmaps (*.bmp)|*.bmp'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open AAM Image'
    Left = 768
    Top = 8
  end
end
