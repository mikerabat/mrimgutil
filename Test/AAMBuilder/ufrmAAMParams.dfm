object frmModelParams: TfrmModelParams
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  ClientHeight = 599
  ClientWidth = 733
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBaseModelParams: TPanel
    Left = 0
    Top = 0
    Width = 733
    Height = 599
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 4
    ExplicitTop = 15
    ExplicitWidth = 488
    ExplicitHeight = 546
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 733
      Height = 265
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 488
      object imgMeanImg: TImage
        Left = 0
        Top = 16
        Width = 733
        Height = 249
        Align = alClient
        Center = True
        ExplicitLeft = -2
        ExplicitTop = 22
        ExplicitWidth = 488
        ExplicitHeight = 274
      end
      object Label9: TLabel
        Left = 0
        Top = 0
        Width = 733
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Mean Model'
        ExplicitLeft = 112
        ExplicitWidth = 69
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 265
      Width = 733
      Height = 334
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 488
      ExplicitHeight = 281
      object imgDisturbed: TImage
        Left = 152
        Top = 25
        Width = 581
        Height = 309
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        Center = True
        ExplicitTop = 40
        ExplicitWidth = 336
        ExplicitHeight = 241
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 733
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Disturbed Model'
        TabOrder = 0
        ExplicitWidth = 488
      end
      object pnlParams: TPanel
        Left = 0
        Top = 25
        Width = 152
        Height = 309
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitHeight = 256
        object lblDisturbanceParams: TLabel
          Left = 0
          Top = 0
          Width = 152
          Height = 13
          Align = alTop
          Alignment = taCenter
          Caption = 'Disturbance Params'
          Layout = tlCenter
          ExplicitWidth = 95
        end
      end
    end
  end
end
