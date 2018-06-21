unit SE_ConfirmReplaceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Vcl.Imaging.pngimage;

type
  TSEConfirmReplaceForm = class(TForm)
    Image1: TImage;
    lblConfirm: TLabel;
    btnCancel: TButton;
    btnSkip: TButton;
    btnReplaceAll: TButton;
    btnReplace: TButton;
  public
    procedure PrepareShow(aEditorRect: TRect; X, Y1, Y2: Integer;
                          aReplaceText: string);
  end;

implementation
{$R *.dfm}

procedure TSEConfirmReplaceForm.PrepareShow(aEditorRect: TRect;
                                            X, Y1, Y2: Integer;
                                            aReplaceText: string);
var
  nW,
  nH: Integer;
begin
  lblConfirm.Caption := Format('Replace this occurence of "%s"?', [aReplaceText]);
  nW                 := aEditorRect.Right - aEditorRect.Left;
  nH                 := aEditorRect.Bottom - aEditorRect.Top;

  if nW <= Width then
    X := aEditorRect.Left - (Width - nW) div 2
  else if X + Width > aEditorRect.Right then
    X := aEditorRect.Right - Width;

  if Y2 > aEditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);

  SetBounds(X, Y2, Width, Height);
end;

end.
