unit SE_SaveModifiedForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TSESaveModifiedForm = class(TForm)
    lblConfirm: TLabel;
    Image1: TImage;
    btnSave: TButton;
    btnSaveAs: TButton;
    btnCancel: TButton;
    btnNo: TButton;
  end;

implementation

{$R *.dfm}

end.
