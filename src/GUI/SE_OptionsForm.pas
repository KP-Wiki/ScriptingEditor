unit SE_OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnMan, ActnColorMaps, ExtCtrls, Vcl.Samples.Spin;

type
  TSEOptionsForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    btnCancel: TButton;
    btnSave: TButton;
    Label2: TLabel;
    seFontSize: TSpinEdit;
    cbFontName: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure CollectFonts(aFontList: TStringList);
  public
    procedure ApplySettings;
  end;

function EnumFontsProc(var aLogFont: TLogFont; var aTextMetric: TTextMetric;
                       aFontType: Integer; aData: Pointer): Integer; stdcall;

implementation
{$R *.dfm}
uses
  SE_Globals;

function EnumFontsProc(var aLogFont: TLogFont; var aTextMetric: TTextMetric;
                       aFontType: Integer; aData: Pointer): Integer; stdcall;
var
  S:    TStrings;
  Temp: string;
begin
  S    := TStrings(aData);
  Temp := aLogFont.lfFaceName;

  if (S.Count = 0) or (CompareText(S[S.Count - 1], Temp) <> 0) then
    S.Add(Temp);

  Result := 1;
end;

procedure TSEOptionsForm.FormCreate(Sender: TObject);
var
  FontList: TStringList;
  I: Integer;
begin
  FontList := TStringList.Create;
  CollectFonts(FontList);
  cbFontName.Items.AddStrings(FontList);
  FreeAndNil(FontList);

  for I := 0 to cbFontName.Items.Count - 1 do
    if CompareText(cbFontName.Items[I], gOptions.Font.Name) = 0 then
      cbFontName.ItemIndex := I;

  seFontSize.Value := gOptions.Font.Size;
end;

procedure TSEOptionsForm.CollectFonts(aFontList: TStringList);
var
  DC:    HDC;
  LFont: TLogFont;
begin
  ZeroMemory(@LFont, sizeof(LFont));
  DC              := GetDC(0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LPARAM(aFontList), 0);
  ReleaseDC(0, DC);
end;

procedure TSEOptionsForm.ApplySettings;
begin
  gOptions.Font.Name := TFontName(cbFontName.Text);
  gOptions.Font.Size := seFontSize.Value;
end;

end.
