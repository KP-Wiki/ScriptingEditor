unit SE_GoToLineForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.Samples.Spin;

type
  TSEGoToLineForm = class(TForm)
    btnCancel: TButton;
    btnGo: TButton;
    seLine: TSpinEdit;
    Label1: TLabel;
  private
    function GetLine:    Integer;
    function GetLineMax: Integer;
    procedure SetLine(const Value: Integer);
    procedure SetLineMax(const Value: Integer);
  public
    property Line:    Integer read GetLine    write SetLine;
    property LineMax: Integer read GetLineMax write SetLineMax;
  end;

implementation

{$R *.dfm}

function TSEGoToLineForm.GetLine: Integer;
begin
  Result := seLine.Value;
end;

function TSEGoToLineForm.GetLineMax: Integer;
begin
  Result := seLine.MaxValue;
end;

procedure TSEGoToLineForm.SetLine(const Value: Integer);
begin
  seLine.Value := Value;
end;

procedure TSEGoToLineForm.SetLineMax(const Value: Integer);
begin
  seLine.MaxValue := Value;
end;


end.
