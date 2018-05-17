unit SE_ValidationPlugin;

interface
uses
  Windows, Graphics,
  SynEdit, SynEditPlugins;

type
  TGutterPainEvent = procedure(aCanvas: TCanvas; const aClip: TRect;
                               aFirstLine, aLastLine: Integer) of object;
  TLineChangeEvent = procedure(aFirstLine, aCount: Integer) of object;

  TSEValidationPlugin = class(TSynEditPlugin)
  strict private
    fOnGutterPaint: TGutterPainEvent;
    fOnLinesInserted: TLineChangeEvent;
    fOnLinesDeleted: TLineChangeEvent;
  protected
    procedure AfterPaint(aCanvas: TCanvas; const aClip: TRect;
                         aFirstLine, aLastLine: Integer); override;
    procedure LinesInserted(aFirstLine, aCount: Integer); override;
    procedure LinesDeleted(aFirstLine, aCount: Integer); override;
  public
    property OnGutterPaint:   TGutterPainEvent read fOnGutterPaint   write fOnGutterPaint;
    property OnLinesInserted: TLineChangeEvent read fOnLinesInserted write fOnLinesInserted;
    property OnLinesDeleted:  TLineChangeEvent read fOnLinesDeleted  write fOnLinesDeleted;
  end;

implementation

procedure TSEValidationPlugin.AfterPaint(aCanvas: TCanvas; const aClip: TRect;
                                         aFirstLine, aLastLine: Integer);
begin
  if assigned(fOnGutterPaint) then
    fOnGutterPaint(aCanvas, aClip, aFirstLine, aLastLine);
end;

procedure TSEValidationPlugin.LinesInserted(aFirstLine, aCount: Integer);
begin
  if assigned(fOnLinesInserted) then
    fOnLinesInserted(aFirstLine, aCount);
end;

procedure TSEValidationPlugin.LinesDeleted(aFirstLine, aCount: Integer);
begin
  if assigned(fOnLinesDeleted) then
    fOnLinesDeleted(aFirstLine, aCount);
end;

end.
