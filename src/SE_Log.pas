unit SE_Log;

interface
uses
  SysUtils, Classes;

type
  //Logging system
  TSELog = class
  private
    fl:            TextFile;
    fLogPath:      UnicodeString;
    fFirstTick,
    fPreviousTick: Cardinal;
    fPreviousDate: TDateTime;
    procedure AddLineTime(const aText: string);
    procedure AddLineNoTime(const aText: string);
    function GetTimeSince(aTime: Cardinal): Cardinal;
  public
    constructor Create(const aPath: string);
    procedure InitLog;
    // AppendLog adds the line to Log along with time passed since previous line added
    procedure AddTime(const aText: string);
    // AddToLog simply adds the text
    procedure AddNoTime(const aText: UnicodeString);
    // Add line if TestValue=false
    procedure AddAssert(const aMessageText: string);
    procedure DeleteOldLogs;
    property LogPath: UnicodeString read fLogPath; //Used by dedicated server
  end;

var
  gLog: TSELog;

implementation
uses
  MMSystem, SE_Globals;

//New thread, in which old logs are deleted (used internally)
type
  TSEOldLogDeleter = class(TThread)
  private
    fPathToLogs: string;
  public
    constructor Create(const aPathToLogs: string);
    procedure Execute; override;
  end;


{ TKMOldLogsDeleter }
constructor TSEOldLogDeleter.Create(const aPathToLogs: string);
begin
  // Thread isn't started until all constructors have run to completion
  // so Create(False) may be put in front as well
  inherited Create(False);

  // Must set these values BEFORE starting the thread
  FreeOnTerminate := True; // Object can be automatically removed after its termination
  fPathToLogs     := aPathToLogs;
end;


procedure TSEOldLogDeleter.Execute;
var
  SearchRec:    TSearchRec;
  fileDateTime: TDateTime;
begin
  if not DirectoryExists(fPathToLogs) then Exit;

  if FindFirst(fPathToLogs + 'SE_*.log', faAnyFile - faDirectory, SearchRec) = 0 then
  repeat
    Assert(FileAge(fPathToLogs + SearchRec.Name, fileDateTime), 'How is that it does not exists any more?');

    if (Abs(Now - fileDateTime) > 14) then
      DeleteFile(fPathToLogs + SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


{ TKMLog }
constructor TSELog.Create(const aPath: string);
begin
  inherited Create;
  fLogPath      := aPath;
  fFirstTick    := TimeGetTime;
  fPreviousTick := TimeGetTime;
  InitLog;
end;


procedure TSELog.InitLog;
begin
  ForceDirectories(ExtractFilePath((fLogPath)));

  AssignFile(fl, fLogPath);
  Rewrite(fl);
  //           hh:nn:ss.zzz 12345.678s 1234567ms     text-text-text
  WriteLn(fl, '   Timestamp    Elapsed     Delta     Description');
  CloseFile(fl);

  AddLineTime('Log is up and running. SE version: ' + FULL_VERSION);
end;


//Run thread to delete old logs.
procedure TSELog.DeleteOldLogs;
begin
  if Self = nil then
    Exit;

  //No need to remember the instance, it's set to FreeOnTerminate
  TSEOldLogDeleter.Create(ExtractFilePath(fLogPath));
end;


//Add line with timestamp
procedure TSELog.AddLineTime(const aText: UnicodeString);
begin
  if not FileExists(fLogPath) then
    InitLog;  // Recreate log file, if it was deleted

  AssignFile(fl, fLogPath);
  Append(fl);

  //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
  if Abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
  begin
    WriteLn(fl, '========================');
    WriteLn(fl, '    Date: ' + FormatDateTime('yyyy/mm/dd', Now));
    WriteLn(fl, '========================');
  end;

  WriteLn(fl, Format('%12s %9.3fs %7dms     %s', [
                FormatDateTime('hh:nn:ss.zzz', Now),
                GetTimeSince(fFirstTick) / 1000,
                GetTimeSince(fPreviousTick),
                aText]));
  CloseFile(fl);
  fPreviousTick := TimeGetTime;
  fPreviousDate := Now;
end;


//Same line but without timestamp
procedure TSELog.AddLineNoTime(const aText: UnicodeString);
begin
  if not FileExists(fLogPath) then
    InitLog;  // Recreate log file, if it was deleted

  AssignFile(fl, fLogPath);
  Append(fl);
  WriteLn(fl, '                                      ' + aText);
  CloseFile(fl);
end;

function TSELog.GetTimeSince(aTime: Cardinal): Cardinal;
begin
  //TimeGet will loop back to zero after ~49 days since system start
  Result := (Int64(TimeGetTime) - Int64(aTime) + Int64(High(Cardinal))) mod Int64(High(Cardinal));
end;


procedure TSELog.AddTime(const aText: UnicodeString);
begin
  if Self = nil then
    Exit;

  AddLineTime(aText);
end;


procedure TSELog.AddNoTime(const aText: UnicodeString);
begin
  if Self = nil then
    Exit;

  AddLineNoTime(aText);
end;


procedure TSELog.AddAssert(const aMessageText: UnicodeString);
begin
  if Self = nil then
    Exit;

  AddLineNoTime('ASSERTION FAILED! Msg: ' + aMessageText);
  raise Exception.Create('ASSERTION FAILED! Msg: ' + aMessageText);
end;

end.
