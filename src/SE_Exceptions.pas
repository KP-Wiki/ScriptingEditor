unit SE_Exceptions;

interface
uses
  SysUtils, MadExcept, madNVAssistant;

type
  TSEExceptions = class
  private
    procedure DoException(const ExceptIntf: IMEException; var Handled: boolean);
  public
    constructor Create;
  end;

var
  fExceptions : TSEExceptions;

implementation
uses
  SE_Log, SE_GLOBALS;

{ TKMExceptions }
constructor TSEExceptions.Create;
begin
  inherited;
  RegisterExceptionHandler(DoException, stTrySyncCallAlways);
  MESettings.HttpServer := 'https://kp-wiki.org/Crashes/upload.php?vers=' + VERSION;
end;


procedure TSEExceptions.DoException(const ExceptIntf: IMEException; var Handled: boolean);
var
  LogMessage, CrashFile: string;
begin
  if gLog = nil then
    Exit; //Could crash very early before even the log file is created

  //It's nice to know when the exception happened in our log if the user decides to play on and sends the report later
  LogMessage := 'Exception occurred: ' + ExceptIntf.ExceptClass +
                ': '                   + ExceptIntf.ExceptMessage;
  gLog.AddTime(LogMessage);
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime('                                START BUG REPORT                                ');
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime(ExceptIntf.BugReport);
  gLog.AddNoTime('================================================================================');
  gLog.AddNoTime('                                 END BUG REPORT                                 ');
  gLog.AddNoTime('================================================================================');
  ExceptIntf.ExceptMsg     := 'An error occurred in the application. ' +
                              'Please click Send Bug Report so we can investigate this issue. ' +
                              'Thanks for your help!'#13#10#13#10 + LogMessage;
  //We want to add some of our own files to the report
  CrashFile                := 'SE_Crash_' + VERSION + '_' +
                              FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.zip';
  MESettings.BugReportZip  := CrashFile; //Exception info also goes in the zip
  MESettings.ScreenShotZip := CrashFile; //Screenshot also goes in the zip
  ExceptIntf.AdditionalAttachments.Add(gLog.LogPath, '', CrashFile);
end;

end.
