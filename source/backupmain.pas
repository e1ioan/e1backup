unit backupmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  Data.Cloud.CloudAPI, Data.Cloud.AmazonAPI, IndyPeerImpl, IPPeerClient;

type
  TE1BackupS = class(TService)
    AmazonConnectionInfo1: TAmazonConnectionInfo;
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
  private
    { Private declarations }
    FAmazonEnabled: boolean;
    FAmazonKey: string;
    FAmazonSecret: string;
    FAmazonBucket: string;
    FStorageEndpoint: string;
    FBackupTime: string;
    FBackupDestFolder: string;
    FAppPath: string;
    FLstBackupFolders: TStringList;
  public
    { Public declarations }
    function UploadFile(AFileName: string): Boolean;
    function GetServiceController: TServiceController; override;
    procedure ZipFolder(AZipFile: string; AFolder: string);
    procedure Log(s: string);
  end;

var
  E1BackupS: TE1BackupS;

implementation

{$R *.DFM}
uses
  System.NetEncoding,
  Winapi.ActiveX,
  IniFiles,
  DateUtils,
  IOUtils,
  StrUtils,
  Zip;


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  E1BackupS.Controller(CtrlCode);
end;

function TE1BackupS.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TE1BackupS.Log(s: string);
var
  F: TextFile;
  fn: string;
begin
  if s = '' then
    Exit;
  fn := FAppPath + 'log-' + FormatDateTime('yyyy', Now) + '.txt';
{$I+}
  try
    AssignFile(F, fn);
    if FileExists(fn) then
      Append(F)
    else
      Rewrite(F);
    Writeln(F, DateTimeToStr(Now) + ': ' + s);
    CloseFile(F);
  except
  end;
{$I-}
end;

procedure TE1BackupS.ZipFolder(AZipFile: string; AFolder: string);
var
  ZipFile: TZipFile;
  ZipDocument: string;
  ArchiveName: string;
  fName: string;
  I: Integer;
begin
  try
    ZipDocument := AZipFile;
    ZipFile := TZipFile.Create;
    try
      if FileExists(ZipDocument) then
        ZipFile.Open(ZipDocument, zmReadWrite)
      else
        ZipFile.Open(ZipDocument, zmWrite);
      for fName in TDirectory.GetFiles(AFolder, '*.*', TSearchOption.soAllDirectories)  do
      begin
        ArchiveName := Copy(fName, Length(ExtractFileDrive(fName)) + Length(DriveDelim) + Length(PathDelim) , MAX_PATH);
        ZipFile.Add(fName, ArchiveName);
        Sleep(0);
      end;
      ZipFile.Close;
    finally
      ZipFile.Free;
    end;
  except
    on e: Exception do
      Log(e.Message);
  end;
end;

function TE1BackupS.UploadFile(AFileName: string): Boolean;
var
  FileContent: TBytes;
  fReader: TBinaryReader;
  s3Service: TAmazonStorageService;
  ResponseInfo: TCloudResponseInfo;
  DestinationFileName: string;
  Response: string;
  Msg: string;
begin
  CoInitialize(nil);
  result := false;
  { AccessKeyID }
  AmazonConnectionInfo1.AccountName := FAmazonKey;
 { SecretAccessKeyID }
  AmazonConnectionInfo1.AccountKey := FAmazonSecret;
  AmazonConnectionInfo1.StorageEndpoint := FStorageEndpoint;
  s3Service := TAmazonStorageService.Create(AmazonConnectionInfo1);
  Response := s3Service.ListMultipartUploadsXML(FAmazonBucket, nil);
  ResponseInfo := TCloudResponseInfo.Create;
  try
    if ContainsText(Response, '<error>') then
    begin
      Log(Response);
      Result := false;
    end
    else
    begin
      fReader := TBinaryReader.Create(AFileName);
      try
        FileContent := fReader.ReadBytes(fReader.BaseStream.Size);
      finally
        fReader.Free;
      end;
      DestinationFileName := TNetEncoding.URL.Encode(ExtractFileName(AFileName));
      if s3Service.UploadObject(FAmazonBucket, DestinationFileName, FileContent, false, nil, nil, amzbaPublicRead, ResponseInfo) then
        Log(DestinationFileName + ' uploaded sucessfully')
      else
      begin
        Msg := 'Error trying to upload + DestinationFileName + into bucket ' + FAmazonBucket + ': ';
        if ResponseInfo.StatusCode = 404 then
          Msg := Msg + ' The bucket does not exists ';
        Log(Msg + ResponseInfo.StatusMessage);
      end;
    end;
  finally
    ResponseInfo.Free;
    s3Service.Free;
    CoUninitialize;
  end;
end;

procedure TE1BackupS.ServiceCreate(Sender: TObject);
begin
  FAppPath:= ExtractFilePath(ParamStr(0));
  FBackupDestFolder := FAppPath + 'backups' + PathDelim;

  FLstBackupFolders := TStringList.Create;

  with TIniFile.Create(FAppPath + 'ini' + PathDelim + 'params.ini') do
  try
    FAmazonEnabled := (ReadInteger('Amazon', 'Enabled', 0) = 1);
    FAmazonKey := ReadString('Amazon', 'Key', '');
    FAmazonSecret := ReadString('Amazon', 'Secret', '');
    FAmazonBucket := ReadString('Amazon', 'Bucket', '');
    FStorageEndpoint := ReadString('Amazon', 'StorageEndpoint', 's3-us-west-2.amazonaws.com');
    FBackupTime := ReadString('Schedule', 'StartTime', '12:00 AM');
    ReadSectionValues('Backup', FLstBackupFolders);
  finally
    Free;
  end;
end;

procedure TE1BackupS.ServiceDestroy(Sender: TObject);
begin
  FLstBackupFolders.Free;
end;

function IsFileInUse(fName: string) : boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(fName) then begin
    Exit;
  end;

  HFileRes := CreateFile(PChar(fName)
    ,GENERIC_READ or GENERIC_WRITE
    ,0
    ,nil
    ,OPEN_EXISTING
    ,FILE_ATTRIBUTE_NORMAL
    ,0);

  Result := (HFileRes = INVALID_HANDLE_VALUE);

  if not(Result) then begin
    CloseHandle(HFileRes);
  end;
end;

{.$DEFINE _DEBUG_ME_}

procedure TE1BackupS.ServiceExecute(Sender: TService);
var
  lastbackuptime: TDateTime;
  backupfname, tmpstr: string;
  I: Integer;
begin
  lastbackuptime := IncDay(Now, -5);
  { Execute until we're told to stop }
{$IFNDEF _DEBUG_ME_}
  while not Terminated do
{$ENDIF}
  begin
    try
      Sleep(MSecsPerSec);
{$IFNDEF _DEBUG_ME_}
      if SameText(FBackupTime, FormatDateTime('h:nn AM/PM', Now)) and not IsToday(lastbackuptime) then
{$ELSE}
      // if _DEBUG_ME_ defined, wait for 20 seconds - enugh time to attach the debugger to the process
      Sleep(20 * MSecsPerSec);
{$ENDIF}
      begin
        lastbackuptime := Today;
        {check if the back-up dir exists, if not, create it}
        if not SysUtils.DirectoryExists(FBackupDestFolder) then
          SysUtils.CreateDir(FBackupDestFolder);

        {do the backup}
        Log('Backup started...');
        for I := 0 to FLstBackupFolders.Count - 1 do
        begin
          {set the backup name}
          tmpstr := StringReplace(FLstBackupFolders.ValueFromIndex[i], '\', '_', [rfReplaceAll]);
          tmpstr := StringReplace(tmpstr, ':', '', [rfReplaceAll]);

          if EndOfTheDay(Today) = EndOfTheYear(Today) then
            backupfname := FBackupDestFolder + tmpstr + '-YR' + IntToStr(YearOf(Now)) + '.zip'
          else if EndOfTheDay(Today) = EndOfTheMonth(Today) then
            backupfname := FBackupDestFolder + tmpstr + '-MT' + IntToStr(MonthOfTheYear(Now)) + '.zip'
          else if EndOfTheDay(Today) = EndOfTheWeek(Today) then
            backupfname := FBackupDestFolder + tmpstr + '-WK' + IntToStr(WeekOfTheMonth(Now)) + '.zip'
          else
            backupfname := FBackupDestFolder + tmpstr + '-DL' + IntToStr(DayOfTheWeek(Now)) + '.zip';

          Log('Current file: ' + ExtractFileName(backupfname));
          if FileExists(backupfname) then
            SysUtils.DeleteFile(backupfname);

          ZipFolder(backupfname, FLstBackupFolders.Values['Folder'+IntToStr(I+1)]);

          // wait a little to make sure the zip file is not in use anymore
          sleep(20 * MSecsPerSec); // wait 20 seconds

          // check if file is in use every 20 seconds
          while IsFileInUse(backupfname) do
            sleep(20 * MSecsPerSec);

          if FAmazonEnabled then
          begin
            // TODO : Upload the zip files in a separate thread
            if UploadFile(backupfname) then
            begin
              Log('S3 backup done: '+ ExtractFileName(backupfname));
              // check if file is in use every 20 seconds
              while IsFileInUse(backupfname) do
                sleep(20 * MSecsPerSec);
              SysUtils.DeleteFile(backupfname);
            end;
          end;
        end;

        Log('Backup done.');
      end;
      ServiceThread.ProcessRequests(FALSE);
    except
      on e: Exception do
        Log(e.Message);
    end;
  end; { while not Terminated }
end;

procedure TE1BackupS.ServiceStart(Sender: TService; var Started: Boolean);
begin
  { tell the OS that we're starting }
  Started := TRUE;
end;

procedure TE1BackupS.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  { Tell the service thread to terminate }
  ServiceThread.Terminate;
  { Tell the OS that we're stopping }
  Stopped := TRUE;
end;

end.

