program e1backup;

uses
  SvcMgr,
  backupmain in 'backupmain.pas' {E1BackupS: TService};

{$R *.RES}
{$R '..\uac\administrator.res' '..\uac\administrator.rc'}

begin
  Application.Initialize;
  Application.CreateForm(TE1BackupS, E1BackupS);
  Application.Run;
end.
