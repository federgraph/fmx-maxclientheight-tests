program ClientAreaTests;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
