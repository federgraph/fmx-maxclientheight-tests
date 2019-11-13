unit FrmMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    mch: Integer;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
var
  scale: single;
  MaxClientHeight: Integer;
  h: Integer;
  ch: Integer;
begin
  scale := self.Handle.Scale;
  h := Height;
  ch := ClientHeight;

  MaxClientHeight := Screen.WorkAreaHeight - Round(scale * (h - ch));
  mch := Round(MaxClientHeight / scale);

  { MaxClientHeight in 'Pixel-Units' }
  mch := Round(MaxClientHeight / scale);
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  ClientWidth := Round(0.8 * mch); // must be <> mch
  ClientHeight := Round(1.1 * mch); // must be > mch
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  ClientWidth := mch;
  ClientHeight := Round(2 * mch); // must be > mch
end;

end.
