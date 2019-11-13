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
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Memo, FMX.Objects,
  FMX.Menus;

type
  TFormMain = class(TForm)
    MaxBtn: TButton;
    ResetBtn: TButton;
    procedure MaxBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    ML: TStringList;
    fa: Integer;
    FScale: single;
    FMaxClientHeight: Integer;
    WantNormal: Boolean;
    ReportCounter: Integer;
    procedure GotoLandscape;
    procedure GotoPortrait;
    procedure GotoSquare;
    procedure GotoNormal;
    procedure UpdateReport;
    procedure ShowHelp;
    procedure UpdateMemo;
    procedure MoveTop0;
    function AddMenu(M: TMainMenu; Caption: string): TMenuItem;
    procedure InitItem(I: TMenuItem; fa: Integer);
    procedure InitMenu;
  protected
    procedure InitMCH;
    procedure InitHelpText;
    procedure InitMemo;
    procedure InitMemoText;
    procedure InitRectangle;
  public
    Memo: TMemo;
    MemoText: TText;
    MainMenu: TMainMenu;
    Rectangle: TRectangle;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

//uses
//  System.Math,
//  FMX.Platform.Win;

const
  BoolStr: array[Boolean] of string = ('False', 'True');

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FormatSettings.DecimalSeparator := '.';

  ML := TStringList.Create;

  FScale := Handle.Scale;
  InitMCH; // will set FMaxClientHeight

//  InitMemo; //use TMemo for reporting
  InitMemoText; // use TText instead of TMemo

  Caption := 'press h for help';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormMain.InitMCH;
var
  s: single;
  h:Integer;
  ch: Integer;
begin
  s := Handle.Scale;
  h := Height;
  ch := ClientHeight;
  FMaxClientHeight := Screen.WorkAreaHeight - Round(s *(h - ch));
  FMaxClientHeight := Round(FMaxClientHeight / s);
end;

procedure TFormMain.InitMemoText;
begin
  if MemoText <> nil then
    Exit;

  MemoText := TText.Create(self);
  MemoText.Parent := self;
  MemoText.Position.X := 10.0;
  MemoText.Position.Y := 60.0;

  MemoText.TextSettings.WordWrap := False;
  MemoText.AutoSize := True;
  MemoText.Font.Family := 'Consolas';
  MemoText.Font.Size := 14;
  MemoText.TextSettings.FontColor := claBlue;
  MemoText.TextSettings.HorzAlign := TTextAlign.Leading;
  MemoText.TextSettings.VertAlign := TTextAlign.Leading;
end;

procedure TFormMain.InitMemo;
begin
  if Memo <> nil then
    Exit;

  Memo := TMemo.Create(self);
  Memo.Parent := self;
  Memo.Position.Y := 60;
  Memo.Position.X := 10;
  Memo.Width := ClientWidth - 20;
  Memo.Height := ClientHeight - 10 - Memo.Position.Y;

  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 15;
  Memo.TextSettings.FontColor := claBlue;

  Memo.Anchors := [
    TAnchorKind.akLeft,
    TAnchorKind.akTop,
    TAnchorKind.akRight,
    TAnchorKind.akBottom
    ];
end;

procedure TFormMain.InitMenu;
var
  i: Integer;
begin
  if MainMenu <> nil then
    Exit;

  MainMenu := TMainMenu.Create(self);
  MainMenu.Parent := self;
  for i in [1..16] do
    AddMenu(MainMenu,'Menu' + IntToStr(i));
end;

function TFormMain.AddMenu(M: TMainMenu; Caption: string): TMenuItem;
var
  j: Integer;
begin
  result := TMenuItem.Create(M);
  result.Text := Caption;
  M.AddObject(result);
  for j in [1..2] do
  begin
    Inc(fa);
    InitItem(result, fa);
  end;
end;

procedure TFormMain.InitItem(I: TMenuItem; fa: Integer);
var
  t: TMenuItem;
begin
  t := TMenuItem.Create(I);
  t.Width := 50;
  t.Height := 50;
  t.Opacity := 1.0;
  t.Font.Size := 24;
  t.Text := 'Item' + IntToStr(fa);
  t.Enabled := True;
  t.Visible := True;
  t.Tag := Ord(fa);
  I.AddObject(t);
end;

procedure TFormMain.InitRectangle;
begin
  Rectangle := TRectangle.Create(self);
  Rectangle.Parent := self;
  Rectangle.Fill.Color := claNull;
  Rectangle.Stroke.Color := claRed;
  Rectangle.Stroke.Thickness := 5.0;
  Rectangle.Align := TAlignLayout.Client;
end;

procedure TFormMain.MaxBtnClick(Sender: TObject);
begin
  Top := 0;
  Height := Screen.WorkAreaHeight;
end;

procedure TFormMain.ResetBtnClick(Sender: TObject);
begin
  Top := 100;
  Height := 400;
end;

procedure TFormMain.MoveTop0;
begin
  Top := 0;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if KeyChar = 'r' then
  begin
    Top := 100;
    Left := 100;
    Width := 600;
    Height := 500;
  end

  else if KeyChar = '0' then
  begin
    ClientWidth := FMaxClientHeight;
    ClientHeight := FMaxClientHeight;
  end

  else if KeyChar = '1' then
  begin
    ClientWidth := FMaxClientHeight-1;
    ClientHeight := FMaxClientHeight-1;
  end

  else if KeyChar = '6' then
  begin
    ClientWidth := 600;
    ClientHeight := 600;
  end

  else if KeyChar = '7' then
  begin
    ClientWidth := 700;
    ClientHeight := 700;
  end

  else if KeyChar = '8' then
  begin
    ClientWidth := 800;
    ClientHeight := 800;
  end

  else if KeyChar = '9' then
  begin
    ClientWidth := 900;
    ClientHeight := 900;
  end

  else if KeyChar = 'a' then
  begin
    ClientWidth := 1200;
    ClientHeight := 1200;
  end

  else if KeyChar = 'b' then
  begin
    ClientWidth := 1600;
    ClientHeight := 1600;
  end

  else if KeyChar = 'e' then
  begin
    ClientWidth := FMaxClientHeight;
    ClientHeight := FMaxClientHeight * 2; // too big
  end

  else if KeyChar = 'w' then
  begin
    ClientWidth := 1000;
  end

  else if KeyChar = 'n' then
  begin
    ClientWidth := 600;
  end

  else if KeyChar = 't' then MoveTop0
  else if KeyChar = 'l' then GotoLandscape
  else if KeyChar = 'p' then GotoPortrait
  else if KeyChar = 's' then GotoSquare
  else if KeyChar = 'u' then UpdateReport
  else if KeyChar = 'h' then ShowHelp
  else if KeyChar = 'm' then
  begin
    if MainMenu <> nil then
      FreeAndNil(MainMenu)
    else
      InitMenu;
  end
  else if KeyChar = 'g' then
  begin
    WantNormal := not WantNormal;
    UpdateReport;
  end;

//  UpdateReport;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  UpdateReport;
end;

procedure TFormMain.InitHelpText;
begin
  ML.Clear;
  ML.Add('h = Show Help');
  ML.Add('u = Update Report');
  ML.Add('');
  ML.Add('r = Reset');
  ML.Add('t = Move to Top');
  ML.Add('w = Wide');
  ML.Add('n = Narrow');
  ML.Add('');
  ML.Add('l = Landscape');
  ML.Add('s = Square');
  ML.Add('p = Portrait');
  ML.Add('');
  ML.Add('0 = MaxClientHeight Square');
  ML.Add('1 = MaxClientHeight-1 Square');
  ML.Add('6, 7, 8, 9 = square test (600, 600), (700, 700), ... ');
  ML.Add('a = 1200 square');
  ML.Add('b = 1600 square');
  ML.Add('e = (MaxClientHeight, MaxClientHeight * 2)');
  ML.Add('');
  ML.Add('a, b test = toggle between 1200 square and 1600 square');
  ML.Add('e test = e, drag width, e, drag width, ...');
  ML.Add('w, n test = toggle between wide and narrow');
  ML.Add('');
  ML.Add('m = toggle Menu');
  ML.Add('g = toggle WantNormal');
end;

procedure TFormMain.UpdateReport;
var
  scale: single;
  h:Integer;
  ch: Integer;
  wah: Integer;
  MaxClientHeight: Integer;
  mch: Integer;
begin
  Inc(ReportCounter);
  scale := Handle.Scale;
  h := Height;
  ch := ClientHeight;
  wah := Screen.WorkAreaHeight;
  MaxClientHeight := Screen.WorkAreaHeight - Round(scale *(h - ch));
  mch := Round(MaxClientHeight / scale);

  { ClientHeight := mch; // this is the intended use of mch }
  { mch is the actual MaxClientHeight to be used. }

  { ML is a TStringList }
  ML.Clear;
  ML.Add(Format('ReportCounter = %d', [ReportCounter]));
  ML.Add('WantNormal = ' + BoolStr[WantNormal]);
  ML.Add('');
  ML.Add(Format('Handle.Scale = %.1f = scale', [scale]));
  ML.Add(Format('Screen.WorkAreaHeight = %d = wah', [wah]));
  ML.Add(Format('MaxClientHeight = %d', [MaxClientHeight]));
  ML.Add('');
  ML.Add(Format('Screen-W-H = (%d, %d)', [Screen.Width, Screen.Height]));
  ML.Add(Format('(Form)-W-H = (%d, %d)', [Width, Height]));
  ML.Add(Format('Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));

  ML.Add('');
  ML.Add(Format('MaxClientHeight :=  wah - Round(scale * (h - ch));', []));
  ML.Add(Format('           %d := %d - Round(%.1f * (%d - %d));',
    [MaxClientHeight, wah, scale, h, ch]));
  ML.Add('');
  ML.Add(Format('mch = MaxClientHeight / scale = %d', [mch]));

  UpdateMemo;
end;

procedure TFormMain.GotoNormal;
begin
  if WantNormal then
  begin
    Top := 100;
    Left := 100;
    ClientWidth := 800;
    ClientHeight := 600;
  end;
end;

procedure TFormMain.GotoLandscape;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

//  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    // normal screen
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 4 / 3);
    Top := 0;
  end
  else
  begin
    // portrait screen
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth * 3 / 4);
    Left := 0;
  end;
//  FormResizeEnd(nil);
end;

procedure TFormMain.GotoPortrait;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    // normal screen
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 3 / 4);
    Top := 0;
  end
  else
  begin
    // portrait screen
    Width := Screen.WorkAreaWidth;
    ClientHeight := Round(ClientWidth * 4 / 3);
    Left := 0;
    Top := 0;
  end;
//  FormMain.FormResizeEnd(nil);
end;

procedure TFormMain.GotoSquare;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    // normal screen
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight);
    Top := 0;
  end
  else
  begin
    // portrait screen
    Width := Screen.WorkAreaWidth;
    ClientHeight := Round(ClientWidth);
    Left := 0
  end;
//  FormResizeEnd(nil);
end;

procedure TFormMain.ShowHelp;
begin
  InitHelpText;
  UpdateMemo;
end;

procedure TFormMain.UpdateMemo;
begin
  { Memo is a component on the form }
  if Memo <> nil then
    Memo.Text := ML.Text;

  if MemoText <> nil then
    MemoText.Text := ML.Text;
end;

end.
