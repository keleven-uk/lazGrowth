unit formAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls, ShellApi, growthUntils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit         : TButton;
    btnAboutMSInfo       : TButton;
    Image1               : TImage;
    lblProgramName       : TLabel;
    lblProgramDescription: TLabel;
    lblProgrammer        : TLabel;
    lblSysUpTime         : TLabel;
    lblAppUpTime         : TLabel;
    lblSystemUpTime      : TLabel;
    lblApplicationUpTime : TLabel;
    LstBxInfo            : TListBox;
    LstBxDiscSpace       : TListBox;
    Panel1               : TPanel;
    Panel2               : TPanel;
    Panel3               : TPanel;
    Panel4               : TPanel;
    tmrUpTime            : TTimer;

    procedure btnAboutExitClick(Sender: TObject);
    procedure btnAboutMSInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrUpTimeTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

uses
  Main;

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.btnAboutExitClick(Sender: TObject);
{  Close About form.  }
begin
  tmrUpTime.Enabled := false;
  Close;
end;

procedure TfrmAbout.btnAboutMSinfoClick(Sender: TObject);
{  Run the external application MSinfo.
   If ShellExecute returns 32 or less this indicates an error, just inform the user.
}
begin
  if ShellExecute(0, nil, PChar('"msinfo32.exe"'), nil, nil, 1) < 33 then
    ShowMessage('ERROR : running MSinfo');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  dskSize  : string;
  dskFree  : string;
  message  : string;
  cmpDate  : string;
  cmpUkDate: string;
  i        : integer;
begin
  tmrUpTime.Enabled             := True;
  lblAppUpTime.Caption          := getUpTime('Application');
  lblSysUpTime.Caption          := getUpTime('System');
  lblProgrammer.Caption         := userOptions.legalCopyright;
  lblProgramDescription.Caption := userOptions.productName;

  //  {$I %DATE%} returns the compile date, but in american [ignores local date format]
  //  So, we string slice it to give good old english date format.
  cmpDate               := {$I %DATE%};
  cmpUkDate             := format('%s/%s/%s', [copy(cmpDate, 9, 2),
                                               copy(cmpDate, 6, 2),
                                               copy(cmpDate, 1, 4)]);

  lstBxInfo.Items.add(userOptions.fileDescription);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(format('%s Build   :: %s', [userOptions.productName, userOptions.productVersion]));
  lstBxInfo.Items.add(format('%s Version :: %s', [userOptions.productName, userOptions.fileVersion]));
  lstBxInfo.Items.add(format('%s Built   :: %s', [userOptions.productName, cmpUkDate + ' @ ' + {$I %TIME%}]));

  {$ifdef WIN32}
    lstBxInfo.Items.add(format('Built with 32 bit Lazarus Version :: %s', [lcl_version]));
  {$else}
    lstBxInfo.Items.add(format('Built with 64 bit Lazarus Version :: %s', [lcl_version]));
  {$endif}
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(getWindowsVersion);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(userOptions.CompanyName);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add(userOptions.Comments);
  lstBxInfo.Items.add('');
  lstBxInfo.Items.add('App Dir : ' + ExtractFilePath(Application.ExeName));

  // Display the free space on drives B, C, D, E, F, where present
  for i := 2 to 10 do
  begin
    dskFree := FloatToStrF(DiskFree(i) / 1073741824, ffFixed, 3, 2);
    dskSize := FloatToStrF(DiskSize(i) / 1073741824, ffFixed, 3, 2);

    if DiskSize(i) >= 0 then
    begin
      message := format(' Disk %s : Free / Size :: %s / %s Gbytes', [Chr(i + 64), dskFree, dskSize]);
      LstBxDiscSpace.Items.Add(message);
    end;
  end;

end;

procedure TfrmAbout.tmrUpTimeTimer(Sender: TObject);
{  Update the labels in real time.    }
begin
  lblAppUpTime.Caption := getUpTime('Application');
  lblSysUpTime.Caption := getUpTime('System');
end;

end.

