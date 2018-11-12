unit formLicence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmLicence }

  TfrmLicence = class(TForm)
    btnLicenceExit: TButton;
    lblComments   : TLabel;
    lblCopyRight  : TLabel;
    lblVersion    : TLabel;
    mmoLicence    : TMemo;
    Panel1        : TPanel;
    Panel2        : TPanel;

    procedure btnLicenceExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLicence: TfrmLicence;

implementation

uses
  Main;

{$R *.lfm}

{ TfrmLicence }

procedure TfrmLicence.FormCreate(Sender: TObject);
begin
  try
    mmoLicence.Lines.LoadFromFile('GNU GENERAL PUBLIC LICENSE.txt');
  except
    on Exception do begin
      mmoLicence.Append(userOptions.productName);
      mmoLicence.Append('');
      mmoLicence.Append(' help License not found.');
      mmoLicence.Append('');
      mmoLicence.Append(' The application is issued under the GNU GENERAL PUBLIC LICENSE.');
    end;
  end;

  lblComments.Caption  := format('%s :: %s', [userOptions.productName, userOptions.fileDescription]);
  lblCopyRight.Caption := userOptions.legalCopyright;
  lblVersion.Caption   := format('%s Version :: %s', [userOptions.productName, userOptions.fileVersion]);
end;

procedure TfrmLicence.btnLicenceExitClick(Sender: TObject);
begin
  Close;
end;

end.

