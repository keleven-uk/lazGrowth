unit Main;

{$mode objfpc}{$H+}

interface

{
Windows unit also has TBitmap, but it represents Windows bitmap handle
instead of LCL TBitmap class. Simply reorder your uses clause, ensure
Windows comes before Graphics.
}
uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Spin, uPointStore, Windows, uOptions, formAbout, formhelp,
  formLicence, Graphics;

type

  { TfrmGrowth }

  TfrmGrowth = class(TForm)
    btnStart        : TButton;
    btnClose        : TButton;
    btnLoad         : TButton;
    btnSave         : TButton;
    chckBxShowTail  : TCheckBox;
    lblTailLength   : TLabel;
    mnuFile         : TMenuItem;
    mnuhelp         : TMenuItem;
    mnuItmExit      : TMenuItem;
    mnuItmHelp      : TMenuItem;
    mnuItmLicense   : TMenuItem;
    mnuItmAbout     : TMenuItem;
    mnuMain         : TMainMenu;
    OpenDialog1     : TOpenDialog;
    pnlGrow         : TPanel;
    Panel2          : TPanel;
    spnEdtTailLength: TSpinEdit;
    StatusBar1      : TStatusBar;

    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure chckBxShowTailChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuItmClick(Sender: TObject);
    procedure spnEdtTailLengthChange(Sender: TObject);
  private
    procedure grow;
    procedure move(VAR x: Integer; VAR y:integer);
    procedure clearTail;
    procedure showTail(x: integer; y:integer);
    procedure loadGrowth;
    function minsPerHit(hits: integer; elapsedTime: int64):double;
  public

  end;

CONST
  MAX_HEIGHT = 600;
  MAX_WIDTH  = 800;
var
  frmGrowth   : TfrmGrowth;
  userOptions : Options;
  abort       : boolean;
  ps          : pointStore;          //  Stores the points of the random walk.
  hs          : pointStore;          //  Stores the points of the growth.
  fileName    : string;
  runningTime : int64;
  appStartTime: int64;          //  used by formAbout to determine how long the app has been running.
implementation

{$R *.lfm}

{ TfrmGrowth }


procedure TfrmGrowth.FormCreate(Sender: TObject);
begin
  ps := pointStore.Create;
  hs := pointStore.Create;

  Randomize;  //  This way we generate a new sequence every time the program is run

  // create options file as c:\Users\<user>\AppData\Local\lazGrowth\Options.xml
  userOptions := Options.Create;

  frmGrowth.Top  := UserOptions.formTop;
  frmGrowth.Left := UserOptions.formLeft;

  chckBxShowTail.checked   := userOptions.showTail;
  lblTailLength.Enabled    := userOptions.showTail;
  spnEdtTailLength.Enabled := userOptions.showTail;
  btnSave.Enabled          := false;

  appStartTime   := GetTickCount64;  //  tick count when application starts.
  runningTime    := 0;
  fileName       := format('%s_%s.txt', [userOptions.productName, FormatDateTime('DDMMMMYYYY', now)]);
  pnlGrow.Height := MAX_HEIGHT;
  pnlGrow.Width  := MAX_WIDTH;
  pnlGrow.Color  := clBlack;
  abort          := false;
end;

procedure TfrmGrowth.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  Free stuff when closing.    }
begin
  UserOptions.formTop  := frmGrowth.Top;
  UserOptions.formLeft := frmGrowth.Left;

  userOptions.writeCurrentOptions;  // write out options file

  userOptions.Free;
  ps.Free;
  hs.Free;

  CloseAction:= caFree;
end;
//
//  control stuff
//
procedure TfrmGrowth.btnCloseClick(Sender: TObject);
{  Can either be used to stop or close the app.
   If the app is stopped it should be able to be re-started.
}
begin
  if btnClose.Caption = 'Stop' then
    begin
      btnClose.Caption := 'Close';
      btnStart.Enabled := true;
      abort            := true;
    end
  else
    close;
end;

procedure TfrmGrowth.btnLoadClick(Sender: TObject);
{  If executed, a new growth is created form a saved text file.

   NO ERROR CHECKING YET.
}
begin
  if OpenDialog1.Execute then
    begin
      hs.load(OpenDialog1.FileName);
      loadGrowth;
      btnLoad.Enabled := false;
      runningTime     := hs.Elapsed;
      StatusBar1.Panels.Items[0].Text := format('hits = %d  mins per hit = %f',
                                [hs.Index, minsPerHit(hs.Index, runningTime)]);
    end;

  btnSave.Enabled := true;
end;

procedure TfrmGrowth.btnSaveClick(Sender: TObject);
{  Saves the current growth to a BMP file.    }
VAR
  bitmap    : TBitmap;
  rectangle : TRect;
  bitMapName: string;
begin
  btnSave.Enabled := false;

  bitmap := TBitmap.Create;
  bitmap.SetSize(pnlGrow.Canvas.Width, pnlGrow.Canvas.Height);   //  Important.
  bitmap.Canvas.Brush.Color := clBlack;                          //  maybe not needed.

  rectangle := TRect.Create(0, 0, pnlGrow.Canvas.Width, pnlGrow.Canvas.Height);

  bitmap.Canvas.CopyRect(rectangle, pnlGrow.Canvas, rectangle);

  bitMapName := format('%s_%s.bmp', [userOptions.productName, FormatDateTime('ddmmyyy_hhnnss', now)]);
  bitmap.SaveToFile(bitMapName);

  bitmap.Free;

  btnSave.Enabled := true;
end;

procedure TfrmGrowth.btnStartClick(Sender: TObject);
{  Start the growth, the close button is now used to stop the app.    }
begin
  btnClose.Caption := 'Stop';
  btnStart.Enabled := false;
  btnLoad.Enabled  := false;
  btnSave.Enabled  := true;
  grow;
end;

procedure TfrmGrowth.chckBxShowTailChange(Sender: TObject);
{  Amended use options to show tail or not.}
begin
  userOptions.showTail     := chckBxShowTail.checked;
  lblTailLength.Enabled    := chckBxShowTail.checked;
  spnEdtTailLength.Enabled := chckBxShowTail.checked;

  userOptions.writeCurrentOptions;
  clearTail;                      //  Clear the tail.
end;

procedure TfrmGrowth.spnEdtTailLengthChange(Sender: TObject);
begin
  userOptions.tailLength := spnEdtTailLength.Value;
  userOptions.writeCurrentOptions;
  clearTail;                      //  Clear the tail.
end;

//
//  Drawing stuff
//
procedure TfrmGrowth.grow;
{  Generates a random walk from top to bottom, by repeatedly calling move.
   If the pixel landed on is part of the existing growth, the previous
   position in the random walk then becomes part of the growth [a hit].

   When a hit is found, the random walk then start again and the
   hit is added to the hitStore and the pointStore is cleared.
}
VAR
  x, y    : integer;
  x1, y1  : integer;
  hits    : integer;
  growTime: int64;
  p       : TPoint;
  elapsedTime: integer;
begin
  x         := MAX_WIDTH DIV 2;     //  Starting position of the random walk.
  y         := 0;
  hits      := hs.Index;

  pnlGrow.Canvas.Pen.Color    := clLime;
  pnlGrow.Canvas.Pen.Width    := 1;
  pnlGrow.Canvas.Pen.Style    := psSolid;
  pnlGrow.Canvas.Line(0, MAX_HEIGHT-5, MAX_WIDTH, MAX_HEIGHT-5);

  pnlGrow.Canvas.Pixels[x, y] := clGreen;

  growTime    := GetTickCount64;
  repeat
    Application.Processmessages; //  Look outside the app, so PC is not sluggish.

    x1 := x;
    y1 := y;

    move(x, y);                   //  Generate the next point in the random walk.

    if pnlGrow.Canvas.Pixels[x, y] = clLime then  //  we have a hit.
      begin
        elapsedTime := (GetTickCount64 - growTime);
        pnlGrow.Canvas.Pixels[x1, y1] := clLime;
        p.x := x1;
        p.y := y1;
        hs.push(p, elapsedTime);                  //  Store the new hit point.
        hs.save(fileName);                        //  save hitStore to text file.
        x := MAX_WIDTH DIV 2;                     //  Reset the starting position of the random walk.
        y := 0;
        inc(hits);
        StatusBar1.Panels.Items[0].Text := format('hits = %d  mins per hit = %f',
                                          [hits, minsPerHit(hits, elapsedTime)]);
        if userOptions.showTail then clearTail;         //  Clear the tail after a hit.
        ps.clear;                                       //  Clear the pointStore [random walk].
        growTime := GetTickCount64;
      end
    else            //  we have a miss.
    begin
      p.x := x;
      p.y := y;
      ps.push(p, 0);                 //  Store the next point in the random walk.
                                     //  No need to store time for the random walk.

      if userOptions.showTail then showTail(x, y);
     end;   //  pnlGrow.Canvas.Pixels[x, y] = clLime then
  until abort;
end;

procedure TfrmGrowth.showTail(x: integer; y:integer);
{  Draws the tail, if needed.    }
VAR
  p: TPoint;
begin
  pnlGrow.Canvas.Pixels[x, y] := clGreen;

  p := ps.pop(ps.Index - userOptions.tailLength);   //  Request the end of the tail
                                                    //  and set to black if valid.
  if p.x <> -1 then pnlGrow.Canvas.Pixels[p.x, p.y] := clBlack;         ;
end;

procedure TfrmGrowth.clearTail;
{  When we have a hit, that is we have added to the growth the
   pointStore is cleared, this will leave the last tail on the screen.
   So just before we clear the pointsStore, this routine clears the
   remaining points of the random walk.}
var
  f: integer;
  p: TPoint;
begin
  if ps.Index = 0 then exit;
  for f := (ps.Index-2) downto (ps.Index - userOptions.tailLength) do
  begin
    p := ps.pop(f);
    if p.x <> -1 then pnlGrow.Canvas.Pixels[p.x, p.y] := clBlack;
  end;
end;

function TfrmGrowth.minsPerHit(hits: integer; elapsedTime: int64):double;
{  Calculates of long it takes for each hit - in minutes per hit.
   NB this an average.
}
VAR
  noOfsecs : integer;
  noOfMins : integer;
begin
  runningTime += elapsedTime;
  noOfsecs    := runningTime DIV 1000;
  noOfMins    := noOfSecs DIV 60;

  result := noOfMins / hits;
end;

procedure TfrmGrowth.move(VAR x: Integer; VAR y:integer);
{  Moves the point in a random walk.

   x=0, y=0 is top left hand corner.
   x = 0 -> MAX_WIDTH
   y = 0 |
         | MAX_HEIGHT

   Currently in eight directions.

                            7  0  1
                             \ | /
                           6 -- -- 2
                             / | \
                            5  4  3
}
VAR
  direction: integer;
begin
  direction := Random(8);      //  pick a random number 0 - 3

  case direction of
    0: dec(y);
    1:
      begin
        dec(y);
        inc(x);
      end;
    2: inc(x);
    3:
      begin
        inc(x);
        inc(y);
      end;
    4:inc(y)      ;
    5:
      begin
        inc(y);
        dec(x);
      end;
    6: dec(x);
    7:
      begin
        dec(x);
        dec(y);
      end;
  end;  //  case direction of

  //  check for limit conditions.
  if x < 0 then x := 0;
  if y < 0 then y := 0;
  if x > MAX_WIDTH  then x := MAX_WIDTH;
  if y > MAX_HEIGHT then y := MAX_HEIGHT;
end;

procedure TfrmGrowth.loadGrowth;
{  Draws a growth from an existing pointStore.
   This could be called when the program is first loaded.
}
VAR
  f: integer;
  p: TPoint;
begin
  if hs.Index = 0 then exit;

  for f := 0 to hs.Index - 1 do
  begin
    p := hs.pop(f);
    pnlGrow.Canvas.Pixels[p.x, p.y] := clLime;
  end;
end;

//
// ********************************************************* Menu Items *********
//
procedure TfrmGrowth.mnuItmClick(Sender: TObject);
{  A generic click routine called by each menu item.

   The action of the menu is determined from the item name.
}
VAR
  itemName   : string;
begin
  itemName := '';

  //  set the appropriate name.
  if (Sender is TMenuItem) then
    itemName := TMenuItem(Sender).Name;

  if itemName = '' then exit;    //  not called by a TMenuItem

  case itemName of
  // ********************************************************* File Menu *********
  'mnuItmExit': close;
  // ********************************************************* Help Menu *********
  'mnuItmHelp':
  begin
    frmhelp := TfrmHelp.Create(Nil);
    frmhelp.ShowModal;
    FreeAndNil(frmHelp);
  end;
  'mnuItmAbout':                                                      //  Calls the About screen.
  begin
    frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
    frmAbout.ShowModal;                 //frmAbout is displayed
    FreeAndNil(frmAbout);               //frmAbout is released
  end;
  'mnuItmLicense':                                                      //  Calls the License screen.
  begin
    frmLicence := TfrmLicence.Create(Nil);
    frmLicence.ShowModal;
    FreeAndNil(frmLicence);
  end;
  end;
end;

end.

