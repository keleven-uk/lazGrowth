unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, uPointStore, Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart   : TButton;
    btnClose   : TButton;
    btnLoad    : TButton;
    OpenDialog1: TOpenDialog;
    pnlGrow    : TPanel;
    Panel2     : TPanel;
    StatusBar1 : TStatusBar;

    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure grow;
    procedure move(VAR x: Integer; VAR y:integer);
    procedure clearTail;
    procedure loadGrowth;
    function minsPerHit(hits: integer; elapsedTime: int64):double;
  public

  end;

CONST
  MAX_HEIGHT = 600;
  MAX_WIDTH  = 800;
var
  Form1      : TForm1;
  abort      : boolean;
  ps         : pointStore;          //  Stores the points of the random walk.
  hs         : pointStore;          //  Stores the points of the growth.
  tailLength : integer;
  fileName   : string;
  runningTime: int64;
implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;  //  This way we generate a new sequence every time the program is run

  fileName       := format('growth_%s.txt', [FormatDateTime('DDMMMMYYYY', now)]);
  pnlGrow.Height := MAX_HEIGHT;
  pnlGrow.Width  := MAX_WIDTH;
  pnlGrow.Color  := clBlack;
  abort          := false;
  tailLength     := 50;

  ps := pointStore.Create;
  hs := pointStore.Create;

  runningTime := 0;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  Free stuff when closing.    }
begin
  ps.Free;
  hs.Free;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
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

procedure TForm1.btnLoadClick(Sender: TObject);
{  If exewcuted a new growth is created form a saved text file.

   NO ERROR CHECKING YET.
}
begin
  if OpenDialog1.Execute then
    begin
      hs.load(OpenDialog1.FileName);
      loadGrowth;
      btnLoad.Enabled := false;
      runningTime := hs.Elapsed;
      StatusBar1.Panels.Items[0].Text := format('hits = %d  mins per hit = %f',
                                [hs.Index, minsPerHit(hs.Index, runningTime)]);
    end
end;

procedure TForm1.btnStartClick(Sender: TObject);
{  Start the growth, the close button is now used to stop the app.    }
begin
  btnClose.Caption := 'Stop';
  btnStart.Enabled := false;
  btnLoad.Enabled  := false;
  grow;
end;

procedure TForm1.grow;
{  Generates a random walk from top to bottom, by repeatly calling move.
   If the pixel landed on is part of the existing growthm, the previous
   position in the random walk then becomes part of the growth [a hit].

   When a hit is found, the random walk then start again and the
   hit is added to the hitStore and the pointStore is cleared.
}
VAR
  x, y    : integer;
  x1, y1  : integer;
  hits    : integer;
  growTime: int64;
  p, p1   : TPoint;
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
  growTime := GetTickCount64;
  repeat
    Application.Processmessages; //  Look outside the app, so PC is not sluggish.

    x1 := x;
    y1 := y;

    move(x, y);                   //  Generate the next point in the random walk.

    if pnlGrow.Canvas.Pixels[x, y] = clLime then  //  we have a hit.
      begin
        pnlGrow.Canvas.Pixels[x1, y1] := clLime;
        p.x := x;
        p.y := y;
        elapsedTime := (GetTickCount64 - growTime);
        hs.push(p, elapsedTime);    //  Store the new hit point.
        hs.save(fileName);                        //  save hitStore to text file.
        x := MAX_WIDTH DIV 2;                     //  Reset the starting position of the random walk.
        y := 0;
        inc(hits);
        StatusBar1.Panels.Items[0].Text := format('hits = %d  mins per hit = %f',
                                          [hits, minsPerHit(hits, elapsedTime)]);
        clearTail;                //  Clear the tail after a hit.
        ps.clear;                 //  Clear the pointStore [random walk].
        growTime := GetTickCount64;
      end
    else
    begin
      pnlGrow.Canvas.Pixels[x, y] := clGreen;
      p.x := x;
      p.y := y;
      ps.push(p, 0);                 //  Store the next point in the random walk.
                                     //  No need to store time for the random walk.

      p1 := ps.pop(ps.Index - tailLength);   //  Request the end of the tail
                                             //  and set to black if valid.
      if p1.x <> -1 then pnlGrow.Canvas.Pixels[p1.x, p1.y] := clBlack;
     end;
  until abort;
end;

procedure TForm1.clearTail;
{  When we have a hit, that is we have added to the growth the
   pointStore is cleared, this will leave the last tail on the screen.
   So just before we clear the pointsStore, this routine clears the
   remaing points of the random walk.}
var
  f: integer;
  p: TPoint;
begin
  for f := ps.Index-2 downto ps.Index-tailLength do
  begin
    p := ps.pop(f);
    if p.x <> -1 then pnlGrow.Canvas.Pixels[p.x, p.y] := clBlack;
  end;
end;

function TForm1.minsPerHit(hits: integer; elapsedTime: int64):double;
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

procedure TForm1.move(VAR x: Integer; VAR y:integer);
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

procedure TForm1.loadGrowth;
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

end.

