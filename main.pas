unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnClose: TButton;
    pnlGrow: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    procedure btnCloseClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure grow;
    procedure move(VAR x: Integer; VAR y:integer);
  public

  end;

CONST
  MAX_HEIGHT = 600;
  MAX_WIDTH  = 800;
var
  Form1: TForm1;
  abort: boolean;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;  //  This way we generate a new sequence every time the program is run

  pnlGrow.Height := MAX_HEIGHT;
  pnlGrow.Width  := MAX_WIDTH;
  abort          := false;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
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

procedure TForm1.btnStartClick(Sender: TObject);
begin
  btnClose.Caption := 'Stop';
  btnStart.Enabled := false;
  grow;
end;

procedure TForm1.grow;
VAR
  x, y  : integer;
  x1, y1: integer;
begin
  x := MAX_WIDTH DIV 2;
  y := 0;

  pnlGrow.Canvas.Pen.Color    := clLime;
  pnlGrow.Canvas.Pen.Width    := 2;
  pnlGrow.Canvas.Pen.Style    := psSolid;
  pnlGrow.Canvas.Line(0, MAX_HEIGHT-5, MAX_WIDTH, MAX_HEIGHT-5);
  pnlGrow.Canvas.Pixels[x, y] := clGreen;

  repeat
    Application.Processmessages;

    x1 := x;
    y1 := y;

    //pnlGrow.Canvas.Pixels[x, y] := clBlack;
    move(x, y);

    if pnlGrow.Canvas.Pixels[x, y] = clLime then
      begin
        pnlGrow.Canvas.Pixels[x1, y1] := clLime;
        x := MAX_WIDTH DIV 2;
        y := 0;
      end
    else
      pnlGrow.Canvas.Pixels[x, y] := clGreen;
  until abort;
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

  StatusBar1.Panels.Items[0].Text := format('direction = %d  x = %d  y = %d', [direction, x, y]);
end;

end.

