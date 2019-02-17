unit uPointStore;
{  A Class that remembers a series of TPoint i.e. pixel co-ordinates.
   Also, if used, will store the elapsed time taken to generate the point.

   the store acts a LIFO - last in First out.

   To create -- ps := pointStore.Create;
   To free   -- ps.Free;

   ps.push           - adds a TPoint and elapsed time to the store.
   ps.pop(n)         - returns a TPoint, that is n places from the top.
   ps.clear          - clears the store.
   ps.save(filename) - save the store to a text file named filename.
   ps.load(filename) - loads the store form a text file name filename

   ps.Indes   - point to the top of the points store.
   ps.Elapsed - the sum of all the point elapsed times.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type

  pointStore = class

    private
      Points  : Array of TPoint;         //  The points store.
      Ticks   : Array of integer;        //  Time duration to generate the point, in ticks.
      _index  : integer;                 //  Point to the top of the store.
      _elapsed: int64;                   //  Sum of the elapsed times - used on load.

    public
      property Index   : integer read _index   write _index;
      property Elapsed : int64   read _elapsed write _elapsed;

      constructor Create; overload;
      destructor Destroy; override;
      procedure clear;
      procedure push(p: TPoint; t: integer);
      function pop(position: integer): TPoint;
      procedure save(fileName: string);
      procedure load(fileName: string);
  end;

CONST
  NO_OF_POINTS = 5000;          //  used to set inital size of store.

implementation

constructor pointStore.Create; overload;
begin
  SetLength(Points, NO_OF_POINTS);
  SetLength(Ticks, NO_OF_POINTS);
  Index   := 0;
  Elapsed := 0;
end;

destructor pointStore.Destroy;
begin
  SetLength(Points, 0);
  SetLength(Points, 0);
end;

procedure pointStore.clear;
{  Clears the store, accomplished by setting to zero and then to desired limit.
   Pointer is also set to zero.
}
begin
  SetLength(Points, 0);
  SetLength(Points, NO_OF_POINTS);
  SetLength(Ticks, 0);
  SetLength(Ticks, NO_OF_POINTS);
  Index := 0;
end;

function pointStore.pop(position: integer): TPoint;
{  Returns a TPoint from the store.

   if the requested position is less the size of the store (-1, -1) is returned.
}
begin
  if (position > 0) and (position < Index) then
    begin
      result := Points[position];
    end
  else
    result := Point(-1, -1);   // return error
end;

procedure pointStore.push(p: TPoint; t: integer);
{  Add a TPoint and elapsed time to the store.
   The pointer is moved to the next free position in the store.
   If the pointer exceeds the size of the store, the store grows dynamically.
}
begin
  Points[Index] := p;    //  point [x, y]
  Ticks[Index]  := t;    //  elapsed time
  Index := Index + 1;

  if Index >= Length(Points) then
  begin
    SetLength(Points, (Index + NO_OF_POINTS));
    SetLength(Ticks,  (Index + NO_OF_POINTS));
  end;
end;

procedure pointStore.save(fileName: string);
{  Save the store to a text file.    }
VAR
  f: integer;
begin
  with TStringList.Create do
    try
      for f := 0 to Index - 1 do
          Add(format('%d, %d, %d', [Points[f].x, Points[f].y, Ticks[f]]));

      SaveToFile(fileName);
    finally
      Free;
    end;
end;

procedure pointStore.load(fileName: string);
{  Loads the contents of a text file into the store.
   The first performs a store clear - so any existing data is lost.
}
var
  lines: TStringList;
  line : string;
  split: TStringArray;
  p    : TPoint;
  t    : integer = 0;
begin
  clear;
  lines := TStringList.create;

  try
    lines.LoadFromFile(fileName);
    for line in lines do
    begin
      split := line.Split(',');
      p.x   := strToint(split[0]);
      p.y   := strToint(split[1]);
      t     := strToint(split[2]);    //  elapsed time

      Elapsed := Elapsed + t;

      push(p, t);
    end;
   finally
    lines.Free;
  end;  // try
end;

end.

