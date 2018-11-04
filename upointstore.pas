unit uPointStore;
{  A Class that remembers a series of TPoint i.e. pixel co-oridantes.

   the store acts a LIFO - last in First out.

   To create -- ps := pointStore.Create;
   To free   -- ps.Free;

   ps.push   - adds a TPoint to the store.
   ps.pop(n) - returns a TPoint, that is n places from the top.
   ps.clear  - clears the store.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type

  pointStore = class

    private
      Points: Array of TPoint;       //  The points store.
      _Index: integer;               //  Point to the top of the store.

    public
      property Index : integer read _Index write _Index;

      constructor Create; overload;
      destructor Destroy; override;
      procedure clear;
      procedure push(p: TPoint);
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
  Index := 0;
end;

destructor pointStore.Destroy;
begin
  SetLength(Points, 0);
end;

procedure pointStore.clear;
{  Clears the store, accomplished by setting to zero and then to desired limit.
   Pointer is also set to zero.
}
begin
  SetLength(Points, 0);
  SetLength(Points, NO_OF_POINTS);
  Index := 0;
end;

function pointStore.pop(position: integer): TPoint;
{  Returns a TPoint from the store.

   if the requested position is less the size of the store (-1, -1) is returned.
}
begin
  if position < Index then
    begin
      result := Points[position];
    end
  else
    result := Point(-1, -1);   // return error
end;

procedure pointStore.push(p: TPoint);
{  Add a TPoint to the store.
   The pointer is moved to the next free position in the store.
   If the pointer exceeds the size of the store, the store grows dynamacaly.
}
begin
  Points[Index] := p;
  Index := Index + 1;

  if Index >= Length(Points) then SetLength(Points, (Index + NO_OF_POINTS));
end;

procedure pointStore.save(fileName: string);
VAR
  f: integer;
begin
  with TStringList.Create do
    try
      for f := 0 to Index - 1 do
          Add(format('%d, %d', [Points[f].x, Points[f].y]));

      SaveToFile(fileName);
    finally
      Free;
    end;
end;

procedure pointStore.load(fileName: string);
var
  lines: TStringList;
  line : string;
  split: TStringArray;
  p    : TPoint;
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
      push(p);
    end;
   finally
    lines.Free;
  end;  // try
end;

end.

