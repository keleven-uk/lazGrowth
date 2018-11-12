unit growthUntils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, process, Graphics;

function FontToString(f: TFont): string;
function StringToFont(s: string): TFont;
function getUpTime(system: string): string;
function getWindowsVersion: string;

implementation

uses
  Main;

function FontToString(f: TFont): string;
{  Produces a string representation of a given font.
   All the font attributes are converted to strings and packed together.

   Does not yet handle errors - will there be any ;o)                                              }
var
  rtnStr: string;
begin
  rtnStr := '';
  rtnStr := rtnStr + IntToStr(f.Charset) + ':';
  rtnStr := rtnStr + ColorToString(f.Color) + ':';
  rtnStr := rtnStr + IntToStr(f.Height) + ':';
  rtnStr := rtnStr + (f.Name) + ':';
  rtnStr := rtnStr + IntToStr(f.Orientation) + ':';
  rtnStr := rtnStr + IntToStr(f.size) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Pitch)) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Quality)) + ':';

  //                             TFontStyles is a set and scanned differently.
  if fsBold in f.Style then
    rtnStr := rtnStr + 'B'
  else
    rtnStr := rtnStr + '.';

  if fsItalic in f.Style then
    rtnStr := rtnStr + 'I'
  else
    rtnStr := rtnStr + '.';

  if fsStrikeOut in f.Style then
    rtnStr := rtnStr + 'S'
  else
    rtnStr := rtnStr + '.';

  if fsUnderline in f.Style then
    rtnStr := rtnStr + 'U'
  else
    rtnStr := rtnStr + '.';


  Result := rtnStr;
end;

function StringToFont(s: string): TFont;
{  Produces a font from a given string representation [produced by FonttoString.
   The string is read a bit at at time [up to the next :] and this is converted
   to the given font attribute.

   NB :: pos starts from position 1, where copy start at position 1.
         TFontStyles is a set and scanned differently.

   Does not yet handle errors - will there be any ;o)                                              }
var
  p   : integer;
  err : integer;
  chrs: integer;
  clr : TColor;
  Hght: integer;
  nme : string;
  Ortn: integer;
  sze : integer;
  ptch: integer;
  qlty: integer;

  fnt : TFont;
  fstyles: TFontStyles;
begin
  fnt := TFont.Create;
  fstyles := [];             //  empty set ??

  p := Pos(':', s);                                   //  Character set of font
  val(copy(s, 0, p - 1), chrs, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                   //  colour of font
  clr := StringToColor(copy(s, 0, p - 1));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  height of font
  val(copy(s, 0, p - 1), Hght, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  name of font
  nme := copy(s, 0, p - 1);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  orientation of font
  val(copy(s, 0, p - 1), Ortn, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  size of font
  val(copy(s, 0, p - 1), sze, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  pitch of font
  val(copy(s, 0, p - 1), ptch, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  quality of font
  val(copy(s, 0, p - 1), qlty, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  if Pos('B', s) <> 0 then
    include(fstyles, fsBold);
  if Pos('I', s) <> 0 then
    include(fstyles, fsItalic);
  if Pos('S', s) <> 0 then
    include(fstyles, fsStrikeOut);
  if Pos('U', s) <> 0 then
    include(fstyles, fsUnderline);

  fnt.Charset := chrs;                     //  Character set of font
  fnt.Color := clr;                        //  colour of font
  fnt.Height := Hght;                      //  height of font
  fnt.Name := nme;                         //  name of font
  fnt.Orientation := Ortn;                 //  orientation of font
  fnt.Size := sze;                         //  size of font
  fnt.Pitch := TFontPitch(ptch);           //  pitch of font
  fnt.Quality := TFontQuality(qlty);       //  quality of font
  fnt.Style := fstyles;

  Result := fnt;
end;

function getUpTime(system: string): string;
{  Determines the up time of either System or Application - depending on argument S or A.
   Used in formAbout & uKlockUtils.

   appStartTime := GetTickCount64; needs to be run when the app starts.

   NOTE :: Windows Only - use LclIntf.GetTickCount for cross platform.

   TODO : Need to check for roll over and account for it.
}
var
  noTicks    : int64;
  noSeconds  : integer;
  noOfDays   : integer;
  noOfHours  : integer;
  noOfMinutes: integer;
  noOfSeconds: integer;
begin
  system := AnsiLowerCase(system);

  if AnsiStartsStr('s', system) then
    noTicks := GetTickCount64                     //  How long has the system been running.
  else
    noTicks := GetTickCount64 - appStartTime;     //  How long has the application been running.

  noSeconds := noTicks div 1000;                  //  1000 ticks per second.

  noOfDays    := noSeconds div 86400;
  noSeconds   := noSeconds - (noOfDays * 86400);
  noOfHours   := noSeconds div 3600;
  noSeconds   := noSeconds - (noOfHours * 3600);
  noOfMinutes := noSeconds div 60;
  noSeconds   := noSeconds - (noOfMinutes * 60);
  NoOfSeconds := noSeconds;

  Result := format('%d days : %d hours : %d mins : %d secs', [noOfDays, noOfHours, noOfMinutes, noOfSeconds]);
end;

function getWindowsVersion: string;
{  Gets the version of the windows OS.
   This is achieved by capturing the output from the DOS command ver.
   The ver command is run  into a file and then the file is read back.
}
VAR
  AProcess   : TProcess;
  winVer     : TStringList;
  tmpWinVer  : string;
  tmpFileName: string;
begin
  tmpFileName := GetTempDir(true) + 'ver.txt';

  AProcess            := TProcess.Create(nil);
  AProcess.Options    := [poWaitOnExit, poUsePipes];
  AProcess.Executable := 'CMD ';
  AProcess.Parameters.Add('/C ver >' + tmpFileName);
  AProcess.Execute;

  winVer := TStringList.Create;
  winVer.LoadFromFile(tmpFileName);
  tmpWinVer := winVer[1];

  AProcess.Free;
  winVer.Free;

  Result := tmpWinVer;
end;


end.

