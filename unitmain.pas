unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuDraw: TMenuItem;
    MenuOptions: TMenuItem;
    MenuSolve: TMenuItem;
    procedure MenuDrawClick(Sender: TObject);
    procedure MenuOptionsClick(Sender: TObject);
    procedure MenuSolveClick(Sender: TObject);
  private

  public

  end;

type
  tmat = (clear, wall, current, visited);
var
  FormMain: TFormMain;
  wallsize: integer = 10;
  mat: array of array of tmat;
  xmax, ymax: integer;

implementation

uses
  unitoptions;

{$R *.lfm}

{ TFormMain }

procedure DrawMat();
var
  x, y, x2, y2: integer;
begin
  FormMain.canvas.Clear;
  FormMain.canvas.Pen.Width := wallsize div 4;
  for y := 0 to ymax div 2 do
    for x := 0 to xmax div 2 do
    begin
      x2 := x * 2;
      y2 := y * 2;
      //FormMain.canvas.Pixels[x2*wallsize,y2*wallsize] := clGreen;
      if mat[y2,x2+1] = wall then // horizontal
      begin
        FormMain.canvas.MoveTo(x2*wallsize, y2*wallsize);
        FormMain.canvas.LineTo((x2+2)*wallsize, y2*wallsize);
      end;
      if mat[y2+1,x2] = wall then // vertical
      begin
        FormMain.canvas.MoveTo(x2*wallsize, y2*wallsize);
        FormMain.canvas.LineTo(x2*wallsize, (y2+2)*wallsize);
      end;
    end;

  y := 1;
  while y < ymax do
  begin
    x := 1;
    while x < xmax do
    begin
      if mat[y,x] = visited then
        FormMain.canvas.Pixels[x*wallsize, y*wallsize] := clGreen;
      inc(x,2);
    end;
    inc(y,2);
  end;
end;

procedure TFormMain.MenuDrawClick(Sender: TObject);
var
  x, y, dir: integer;
  HasWork: boolean;
begin
  ymax := FormMain.ClientHeight div wallsize div 2 * 2;
  xmax := FormMain.ClientWidth div wallsize div 2 * 2;
  setlength(mat, ymax + 2, xmax + 2);
  // clear
  for y := 0 to ymax do
    for x := 0 to xmax do
    begin
      mat[y, x] := clear;
    end;

  for x := 0 to xmax do
  begin
    mat[0, x] := wall;
    mat[ymax, x] := wall;
  end;

  for y := 0 to ymax do
  begin
    mat[y, 0] := wall;
    mat[y, xmax] := wall;
  end;

  mat[1, 0] := clear; // start
  mat[ymax - 1, xmax] := clear; //end

  repeat
    HasWork := false;
    y := 0;
    while y <= ymax do
    begin
      x := 0;
      while x <= xmax do
      begin
        if mat[y,x] <> wall then
        begin
          HasWork := true;
          inc(x, 2);
          continue;
        end;

        dir := random(4);
        case dir of
          0:
            if (x + 2 <= xmax) and (mat[y, x + 2] <> wall) then
            begin
              mat[y, x + 1] := wall;
              mat[y, x + 2] := wall;
            end;
          1:
            if (x - 2 >= 0) and (mat[y, x - 2] <> wall) then
            begin
              mat[y, x - 1] := wall;
              mat[y, x - 2] := wall;
            end;
          2:
            if (y + 2 <= ymax) and (mat[y + 2, x] <> wall) then
            begin
              mat[y + 1, x] := wall;
              mat[y + 2, x] := wall;
            end;
          3:
            if (y - 2 >= 0) and (mat[y - 2, x] <> wall) then
            begin
              mat[y - 1, x] := wall;
              mat[y - 2, x] := wall;
            end;
        end;
        inc(x, 2);
      end;
      inc(y, 2);
    end;
  until not HasWork;

  DrawMat;
end;

procedure TFormMain.MenuOptionsClick(Sender: TObject);
begin
  FormOptions.show;
end;

procedure TFormMain.MenuSolveClick(Sender: TObject);
var
  x, y: integer;
  HasWork: boolean;
begin
  repeat
    HasWork := false;
    y := 1;
    while y < ymax do
    begin
      x := 1;
      while x < xmax do
      begin
        if (mat[y,x] <> visited) and (integer(mat[y,x-1]) + integer(mat[y,x+1]) + integer(mat[y+1,x]) + integer(mat[y-1,x]) >= 3) then
        begin
          mat[y,x]:=visited;
          mat[y-1,x]:=wall;
          mat[y+1,x]:=wall;
          mat[y,x-1]:=wall;
          mat[y,x+1]:=wall;
          HasWork := true;
        end;
        inc(x, 2);
      end;
      inc(y, 2);
    end;
  until not HasWork;
  DrawMat;
end;

begin
  randomize;
end.

