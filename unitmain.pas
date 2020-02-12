unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Menus, lcltype;

type

  { TFormMain }

  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuGenerate: TMenuItem;
    MenuOptions: TMenuItem;
    MenuSolve: TMenuItem;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuGenerateClick(Sender: TObject);
    procedure MenuOptionsClick(Sender: TObject);
    procedure MenuSolveClick(Sender: TObject);
  private

  public

  end;

type
  tmat = (clear, wall, current, visited, deadend);

var
  FormMain: TFormMain;
  wallsize: integer = 10;
  mat: array of array of tmat;
  maxx, maxy: integer;
  curx, cury: integer;
  ColorWall: tcolor = clBlack;
  ColorCur: tcolor = clGreen;
  ColorDeadEnd: tcolor = clRed;
  finished: boolean = False;
  bm: tbitmap;

implementation

uses
  unitoptions;

{$R *.lfm}

{ TFormMain }

procedure DrawMat();
var
  x, y, x2, y2: integer;
begin
  bm.Width:=formmain.ClientWidth;
  bm.Height:=formmain.ClientHeight;
  bm.canvas.brush.color:=clWhite;
  bm.canvas.fillrect(bm.canvas.ClipRect);
  bm.canvas.Pen.Width := wallsize div 4;
  bm.canvas.pen.color := colorwall;
  for y := 0 to maxy div 2 do
    for x := 0 to maxx div 2 do
    begin
      x2 := x * 2;
      y2 := y * 2;
      if mat[y2, x2 + 1] = wall then // horizontal
      begin
        bm.canvas.Line(x2 * wallsize, y2 * wallsize, (x2 + 2) *
          wallsize, y2 * wallsize);
      end;
      if mat[y2 + 1, x2] = wall then // vertical
      begin
        bm.canvas.Line(x2 * wallsize, y2 * wallsize, x2 * wallsize,
          (y2 + 2) * wallsize);
      end;
    end;

  y := 1;
  while y < maxy do
  begin
    x := 1;
    while x < maxx do
    begin
      if mat[y, x] = visited then
        bm.canvas.Pixels[x * wallsize, y * wallsize] := ColorDeadEnd;
      Inc(x, 2);
    end;
    Inc(y, 2);
  end;
end;

procedure TFormMain.MenuGenerateClick(Sender: TObject);
var
  x, y, dir: integer;
  HasWork: boolean;
begin
  finished := False;
  maxy := FormMain.ClientHeight div wallsize div 2 * 2;
  maxx := FormMain.ClientWidth div wallsize div 2 * 2;
  setlength(mat, maxy + 2, maxx + 2);
  // clear
  for y := 0 to maxy + 1 do
    for x := 0 to maxx + 1 do
    begin
      mat[y, x] := Clear;
    end;

  for x := 0 to maxx do
  begin
    mat[0, x] := wall;
    mat[maxy, x] := wall;
  end;

  for y := 0 to maxy do
  begin
    mat[y, 0] := wall;
    mat[y, maxx] := wall;
  end;

  mat[1, 0] := Clear; // start
  mat[maxy - 1, maxx] := Clear; //end

  repeat
    HasWork := False;
    y := 0;
    while y <= maxy do
    begin
      x := 0;
      while x <= maxx do
      begin
        if mat[y, x] <> wall then
        begin
          HasWork := True;
          Inc(x, 2);
          continue;
        end;

        dir := random(4);
        case dir of
          0:
            if (x + 2 <= maxx) and (mat[y, x + 2] <> wall) then
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
            if (y + 2 <= maxy) and (mat[y + 2, x] <> wall) then
            begin
              mat[y + 1, x] := wall;
              mat[y + 2, x] := wall;
            end;
          else
            if (y - 2 >= 0) and (mat[y - 2, x] <> wall) then
            begin
              mat[y - 1, x] := wall;
              mat[y - 2, x] := wall;
            end;
        end;
        Inc(x, 2);
      end;
      Inc(y, 2);
    end;
  until not HasWork;

  DrawMat;
  curx := 1;
  cury := 1;
  mat[cury, curx] := visited;
  bm.canvas.Pen.Color := ColorCur;
  bm.canvas.line(0 * wallsize, 1 * wallsize, curx * wallsize, cury * wallsize);
  invalidate;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  oldx, oldy: integer;
begin
  oldx := curx;
  oldy := cury;
  case key of
    vk_up:
      if mat[cury - 1, curx] <> wall then
        Dec(cury, 2);
    vk_down:
      if mat[cury + 1, curx] <> wall then
        Inc(cury, 2);
    vk_left:
      if (curx > 1) and (mat[cury, curx - 1] <> wall) then
        Dec(curx, 2);
    vk_right:
      if (curx < maxx - 1) and (mat[cury, curx + 1] <> wall) then
        Inc(curx, 2);
  end;
  if mat[cury, curx] <> visited then
  begin
    bm.canvas.Pen.Color := ColorCur;
  end
  else
  begin
    mat[oldy, oldx] := Clear;
    bm.canvas.Pen.Color := ColorDeadEnd;
  end;
  mat[cury, curx] := visited;
  bm.canvas.Line(oldx * wallsize, oldy * wallsize, curx * wallsize, cury * wallsize);
  invalidate;
  if not finished and (curx = maxx - 1) and (cury = maxy - 1) then
  begin
    finished := True;
    ShowMessage('Congratulations!');
  end;
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
  canvas.draw(0, 0, bm);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  MenuGenerateClick(nil);
end;

procedure TFormMain.MenuOptionsClick(Sender: TObject);
begin
  FormOptions.Show;
end;

function MatBlocked(x, y: integer): integer;
begin
  if mat[y, x] = clear then
    Result := 0
  else
    Result := 1;
end;

procedure TFormMain.MenuSolveClick(Sender: TObject);
var
  x, y: integer;
  HasWork: boolean;
begin
  repeat
    HasWork := False;
    y := 1;
    while y < maxy do
    begin
      x := 1;
      while x < maxx do
      begin
        if (mat[y, x] <> visited) and (MatBlocked(x + 1, y) +
          MatBlocked(x - 1, y) + MatBlocked(x, y + 1) + MatBlocked(x, y - 1) >= 3) then
        begin
          mat[y, x] := visited;
          if mat[y - 1, x] = clear then
            mat[y - 1, x] := deadend;
          if mat[y + 1, x] = clear then
            mat[y + 1, x] := deadend;
          if mat[y, x - 1] = clear then
            mat[y, x - 1] := deadend;
          if mat[y, x + 1] = clear then
            mat[y, x + 1] := deadend;
          HasWork := True;
        end;
        Inc(x, 2);
      end;
      Inc(y, 2);
    end;
  until not HasWork;
  DrawMat;
  Invalidate;
end;

begin
  randomize;
  bm:=tbitmap.create;
end.
