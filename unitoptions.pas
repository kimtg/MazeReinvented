unit unitoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  MaskEdit;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    EditWidth: TEdit;
    Label1: TLabel;
    UpDown1: TUpDown;
    procedure EditWidthChange(Sender: TObject);
  private

  public

  end;

var
  FormOptions: TFormOptions;

implementation

uses unitmain;

{$R *.lfm}

{ TFormOptions }

procedure TFormOptions.EditWidthChange(Sender: TObject);
begin
    unitmain.wallsize := strtoint(editwidth.Text);
end;

end.

