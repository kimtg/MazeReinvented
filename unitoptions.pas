unit unitoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    EditWidth: TEdit;
    Label1: TLabel;
    UpDown1: TUpDown;
  private

  public

  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

{ TFormOptions }

end.

