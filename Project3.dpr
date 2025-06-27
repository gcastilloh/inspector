program Project3;

uses
  Vcl.Forms,
  UFrInspector in 'UFrInspector.pas' {FrInspector: TFrame},
  Unit1 in 'Unit1.pas' {Form1},
  UPatronObservador in 'UPatronObservador.pas',
  UAnimales in 'UAnimales.pas',
  UDiccionarioPropiedades in 'UDiccionarioPropiedades.pas',
  UFuncionesAuxiliaresDatos in 'UFuncionesAuxiliaresDatos.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
