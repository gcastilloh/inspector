program Project3;

uses
  Vcl.Forms,
  UDataModuleInspector in 'UDataModuleInspector.pas' {DM: TDM},
  UFrInspector in 'UFrInspector.pas' {FrInspector: TFrame},
  Unit1 in 'Unit1.pas' {Form1},
  UPatronObservador in 'UPatronObservador.pas',
  UAnimales in 'UAnimales.pas',
  UPropiedad in 'UPropiedad.pas',
  UDiccionarioPropiedades in 'UDiccionarioPropiedades.pas',
  UFuncionesCallBack in 'UFuncionesCallBack.pas',
  UListaPropiedades in 'UListaPropiedades.pas',
  UFuncionesAuxiliaresDatos in 'UFuncionesAuxiliaresDatos.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
