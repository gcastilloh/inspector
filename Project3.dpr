program Project3;

uses
  Vcl.Forms,
  UFrInspector in 'UFrInspector.pas' {FrInspector: TFrame},
  Unit1 in 'Unit1.pas' {Form1},
  UPatronObservador in 'UPatronObservador.pas',
  UAnimales in 'UAnimales.pas',
  UPropiedad in 'UPropiedad.pas',
  UDiccionarioPropiedades in 'UDiccionarioPropiedades.pas',
  UFuncionesCallBack in 'UFuncionesCallBack.pas',
  UDataModuleInspector in 'UDataModuleInspector.pas' {DataModule2: TDataModule},
  UListaPropiedades in 'UListaPropiedades.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.Run;
end.
