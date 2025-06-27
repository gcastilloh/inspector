unit Unit1;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvPanel, ToolPanels, Vcl.ExtCtrls, UFrInspector, Vcl.StdCtrls, InspectorBar, RTTIInspectorBar;

type
   TForm1 = class(TForm)
      BtPerro : TButton;
      btnGato : TButton;
    FrInspector1: TFrInspector;
      procedure BtPerroClick(Sender : TObject);
      procedure btnGatoClick(Sender : TObject);
    procedure FormCreate(Sender: TObject);
   private
      { Private declarations }
   public
      { Public declarations }
   end;

var
   Form1 : TForm1;

implementation

uses UAnimales, UFuncionesAuxiliaresDatos;

{$R *.dfm}

procedure TForm1.btnGatoClick(Sender : TObject);
var
   Gato : TGato;
begin
Gato := TGato.Create;
Gato.Nombre := NombreAleatorio;
Gato.Edad := EdadAleatoria;
Gato.Color := ColorAleatorio;
FrInspector1.registraObjeto(Gato.Nombre, Gato);
end;

procedure TForm1.BtPerroClick(Sender : TObject);
var
   Perro : TPerro;
begin
Perro := TPerro.Create;
Perro.Nombre := NombreAleatorio;
Perro.Edad := EdadAleatoria;
Perro.Raza := RazaAleatoria;
FrInspector1.registraObjeto(Perro.Nombre, Perro);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FrInspector1.creaPanel('General');
FrInspector1.creaPanel('Especificas');
end;

end.
