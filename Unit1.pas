unit Unit1;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvPanel, ToolPanels, Vcl.ExtCtrls, UFrInspector, Vcl.StdCtrls, InspectorBar, RTTIInspectorBar;

type
   TForm1 = class(TForm)
      BtPerro : TButton;
      btnGato : TButton;
    RTTIInspectorBar1: TRTTIInspectorBar;
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

uses UAnimales;

{$R *.dfm}

function NombreAleatorio : string;
const
  Nombres: array [0 .. 29] of string = (
    'Max',
    'Luna',
    'Rocky',
    'Milo',
    'Nina',
    'Bella',
    'Simba',
    'Toby',
    'Lola',
    'Coco',
    'Thor',
    'Maya',
    'Bruno',
    'Kira',
    'Zeus',
    'Sasha',
    'Boby',
    'Misha',
    'Tommy',
    'Nala',
    'Rex',
    'Chispa',
    'Leo',
    'Mimi',
    'Duke',
    'Olivia',
    'Lucas',
    'Frida',
    'Benji',
    'Gala'
  );
begin
Result := Nombres[Random(Length(Nombres))];
end;

function EdadAleatoria : Integer;
begin
Result := Random(15) + 1; // 1 a 15 años
end;

function RazaAleatoria : string;
const
  Razas: array [0 .. 19] of string = (
    'Labrador',
    'Pug',
    'Pastor Alemán',
    'Beagle',
    'Border Collie',
    'Golden Retriever',
    'Chihuahua',
    'Bulldog Francés',
    'Dálmata',
    'Boxer',
    'Doberman',
    'Shih Tzu',
    'Schnauzer',
    'Husky Siberiano',
    'Cocker Spaniel',
    'Gran Danés',
    'Rottweiler',
    'Basset Hound',
    'Mastín Napolitano',
    'Pomerania'
  );

begin
Result := Razas[Random(Length(Razas))];
end;

function ColorAleatorio : string;
const
  Colores: array [0 .. 14] of string = (
    'Negro',
    'Blanco',
    'Gris',
    'Atigrado',
    'Naranja',
    'Marrón',
    'Beige',
    'Crema',
    'Canela',
    'Dorado',
    'Azul (grisáceo)',
    'Chocolate',
    'Tricolor',
    'Bicolor',
    'Carey'
  );
begin
Result := Colores[Random(Length(Colores))];
end;

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
end;

end.
