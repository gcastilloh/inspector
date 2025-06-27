unit UFuncionesAuxiliaresDatos;

interface

function NombreAleatorio : string;
function ColorAleatorio : string;
function EdadAleatoria : Integer;
function RazaAleatoria : string;

implementation

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


end.
