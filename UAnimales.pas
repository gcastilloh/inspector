unit UAnimales;

interface

uses Classes, UListaPropiedades;

type
   TAnimal = class(TPersistent, IPropiedades)
   private
      FNombre : string;
      FEdad : Integer;
      { Interfaz IPropiedades }
      flistaPropiedades : TListaPropiedades;
      function GetPropiedades : TListaPropiedades;
      { Implementación manual de IInterface }
      function QueryInterface(const IID : TGUID; out Obj) : HResult; stdcall;
      function _AddRef : Integer; stdcall;
      function _Release : Integer; stdcall;
   public
      { Interfaz IPropiedades }
      constructor create;
      property Propiedades : TListaPropiedades read GetPropiedades;
   published
      property Nombre : string read FNombre write FNombre;
      property Edad : Integer read FEdad write FEdad;
   end;

   TPerro = class(TAnimal)
   private
      FRaza : string;
   public
      constructor create;
   published
      property Raza : string read FRaza write FRaza;
   end;

   TGato = class(TAnimal)
   private
      FColor : string;
   public
      constructor create;
   published
      property Color : string read FColor write FColor;
   end;

implementation

uses UDataModuleInspector;

{ TAnimal }

constructor TAnimal.create;
begin
flistaPropiedades := TListaPropiedades.create;
end;

function TAnimal.GetPropiedades : TListaPropiedades;
begin
result := flistaPropiedades;
end;

function TAnimal._AddRef: Integer;
begin
  Result := -1; // sin manejo automático de referencias
end;

function TAnimal._Release: Integer;
begin
  Result := -1;
end;

function TAnimal.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TPerro }

constructor TPerro.create;
begin
inherited;
propiedades.SetPropiedad('Nombre', 1, 'Nombre del perro', 1);
propiedades.SetPropiedad('Edad', 1, 'Edad', 2);
propiedades.SetPropiedad('Raza', 1, 'Raza', 3);
end;

{ TGato }

constructor TGato.create;
begin
inherited;
Propiedades.SetPropiedad('Nombre', 1, 'Nombre del gato', 1);
Propiedades.SetPropiedad('Edad', 1, 'Edad del gato', 2);
Propiedades.SetPropiedad('Color', 1, 'Color del gato', 3);
end;

end.
