﻿unit UAnimales;

interface

uses Classes, UDiccionarioPropiedades;

type
   TAnimal = class(TPersistent, IPropiedades)
   private
      FNombre : string;
      FEdad : Integer;
      function GetPropiedades : TDiccionarioPropiedades; virtual; abstract;
      function GetAyudaProc : TProcedimientoDeAyuda; virtual;

      { Implementación manual de IInterface }
      function QueryInterface(const IID : TGUID; out Obj) : HResult; stdcall;
      function _AddRef : Integer; stdcall;
      function _Release : Integer; stdcall;
   public
      constructor Create;
      property Propiedades : TDiccionarioPropiedades read GetPropiedades;
   published
      property Nombre : string read FNombre write FNombre;
      property Edad : Integer read FEdad write FEdad;
   end;

   TPerro = class(TAnimal)
   private
      FRaza : string;
      fAgresividad : Integer;
      class var FDiccionarioPropiedades : TDiccionarioPropiedades;
      function GetPropiedades : TDiccionarioPropiedades; override;
      function GetAyudaProc : TProcedimientoDeAyuda; override;
      procedure ayuda();
   public
      constructor Create;
      class constructor ClassCreate;
      class destructor ClassDestroy;
   published
      property Raza : string read FRaza write FRaza;
      property agresividad : Integer read fAgresividad write fAgresividad;
   end;

   TGato = class(TAnimal)
   class var
      FDiccionarioPropiedades : TDiccionarioPropiedades;
      function GetPropiedades : TDiccionarioPropiedades; override;
   private
      FColor : string;
      fHumor : string;
      fPeso : double;
   public
      constructor Create;
      class constructor ClassCreate;
      class destructor ClassDestroy;
   published
      property Color : string read FColor write FColor;
      property peso : double read fPeso write fPeso;
      property humor : string read fHumor write fHumor;
   end;

implementation

uses Vcl.dialogs;

{ TAnimal }

constructor TAnimal.Create;
begin
inherited Create;
end;

function TAnimal.GetAyudaProc : TProcedimientoDeAyuda;
begin
result := nil;
end;

function TAnimal.QueryInterface(const IID : TGUID; out Obj) : HResult;
begin
if GetInterface(IID, Obj) then
   result := 0
else
   result := E_NOINTERFACE;
end;

function TAnimal._AddRef : Integer;
begin
result := -1;
end;

function TAnimal._Release : Integer;
begin
result := -1;
end;

{ TPerro }

constructor TPerro.Create;
begin
inherited Create;
end;

class constructor TPerro.ClassCreate;
var
   propiedad : TPropiedad;
begin
// No se puede usar 'Propiedades' aquí porque es una propiedad de instancia,
// y este es un constructor de clase (no hay instancia todavía).
// Por eso usamos directamente FListaPerro, que es una variable de clase compartida.
FDiccionarioPropiedades := TDiccionarioPropiedades.Create;
// Aquí se agregan las propiedades específicas para Perro
propiedad := FDiccionarioPropiedades.SetPropiedad('Nombre', 0, 'nombre del perro', 0, nil);
propiedad.hint := propiedad.propertyName + '(Perro)';
propiedad.CanModify := false;
FDiccionarioPropiedades.SetPropiedad('Raza', 0, 'raza del perro', 1, nil);
FDiccionarioPropiedades.SetPropiedad('Agresividad', 0, '-10 cariñozo a 10 peligros', 1, TiposEditores.entero);
end;

procedure TPerro.ayuda;
begin
// ----------------------------------------------------
showMessage('Hola!!! ' + Nombre + ' eres un perro de raza ' + Raza);
end;

class destructor TPerro.ClassDestroy;
begin
FDiccionarioPropiedades.Free;
end;

function TPerro.GetAyudaProc : TProcedimientoDeAyuda;
begin
result := ayuda;
end;

function TPerro.GetPropiedades : TDiccionarioPropiedades;
begin
result := FDiccionarioPropiedades;
end;

{ TGato }

constructor TGato.Create;
begin
inherited Create;
end;

class constructor TGato.ClassCreate;
begin
// ⚠️ No se puede usar 'Propiedades' aquí porque es una propiedad de instancia,
// y este es un constructor de clase (no hay instancia todavía).
// Por eso usamos directamente FListaGato, que es una variable de clase compartida.
FDiccionarioPropiedades := TDiccionarioPropiedades.Create;
// Aquí se agregan las propiedades específicas para Gato
FDiccionarioPropiedades.SetPropiedad('Nombre', 0, 'Nombre', 0);
FDiccionarioPropiedades.SetPropiedad('Color', 0, 'Color', 2);
FDiccionarioPropiedades.SetPropiedad('Edad', 0, 'Edad', 1, TiposEditores.enteroPositivo);
FDiccionarioPropiedades.SetPropiedad('Peso', 1, 'Peso', 0, TiposEditores.real2d);
FDiccionarioPropiedades.SetPropiedad('Humor', 1, 'Humor', 1);
end;

class destructor TGato.ClassDestroy;
begin
FDiccionarioPropiedades.Free;
end;

function TGato.GetPropiedades : TDiccionarioPropiedades;
begin
result := FDiccionarioPropiedades;
end;

end.
