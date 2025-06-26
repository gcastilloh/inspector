unit UListaPropiedades;

interface

uses System.Generics.Collections, System.SysUtils, InspLinks, UPropiedad, UFuncionesCallBack;

type

   TListaPropiedades = class
   private
      FDiccionario : TObjectDictionary<string, TPropiedad>;
   public
      constructor Create;
      destructor Destroy; override;

      procedure agregarPropiedad(const Nombre : string; Prop : TPropiedad);
      function obtenerPropiedad(const Nombre : string) : TPropiedad;
      procedure retirarPropiedad(const Nombre : string);
      function existeLaPropiedad(const Nombre : string) : Boolean;
      procedure Clear;
      // Iterador para for..in (retorna las propiedades)
      function GetEnumerator : TDictionary<string, TPropiedad>.TValueEnumerator;
      // Iterador de pares (clave-valor)
      function GetParesEnumerator : TDictionary<string, TPropiedad>.TPairEnumerator;
      // diferentes maneras de agregar una propiedad y su nombre
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; Visible : Boolean; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; Visible : Boolean; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer) : TPropiedad; overload;
      // busca una propiedad con base en su nombre
      function buscaPropiedadPorNombre(Nombre : string) : TPropiedad;

      property Items[const Nombre : string] : TPropiedad read obtenerPropiedad; default;
      property Pares : TDictionary<string, TPropiedad>.TPairEnumerator read GetParesEnumerator;
   end;

   IPropiedades = interface
      ['{D4B20A1E-1234-4F76-9FDE-FA1234567890}']
      // GUID único
      function GetPropiedades : TListaPropiedades;
      property Propiedades : TListaPropiedades read GetPropiedades;
   end;

implementation

constructor TListaPropiedades.Create;
begin
// El diccionario es dueño de las TPropiedad y las libera automáticamente
FDiccionario := TObjectDictionary<string, TPropiedad>.Create([doOwnsValues]);
end;

destructor TListaPropiedades.Destroy;
begin
FDiccionario.Free;
inherited;
end;

function TListaPropiedades.existeLaPropiedad(const Nombre : string) : Boolean;
begin
Result := FDiccionario.ContainsKey(Nombre);
end;

procedure TListaPropiedades.agregarPropiedad(const Nombre : string; Prop : TPropiedad);
begin
if not existeLaPropiedad(Nombre) then
   FDiccionario.Add(Nombre, Prop)
else
   raise Exception.CreateFmt('Propiedad "%s" ya fue registrada previamente', [Nombre]);
end;

function TListaPropiedades.obtenerPropiedad(const Nombre : string) : TPropiedad;
begin
if not FDiccionario.TryGetValue(Nombre, Result) then
   raise Exception.CreateFmt('Propiedad "%s" no encontrada', [Nombre]);
end;

procedure TListaPropiedades.retirarPropiedad(const Nombre : string);
begin
FDiccionario.Remove(Nombre);
end;

function TListaPropiedades.buscaPropiedadPorNombre(Nombre : string) : TPropiedad;
begin
if not FDiccionario.TryGetValue(Nombre, Result) then
   Result := nil;
end;

procedure TListaPropiedades.Clear;
begin
FDiccionario.Clear;
end;

function TListaPropiedades.GetEnumerator : TDictionary<string, TPropiedad>.TValueEnumerator;
begin
Result := FDiccionario.Values.GetEnumerator;
end;

function TListaPropiedades.GetParesEnumerator : TDictionary<string, TPropiedad>.TPairEnumerator;
begin
Result := FDiccionario.GetEnumerator;
end;

(* ========================================================================== *)

// las siguinetes funciones les hace falta acabar de pulirlas, son todas iguales! GCH2018

(* ========================================================================== *)

// Con propiedad Visible y con Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; Visible : Boolean; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad;
var
   propiedad : TPropiedad;
begin
propiedad := TPropiedad.CreateP(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, fv);
agregarPropiedad(PropertyName, propiedad);
Result := propiedad;
end;

(* ========================================================================== *)
// Con propiedad Visible y sin Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; Visible : Boolean; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, nil);
end;

(* ========================================================================== *)
// Visible=TRUE y con Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y con Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; fv : TValidaFunc) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, nil, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y sin Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, nil, nil);
end;

(* ========================================================================== *)
// Visible=TRUE  y sin Funcion de Validacion
function TListaPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : integer; PropertyCaption : string; POS : integer; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, nil);;
end;

end.
