unit UDiccionarioPropiedades;

interface

uses System.Generics.Collections, System.SysUtils, InspLinks, UPropiedad, UListaPropiedades, UFuncionesCallBack;

type

   TDiccionarioPropiedades = class
   private
      FDiccionario : TObjectDictionary<string, TPropiedad>;
      function GetCount : Integer;
   public
      property Count : Integer read GetCount;

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
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : Boolean; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : Boolean; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TValidaFunc) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer) : TPropiedad; overload;

      function GetPropiedadesPorPanel(const APanelID : Integer) : TLIstaPropiedades;

      property Items[const Nombre : string] : TPropiedad read obtenerPropiedad; default;
      property Pares : TDictionary<string, TPropiedad>.TPairEnumerator read GetParesEnumerator;

   end;

   IPropiedades = interface
      ['{D4B20A1E-1234-4F76-9FDE-FA1234567890}']
      // GUID único
      function GetPropiedades : TDiccionarioPropiedades;
      function GetAyudaProc : TAyudaProc;
      property Propiedades : TDiccionarioPropiedades read GetPropiedades;
      property ayuda : TAyudaProc read GetAyudaProc;
   end;

implementation

uses System.Generics.Defaults;

constructor TDiccionarioPropiedades.Create;
begin
// El diccionario es dueño de las TPropiedad y las libera automáticamente
FDiccionario := TObjectDictionary<string, TPropiedad>.Create([doOwnsValues]);
end;

destructor TDiccionarioPropiedades.Destroy;
begin
FDiccionario.Free;
inherited;
end;

function TDiccionarioPropiedades.existeLaPropiedad(const Nombre : string) : Boolean;
begin
Result := FDiccionario.ContainsKey(Nombre);
end;

procedure TDiccionarioPropiedades.agregarPropiedad(const Nombre : string; Prop : TPropiedad);
begin
if not existeLaPropiedad(Nombre) then
   FDiccionario.Add(Nombre, Prop)
else
   raise Exception.CreateFmt('Propiedad "%s" ya fue registrada previamente', [Nombre]);
end;

function TDiccionarioPropiedades.obtenerPropiedad(const Nombre : string) : TPropiedad;
begin
if not FDiccionario.TryGetValue(Nombre, Result) then
   Result := nil;
end;

procedure TDiccionarioPropiedades.retirarPropiedad(const Nombre : string);
begin
FDiccionario.Remove(Nombre);
end;

procedure TDiccionarioPropiedades.Clear;
begin
FDiccionario.Clear;
end;

function TDiccionarioPropiedades.GetCount : Integer;
begin
Result := FDiccionario.Count;
end;

function TDiccionarioPropiedades.GetEnumerator : TDictionary<string, TPropiedad>.TValueEnumerator;
begin
Result := FDiccionario.Values.GetEnumerator;
end;

function TDiccionarioPropiedades.GetParesEnumerator : TDictionary<string, TPropiedad>.TPairEnumerator;
begin
Result := FDiccionario.GetEnumerator;
end;

/// obtiene una lista de propiedades del mismo panel y ordenadas pcon base en POS
function TDiccionarioPropiedades.GetPropiedadesPorPanel(const APanelID : Integer) : TListaPropiedades;
var
   Propiedad : TPropiedad;
begin
Result := TListaPropiedades.create;
for Propiedad in FDiccionario.Values do
   begin
   if Propiedad.ID_PANEL = APanelID then
      Result.Add(Propiedad);
   end;

// Ordenar por Pos
Result.Sort(TComparer<TPropiedad>.Construct(function(const Left, Right : TPropiedad) : Integer
   begin
   Result := Left.Posicion - Right.Posicion;
   end));
end;

// Con propiedad Visible y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : Boolean; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad;
var
   Propiedad : TPropiedad;
begin
Propiedad := TPropiedad.CreateP(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, fv);
agregarPropiedad(PropertyName, Propiedad);
Result := Propiedad;
end;

(* ========================================================================== *)
// Con propiedad Visible y sin Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : Boolean; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, nil);
end;

(* ========================================================================== *)
// Visible=TRUE y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TValidaFunc) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TValidaFunc) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, nil, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y sin Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, nil, nil);
end;

(* ========================================================================== *)
// Visible=TRUE  y sin Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, nil);;
end;

end.
