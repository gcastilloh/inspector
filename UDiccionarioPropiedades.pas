unit UDiccionarioPropiedades;

interface

/// TPropiedad: Define las caracteristicas con las que se mostrará una propiedad en el inspector
/// TDiccionarioPropiedades: Diccionario de propiedades que cada objeto que se registra en el inspector debe poseer
/// IPropiedades: Interface que todo objeto debe cumplir para ser registrado en el inspector
/// TListaPropiedades: Lista extraida del TDiccionarioPropiedades que contiene las propiedades para un panel en particular y oredenadas de acuerdo a cómo deben aparecen en el inspector

uses Classes, Types, Generics.Collections, System.SysUtils, Vcl.graphics, InspLinks, InspectorBar, advEdit;

type

   TValidaFuncion = function(PropertyName : string; Value : string) : boolean of object;
   TAyudaProc = procedure() of object;

   TiposEditores = class
   private
      class procedure setSigno(Sender : TObject; R : TRect; Item : TinspectorItem);
   public
      class var enteroPositivo : TAEInspectorEditLink;
      class var entero : TAEInspectorEditLink;
      class constructor ClassCreate;
      class destructor ClassDestroy;
   end;

   TPropiedad = class(TPersistent)
   private
      fPropertyName : string;
      fID_PANEL : Integer;
      fCaption : string;
      fPosicion : Integer;
      fEditLink : TInspectorEditLink;
      fVisible : boolean;
      fPropertyType : TPropertyType;
      fValueisEmpty : boolean;

      fhint : string;

      function GetValidaisNull() : boolean;

      (* ------------------------------------------------------------------- *)
   public
      FuncionValidacion : TValidaFuncion;
      CanModify : boolean;
      // esta propiedad sirve para indicarle al sistema que el valor inicial de la propiedad debe ser utilizada en el editor
      // ver el caso de MomentoI que requiere un valor inicial
      // if not PropiedadZ.ValueisEmpty then
      // IPanel.Items[POS].TextValue := valorDfault;

      property ValueisEmpty : boolean read fValueisEmpty write fValueisEmpty;
      property PropertyName : string read fPropertyName write fPropertyName;
      property hint : string read fhint write fhint;
      property ID_PANEL : Integer read fID_PANEL write fID_PANEL;
      property Caption : string read fCaption write fCaption;
      property Posicion : Integer read fPosicion write fPosicion;
      property EditLink : TInspectorEditLink read fEditLink write fEditLink;
      property Visible : boolean read fVisible write fVisible;
      property PropertyType : TPropertyType read fPropertyType write fPropertyType;
      property FuncionValidaconisNull : boolean read GetValidaisNull;

      constructor CreateP(PropertyName : string; ID_PANEL : Integer; Caption : string; Posicion : Integer; Visible : boolean; EditLink : TAEInspectorEditLink;FuncionValidacion : TValidaFuncion);
      procedure Assign(Source : TPersistent); override;
   end;

   TListaPropiedades = class(TList<TPropiedad>)
   private
      function GetByNombre(const ANombre : string) : TPropiedad;
   public
      property ItemsByNombre[const ANombre : string] : TPropiedad read GetByNombre; default;
      function BuscarPorPos(const APos : Integer) : TPropiedad;
   end;

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
      function existeLaPropiedad(const Nombre : string) : boolean;
      procedure Clear;
      // Iterador para for..in (retorna las propiedades)
      function GetEnumerator : TDictionary<string, TPropiedad>.TValueEnumerator;
      // Iterador de pares (clave-valor)
      function GetParesEnumerator : TDictionary<string, TPropiedad>.TPairEnumerator;
      // diferentes maneras de agregar una propiedad y su nombre
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TValidaFuncion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink; fv : TValidaFuncion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TValidaFuncion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer) : TPropiedad; overload;

      function GetPropiedadesPorPanel(const APanelID : Integer) : TListaPropiedades;

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

/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------
/// TListaPropiedades
/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------

constructor TPropiedad.CreateP(PropertyName : string; ID_PANEL : Integer; Caption : string; Posicion : Integer; Visible : boolean; EditLink : TAEInspectorEditLink;FuncionValidacion : TValidaFuncion);
begin
self.PropertyName := PropertyName;
self.hint := '';
self.Visible := Visible;
self.ID_PANEL := ID_PANEL;
self.Caption := Caption;
self.Posicion := Posicion;
self.EditLink := EditLink;
self.FuncionValidacion := FuncionValidacion;

// Asignación de valores por omisión
self.ValueisEmpty := False;
self.CanModify := True;
end;

procedure TPropiedad.Assign(Source : TPersistent);
begin
if Source is TPropiedad then
   begin
   PropertyName := TPropiedad(Source).PropertyName;
   ID_PANEL := TPropiedad(Source).ID_PANEL;
   Caption := TPropiedad(Source).Caption;
   Posicion := TPropiedad(Source).Posicion;
   EditLink := TPropiedad(Source).EditLink;
   Visible := TPropiedad(Source).Visible;
   PropertyType := TPropiedad(Source).PropertyType;
   ValueisEmpty := TPropiedad(Source).ValueisEmpty;
   FuncionValidacion := TPropiedad(Source).FuncionValidacion;
   end
else
   inherited;
end;

function TPropiedad.GetValidaisNull : boolean;
begin
if assigned(FuncionValidacion) then
   Result := False
else
   Result := True;
end;

/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------
/// TDiccionarioPropiedades
/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------

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

function TDiccionarioPropiedades.existeLaPropiedad(const Nombre : string) : boolean;
begin
Result := FDiccionario.ContainsKey(Nombre);
end;

procedure TDiccionarioPropiedades.agregarPropiedad(const Nombre : string; Prop : TPropiedad);
begin
if not existeLaPropiedad(Nombre) then
   FDiccionario.Add(Nombre, Prop)
else
   /// cualquier popiedad solo puede registrarse una sola vez
   /// y por lo tanto solo puede aparecer en un solo panel no se permite que se repitan propiedades en varios paneles
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
Result := TListaPropiedades.Create;
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
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink; fv : TValidaFuncion) : TPropiedad;
var
   Propiedad : TPropiedad;
begin
Propiedad := TPropiedad.CreateP(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, fv);
agregarPropiedad(PropertyName, Propiedad);
Result := Propiedad;
end;

(* ========================================================================== *)
// Con propiedad Visible y sin Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, nil);
end;

(* ========================================================================== *)
// Visible=TRUE y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TValidaFuncion) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TValidaFuncion) : TPropiedad;
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

/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------
/// TListaPropiedades
/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------

function TListaPropiedades.GetByNombre(const ANombre : string) : TPropiedad;
var
   P : TPropiedad;
begin
for P in self do
   if SameText(P.PropertyName, ANombre) then
      Exit(P);
Result := nil;
end;

function TListaPropiedades.BuscarPorPos(const APos : Integer) : TPropiedad;
var
   P : TPropiedad;
begin
for P in self do
   if P.Posicion = APos then
      Exit(P);
Result := nil;
end;

{ TTiposEditores }

class procedure TiposEditores.setSigno(Sender : TObject; R : TRect; Item : TinspectorItem);
var
   edit : TAdvEdit;
begin
edit := TAdvEdit(entero.GetEditor);
edit.Signed := True;
end;

class constructor TiposEditores.ClassCreate;
begin
enteroPositivo := TAEInspectorEditLink.Create(nil);
with enteroPositivo do
   begin
   name := 'enteroPositivo';
   EditAlign := eaLeft;
   EditColor := clWhite; // declarado en Vcl.graphics
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 0;
   end;

Ya quedo lo del signo falta actualizar el objeto una vez que se hace un cambio cuando se usa un TAEInspector


entero := TAEInspectorEditLink.Create(nil);
with entero do
   begin
   name := 'enteroPositivo';
   EditAlign := eaLeft;
   EditColor := clWhite; // declarado en Vcl.graphics
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 0;
   entero.OnSetProperties := setSigno;
   end;
end;

class destructor TiposEditores.ClassDestroy;
begin
enteroPositivo.Free;
end;

end.
