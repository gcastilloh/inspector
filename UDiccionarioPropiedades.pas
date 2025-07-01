unit UDiccionarioPropiedades;

interface

/// TPropiedad: Define las caracteristicas con las que se mostrar� una propiedad en el inspector
/// TDiccionarioPropiedades: Diccionario de propiedades que cada objeto que se registra en el inspector debe poseer
/// IPropiedades: Interface que todo objeto debe cumplir para ser registrado en el inspector
/// TListaPropiedades: Lista extraida del TDiccionarioPropiedades que contiene las propiedades para un panel en particular y oredenadas de acuerdo a c�mo deben aparecen en el inspector

uses Classes, Types, Generics.Collections, System.SysUtils, Vcl.graphics, InspLinks, InspectorBar, advEdit;

type

   TFuncionDeValidacion = function(PropertyName : string; Value : string) : boolean of object;
   TProcedimientoDeAyuda = procedure() of object;

   TiposEditores = class
   private
      class procedure setSigno(Sender : TObject; R : TRect; Item : TinspectorItem);
   public
      class var enteroPositivo : TAEInspectorEditLink;
      class var entero : TAEInspectorEditLink;
      class var real1d : TAEInspectorEditLink;
      class var real2d : TAEInspectorEditLink;
      class var real3d : TAEInspectorEditLink;
      class var real4d : TAEInspectorEditLink;
      class var real5d : TAEInspectorEditLink;
      class var real6d : TAEInspectorEditLink;
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
      FuncionValidacion : TFuncionDeValidacion;
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
      property EditLinkAUsar : TInspectorEditLink read fEditLink write fEditLink;
      property Visible : boolean read fVisible write fVisible;
      property PropertyType : TPropertyType read fPropertyType write fPropertyType;
      property FuncionValidaconisNull : boolean read GetValidaisNull;

      constructor CreateP(PropertyName : string; ID_PANEL : Integer; Caption : string; Posicion : Integer; Visible : boolean; EditLink : TAEInspectorEditLink;FuncionValidacion : TFuncionDeValidacion);
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
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TFuncionDeValidacion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink; fv : TFuncionDeValidacion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TFuncionDeValidacion) : TPropiedad; overload;
      function SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer) : TPropiedad; overload;

      function GetPropiedadesPorPanel(const APanelID : Integer) : TListaPropiedades;

      property Items[const Nombre : string] : TPropiedad read obtenerPropiedad; default;
      property Pares : TDictionary<string, TPropiedad>.TPairEnumerator read GetParesEnumerator;

   end;

   IPropiedades = interface
      ['{D4B20A1E-1234-4F76-9FDE-FA1234567890}']
      // GUID �nico
      function GetPropiedades : TDiccionarioPropiedades;
      function GetAyudaProc : TProcedimientoDeAyuda;
      property Propiedades : TDiccionarioPropiedades read GetPropiedades;
      property ayuda : TProcedimientoDeAyuda read GetAyudaProc;
   end;

implementation

uses System.Generics.Defaults;

/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------
/// TListaPropiedades
/// ----------------------------------------------------------------------------------------
/// ----------------------------------------------------------------------------------------

constructor TPropiedad.CreateP(PropertyName : string; ID_PANEL : Integer; Caption : string; Posicion : Integer; Visible : boolean; EditLink : TAEInspectorEditLink;FuncionValidacion : TFuncionDeValidacion);
begin
self.PropertyName := PropertyName;
self.hint := '';
self.Visible := Visible;
self.ID_PANEL := ID_PANEL;
self.Caption := Caption;
self.Posicion := Posicion;
self.EditLinkAUsar := EditLink;
self.FuncionValidacion := FuncionValidacion;

// Asignaci�n de valores por omisi�n
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
   EditLinkAUsar := TPropiedad(Source).EditLinkAUsar;
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
// El diccionario es due�o de las TPropiedad y las libera autom�ticamente
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
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink; fv : TFuncionDeValidacion) : TPropiedad;
var
   Propiedad : TPropiedad;
begin
Propiedad := TPropiedad.CreateP(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, fv);
agregarPropiedad(PropertyName, Propiedad);
Result := Propiedad;
//
//  TPropertyType = (ptInteger, ptFloat, ptText, ptColor, ptFont, ptBoolean, ptValues,
//    ptIntSpin, ptTextButton, ptPropButton, ptDate, ptTime, ptCustom, ptValuesList, ptFixedColor, ptButton, ptPassword, ptPicture);
//
//  TAdvEditType = (etString, etNumeric, etFloat, etUppercase, etMixedCase, etLowerCase,
//    etPassword, etMoney, etRange, etHex, etAlphaNumeric, etValidChars, etInvalidChars);
  if EditLink = TiposEditores.enteroPositivo then
     result.PropertyType := ptInteger
  else if EditLink = TiposEditores.entero then
     result.PropertyType := ptInteger
  else if (EditLink = TiposEditores.real1d)
     or   (EditLink = TiposEditores.real2d)
     or   (EditLink = TiposEditores.real3d)
     or   (EditLink = TiposEditores.real4d)
     or   (EditLink = TiposEditores.real5d)
     or   (EditLink = TiposEditores.real6d) then
   result.PropertyType := ptFloat
else
   result.PropertyType := ptText;

end;

(* ========================================================================== *)
// Con propiedad Visible y sin Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; Visible : boolean; EditLink : TAEInspectorEditLink) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, Visible, EditLink, nil);
end;

(* ========================================================================== *)
// Visible=TRUE y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; EditLink : TAEInspectorEditLink; fv : TFuncionDeValidacion) : TPropiedad;
begin
Result := SetPropiedad(PropertyName, ID_PANEL, PropertyCaption, POS, True, EditLink, fv);
end;

(* ========================================================================== *)
// Visible=TRUE y sin EditLink y con Funcion de Validacion
function TDiccionarioPropiedades.SetPropiedad(PropertyName : string; ID_PANEL : Integer; PropertyCaption : string; POS : Integer; fv : TFuncionDeValidacion) : TPropiedad;
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
edit := TAdvEdit((Sender as TAEInspectorEditLink).GetEditor);
edit.Signed := True;
end;


//   TAdvEditType = (etString, etNumeric, etFloat, etUppercase, etMixedCase, etLowerCase,
//    etPassword, etMoney, etRange, etHex, etAlphaNumeric, etValidChars, etInvalidChars);
class constructor TiposEditores.ClassCreate;
var
   edit : TAdvEdit;
begin
enteroPositivo := TAEInspectorEditLink.Create(nil);
with enteroPositivo do
   begin
   name := 'enteroPositivo';
   EditAlign := eaLeft;
   EditColor := clWhite; // declarado en Vcl.graphics
   EditStyle := esInplace;
   EditType := etNumeric;
   Precision := 0;
   end;

entero := TAEInspectorEditLink.Create(nil);
with entero do
   begin
   name := 'entero';
   EditAlign := eaLeft;
   EditColor := clWhite; // declarado en Vcl.graphics
   EditStyle := esInplace;
   EditType := etNumeric;
   Precision := 0;
   // se usa un evento porque en la construccion de la clase aun no se dispone de getEditor
   // se usa este evento para forzar a que los enteros tengan signo.
   entero.OnSetProperties := setSigno;
   end;

real1d := TAEInspectorEditLink.Create(nil);
with real1d do
   begin
   name := 'real1d';
   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 1;
   real1d.OnSetProperties := setSigno;
   end;

real2d := TAEInspectorEditLink.Create(nil);
with real2d do
   begin
   name := 'real2d';
   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 2;
   real2d.OnSetProperties := setSigno;
   end;

real3d := TAEInspectorEditLink.Create(nil);
with real3d do
   begin
   name := 'real3d';

   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 3;
   real3d.OnSetProperties := setSigno;
   end;

real4d := TAEInspectorEditLink.Create(nil);
with real4d do
   begin
   name := 'real4d';
   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 4;
   real4d.OnSetProperties := setSigno;
   end;

real5d := TAEInspectorEditLink.Create(nil);
with real5d do
   begin
   name := 'real5d';
   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 5;
   real5d.OnSetProperties := setSigno;
   end;

real6d := TAEInspectorEditLink.Create(nil);
with real6d do
   begin
   name := 'real6d';
   EditAlign := eaLeft;
   EditColor := clWhite;
   EditStyle := esInplace;
   EditType := etFloat;
   Precision := 6;
   real6d.OnSetProperties := setSigno;
   end;

end;

class destructor TiposEditores.ClassDestroy;
begin
enteroPositivo.Free;
entero.Free;
real1d.Free;
real2d.Free;
real3d.Free;
real4d.Free;
real5d.Free;
real6d.Free;

end;

end.
