unit UFrInspector;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, InspectorBar, RTTIInspectorBar, Vcl.ExtCtrls, Generics.Collections, UPatronObservador,
   UDiccionarioPropiedades;

type

   TFrInspector = class(TFrame, ISujetoModificacionPropuesta)
      cmbObjetos : TComboBox;
      Panel3 : TPanel;
      Panel2 : TPanel;
      Inspector : TRTTIInspectorBar;
      BtnAyuda : TButton;
      procedure cmbObjetosSelect(Sender : TObject);
      procedure BtnAyudaClick(Sender : TObject);
      procedure InspectorEditStart(Sender : TObject; AInspectorPanel : TInspectorPanel; AInspectorItem : TInspectorItem);
      procedure InspectorEditStop(Sender : TObject; AInspectorPanel : TInspectorPanel; AInspectorItem : TInspectorItem);

   private
      { Private declarations }
      FObserverList : TList<IObservadorModificacionPropuesta>;

      /// IPanelGenerales : TRTTIInspectorPanel;
      // IPanelFisicas : TRTTIInspectorPanel;
      // IPanelOperacion : TRTTIInspectorPanel;
      // IPanelNoInfo : TRTTIInspectorPanel;
      // IPanelOtros : TRTTIInspectorPanel;
      // IPanelOtros2 : TRTTIInspectorPanel;

      ayudaProc : TProcedimientoDeAyuda;

      procedure SetValoresDefault();
      { esto es solo para el patron observador }
      procedure NotifyObservers;
      procedure RefrescaNoEditables(AInspectorPanel : TInspectorPanel);
      procedure preparaPaneles;

   public
      { Public declarations }
      constructor Create(AOwner : TComponent); override;
      procedure registraObjeto(nombre : string; objeto : TPersistent);
      function CreaPanel(Caption : string) : TRTTIInspectorPanel;
      procedure seleccionaObjeto(objeto : TPersistent);
      procedure desregistraObjeto(v : TPersistent);
      procedure Oculta_paneles();
      procedure clear;

      { esto es solo para el patron observador }
      procedure RegisterObserver(observador : IObservadorModificacionPropuesta);
      procedure RemoveObserver(observador : IObservadorModificacionPropuesta);
      procedure clearObserverList;

   end;

implementation

uses AdvEdit;

{$R *.dfm}

constructor TFrInspector.Create(AOwner : TComponent);
begin
inherited;
FObserverList := TList<IObservadorModificacionPropuesta>.Create;
SetValoresDefault();

cmbObjetos.Text := '';
cmbObjetos.Visible := true;

// --
BtnAyuda.Visible := true;
Inspector.ShowHint := true;
// creaPanel('Generales');
BtnAyuda.Visible := False;

// RegisterObserver(ObservadorModificacionPropuesta);
end;

procedure TFrInspector.cmbObjetosSelect(Sender : TObject);
begin
seleccionaObjeto(TPersistent(cmbObjetos.Items.Objects[cmbObjetos.itemIndex]));
end;

(* ========================================================================== *)
{ Limpiamos el Inspector }
procedure TFrInspector.clear;
var
   idPanel : integer;
begin
cmbObjetos.clear;
for idPanel := 0 to Inspector.Panels.Count - 1 do
   begin
   Inspector.Panels[idPanel].RTTIComponent := nil;
   end;
end;

procedure TFrInspector.Oculta_paneles;
var
   idPanel : integer;
begin
cmbObjetos.itemIndex := -1;
for idPanel := 0 to Inspector.Panels.Count - 1 do
   begin
   Inspector.Panels[idPanel].Visible := False;
   end;
end;

(* ========================================================================== *)
{ Valores default del inspector }
procedure TFrInspector.SetValoresDefault;
begin
Inspector.EditBtn.ButtonCaption := '···'; // ▪▪▪ ··· •••  …
Inspector.EditBtn.ButtonWidth := 25;
Inspector.CheckTextShow := False;
Inspector.Mode := imMultiPanelActive;
end;

{ Inicializamos los paneles }
function TFrInspector.CreaPanel(Caption : string) : TRTTIInspectorPanel;
var
   IPanel : TRTTIInspectorPanel;
begin
IPanel := Inspector.Panels.add;
IPanel.Caption := Caption;
IPanel.Open := true;
IPanel.Visible := False;
IPanel.Style := psProperties;
IPanel.CaptionWidth := 150;
IPanel.GridLines := true;
IPanel.Indent := 0;
IPanel.EditBorderColor := clGray;
IPanel.EditBox := true;
IPanel.ItemHeight := 26;
IPanel.AllowResize := true;
result := IPanel;
end;

procedure TFrInspector.preparaPaneles;
var
   IPanel : TRTTIInspectorPanel;
   idPanel : integer;
   objeto : TPersistent;
   propiedad : TPropiedad;
   interfazPropiedades : IPropiedades;
   ListaOrdenadaDePropiedades : TListaPropiedades;
   k : integer;
   existeInterfaz : boolean;
   cadenaAux : string;

   procedure Swap(Collection : TInspectorItems; Index1, Index2 : integer);
   var
      Item1, Item2 : TCollectionItem;
   begin
   // Nota importante:
   // TInspectorItems es del tipo TOwnedCollection donde Items[] es un arreglo de TCollectionItem, pero el setter (Items[] := ...) no está expuesto directamente.
   // Entonces, el código anterior no compilará directamente sin un truco:
   // En realidad, necesitas intercambiar las propiedades manualmente o bien modificar el orden indirectamente,
   // ya que TOwnedCollection no permite reasignar Items[].
   //
   // Notas técnicas:
   // Item.Index := NewIndex reordena el elemento dentro de la colección.
   // Este enfoque funciona con cualquier clase basada en TOwnedCollection y TCollectionItem.
   // No necesitas acceder a Items[] := porque Delphi no lo permite directamente.

   if (Collection = nil) or (Index1 = Index2) or (Index1 < 0) or (Index1 >= Collection.Count) or (Index2 < 0) or (Index2 >= Collection.Count) then
      Exit;

   Item1 := Collection.Items[Index1];
   Item2 := Collection.Items[Index2];

   // Reasignar los índices — esto los intercambia
   Item1.Index := Index2;
   Item2.Index := Index1;
   end;

begin
if (cmbObjetos.itemIndex < 0) then
   begin
   Exit;
   end;

objeto := cmbObjetos.Items.Objects[cmbObjetos.itemIndex] as TPersistent;
existeInterfaz := Supports(objeto, IPropiedades, interfazPropiedades);
ayudaProc := interfazPropiedades.ayuda;
BtnAyuda.Visible := assigned(ayudaProc);
for idPanel := 0 to Inspector.Panels.Count - 1 do
   begin
   IPanel := Inspector.Panels[idPanel];
   IPanel.RTTIComponent := objeto;
   IPanel.Visible := False;
   if existeInterfaz then
      begin
      IPanel.Visible := False;
      // obtiene la lisata de propiedades para el panel idPanel ordenada por su posicion
      ListaOrdenadaDePropiedades := interfazPropiedades.Propiedades.GetPropiedadesPorPanel(idPanel);

      // ayudaProc := objeto as
      if ListaOrdenadaDePropiedades.Count > 0 then
         begin
         IPanel.Visible := true;

         // coloca las popiedades visibles de la lista ordenada hasta arriba del panel
         // para cada propiedad del panel determinar k := 0,1,2,...  si esta en la lista ordenada en la posicion posDestino de ser asi intercambiar
         for k := 0 to IPanel.Items.Count - 1 do
            begin
            propiedad := ListaOrdenadaDePropiedades[IPanel.Items[k].Caption];
            if propiedad <> nil then
               begin
               Swap(IPanel.Items, k, propiedad.Posicion);
               // hace visibles unicamente las propuedades que estan registradas en ListaOrdenadaDePropiedades
               // las restantes las oculta
               IPanel.Items[propiedad.Posicion].Visible := true;
               end
            end;

         // establece las caracteristicas de las propiedades visibles del panel
         for k := 0 to IPanel.Items.Count - 1 do
            begin
            // obtiene la propiedad asociada al caption (el nombre de la propiedad es el caption en el inspector)
            propiedad := ListaOrdenadaDePropiedades[IPanel.Items[k].Caption];
            // si la propiedad existe aplica las especificaciones de la propiedad
            IPanel.Items[k].Visible := propiedad <> nil;
            if propiedad <> nil then
               begin
               IPanel.Items[k].ItemObject := propiedad; // la TPropiedad queda registrada en itemObject para futuras referencias
               IPanel.Items[k].Caption := propiedad.Caption; // ojo aqui se cambió el caption de la propiedad
               if propiedad.EditLinkAUsar <> nil then
                  begin
                  cadenaAux := IPanel.Items[k].TextValue;
                  IPanel.Items[k].PropertyType := ptCustom;
                  IPanel.Items[k].EditLink := propiedad.EditLinkAUsar;
                  IPanel.Items[k].TextValue := cadenaAux;
                  end;
               IPanel.Items[k].ReadOnly := not propiedad.CanModify;
               if propiedad.hint <> '' then
                  IPanel.Items[k].hint := propiedad.hint
               else
                  IPanel.Items[k].hint := propiedad.Caption;
               end;

            end;
         end
      end;
   end;

end;

(* ========================================================================== *)
{ Evento que es lanzado cuando se TERMINA a editar un elemento del Inspector }

procedure TFrInspector.BtnAyudaClick(Sender : TObject);
begin
if assigned(ayudaProc) then
   ayudaProc();
end;

/// seleccionaObjeto : agrega el objeto al combo box de objetos, lo selecciona en el combo box
/// y prepara los paneles para que muestren su información (llama a preparaPaneles, este es el único lugar
/// donde se llama a prepararPaneles.
procedure TFrInspector.seleccionaObjeto(objeto : TPersistent);
begin
if objeto <> nil then
   begin
   if cmbObjetos.Items.IndexOfObject(objeto) < 0 then
      Exit;
   cmbObjetos.itemIndex := cmbObjetos.Items.IndexOfObject(objeto);
   preparaPaneles;
   end;
end;

(* ========================================================================== *)
{ Metodo publico para registrar objeto }
{ solamente registra Dispositivos }
{ esete metodo solo registra el objeto en el Combo pero NO LO SELECCIONA PORO LO QUE DEBERA DESPUES llamar a selectObjeto }
procedure TFrInspector.registraObjeto(nombre : string; objeto : TPersistent);
begin
/// Control del Combo box////////////////////////////////////////////////////
if cmbObjetos.Items.IndexOfObject(objeto) < 0 then
   begin
   cmbObjetos.AddItem(nombre, objeto);
   seleccionaObjeto(objeto);
   end;
end;

(* ========================================================================== *)
{ Desregistro del objeto }
procedure TFrInspector.desregistraObjeto(v : TPersistent);
var
   k : integer;
begin
//
Oculta_paneles();
//
k := cmbObjetos.Items.IndexOfObject(v);
cmbObjetos.Items.Delete(k);
if not(cmbObjetos.Items.Count > 0) then
   begin
   clear;
   end;
end;

procedure TFrInspector.InspectorEditStart(Sender : TObject; AInspectorPanel : TInspectorPanel; AInspectorItem : TInspectorItem);
begin
//
end;

procedure TFrInspector.InspectorEditStop(Sender : TObject; AInspectorPanel : TInspectorPanel; AInspectorItem : TInspectorItem);
var
   cadena : string;
   propiedad : TPropiedad;
   valida : TFuncionDeValidacion;
   isCorrect : boolean;
   v : TPersistent;
   Index : integer;
begin

propiedad := TPropiedad(AInspectorItem.ItemObject);

// enteroPositivo.EditStyle := esInplace;
// enteroPositivo.EditType := etFloat;
// entero.EditStyle := esInplace;
// entero.EditType := etFloat;

// Caso 1:Si ha sido asignado un Edit link a la propiedad /////////////////
if (propiedad <> nil) and (propiedad.EditLinkAUsar <> nil) then
   begin
   cadena := AInspectorItem.TextValue;
   if cadena <> '' then
      begin

      // Validacion de la propiedad que se acaba de editar

      if not propiedad.FuncionValidaconisNull then
         begin
         valida := propiedad.FuncionValidacion;
         isCorrect := valida(propiedad.PropertyName, cadena);
         // gch 2019 marzo 10 aqui es donde se hace el juego de cambio de color a rojo
         // no me gusta... pero funciona, cuando Modified se hace true el control
         // recibe la notificacion y cambia a rojo porque la propiedad de color cuando se modifica esta
         // colocada a rojo en el AIEL... del dat module
         if not isCorrect then
            begin
            AInspectorItem.Modified := true; // cambia a rojo
            end
         else
            begin
            NotifyObservers;
            AInspectorItem.Modified := False; // <--- se mantiene en negro
            end;
         end
      else
         begin
         NotifyObservers;
         end;
      // Se experimento cierto comportamiento no  esperado cuando se intentan
      // guardar enteros
      AInspectorItem.PropertyType := propiedad.PropertyType;
      if (propiedad.PropertyType = TPropertyType.ptIntSpin) or (propiedad.PropertyType = TPropertyType.ptInteger) then
         begin // Enteros
         AInspectorItem.IntValue := StrToInt(cadena);
         end
      else
         begin
         AInspectorItem.TextValue := cadena;
         propiedad.ValueisEmpty := False;
         end;
      end
   else
      begin
      { Si planea usar la cadena vacia }
      propiedad.ValueisEmpty := true;
      AInspectorItem.TextValue := '';
      end;
   end;
end;




(* ========================================================================== *)

{
  function TFrInspector.GetModified: boolean;
  begin
  Result := fModified;
  end;
}

procedure TFrInspector.RefrescaNoEditables(AInspectorPanel : TInspectorPanel);
// var
// k : Integer;
begin
// --- este proceso refreca todos los campos que no son editables
// se invoca solo para aquellos campos que tienene EditLink
// for k := 0 to AInspectorPanel.Items.Count do
// begin
// if not TPropiedad(AInspectorPanel.Items[k]).CanModify then
// begin
//
// end;
// end;

end;

{ ========================================================================= }
{ ========================================================================= }
{ modelo del patron observador }
{ ========================================================================= }
{ ========================================================================= }

procedure TFrInspector.NotifyObservers;
var
   observador : IObservadorModificacionPropuesta;
begin
for observador in FObserverList do
   begin
   observador.UpdateObservador(true); // notifica a los observadores el cambio de estado (se ha modificado)
   end;
end;

procedure TFrInspector.RegisterObserver(observador : IObservadorModificacionPropuesta);
begin
if FObserverList.IndexOf(observador) = -1 then
   FObserverList.add(observador);
end;

procedure TFrInspector.RemoveObserver(observador : IObservadorModificacionPropuesta);
begin
FObserverList.Remove(observador);
end;

procedure TFrInspector.clearObserverList;
begin
FObserverList.clear;
end;

end.
