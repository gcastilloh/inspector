unit UFrInspector;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, InspectorBar, RTTIInspectorBar, Vcl.ExtCtrls, Generics.Collections, UPatronObservador,
   UListaPropiedades, UPropiedad, UFuncionesCallBack;

type

   TFrInspector = class(TFrame, ISujetoModificacionPropuesta)
      cmbObjetos : TComboBox;
      Panel3 : TPanel;
      Panel2 : TPanel;
      Inspector : TRTTIInspectorBar;
      BtnAyuda : TButton;
      procedure cmbObjetosSelect(Sender : TObject);
      procedure BtnAyudaClick(Sender : TObject);

   private
      { Private declarations }
      FObserverList : TList<IObservadorModificacionPropuesta>;

      IPanelGenerales : TRTTIInspectorPanel;
      // IPanelFisicas : TRTTIInspectorPanel;
      // IPanelOperacion : TRTTIInspectorPanel;
      // IPanelNoInfo : TRTTIInspectorPanel;
      // IPanelOtros : TRTTIInspectorPanel;
      // IPanelOtros2 : TRTTIInspectorPanel;

      ayudaProc : TAyudaProc;

      procedure SetValoresDefault();
      procedure InicializarPanel(IPanel : TRTTIInspectorPanel);
      { esto es solo para el patron observador }
      procedure NotifyObservers;
      procedure RefrescaNoEditables(AInspectorPanel : TInspectorPanel);
   protected

   public
      { Public declarations }
      constructor Create(AOwner : TComponent); override;
      procedure registraObjeto(nombre : string; objeto : TPersistent);
      procedure seleccionaObjeto(v : TPersistent);
      procedure desregistraObjeto(v : TPersistent);
      procedure Oculta_paneles();
      procedure clear;

      { esto es solo para el patron observador }
      procedure RegisterObserver(observador : IObservadorModificacionPropuesta);
      procedure RemoveObserver(observador : IObservadorModificacionPropuesta);
      procedure clearObserverList;

   end;

implementation

{$R *.dfm}

procedure TFrInspector.cmbObjetosSelect(Sender : TObject);
begin
seleccionaObjeto(TPersistent(cmbObjetos.Items.Objects[cmbObjetos.itemIndex]));
end;

constructor TFrInspector.Create(AOwner : TComponent);
begin
inherited;
FObserverList := TList<IObservadorModificacionPropuesta>.Create;

cmbObjetos.Text := '';
cmbObjetos.Visible := true;

// --
BtnAyuda.Visible := true;
Inspector.ShowHint := true;
IPanelGenerales := Inspector.Panels.add;
IPanelGenerales.Open := true;
IPanelGenerales.Visible := false;
InicializarPanel(IPanelGenerales);

// RegisterObserver(ObservadorModificacionPropuesta);
end;

(* ========================================================================== *)
{ Limpiamos el Inspector }
procedure TFrInspector.clear;
begin
cmbObjetos.clear;
IPanelGenerales.RTTIComponent := nil;
end;

procedure TFrInspector.Oculta_paneles;
begin
cmbObjetos.itemIndex := -1;
IPanelGenerales.Visible := false;
end;

(* ========================================================================== *)
{ Valores default del inspector }
procedure TFrInspector.SetValoresDefault;
begin
Inspector.EditBtn.ButtonCaption := '···'; // ▪▪▪ ··· •••  …
Inspector.EditBtn.ButtonWidth := 25;
Inspector.CheckTextShow := false;
Inspector.Mode := imMultiPanelActive;
end;

{ Inicializamos los paneles }
procedure TFrInspector.InicializarPanel(IPanel : TRTTIInspectorPanel);
begin
IPanel.Style := psProperties;
IPanel.CaptionWidth := 150;
IPanel.GridLines := true;
IPanel.Indent := 0;
IPanel.EditBorderColor := clGray;
IPanel.EditBox := true;
IPanel.ItemHeight := 26;
IPanel.AllowResize := true;
end;

procedure TFrInspector.BtnAyudaClick(Sender : TObject);
begin
if assigned(ayudaProc) then
   ayudaProc();
end;

procedure TFrInspector.seleccionaObjeto(v : TPersistent);

var
   par : TPair<string,TPropiedad>;
   propiedad : TPropiedad;
   interfazPropiedades : IPropiedades;
   listaPropiedades : TListaPropiedades;
   k : Integer;

   procedure registraHints(IPanel : TRTTIInspectorPanel);
   var
      k : Integer;
   begin
   // establece como hint el texto de caption
   for k := 0 to IPanel.Items.Count - 1 do
      begin
      IPanel.Items[k].hint := IPanel.Items[k].caption;
      end;
   end;

begin
if v <> nil then
   begin
   if cmbObjetos.Items.IndexOfObject(v) < 0 then
      exit;
   cmbObjetos.itemIndex := cmbObjetos.Items.IndexOfObject(v);
   IPanelGenerales.Visible := true;
   IPanelGenerales.caption := 'Generales';

   IPanelGenerales.RTTIComponent := v;
   if Supports(v, IPropiedades, interfazPropiedades) then
      begin
      listaPropiedades := interfazPropiedades.Propiedades;
      /// para cada propiedad del objeto decide si:
      /// es visible siempre que exista en la listaPropiedades
      ///   es invisible si no está en ella
      for k := 0 to IPanelGenerales.items.count-1 do
          begin
          propiedad := listaPropiedades.buscaPropiedadPorNombre(IPanelGenerales.items[k].Name);
          IPanelGenerales.items[k].Visible := propiedad <>nil;
          end;
      end;
   registraHints(IPanelGenerales);
   end;
end;

(* ========================================================================== *)
{ Metodo publico para registrar objeto }
{ solamente registra Dispositivos }
{ esete metodo solo registra el objeto en el Combo pero NO LO SELECCIONA PORO LO QUE DEBERA DESPUES selectObjeto }
procedure TFrInspector.registraObjeto(nombre : string; objeto : TPersistent);
var
   IPanel : TRTTIInspectorPanel;
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
   k : Integer;
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

(* ========================================================================== *)

{ modelo del patron observador }

{
  function TFrInspector.GetModified: boolean;
  begin
  Result := fModified;
  end;
}
procedure TFrInspector.NotifyObservers;
var
   observador : IObservadorModificacionPropuesta;
begin
for observador in FObserverList do
   begin
   observador.UpdateObservador(true); // notifica a los observadores el cambio de estado (se ha modificado)
   end;
end;

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
