{ Autor: Carlos Ruiz Aguilar
  Fecha: Agosto , 2014
  Beta 1.0
}
unit UPropiedad;

interface

uses Classes, atDiagram, DiagramUtils, Graphics, SysUtils, Windows, Winapi.Messages, InspectorBar, RTTIInspectorBar, Dialogs, InspLinks;

type
   TValidaFuncion = function(PropertyName : string; Value : string) : boolean of object;

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

      function GetValidaisNull() : boolean;

      (* ------------------------------------------------------------------- *)
   public
      FuncionValidacion : TValidaFuncion;
      CanModify : boolean;
      // esta propiedad sirve para indicarle al sistema que el valor inicial de la propiedad debe ser utilizada en el editor
      // ver el caso de MomentoI que requiere un valor inicial
      //        if not PropiedadZ.ValueisEmpty then
      //            IPanel.Items[POS].TextValue := valorDfault;

      property ValueisEmpty : boolean read fValueisEmpty write fValueisEmpty;
      property PropertyName : string read fPropertyName write fPropertyName;
      property ID_PANEL : Integer read fID_PANEL write fID_PANEL;
      property Caption : string read fCaption write fCaption;
      property Posicion : Integer read fPosicion write fPosicion;
      property EditLink : TInspectorEditLink read fEditLink write fEditLink;
      property Visible : boolean read fVisible write fVisible;
      property PropertyType : TPropertyType read fPropertyType write fPropertyType;
      property FuncionValidaconisNull : boolean read GetValidaisNull;

      constructor CreateP(PropertyNameV : string; ID_PANELV : Integer; CaptionV : string; PosicionV : Integer; V : boolean; EL : TAEInspectorEditLink;fv : TValidaFuncion);
      procedure Assign(Source : TPersistent); override;
   end;

implementation

{ TPropiedad }
// @@ Constructores @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
constructor TPropiedad.CreateP(PropertyNameV : string; ID_PANELV : Integer; CaptionV : string; PosicionV : Integer; V : boolean; EL : TAEInspectorEditLink;fv : TValidaFuncion);
begin
PropertyName := PropertyNameV;
Visible := V;
ID_PANEL := ID_PANELV;
Caption := CaptionV;
Posicion := PosicionV;
EditLink := EL;
FuncionValidacion := fv;
// Asignaciones Default
fValueisEmpty := False;
// gch 2019 aaa fModified := False;
CanModify := True;
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

// @@ Propiedades   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

(* ========================================================================== *)
function TPropiedad.GetValidaisNull : boolean;
begin
if assigned(FuncionValidacion) then
   Result := False
else
   Result := True;
end;

end.
