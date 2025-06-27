unit UListaPropiedades;
interface

uses System.Generics.Collections, System.SysUtils, UPropiedad;

type

   TListaPropiedades = class(TList<TPropiedad>)
   private
      function GetByNombre(const ANombre : string) : TPropiedad;
   public
      property ItemsByNombre[const ANombre : string] : TPropiedad read GetByNombre; default;
      function BuscarPorPos(const APos : Integer) : TPropiedad;
   end;

implementation


function TListaPropiedades.GetByNombre(const ANombre: string): TPropiedad;
var
  P: TPropiedad;
begin
  for P in Self do
    if SameText(P.PropertyName, ANombre) then
      Exit(P);
  Result := nil;
end;

function TListaPropiedades.BuscarPorPos(const APos: Integer): TPropiedad;
var
  P: TPropiedad;
begin
  for P in Self do
    if P.Posicion = APos then
      Exit(P);
  Result := nil;
end;



end.
