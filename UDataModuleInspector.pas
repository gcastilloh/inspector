unit UDataModuleInspector;

interface

uses
  System.SysUtils, System.Classes, InspectorBar, InspLinks;

type
  TDataModule2 = class(TDataModule)
    EditFlotante2dec: TAEInspectorEditLink;
    EditEntero: TAEInspectorEditLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
