unit UDataModuleInspector;

interface

uses
  System.SysUtils, System.Classes, InspectorBar, InspLinks;

type
  TDM = class(TDataModule)
    EditFlotante2dec: TAEInspectorEditLink;
    EditEnteroPositivo: TAEInspectorEditLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
