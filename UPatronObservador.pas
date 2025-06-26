unit UPatronObservador;

interface

type
    IObservadorModificacionPropuesta = interface
        procedure UpdateObservador(modificado:boolean);
    end;

    ISujetoModificacionPropuesta = interface
        procedure clearObserverList;
        procedure RegisterObserver(aObserver: IObservadorModificacionPropuesta);
        procedure RemoveObserver(aObserver: IObservadorModificacionPropuesta);
        procedure NotifyObservers;
    end;

    IObservadorSeleccionPropuesta = interface
        procedure UpdateObservador(elegida:integer);
    end;

    ISujetoSeleccionPropuesta = interface
        procedure clearObserverList;
        procedure RegisterObserver(aObserver: IObservadorSeleccionPropuesta);
        procedure RemoveObserver(aObserver: IObservadorSeleccionPropuesta);
        procedure NotifyObservers;
    end;

implementation


end.
