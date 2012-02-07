unit libClasses;

interface

uses Windows, SysUtils, SyncObjs, ExtCtrls, Classes;

type
	TSyncTimer = class(TTimer)
  	private
    	fLock: TCriticalSection;
    protected
      procedure DoAction(Sender: TObject); virtual; abstract;
  	public
      property Lock: TCriticalSection read fLock;
      constructor Create; reintroduce;
      destructor Destroy; override;
    	procedure Call;
  end;

implementation

constructor TSyncTimer.Create;
begin
	inherited Create(nil);
  Enabled := False;
  Interval := 1;
  fLock := TCriticalSection.Create;
  OnTimer := DoAction;
end;

destructor TSyncTimer.Destroy;
begin
	fLock.Enter;
  inherited;
  fLock.Leave;
  fLock.Free;
end;

procedure TSyncTimer.Call;
begin
	fLock.Enter;
  Enabled := True;
  fLock.Leave;
end;

end.
