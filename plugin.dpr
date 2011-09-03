{
  ! Прочитайте и не удаляйте этот комментарий, проявите уважение к его автору :)
}
// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
library link_plugin;

uses
  Windows,
  SysUtils,
  Classes,
  ExceptionJCLSupport,
  comm_info in 'comm_info.pas',
  comm_plugin in 'comm_plugin.pas',
  comm_data in 'comm_data.pas',
  link in 'link.pas',
  libfunc in 'libfunc.pas',
  libqueue in 'libqueue.pas',
  LongDataTransfer in 'LongDataTransfer.pas';

{$E cfplug}

function PluginStart(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData) : Boolean; stdcall;
begin
  ThisPlugin := TCommPlugin.Create(dwThisPluginID, @func1, @func2);
  //MessageBox(0, PChar( ExtractFileNameEx(GetDllPath, false) ), '', 0);
  result                       := true;
end;

function PluginStop : Byte; stdcall;
begin
  ThisPlugin.Free;
  Result :=1;
end;

procedure PluginProcess(dwMessageID : DWORD; bMessage : TBytes; dwMessageLength : DWORD); stdcall;
begin
  ThisPlugin.CorePlugin.Process(dwMessageID, bMessage, dwMessageLength);
end;

function PluginGetData(dwMessageID : DWORD; bInBuffer : PString; inBufferLength : DWORD; bOutBuffer : PAnsiChar; outBufferLength : DWORD): DWORD; stdcall;
var
  name: string;
  i: DWORD;
begin
  Result:=0;
  case dwMessageID of
    GD_PLUGIN_SERVER_OR_CLIENT:
    begin
      if outBufferLength>0 then
      begin
        I:=1;
        CopyMemory(bOutBuffer, @I, 4);
      end;
    end;

    GD_PLUGIN_NAME:
    begin
      Result:=Length(PLU_NAME)*2+4;
      if outBufferLength>0 then
      begin
        I:=Length(PLU_NAME);
        name:= PLU_NAME;
        CopyMemory(bOutBuffer, @I, 4);
        CopyMemory(bOutBuffer+4, @name[1], I*2);
      end;
    end;
  end;
end;

//function PluginPremoderation(dwMessageID : DWORD; bMessage : PCHAR; dwMessageLength : PDWORD) : boolean; stdcall;
//begin
//  result := ThisPlugin.CorePlugin.Premoderation(dwMessageID, bMessage, dwMessageLength);
//end;

exports PluginStart, PluginStop, PluginProcess, PluginGetData;

end.
