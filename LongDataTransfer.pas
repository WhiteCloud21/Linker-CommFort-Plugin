{ **** UBPFD *********** by delphibase.endimus.com ****
>> ��������� �������� � ������ ������� ������ ������, � ������ ������������
� ��������� ������ �������. �� ���������� TServerSocket,TClientSocket ..SendText

������ ������ �������� �������, ������� ��������� ��������� � ���������� ������� ����� ������.
� ��� ������� �������������� ��������� ������������ � ������ �������.
������ ��������� ������������� ��� �������� ��������� �����, � ����������
������ SendText, ReciveText TCustomSocket � ������������� ��� �������������
� ������������ TClientSocket, TServerSocket � ������ ����������� �� TCustomSocket.
������ ������� ���������� ��������� �������������, ��������� ��������� � �����������:
������������� �������� ������ ������ �������� 1-16000, ���� ���������� 15100 ������ ������.
����������� ��������� ����������� � ���������� ������ �������� �������� ����� ����
������ ��� ��������, ������ � ������������ ������.

����� �������������� ����� ����������� ���������������� ���������, �������
����� ���������� ������ ���, ����� ������� ��������� ���� ������. ������ ���������
������ ����� ���� ������� �������� ���� STRING:

procedure SomeUserProc(S:String);
begin
....
end;

������ �������� 3 �������, �� ������� �������������� ����� ������ 2
function SendLongText(Socket:TCustomWinSocket; S:String):boolean;
function ReceiveLongText(Socket:TCustomWinSocket;MySProc:TMySProc;SafeCalledStr :string = ''):boolean;
������ SendText ������ ��� �������� �������. � �������� ���������� �� ���������
������ TCustomWinSocket (�������� ��� ClientSocket.Socket) � ����������
����������� ������ S (ShortString,AnsiString,WideString).
� ������ �������� �������� ������� ���������� true, ����� false.
��� ��������� ����������� GetLastError().

function ReceiveLongText(Socket:TCustomWinSocket;MySProc:TMySProc;SafeCalledStr :string = ''):boolean;
������������ ��� ���������. ������ ������ ������ ���� ������� � ������� On*Read ����������.
� �������� ���������� ���������� �������� TCustomWinSocket (�������� ServerSocket.Socket) � ��� ���������,
����������� ��� ��������� ������ (��������, ����� �������������� SomeUserProc).
������ �������� ��������� �� �������!!!
��������� FlushBuffers �������� ���������� � ������� ����� ������,
� �������� ������������� ���������� �� ������.

�����������: ScktComp;
�����:       Subfire, subfire@mail.ru, ICQ:55161852, �����-���������
Copyright:   ������ ������ aka Subfire
����:        2 ������� 2002 �.
***************************************************** }

unit LongDataTransfer;

interface
uses Windows, SysUtils, Classes, ScktComp, comm_info, comm_data;
type
  TMySProc = procedure(const S: String);
function SendLongText(Socket: TCustomWinSocket; S: string): boolean;
function ReceiveLongText(Socket: TCustomWinSocket; MySProc: TMySProc;
  SafeCalledStr: AnsiString = ''): boolean;
procedure FlushBuffers;

var
  InputBuf: AnsiString;
  InputDataSize: LongWord;
  InputReceivedSize: LongWord;

implementation

function SendLongText(Socket: TCustomWinSocket; S: string): boolean;
var
  TextSize: DWord;
  SendStream: TMemoryStream;
begin
  Result := True;
  try
    if not Socket.Connected then
      Exit;
    TextSize := Length(S);
    SendStream := TMemoryStream.Create;
    SendStream.WriteBuffer(TextSize, 4);
    SendStream.WriteBuffer(PChar(S)^, TextSize * 2);
    SendStream.Seek(0, soFromBeginning);
    Socket.SendStream(SendStream);
  except Result := False;
  end;
end;

procedure FlushBuffers;
begin
  InputBuf := '';
  InputDataSize := 0;
  InputReceivedSize := 0;
end;

procedure CallSFunc(MySProc: TMySProc; Str: AnsiString); inline;
var
	CallStr: String;
begin
	SetLength(CallStr, Length(Str));
  CopyMemory(@CallStr[1], @Str[1], Length(Str));
  MySProc(CallStr);
end;

function ReceiveLongText(Socket:TCustomWinSocket;MySProc:TMySProc;SafeCalledStr :AnsiString = ''):boolean;
var
	S:AnsiString;
	RDSize:LongWord;
begin
Result:=True;
try
If SafeCalledStr='' then begin
	RDSize:=Socket.ReceiveLength;
	SetLength(S, RDSize);
	Socket.ReceiveBuf(S[1], RDSize);
end
else begin
	S:=SafeCalledStr;
	RDSize:=length(S);
end;
//PCorePlugin^.WriteLog(file_debug, 'Packet: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize));
If (Length(InputBuf)>0) and (InputDataSize = 0) then begin //�������������, � ��� ������
	S:=InputBuf+S; //���� �������������� ��� ���������
	FlushBuffers; //����� ������
end;
If InputBuf='' then
	begin //����� ������ �����;
  	// �������� ���������
  	if Length(S) < 4 then
    begin
      InputBuf := S;
      Exit;
    end;
  	CopyMemory(@InputDataSize, @S[1], 4);
		InputDataSize := InputDataSize * 2;
 		//MessageBox(0, PChar('First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
 		if InputDataSize=RDSize-4 then begin //���� ���� � ������
 			InputBuf:=Copy(S,5,RDSize-4); //�� ������, �� ������������ ���.
 			CallSFunc(MySProc, InputBuf);
 			FlushBuffers;
 			Exit;
		end;
 		if InputDataSize<RDSize-4 then begin //����� �������.
			InputBuf:=Copy(S,5,InputDataSize);
			CallSFunc(MySProc, InputBuf);
			Delete(S,1,InputDataSize+4);
			FlushBuffers;
			ReceiveLongText(Socket,MySProc,S);
			Exit;
 		end;
 		if InputDataSize>RDSize-4 then begin //��� ������ ��������
    	InputBuf:=Copy(S,5,RDSize-4); //�������� ������
    	InputReceivedSize:=RDSize-4;
 		end;
	end
	else begin //����� ������ �� ����
		//InputBuf:=
		If RDSize+InputReceivedSize=InputDataSize then
    begin //�������� ���������
    	InputBuf:=InputBuf+Copy(S,1,RDSize); //�������� �������
      CallSFunc(MySProc, InputBuf); //� ������, ������
      FlushBuffers; // � ������ ������ ���
      Exit;
    end;
		If RDSize+InputReceivedSize<InputDataSize then // ��������
    begin //���������
    	InputBuf:=InputBuf+Copy(S,1,RDSize); //��������
    	InputReceivedSize:=InputReceivedSize+RDSize;
    	Exit;
    end;
		If RDSize+InputReceivedSize>InputDataSize then //�������� ��������
    begin // �� � ������ ���� ��� ������ - �������.
    	InputBuf:=InputBuf+Copy(S,1,InputDataSize-InputReceivedSize);
      CallSFunc(MySProc, InputBuf);
      Delete(S,1,InputDataSize-InputReceivedSize);
      FlushBuffers;
      ReceiveLongText(Socket,MySProc,S);
    end;
	end;
except Result := false;//MessageBox(0, PChar(IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);//Result:=False;
end;
end;

end.
