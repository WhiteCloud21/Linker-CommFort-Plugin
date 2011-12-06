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
uses Windows, SysUtils, ScktComp;
type
  TMySProc = procedure(const S: String);
function SendLongText(Socket: TCustomWinSocket; S: string): boolean;
function ReceiveLongText(Socket: TCustomWinSocket; MySProc: TMySProc;
  SafeCalledStr: string = ''): boolean;
procedure FlushBuffers;
//function ReceiveLongText(Socket: TCustomWinSocket; MySProc: TMySProc;
//  SafeCalledStr: AnsiString = ''): boolean;

var
  //InputBuf: AnsiString;
  InputBuf: String;
  InputDataSize: LongWord;
  InputReceivedSize: LongWord;

implementation

function SendLongText(Socket: TCustomWinSocket; S: string): boolean;
var
  TextSize: DWord;
  TSSig: String;
begin
  Result := True;
  try
    if not Socket.Connected then
      Exit;
    TextSize := Length(S);
    TSSig:='  ';
    CopyMemory(@TSSig[1], @TextSize, 4);
    S := TSSig + S;
    Socket.SendBuf(Pointer(S)^, Length(S)*2);
  except Result := False;
  end;
end;

procedure FlushBuffers;
begin
  InputBuf := '';
  InputDataSize := 0;
  InputReceivedSize := 0;
end;

function ReceiveLongText(Socket: TCustomWinSocket; MySProc: TMySProc;
  SafeCalledStr: string = ''): boolean;
var
  S: string;
  RDSize: LongWord;
begin
  Result := True;
  try
    if SafeCalledStr = '' then
    begin
      RDSize := Socket.ReceiveLength;
      if RDSize < 2 then begin
        Result:=False;
        Exit;
      end;
      SetLength(S, RDSize div 2);
      Socket.ReceiveBuf(S[1], RDSize - RDSize mod 2);
    end
    else
    begin
      S := SafeCalledStr;
      RDSize := length(S)*2;
    end;
    if (Length(InputBuf) < 2) and (Length(InputBuf) > 0) then
    begin //�������������, � ��� ������
      S := InputBuf + S; //���� �������������� ��� ���������
      FlushBuffers; //����� ������
    end;
    if InputBuf = '' then
    begin //����� ������ �����;
    	if Length(S)< 2 then // �������������� ���������
      begin
      	InputBuf := S;
        Exit;
      end;
      CopyMemory(@InputDataSize, @S[1], 4);
      //MessageBox(0, PChar('First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
      if InputDataSize = RDSize div 2 - 2 then
      begin //���� ���� � ������

        InputBuf := Copy(S, 3, RDSize div 2 - 2); //�� ������, �� ������������ ���.
        MySProc(InputBuf);
        FlushBuffers;
        Exit;
      end;
      if InputDataSize < RDSize div 2 - 2 then
      begin //����� �������.
        //MessageBox(0, PChar(IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
        InputBuf := Copy(S, 3, InputDataSize);
        MySProc(InputBuf);
        Delete(S, 1, InputDataSize + 2);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
        Exit;
      end;
      if InputDataSize > RDSize div 2 - 2 then
      begin //��� ������ ��������
        InputBuf := Copy(S, 3, RDSize div 2 - 2); //�������� ������
        InputReceivedSize := RDSize div 2 - 2;
      end;
    end
    else
    begin //����� ������ �� ����
      //InputBuf:=
      //MessageBox(0, PChar('NOT First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);

      if RDSize div 2 + InputReceivedSize = InputDataSize then
      begin //�������� ���������
        InputBuf := InputBuf + Copy(S, 1, RDSize div 2); //�������� �������
        MySProc(InputBuf); //� ������, ������
        FlushBuffers; // � ������ ������ ���
        Exit;
      end;
      if RDSize div 2 + InputReceivedSize < InputDataSize then // ��������
      begin //���������
        InputBuf := InputBuf + Copy(S, 1, RDSize div 2); //��������
        InputReceivedSize := InputReceivedSize + RDSize div 2;
        Exit;
      end;
      if RDSize div 2 + InputReceivedSize > InputDataSize then //�������� ��������
      begin // �� � ������ ���� ��� ������ - �������.
        //MessageBox(0, PChar(IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
        InputBuf := InputBuf + Copy(S, 1, InputDataSize - InputReceivedSize);
        MySProc(InputBuf);
        Delete(S, 1, InputDataSize - InputReceivedSize);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
      end;
    end;
  except Result := False;
  end;
  SetLength(S, 0);
end;

{function ReceiveLongText(Socket: TCustomWinSocket; MySProc: TMySProc;
  SafeCalledStr: AnsiString = ''): boolean;
var
  S: AnsiString;
  RDSize: LongWord;
  F: AnsiString;
begin
  Result := True;
  try
    if SafeCalledStr = '' then
    begin
      RDSize := Socket.ReceiveLength;
      //S := Socket.ReceiveText;
      SetLength(S, RDSize);
      Socket.ReceiveBuf(Pointer(S)^, RDSize);
    end
    else
    begin
      S := SafeCalledStr;
      RDSize := length(S);
    end;
    if (Length(InputBuf) < 4) and (Length(InputBuf) > 0) then
    begin //�������������, � ��� ������
      S := InputBuf + S; //���� �������������� ��� ���������
      FlushBuffers; //����� ������
    end;
    if InputBuf = '' then
    begin //����� ������ �����;
      CopyMemory(@InputDataSize, @F[1], 4);
      InputDataSize:=InputDataSize*2;
      if InputDataSize = RDSize - 4 then
      begin //���� ���� � ������
        InputBuf := Copy(S, 5, RDSize - 4); //�� ������, �� ������������ ���.
        MySProc(string(InputBuf));
        FlushBuffers;
        Exit;
      end;
      if InputDataSize < RDSize - 4 then
      begin //����� �������.
        InputBuf := Copy(S, 5, InputDataSize);
        MySProc(string(InputBuf));
        Delete(S, 1, InputDataSize + 4);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
        Exit;
      end;
      if InputDataSize > RDSize - 4 then
      begin //��� ������ ��������
        InputBuf := Copy(S, 5, RDSize - 4); //�������� ������
        InputReceivedSize := RDSize - 4;
      end;
    end
    else
    begin //����� ������ �� ����
      //InputBuf:=
      if RDSize + InputReceivedSize = InputDataSize then
      begin //�������� ���������
        InputBuf := InputBuf + Copy(S, 0, RDSize); //�������� �������
        MySProc(string(InputBuf)); //� ������, ������
        FlushBuffers; // � ������ ������ ���
        Exit;
      end;
      if RDSize + InputReceivedSize < InputDataSize then // ��������
      begin //���������
        InputBuf := InputBuf + Copy(S, 0, RDSize); //��������
        InputReceivedSize := InputReceivedSize + RDSize;
        Exit;
      end;
      if RDSize + InputReceivedSize > InputDataSize then //�������� ��������
      begin // �� � ������ ���� ��� ������ - �������.
        InputBuf := InputBuf + Copy(S, 0, InputDataSize - InputReceivedSize);
        MySProc(string(InputBuf));
        Delete(S, 1, InputDataSize - InputReceivedSize);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
      end;
    end;
  except Result := False;
  end;
end;}

end.
