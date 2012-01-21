{ **** UBPFD *********** by delphibase.endimus.com ****
>> Процедуры передачи и приема длинных блоков данных, с учетом фрагментации
и возможной слепки пакетов. На компоненты TServerSocket,TClientSocket ..SendText

Данная модуль содержит функции, которые позволяет принимать и отправлять длинные блоки данных.
В код встрена автоматическая обработка фрагментации и слепки пакетов.
Данные процелуры предназначены для передачи текстовых строк, и используют
методы SendText, ReciveText TCustomSocket и предназначены для использования
с компонентами TClientSocket, TServerSocket и других производных от TCustomSocket.
Данные решение отличается простотой использования, скоростью обработки и надежностью:
тестировалось посылкой блоков данных размером 1-16000, было обработано 15100 блоков данных.
Последующее сравнение отправленнх и полученных данных показало отсутвие каких либо
ошибок при передачи, сборки и фрагментации данных.

Перед использованием нужно приготовить пользовательскую процедуру, которая
будет вызываться каждый раз, когда получен очередной БЛОК данных. Данная процедура
должна иметь ОДИН входной параметр типа STRING:

procedure SomeUserProc(S:String);
begin
....
end;

Модуль содержит 3 функции, из которых пользьзователю нужны только 2
function SendLongText(Socket:TCustomWinSocket; S:String):boolean;
function ReceiveLongText(Socket:TCustomWinSocket;MySProc:TMySProc;SafeCalledStr :string = ''):boolean;
Фунция SendText служит для отправки пакетов. В качестве параметров ей пердается
объект TCustomWinSocket (например это ClientSocket.Socket) и собственно
отправляемя строка S (ShortString,AnsiString,WideString).
В случае успешной отправки функция возвращает true, иначе false.
Для обработки используйте GetLastError().

function ReceiveLongText(Socket:TCustomWinSocket;MySProc:TMySProc;SafeCalledStr :string = ''):boolean;
Используется для получения. Даннах фунция должна быть вызвана в событии On*Read компонента.
В качестве параметров необходимо передать TCustomWinSocket (например ServerSocket.Socket) и имя процедуры,
назначенной для обработки данных (например, ранее приготовленная SomeUserProc).
Третий параметр ЗАПОЛНЯТЬ НЕ СЛЕДУЕТ!!!
Процедура FlushBuffers является внутренней и очищает буфер приема,
и напрямую пользователем вызываться не должна.

Зависимости: ScktComp;
Автор:       Subfire, subfire@mail.ru, ICQ:55161852, Санкт-Петербург
Copyright:   Егоров Виктор aka Subfire
Дата:        2 октября 2002 г.
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
If (Length(InputBuf)>0) and (InputDataSize = 0) then begin //Корректировка, в том случае
	S:=InputBuf+S; //если фрагментирован сам заголовок
	FlushBuffers; //блока данных
end;
If InputBuf='' then
	begin //Самый первый пакет;
  	// Фрагмент заголовка
  	if Length(S) < 4 then
    begin
      InputBuf := S;
      Exit;
    end;
  	CopyMemory(@InputDataSize, @S[1], 4);
		InputDataSize := InputDataSize * 2;
 		//MessageBox(0, PChar('First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
 		if InputDataSize=RDSize-4 then begin //Один блок в пакете
 			InputBuf:=Copy(S,5,RDSize-4); //ни слепки, ни фрагментации нет.
 			CallSFunc(MySProc, InputBuf);
 			FlushBuffers;
 			Exit;
		end;
 		if InputDataSize<RDSize-4 then begin //Пакет слеплен.
			InputBuf:=Copy(S,5,InputDataSize);
			CallSFunc(MySProc, InputBuf);
			Delete(S,1,InputDataSize+4);
			FlushBuffers;
			ReceiveLongText(Socket,MySProc,S);
			Exit;
 		end;
 		if InputDataSize>RDSize-4 then begin //это ПЕРВЫЙ фрагмент
    	InputBuf:=Copy(S,5,RDSize-4); //большого пакета
    	InputReceivedSize:=RDSize-4;
 		end;
	end
	else begin //Буфер приема не пуст
		//InputBuf:=
		If RDSize+InputReceivedSize=InputDataSize then
    begin //Получили последний
    	InputBuf:=InputBuf+Copy(S,1,RDSize); //фрагмент целиком
      CallSFunc(MySProc, InputBuf); //в пакете, данных
      FlushBuffers; // в пакете больше нет
      Exit;
    end;
		If RDSize+InputReceivedSize<InputDataSize then // Получили
    begin //очередной
    	InputBuf:=InputBuf+Copy(S,1,RDSize); //фрагмент
    	InputReceivedSize:=InputReceivedSize+RDSize;
    	Exit;
    end;
		If RDSize+InputReceivedSize>InputDataSize then //Поледний фрагмент
    begin // но в пакете есть еще данные - слеплен.
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
