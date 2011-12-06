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
    begin //Корректировка, в том случае
      S := InputBuf + S; //если фрагментирован сам заголовок
      FlushBuffers; //блока данных
    end;
    if InputBuf = '' then
    begin //Самый первый пакет;
    	if Length(S)< 2 then // фрагментирован заголовок
      begin
      	InputBuf := S;
        Exit;
      end;
      CopyMemory(@InputDataSize, @S[1], 4);
      //MessageBox(0, PChar('First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
      if InputDataSize = RDSize div 2 - 2 then
      begin //Один блок в пакете

        InputBuf := Copy(S, 3, RDSize div 2 - 2); //ни слепки, ни фрагментации нет.
        MySProc(InputBuf);
        FlushBuffers;
        Exit;
      end;
      if InputDataSize < RDSize div 2 - 2 then
      begin //Пакет слеплен.
        //MessageBox(0, PChar(IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);
        InputBuf := Copy(S, 3, InputDataSize);
        MySProc(InputBuf);
        Delete(S, 1, InputDataSize + 2);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
        Exit;
      end;
      if InputDataSize > RDSize div 2 - 2 then
      begin //это ПЕРВЫЙ фрагмент
        InputBuf := Copy(S, 3, RDSize div 2 - 2); //большого пакета
        InputReceivedSize := RDSize div 2 - 2;
      end;
    end
    else
    begin //Буфер приема не пуст
      //InputBuf:=
      //MessageBox(0, PChar('NOT First: '+IntToStr(RDSize)+' '+IntToStr(InputReceivedSize)+' '+IntToStr(InputDataSize)), '', 0);

      if RDSize div 2 + InputReceivedSize = InputDataSize then
      begin //Получили последний
        InputBuf := InputBuf + Copy(S, 1, RDSize div 2); //фрагмент целиком
        MySProc(InputBuf); //в пакете, данных
        FlushBuffers; // в пакете больше нет
        Exit;
      end;
      if RDSize div 2 + InputReceivedSize < InputDataSize then // Получили
      begin //очередной
        InputBuf := InputBuf + Copy(S, 1, RDSize div 2); //фрагмент
        InputReceivedSize := InputReceivedSize + RDSize div 2;
        Exit;
      end;
      if RDSize div 2 + InputReceivedSize > InputDataSize then //Поледний фрагмент
      begin // но в пакете есть еще данные - слеплен.
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
    begin //Корректировка, в том случае
      S := InputBuf + S; //если фрагментирован сам заголовок
      FlushBuffers; //блока данных
    end;
    if InputBuf = '' then
    begin //Самый первый пакет;
      CopyMemory(@InputDataSize, @F[1], 4);
      InputDataSize:=InputDataSize*2;
      if InputDataSize = RDSize - 4 then
      begin //Один блок в пакете
        InputBuf := Copy(S, 5, RDSize - 4); //ни слепки, ни фрагментации нет.
        MySProc(string(InputBuf));
        FlushBuffers;
        Exit;
      end;
      if InputDataSize < RDSize - 4 then
      begin //Пакет слеплен.
        InputBuf := Copy(S, 5, InputDataSize);
        MySProc(string(InputBuf));
        Delete(S, 1, InputDataSize + 4);
        FlushBuffers;
        ReceiveLongText(Socket, MySProc, S);
        Exit;
      end;
      if InputDataSize > RDSize - 4 then
      begin //это ПЕРВЫЙ фрагмент
        InputBuf := Copy(S, 5, RDSize - 4); //большого пакета
        InputReceivedSize := RDSize - 4;
      end;
    end
    else
    begin //Буфер приема не пуст
      //InputBuf:=
      if RDSize + InputReceivedSize = InputDataSize then
      begin //Получили последний
        InputBuf := InputBuf + Copy(S, 0, RDSize); //фрагмент целиком
        MySProc(string(InputBuf)); //в пакете, данных
        FlushBuffers; // в пакете больше нет
        Exit;
      end;
      if RDSize + InputReceivedSize < InputDataSize then // Получили
      begin //очередной
        InputBuf := InputBuf + Copy(S, 0, RDSize); //фрагмент
        InputReceivedSize := InputReceivedSize + RDSize;
        Exit;
      end;
      if RDSize + InputReceivedSize > InputDataSize then //Поледний фрагмент
      begin // но в пакете есть еще данные - слеплен.
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
