unit libfunc;

interface

uses
  Windows, IniFiles, comm_info, comm_data, libqueue, SysUtils, Classes, JPEG;

  function GetDllPath: String; stdcall;
  function ExtractFileNameEx(FileName: string; ShowExtension: Boolean): string;

  function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
  function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;

  function WordToStr(const Num: Word): String;
  function DWordToStr(const Num: DWord): String;
  function DoubleToStr(const Num: Double): String;
  function TextToStr(const Str: String): String;
  function ImgToStr(const image: TJpegImage): String;
  function StrToWord(const Str: String; var P: Cardinal): Word;
  function StrToDWord(const Str: String; var P: Cardinal): DWord;
  function StrToDouble(const Str: String; var P: Cardinal): Double;
  function StrToText(const Str: String; var P: Cardinal): String;
  function StrToImg(const Str: String; var P: Cardinal): TJpegImage;

  procedure MsgToChannel(Channel, Msg:string);
  procedure StatusToChannel(Channel, Msg:string);
  procedure PrivateMsg(Name, Msg:string);
  procedure PrivateStatus(Name, Msg:string);
  procedure PersonalMsg(Name, Msg:string; Importance: Dword=0);
  procedure ChangeTopic(Channel, Msg:string);
  procedure ChangeGreeting(Channel, Msg:string);
  procedure ChangeState(Msg:string);
  procedure CreateChannel(Channel:string; Visible,JoinRegime: Integer);
  procedure QuitChannel(Channel:string);
  procedure BanUser(TypeBan,TypeIdent,TypeAnonym: Word; BanTime:Double;
                    Ident,Channel,Reason: String);
  procedure CloseChannel(Channel: String);

  function GetTimeFromStr(Str : String): Word;
  function CheckStr(Str : String): String;
  function unCheckStr(Str : String): String;
  function GetRandomTextFromIni(FileName:String; TextId : String): String;

  procedure Pause(time: Cardinal);

implementation

function GetDllPath: String; stdcall;
var
  TheFileName: array[0..MAX_PATH] of char;
begin
  FillChar(TheFileName, sizeof(TheFileName), #0);
  GetModuleFileName(hInstance, TheFileName, sizeof(TheFileName));
  Result:=TheFileName;
end;


{ **** UBPFD *********** by delphibase.endimus.com ****
>> ��������� ����� ����� �� ���� ��� ��� � ��� �����������.

�����������: ���
�����:       VID, snap@iwt.ru, ICQ:132234868, ���������
Copyright:   VID
����:        18 ������ 2002 �.
***************************************************** }

function ExtractFileNameEx(FileName: string; ShowExtension: Boolean): string;
//������� ���������� ��� �����, ��� ��� � ��� �����������.

//������� ���������
//FileName - ��� �����, ������� ���� ����������
//ShowExtension - ���� TRUE, �� ������� ��������� �������� ��� �����
// (��� ������� ���� ������� � ����), � ����������� ����� �����, �����, ���������
// �������� ��� �����, ��� ���������� ����� �����.
var
  I: Integer;
  S, S1: string;
begin
  //���������� ����� ������� ����� �����
  I := Length(FileName);
  //���� ����� FileName <> 0, ��
  if I <> 0 then
  begin
    //� ����� ����� ��������� FileName ���� ������ "\"
    while (FileName[i] <> '\') and (i > 0) do
      i := i - 1;
    // �������� � ���������� S �������� FileName ������� ����� ����������
    // "\", ����� ������� ���������� S �������� ��� ����� � �����������, �� ���
    // ������� ���� ������� � ����
    S := Copy(FileName, i + 1, Length(FileName) - i);
    i := Length(S);
    //���� ���������� S = '' �� ������ ���������� ''
    if i = 0 then
    begin
      Result := '';
      Exit;
    end;
    //�����, �������� ��� ����� ��� ����������
    while (S[i] <> '.') and (i > 0) do
      i := i - 1;
    //... � �������� ��� ��� ����� � ���������� s1
    S1 := Copy(S, 1, i - 1);
    //���� s1='' �� , ���������� s1=s
    if s1 = '' then
      s1 := s;
    //���� ���� �������� �������� ������� ���������� ��� ����� � ���
    // �����������, �� Result = s,
    //���� ��� ����������, �� Result = s1
    if ShowExtension = TRUE then
      Result := s
    else
      Result := s1;
  end
    //����� ������� ���������� ''
  else
    Result := '';
end;


{ ���������� ������

������������� ��� �������� ���������� �����, ���� 96 ���, ����������
������������.

�����:       Anatoly Podgoretsky, anatoly@podgoretsky.com, Johvi
Copyright:   (c) Anatoly Podgoretsky, 1996
����:        26 ������ 2002 �.
***************************************************** }

{$R-}
{$Q-}
function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Ord(InString[I]) xor (StartKey shr 8));
    StartKey := (Ord(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;

{ ����������� ������

������������� ��� ����������� ������

�����:       Anatoly Podgoretsky, anatoly@podgoretsky.com, Johvi
Copyright:   (c) Anatoly Podgoretsky, 1996
����:        26 ������ 2002 �.
***************************************************** }
function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Ord(InString[I]) xor (StartKey shr 8));
    StartKey := (Ord(InString[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{$R+}
{$Q+}

function WordToStr(const Num: Word): String;
begin
  Result:=' ';
  CopyMemory(@Result[1], @Num, 2);
end;

function DWordToStr(const Num: DWord): String;
begin
  Result:='  ';
  CopyMemory(@Result[1], @Num, 4);
end;

function DoubleToStr(const Num: Double): String;
begin
  Result:='    ';
  CopyMemory(@Result[1], @Num, 8);
end;

function TextToStr(const Str: String): String;
begin
  Result:=DWordToStr(Length(Str))+Str;
end;

function ImgToStr(const image: TJpegImage): String;
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
  image.SaveToStream(msg);
  msg.Seek(0, soBeginning);
  SetLength(Result, msg.Size div 2 + msg.Size mod 2);
	msg.ReadBuffer(Result[1], msg.Size);
  len := msg.Size;
  Result := DWordToStr(len) + Result;
  msg.Free;
end;


function StrToWord(const Str: String; var P: Cardinal): Word;
begin
  CopyMemory(@Result, @Str[P], 2);
  P:=P+1;
end;

function StrToDWord(const Str: String; var P: Cardinal): DWord;
begin
  CopyMemory(@Result, @Str[P], 4);
  P:=P+2;
end;

function StrToDouble(const Str: String; var P: Cardinal): Double;
begin
  CopyMemory(@Result, @Str[P], 8);
  P:=P+4;
end;

function StrToText(const Str: String; var P: Cardinal): String;
var Len: DWord;
begin
  Len:=StrToDword(Str,P);
  Result:=Copy(Str,P,Len);
  P:=P+Len;
end;

function StrToImg(const Str: String; var P: Cardinal): TJpegImage;
var
	Len: DWord;
  msg: TMemoryStream;
begin
	Len:=StrToDword(Str,P);
  if Len > 0 then
  begin
  	msg := TMemoryStream.Create;
  	try
    	msg.WriteBuffer(Str[P], Len);
    	msg.Seek(0, soBeginning);
			Result := TJPEGImage.Create;
    	Result.LoadFromStream(msg);
    finally
    	msg.Free;
    end;
    P:=P + Len div 2 + Len mod 2;
  end
  else
  	Result := nil;
end;


  procedure MsgToChannel(Channel, Msg:string);
  begin
    PCorePlugin^.AddMessageToChannel(BOT_NAME, Channel,0,Msg)
  end;

  procedure StatusToChannel(Channel, Msg:string);
  begin
    PCorePlugin^.AddMessageToChannel(BOT_NAME, Channel,1,Msg);
  end;

  procedure PrivateMsg(Name, Msg:string);
  begin
    PCorePlugin^.AddPrivateMessage(BOT_NAME, 0,Name,Msg);
  end;

  procedure PrivateStatus(Name, Msg:string);
  begin
    PCorePlugin^.AddPrivateMessage(BOT_NAME, 1,Name,Msg);
  end;

  procedure PersonalMsg(Name, Msg:string; Importance: Dword=0);
  begin
    PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name,Msg);
  end;

  procedure ChangeTopic(Channel, Msg:string);
  begin
    PCorePlugin^.AddTheme(BOT_NAME, Channel,Msg);
  end;

  procedure ChangeGreeting(Channel, Msg:string);
  begin
    PCorePlugin^.AddGreeting(BOT_NAME, Channel,Msg);
  end;

  procedure ChangeState(Msg:string);
  begin
    PCorePlugin^.AddState(BOT_NAME, Msg);
  end;

  procedure CreateChannel(Channel:string; Visible,JoinRegime: Integer);
  begin
    PCorePlugin^.AddChannel(BOT_NAME, Channel,Visible,JoinRegime);
  end;

  procedure QuitChannel(Channel:string);
  begin
    PCorePlugin^.LeaveChannel(BOT_NAME, Channel);
  end;

  procedure BanUser(TypeBan,TypeIdent,TypeAnonym: Word; BanTime:Double;
                    Ident,Channel,Reason: String);
  begin
    PCorePlugin^.AddRestriction(BOT_NAME, TypeBan,TypeIdent,
      TypeAnonym,BanTime,Ident,Channel,Reason);
  end;

  procedure CloseChannel(Channel: String);
  begin
    PCorePlugin^.RemoveChannel(BOT_NAME, Channel);
  end;

  function CheckStr(Str : String): String;
  begin
    Result := StringReplace(Str, '[', '&'+IntToStr(Ord('['))+';', [rfReplaceAll]);
    Result := StringReplace(Result, ']', '&'+IntToStr(Ord(']'))+';', [rfReplaceAll]);
    Result := StringReplace(Result, '=', '&'+IntToStr(Ord('='))+';', [rfReplaceAll]);
  end;

  function unCheckStr(Str : String): String;
  begin
    Result := StringReplace(Str, '&'+IntToStr(Ord('['))+';', '[', [rfReplaceAll]);
    Result := StringReplace(Result, '&'+IntToStr(Ord(']'))+';', ']', [rfReplaceAll]);
    Result := StringReplace(Result, '&'+IntToStr(Ord('='))+';', '=', [rfReplaceAll]);
  end;

  function GetRandomTextFromIni(FileName:String; TextId : String): String;
  var
    Ini : TIniFile;
    I, Count: Word;
  begin
    Randomize;
    Ini := TIniFile.Create(FileName);
    Count := Ini.ReadInteger(TextId, 'Count', 1);
    I:=Random(Count)+1;
    Result := Ini.ReadString(TextId, 'Text'+IntToStr(I), '');
    Ini.Free;
  end;

function GetTimeFromStr(Str : String): Word;
begin
  Result:=StrToIntDef(Str, 0);
  if StrToIntDef(Str, 0)<0 then
    Result:=0
  else
    if Result > 360 then
      Result:=360;
end;

procedure Pause(time: Cardinal);
var
  Buf: TBytes;
begin
  //Sleep(time);
  SetLength(Buf, 4);
  CopyMemory(@Buf[0], @time, 4);
  MsgQueue.InsertMsg(QUEUE_MSGTYPE_PAUSE, Buf, 4);
end;

end.

