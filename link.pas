{$I plugin.inc}

unit link;

interface

uses
  IniFiles,
  Windows, WinInet, SysUtils, Classes, SyncObjs,
  Math, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  comm_info, comm_data, libfunc, libqueue, ScktComp, LongDataTransfer, JPEG, LbRSA;

type
  TLinkSocket = class
    public
      {$IFDEF Server}
      S: TServerSocket;
      {$ELSE}
      S: TClientSocket;
      {$ENDIF}
      ConnS: TCustomWinSocket;

      LbRSA1: TLbRSA;

      procedure SendText(Str: string; EncType:Char='E');

      constructor Create();
      destructor Destroy(); override;
      procedure SocketRead(Sender: TObject; Socket: TCustomWinSocket);
      {$IFDEF Server}
      procedure ServerSocketAccept(Sender: TObject; Socket: TCustomWinSocket);
      procedure ServerSocketClientDisconnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ServerSocketClientError(Sender: TObject;
        Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
        var ErrorCode: Integer);
      {$ELSE}
      procedure ClientSocketConnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ClientSocketDisconnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ClientSocketError(Sender: TObject;
        Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
        var ErrorCode: Integer);
      {$ENDIF}
      procedure OnKeysReceive();
  end;

  {$IFNDEF Server}
  TReconnect = class
      Timer: TTimer;
      Sock: TLinkSocket;

      constructor Create(Sock1: TLinkSocket);
      destructor Destroy(); override;
      procedure Reconnect(Sender: TObject);
  end;
  {$ENDIF}

  TSendData = class
      LastUpdate:TDateTime;
      Timer: TTimer;
      TimerBans: TTimer;
      Sock: TLinkSocket;

      ListNames: TStringList;

      constructor Create(Sock1: TLinkSocket);
      destructor Destroy(); override;

      procedure Send(Sender: TObject);
      procedure SendBans(Sender: TObject);

  end;

  TChannelInfo = record
    Name: String;
    Permanent: Boolean;
    Owner: String;
  end;

  procedure ProcessData(const S: String);
  function  IsAdmin(const S: String): Boolean;

  procedure onAuthFail(Name: String; Reason: Integer);
  procedure onMsg(User: TUser; Channel, Text: String; Regime: Integer);
  procedure onImg(User: TUser; Channel: String; Image: TJpegImage);
  procedure onPrivate(Name: String; User: TUser; Text: String; Regime: Integer);
  procedure onPrivateImg(Name: String; User: TUser; Image: TJpegImage);
  procedure onPersonalMsg(Name: String; User: TUser; Text: String);
  procedure onUserJoinChannel(User: TUser; Channel: String);
  procedure onUserLeftChannel(User: TUser; Channel: String);
  procedure onUserJoinChat(User: TUser);
  procedure onUserLeftChat(User: TUser);
  procedure onUserStatusChanged(User: TUser; Text: String);
  procedure onRestrictionAdd(Restriction: TRestriction);
  procedure onRestrictionRemove(Restriction: TRestriction; UnBanModerName: String);

  procedure LoadSettings(Ini:TIniFile);
  function Init():Integer;
  procedure Destroy();

const
	KICK_TIME = 0.00003; // ~2.5s

var
  Sock: TLinkSocket;
  SU: TSendData;
  {$IFNDEF Server}
  RC: TReconnect;
  {$ENDIF}
  VUNames: TStringList;

  ChannelList: array [1..16] of TChannelInfo;
  WhiteStartList: TStringList;
  IgnorePrefixList: TStringList;

  Connected: Boolean;

implementation

constructor TLinkSocket.Create();
begin
  try
    {$IFDEF Server}
    S:=TServerSocket.Create(nil);
    S.OnAccept:=ServerSocketAccept;
    S.OnClientDisconnect:=ServerSocketClientDisconnect;
    S.OnClientError:=ServerSocketClientError;
    S.OnClientRead:=SocketRead;
    {$ELSE}
    S:=TClientSocket.Create(nil);
    S.OnConnect:=ClientSocketConnect;
    S.OnDisconnect:=ClientSocketDisconnect;
    S.OnError:=ClientSocketError;
    S.OnRead:=SocketRead;
    S.Host:=CONNECT_IP;
    {$ENDIF}
    LbRSA1 := TLbRSA.Create(nil);
    S.Port:=CONNECT_PORT;
    S.Open();
    ConnS:=nil;
  except
    PCorePlugin^.WriteLog(file_log, 'Error in creating socket!');
  end;
end;

destructor TLinkSocket.Destroy();
begin
  try
    if S.Active then
      S.Close();
  finally
    FreeAndNil(S);
    FreeAndNil(LbRSA1);
    ConnS:=nil;
  end;
end;

procedure TLinkSocket.SendText(Str: string; EncType:Char='E');
begin
	if not Connected then Exit;
  
  if EncType='E' then
    Str:=EncType+Encrypt(Str, StartKey, MultKey, AddKey)
  else
    Str:=EncType+Str;
  SendLongText(ConnS, Str);

end;

procedure TLinkSocket.SocketRead(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  ReceiveLongText(Socket,ProcessData);
end;

{$IFDEF Server}
procedure TLinkSocket.ServerSocketAccept(Sender: TObject; Socket: TCustomWinSocket);
var
	Users: TUsers;
	Count, I: DWord;
	DataToSend: String;
begin
	if Socket.RemoteAddress<>CONNECT_IP then
	begin
		PCorePlugin^.WriteLog(file_log, 'Denied '+Socket.RemoteAddress);
		Socket.Close;
	end
	else
	begin
		ConnS:=Socket;
		VUNames.Clear;
		SU.TimerBans.Enabled:=False;
		SU.Timer.Enabled:=False;
		SU.ListNames.Clear;
		//Count:=PCorePlugin^.AskUsersInChat(Users);
		//for I := 1 to Count do
		//	if (Users[I].Name<>BOT_NAME) and (VUNames.IndexOf(Users[I].Name)=-1) and  (IniUsers.ReadInteger('Connect', CheckStr(Users[I].Name), 0)=1)  then
		//		SU.ListNames.Add(Users[I].Name);
		//SU.Timer.Enabled:=True;
		//AddKey:=Random(2000000000);
		//Self.SendText(IntToStr(AddKey),'K');
		//DataToSend:=WordToStr(LNK_CODE_SERVICE_SERVERNAME)+TextToStr(SERVER_LOCAL);
		//Self.SendText(DataToSend);
		//SU.LastUpdate:=0;
		//SU.TimerBans.Enabled:=True;
		//Connected:=True;
    LbRSA1.GenerateKeyPair;
    DataToSend:=DwordToStr(PROTOCOL_VER)+TextToStr(LbRSA1.PublicKey.ModulusAsString)+TextToStr(LbRSA1.PublicKey.ExponentAsString);
    Self.SendText(DataToSend, 'K');
  end;
end;

{$ELSE}

procedure TLinkSocket.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  //SU.LastUpdate:=0;
  //SU.TimerBans.Enabled:=True;
  ConnS:=S.Socket;
  SU.TimerBans.Enabled:=False;
  SU.Timer.Enabled:=False;
end;

{$ENDIF}

procedure TLinkSocket.OnKeysReceive();
var
  Users: TUsers;
  Count, I: DWord;
  DataToSend: String;
  Text: String;
begin
  try
    VUNames.Clear;
  	SU.TimerBans.Enabled:=False;
    SU.Timer.Enabled:=False;
    SU.ListNames.Clear;
    DataToSend:=WordToStr(LNK_CODE_SERVICE_SERVERNAME)+TextToStr(SERVER_LOCAL);
    Self.SendText(DataToSend);

    // Отправка списка временных каналов
    Count := 0;
    DataToSend := '';
    for I := 1 to 16 do
    begin
      if not ChannelList[I].Permanent and (ChannelList[I].Owner<>'') and (ChannelList[I].Name<>'') then
      begin
      	Inc(Count);
        DataToSend := DataToSend + WordToStr(I) + TextToStr(ChannelList[I].Name);
      end;
    end;
    DataToSend:=WordToStr(LNK_CODE_SERVICE_TEMPCHANLIST)+WordToStr(Count)+DataToSend;
    Self.SendText(DataToSend);

    Count:=PCorePlugin^.AskUsersInChat(Users);
    for I := 1 to Count do
      if (Users[I].Name<>BOT_NAME) and (VUNames.IndexOf(Users[I].Name)=-1) and (IniUsers.ReadInteger('Connect', CheckStr(Users[I].Name), 0)=1) then
        SU.ListNames.Add(Users[I].Name);
    SU.LastUpdate:=0;
    SU.Timer.Enabled:=True;
    SU.TimerBans.Enabled:=True;
    Connected:=True;
  except
    on e:Exception do
    begin
      Text:='--------------OnKeysReceiveException--------'+Chr(13)+Chr(10);
      PCorePlugin^.onError(PCorePlugin^, e, Text);
    end;
  end;
end;

{$IFDEF Server}
procedure TLinkSocket.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
var I: LongInt;
begin
  if (ConnS = Socket) then
  begin
  	try
  		ConnS:=nil;
  		Connected:=False;
  		SU.Timer.Enabled:=False;
  		SU.ListNames.Clear;
  		for I := 0 to VUNames.Count - 1 do
    		PCorePlugin^.LeaveVirtualUser(VUNames[I]);
  		VUNames.Clear;
  		SU.TimerBans.Enabled:=False;
  		PCorePlugin^.AddState(BOT_NAME, '');
    except
    	on e:Exception do
      	PCorePlugin^.onError(PCorePlugin^, e, '--------------OnDisconnectException--------'+Chr(13)+Chr(10));
  	end;
  end;
end;

{$ELSE}
procedure TLinkSocket.ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
var I: LongInt;
begin
	try
  	ConnS:=nil;
  	Connected:=False;
  	SU.Timer.Enabled:=False;
  	SU.ListNames.Clear;
  	for I := 0 to VUNames.Count - 1 do
    	PCorePlugin^.LeaveVirtualUser(VUNames[I]);
  	VUNames.Clear;
  	SU.TimerBans.Enabled:=False;
  	PCorePlugin^.AddState(BOT_NAME, '');
  except
    on e:Exception do
      PCorePlugin^.onError(PCorePlugin^, e, '--------------OnDisconnectException--------'+Chr(13)+Chr(10));
  end;
end;
{$ENDIF}

{$IFDEF Server}
procedure TLinkSocket.ServerSocketClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  PCorePlugin^.WriteLog(file_log,'Error '+IntToStr(ErrorCode));
  ErrorCode:=0;
end;

{$ELSE}
procedure TLinkSocket.ClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
        var ErrorCode: Integer);
begin
  PCorePlugin^.WriteLog(file_log,'Error '+IntToStr(ErrorCode));
  ErrorCode:=0;
end;
{$ENDIF}

function  IsAdmin(const S: String): Boolean;
var
  Ini: TIniFile;
begin
  Ini:=TIniFile.Create(file_config);
  Result:=(Ini.ReadInteger('Admins', CheckStr(S), 0)=1);
  Ini.Free;
end;

procedure KickUser(UserName: String);
var
    Count, Index: DWord;
    I: LongInt;
    Users: TUsers;
    Flag: Boolean;
begin
	try
  	for I := 1 to 16 do
    	if ChannelList[I].Name<>'' then
    	begin
      	Count:=PCorePlugin^.AskUsersInChannel(BOT_NAME, ChannelList[I].Name, Users);
      	Flag:=False;
      	Index:=1;
      	while not Flag and (Index<=Count) do
      	begin
       		 Flag:=(Users[Index].Name=UserName);
        	Inc(Index);
    		end;
      	if Flag then
        	PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, UserName, ChannelList[I].Name, 'Вам был запрещен доступ к каналу в связи с вашим отключением от сервера '+SERVER_REMOTE);
      	end;
  	if IniUsers.ReadInteger('Message', CheckStr(UserName), 2)<>2 then
    	IniUsers.DeleteKey('Message', CheckStr(UserName));
  except
    on e:Exception do
      PCorePlugin^.onError(PCorePlugin^, e, '--------------KickUserException--------');
  end;
end;

procedure ProcessData(const S: String);
var Code, Icon, BanUT: Word;
    Name, Channel, Text, Pass: String;
    Image: TJpegImage;
    Count, P, Ident, BanT: DWord;
    Remain: Double;
    I, K, CountRest: LongInt;
    Index: LongInt;
    Str: String;
    DataToSend: String;
    User: TUser;
    Channels: TChannels;
    Restrictions:TRestrictions;
    Flag: Boolean;
    ChannelsUpdated: array[1..16] of Boolean;
begin
  Code:=0;
  try
  Str:=Copy(S, 1, 1);
  if Str='E' then
    Str:=Decrypt(Copy(S,2,Length(S)-1), StartKey, MultKey, AddKey)
  {$IFDEF Server}
  else if Str='K' then
  begin
    Str:=Copy(S,2,Length(S)-1);
    P:=1;
    Ident := StrToDword(Str, P);
    if Ident <> PROTOCOL_VER then
    begin
      PCorePlugin^.WriteLog(file_log, 'Соединение не установлено. Версия протокола: '+IntToStr(PROTOCOL_VER)+'. Версия протокола клиента:'+IntToStr(Ident));
      Sock.ConnS.Close;
    	Exit;
    end;
    Count := 1;
    //AddKey:=StrToIntDef(Copy(S,2,Length(S)-1), 0);
    AddKey := StrToDword(Sock.LbRSA1.DecryptStringW(StrToText(Str, P)), Count);
    Count := 1;
    MultKey := StrToDword(Sock.LbRSA1.DecryptStringW(StrToText(Str, P)), Count);
    Sock.OnKeysReceive();
    Exit;
  end
  {$ELSE}
  else if Str='K' then
  begin
    //AddKey:=StrToIntDef(Copy(S,2,Length(S)-1), 0);
    Str:=Copy(S,2,Length(S)-1);
    P:=1;
    Ident := StrToDword(Str, P);
    if Ident <> PROTOCOL_VER then
    begin
      PCorePlugin^.WriteLog(file_log, 'Соединение не установлено. Версия протокола: '+IntToStr(PROTOCOL_VER)+'. Версия протокола сервера:'+IntToStr(Ident));
      Sock.ConnS.Close;
      RC.Timer.Interval := 360000;
    	Exit;
    end;
    Sock.LbRSA1.PublicKey.Clear;
    Sock.LbRSA1.PublicKey.ModulusAsString := StrToText(Str, P);
    Sock.LbRSA1.PublicKey.ExponentAsString := StrToText(Str, P);
    AddKey:=Random(2000000000);
    DataToSend := DwordToStr(PROTOCOL_VER) + TextToStr(Sock.LbRSA1.EncryptStringW(DwordToStr(AddKey))) + TextToStr(Sock.LbRSA1.EncryptStringW(DwordToStr(MultKey)));
    Sock.SendText(DataToSend,'K');
    Sock.OnKeysReceive();
    Exit;
  end
  {$ENDIF}
  else
    Str:=Copy(S,2,Length(S)-1);
    // <--
    P:=1;
    Code:=StrToWord(Str,P);
    case CODE of
      LNK_CODE_JOIN:
        begin
          Name:=StrToText(Str,P);
          Text:=StrToText(Str,P); //IP
          Icon:=StrToWord(Str, P);
          Channel:=StrToText(Str, P); //CompID
          Pass:=IniUsers.ReadString('Users',CheckStr(Name),'');
          if Pass='' then
          begin
            for I := 0 to Random(5)+10 do
              Pass:=Pass+RandomStr[Random(Length(RandomStr))+1];
            IniUsers.WriteString('Users',CheckStr(Name), Pass);
          end;
          PCorePlugin^.JoinVirtualUser(name_prefix+Name+name_postfix,Text,0,Pass, Icon, Channel);
        end;
      LNK_CODE_LEFT:
        begin
          Name:=StrToText(Str,P);
          Index:=VUNames.IndexOf(name_prefix+Name+name_postfix);
          if Index<>-1 then
            VUNames.Delete(VUNames.IndexOf(name_prefix+Name+name_postfix));
          Count:=PCorePlugin^.AskUserChannels(name_prefix+Name+name_postfix, Channels);
          for I := 1 to Count do
            PCorePlugin^.LeaveChannel(name_prefix+Name+name_postfix, Channels[I].Name);
          PCorePlugin^.LeaveVirtualUser(name_prefix+Name+name_postfix);
        end;
      LNK_CODE_JOINCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          //if VUNames.IndexOf(name_prefix+Name)=-1 then // Забанен?
          //begin
          //  DataToSend:=WordToStr(LNK_CODE_SERVICE_JOINCHANFAIL)+TextToStr(Name)+WordToStr(I); // сервисное сообщение на другой сервер
          //  Sock.SendText(DataToSend);
          //end
          //else
            PCorePlugin^.AddChannel(name_prefix+Name+name_postfix,Channel,0,0);
        end;
      LNK_CODE_LEFTCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          PCorePlugin^.LeaveChannel(name_prefix+Name+name_postfix,Channel);
        end;
      LNK_CODE_CMSG:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          Text:=StrToText(Str,P);
          Icon:=StrToWord(Str, P);
          PCorePlugin^.AddChannel(name_prefix+Name+name_postfix,Channel,0,0);
          PCorePlugin^.AddMessageToChannel(name_prefix+Name+name_postfix, Channel, Icon, Text);
        end;
      LNK_CODE_CIMG:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          Image:=StrToImg(Str,P);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddChannel(name_prefix+Name+name_postfix,Channel,0,0);
          		PCorePlugin^.AddImageToChannel(name_prefix+Name+name_postfix, Channel, Image);
            finally
            	Image.Free;
            end;
          end;
        end;
      LNK_CODE_PRIV:
        begin
          Name:=StrToText(Str,P);
          Channel:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          Icon:=StrToWord(Str, P);
          PCorePlugin^.AddPrivateMessage(name_prefix+Name+name_postfix, Icon, Channel, Text);
        end;
      LNK_CODE_PRIVIMG:
        begin
          Name:=StrToText(Str,P);
          Channel:=StrToText(Str,P);
          Image:=StrToImg(Str,P);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddPrivateImage(name_prefix+Name+name_postfix, Channel, Image);
            finally
            	Image.Free;
            end;
          end;
        end;
      LNK_CODE_PMSG:
        begin
          Name:=StrToText(Str,P);
          Channel:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          PCorePlugin^.AddPersonalMessage(name_prefix+Name+name_postfix, 0, Channel, Text);
        end;
      LNK_CODE_STATUSCHNG:
        begin
          Name:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          PCorePlugin^.AddState(name_prefix+Name+name_postfix, Text);
        end;

      LNK_CODE_BAN:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы были забанены на сервере '+SERVER_REMOTE);
        end;

      LNK_CODE_BANLIST:
        begin
          Count:=StrToDWord(Str,P);
          BanUT:=StrToWord(Str,P);
          if BanUT=0 then
            CountRest:=PCorePlugin^.AskRestrictions(Restrictions)
          else
            CountRest:=0;
          for I := 1 to Count do
          begin
            Name:=StrToText(Str, P);
            Remain:=StrToDouble(Str, P);
            Ident:=StrToDWord(Str, P);
            BanT:=StrToDWord(Str, P);
            Icon:=StrToWord(Str, P);  // номер канала
            if not Icon in [1..16] then
              Channel:=' '
            else
              Channel:=ChannelList[Icon].Name;
            if Channel='' then
              Channel:=' ';
            Text:=StrToText(Str, P);
            Text:=StrToText(Str, P)+' ('+Text+')';
            Flag:=False;
            K := 1;
            while not Flag and (K<=CountRest) do
            begin
              Flag:=(Restrictions[K].ident=Ident) and (Restrictions[K].Name=Name) and (Restrictions[K].banType=BanT) and ((Restrictions[K].channel=Channel) or (Restrictions[K].channel=''));
              Inc(K);
            end;
            if (not Flag) and (BanT>1) then
              PCorePlugin^.AddRestriction(BOT_NAME, BanT,Ident,0,Remain,Name,Channel,Text);
          end;
          Restrictions := nil;
        end;

      LNK_CODE_UNBAN:
        begin
          Name:=StrToText(Str, P);
          Ident:=StrToDWord(Str, P);
          BanT:=StrToDWord(Str, P);
          Icon:=StrToWord(Str, P);  // номер канала
          Text:=StrToText(Str, P);
          if not Icon in [1..16] then
          	Channel:=''
          else
          	Channel:=ChannelList[Icon].Name;
          CountRest:=PCorePlugin^.AskRestrictions(Restrictions);
          I := CountRest;
          Flag := false;
          while (i >= 1) and not Flag do
          begin
          	Flag:=(Restrictions[I].moder = BOT_NAME) and (Restrictions[I].ident=Ident) and (Restrictions[I].Name=Name) and (Restrictions[I].banType=BanT) and (Restrictions[I].channel=Channel);
            if Flag then
              PCorePlugin^.RemoveRestriction(BOT_NAME, Restrictions[I].restID, StrToText(Str, P) + '(' + Text + ')');
            Dec(i);
          end;
          Restrictions := nil;
        end;

      //-------------------------------- Сервисные сообщения -------------------
      LNK_CODE_SERVICE_SERVERNAME:
        begin
          SERVER_REMOTE:=StrToText(Str,P);
          PCorePlugin^.AddState(BOT_NAME, 'Подключен к серверу '+SERVER_REMOTE);
        end;

      LNK_CODE_SERVICE_UCSCHECK:
        begin
          Name:=StrToText(Str,P);
          if VUNames.IndexOf(name_prefix+Name+name_postfix)=-1 then
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(1)+TextToStr('') // сервисное сообщение на другой сервер
          else
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(0)+TextToStr(name_prefix+Name+name_postfix); // сервисное сообщение на другой сервер
          Sock.SendText(DataToSend);
        end;

      LNK_CODE_SERVICE_UCSREPLY:
        begin
          Name:=StrToText(Str,P);
          Icon:=StrToWord(Str,P);  // Статус
          Text:=StrToText(Str,P);
          case Icon of
            0: PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы подключены к серверу '+SERVER_REMOTE+' под именем [i]'+Text+'[/i].');
            1: PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не подключены к серверу '+SERVER_REMOTE+'. Возможные причины: ограничение на доступ к серверу, Ваш ник не соответствует требованиям или нестабильная связь между серверами.');
          end;
        end;

      LNK_CODE_SERVICE_BADNICK:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: вероятно, слишком длинный ник.');
        end;

      LNK_CODE_SERVICE_BANNED:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: бан на доступ к серверу.');
        end;

      LNK_CODE_SERVICE_NICKDENIED:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: в Вашем нике содержатся запрещенные символы или слова.');
        end;

      LNK_CODE_SERVICE_NICKLEXISTS:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: схожее имя зарегистрировано на сервере.');
        end;

      LNK_CODE_SERVICE_MAXNICKLIMIT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: с Вашего IP зарегистрировано максимально возможное число учетных записей.');
        end;

      LNK_CODE_SERVICE_MAXUSERLIMIT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина: достигнуто максимальное количество пользователей онлайн.');
        end;

      LNK_CODE_SERVICE_CANNOTCONNECT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы не были подключены к серверу '+SERVER_REMOTE+'. Причина сообщена администратору. Пожалуйста, подождите.');
        end;

      LNK_CODE_SERVICE_CONNECTION_OK:
        begin
          Name:=StrToText(Str,P);
          if (IniUsers.ReadInteger('Message', CheckStr(Name), 2)=0) then
          begin
            Text:= 'Вы подключены к серверу '+SERVER_REMOTE+'. Вы можете отключиться в любой момент, написав мне "disconnect"'+Chr(13)+Chr(10)+'Список общих каналов:';
            for I := 1 to 16 do
              if (ChannelList[I].Name<>'') and ChannelList[I].Permanent then
                Text:=Text+Chr(13)+Chr(10)+'[url=/channel: '+ChannelList[I].Name+']'+ChannelList[I].Name+'[/url]';
            Text:=Text+Chr(13)+Chr(10)+Chr(13)+Chr(10)+'Если Вы больше не хотите получать это сообщение, напишите мне "silent"';
            PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name,Text);
            IniUsers.WriteInteger('Message', CheckStr(Name), 1);
          end;
        end;

      LNK_CODE_SERVICE_RESENDMEUSER:
        begin
          Name:=StrToText(Str,P);
          User:=PCorePlugin^.AskUserInfo(Name, Text);
          if User.IP<>'UNCONNECTED' then
          begin
            DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
            Sock.SendText(DataToSend);
            if Text<>'' then
            begin
              DataToSend:=WordToStr(LNK_CODE_STATUSCHNG)+TextToStr(User.Name)+TextToStr(Text);
              Sock.SendText(DataToSend);
            end;
          end;
        end;

      LNK_CODE_SERVICE_JOINCHANFAIL:
        begin
          Name:=StrToText(Str,P);
          Icon:=StrToWord(Str,P); // номер канала
          if not Icon in [1..16] then
            Exit;
          if ChannelList[Icon].Name='' then
            Exit;
          PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, Name, ChannelList[Icon].Name, 'Вам был запрещен доступ к каналу в связи с вашим отключением от сервера '+SERVER_REMOTE);
        end;

       LNK_CODE_SERVICE_TEMPCHANLIST:
        begin
        	for I := 1 to 16 do
          	ChannelsUpdated[I] := False;
        	Count := StrToWord(Str,P);  // Количество каналов
          for K := 1 to Count do
          begin
          	Icon := StrToWord(Str,P);  // Номер канала
          	Name := StrToText(Str,P);	 // Имя канала
          	if not Icon in [1..16] then
            	Exit;
          	if ChannelList[Icon].Permanent or (ChannelList[Icon].Owner<>'') then
            	Exit;
            // уже создан
            if ChannelList[Icon].Name = Name then
            	Exit;
            // закрытие старого канала
            if (ChannelList[Icon].Name <> '') then
            	CloseChannel(ChannelList[Icon].Name);
          	Flag := false;
          	for I := 0 to WhiteStartList.Count - 1 do
          	begin
          		Flag := (Length(Name) >= Length(WhiteStartList[I])) and (Copy(Name, 1, Length(WhiteStartList[I])) = WhiteStartList[I]);
            	if Flag then
            		break;
          	end;
          	if Flag then
          	begin
          		ChannelList[Icon].Name := Name;
            	CreateChannel(Name, 0, 0);
            	ChannelsUpdated[Icon] := True;
          	end;
          end;
        	for I := 1 to 16 do
          	if not ChannelsUpdated[I] and not ChannelList[I].Permanent and
            	(ChannelList[I].Owner = '') and (ChannelList[I].Name <> '') then
              begin
              	QuitChannel(ChannelList[I].Name);
            		CloseChannel(ChannelList[I].Name);
              end;
        end;

       LNK_CODE_SERVICE_CREATECHANNEL:
        begin
          Icon:=StrToWord(Str,P);  // Номер канала
          Name:=StrToText(Str,P);	 // Имя канала
          if not Icon in [1..16] then
            Exit;
          if ChannelList[Icon].Permanent or (ChannelList[Icon].Owner<>'') then
            Exit;
          Flag := false;
          for I := 0 to WhiteStartList.Count - 1 do
          begin
          	Flag := (Length(Name) >= Length(WhiteStartList[I])) and (Copy(Name, 1, Length(WhiteStartList[I])) = WhiteStartList[I]);
            if Flag then
            	break;
          end;
          if Flag then
          begin
          	ChannelList[Icon].Name := Name;
            CreateChannel(Name, 0, 0);
          end;
        end;

       LNK_CODE_SERVICE_CLOSECHANNEL:
        begin
          Icon:=StrToWord(Str,P);  // Номер канала
          if not Icon in [1..16] then
            Exit;
          if ChannelList[Icon].Permanent or (ChannelList[Icon].Owner<>'') or (ChannelList[Icon].Name='') then
            Exit;
          QuitChannel(ChannelList[Icon].Name);
          CloseChannel(ChannelList[Icon].Name);
          ChannelList[Icon].Name := '';
        end;
    end;
  except
    on e:Exception do
    begin
      Text:='--------------ReceiveDataException--------'+Chr(13)+Chr(10)+'CODE: '+IntToStr(Code)+' DataLength: '+IntToStr(Length(Str))+Chr(13)+Chr(10)+' Data: ';
      for I := 1 to Length(Str) do
        Text:=Text+IntToStr(Ord(Str[I]))+' ';
      PCorePlugin^.onError(PCorePlugin^, e, Text);
    end;
  end;
  SetLength(Str, 0);
end;

constructor TSendData.Create(Sock1: TLinkSocket);
begin
  try
    Timer:=TTimer.Create(nil);
    Timer.OnTimer:=Send;
    Timer.Interval:=3000;
    TimerBans:=TTimer.Create(nil);
    TimerBans.OnTimer:=SendBans;
    TimerBans.Interval:=60000;
    ListNames:=TStringList.Create;
    Sock:=Sock1;
  except
    PCorePlugin^.WriteLog(file_log, 'Error in creating timer!');
  end;
end;

destructor TSendData.Destroy();
begin
  try
    if Assigned(Timer) then
      Timer.Free;
    if Assigned(TimerBans) then
      TimerBans.Free;
    if Assigned(ListNames) then
      ListNames.Free;
  finally
  end;
end;

procedure TSendData.Send(Sender: TObject);
var
  SendCount: LongInt;
  I: DWord;
  User:TUser;
  Status: String;
  DataToSend: String;
  Text: String;
begin
  try
  	if ListNames.Count>0 then
  	begin
    	if ListNames.Count>10 then
      	SendCount:= 10
    	else
      	SendCount:= ListNames.Count;
    	for I := 0 to SendCount - 1 do
    	begin
      	User:=PCorePlugin^.AskUserInfo(ListNames[0], Status);
      	ListNames.Delete(0);
      	DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
      	Sock.SendText(DataToSend);
      	if Status<>'' then
      	begin
        	DataToSend:=WordToStr(LNK_CODE_STATUSCHNG)+TextToStr(User.Name)+TextToStr(Status);
        	Sock.SendText(DataToSend);
      	end;
    	end;
  	end
  	else
    	Timer.Enabled:=False;
  except
    on e:Exception do
    begin
      Text:='--------------SendData.Send Exception--------'+Chr(13)+Chr(10);
      PCorePlugin^.onError(PCorePlugin^, e, Text);
    end;
  end;
end;

procedure TSendData.SendBans(Sender: TObject);
var
  SendCount: DWord;
  Count: LongInt;
  I: LongInt;
  K: Word;
  Restrictions:TRestrictions;
  DataToSend: String;
  Flag:Boolean;
  Text: String;
begin
	try
  	Count:=PCorePlugin^.AskRestrictions(Restrictions);
  	DataToSend:='';
  	SendCount:=0;
  	for I := 1 to Count do
  	begin
    	if (Restrictions[I].date>=LastUpdate) and
      		StrStartsWith(Restrictions[I].Name, name_prefix) and StrEndsWith(Restrictions[I].Name, name_postfix) and
          (Restrictions[I].ident>2) then
      	if Restrictions[I].banType>=2 then
      	begin
        	K:=1;
        	Flag:=False;
        	while not Flag and (K<=16) do
        	begin
          	Flag:=(ChannelList[K].Name=Restrictions[I].channel) and (ChannelList[K].Name<>'');
          	Inc(K);
          end;
        	if Flag then
        	begin
          	DataToSend:=DataToSend+TextToStr(Copy(Restrictions[I].Name, Length(name_prefix)+1, Length(Restrictions[I].Name)-Length(name_prefix)-Length(name_postfix)))+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(K-1)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
          	Inc(SendCount);
        	end;
      	end;
      	{else
      	begin
        	DataToSend:=DataToSend+TextToStr(Copy(Restrictions[I].Name, Length(name_prefix)+1, Length(Restrictions[I].Name)-Length(name_prefix)))+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(0)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
        	Inc(SendCount);
      	end;}
  	end;
  	if LastUpdate=0 then
    	DataToSend:=WordToStr(LNK_CODE_BANLIST)+DWordToStr(SendCount)+WordToStr(0)+DataToSend
  	else
    	DataToSend:=WordToStr(LNK_CODE_BANLIST)+DWordToStr(SendCount)+WordToStr(1)+DataToSend;
  	LastUpdate:=Now;
  	SU.TimerBans.Enabled:=False;
  	Sock.SendText(DataToSend);
  except
    on e:Exception do
    begin
      Text:='--------------SendData.SendBans Exception--------'+Chr(13)+Chr(10);
      PCorePlugin^.onError(PCorePlugin^, e, Text);
    end;
  end;
end;

{$IFNDEF Server}
constructor TReconnect.Create(Sock1: TLinkSocket);
begin
  try
    Timer:=TTimer.Create(nil);
    Timer.OnTimer:=Reconnect;
    Timer.Interval:=60000;
    Timer.Enabled:=True;
    Sock:=Sock1;
  except
    PCorePlugin^.WriteLog(file_log, 'Error in creating timer!');
  end;
end;

destructor TReconnect.Destroy();
begin
  try
    if Assigned(Timer) then
      Timer.Free;
  finally
  end;
end;

procedure TReconnect.Reconnect(Sender: TObject);
begin
	Timer.Interval:=60000;
  if not Sock.S.Active then
     Sock.S.Active:=True;
end;
{$ENDIF}

procedure ConnectUser(User: TUser);
var
	DataToSend: string;
begin
	if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0) = 0) then
	begin
		IniUsers.DeleteKey('Message', CheckStr(User.Name));
    //User := PCorePlugin^.AskUserInfo(User.Name, DataToSend);
    DataToSend := WordToStr(LNK_CODE_JOIN) + TextToStr(User.Name) + TextToStr(User.IP) + WordToStr(User.sex) + TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  IniUsers.WriteInteger('Connect', CheckStr(User.Name), 1);
end;

procedure CheckAndCloseChan(Index: Word);
var
	DataToSend: String;
begin
	if (Index > 0) and (Index <= 16) and not (ChannelList[Index].Permanent) and (ChannelList[Index].Name <> '') then
  begin
  	QuitChannel(ChannelList[Index].Name);
  	CloseChannel(ChannelList[Index].Name);
    DataToSend:=WordToStr(LNK_CODE_SERVICE_CLOSECHANNEL)+WordToStr(Index);
    Sock.SendText(DataToSend);
  end;
end;

procedure onAuthFail(Name: String; Reason: Integer);
var
  DataToSend: String;
  Name2: String;
  Index: Integer;
  Ini: TIniFile;
  StrList: TStringList;
  I: Integer;
begin
  Index:=VUNames.IndexOf(Name);
  if Index<>-1 then
    VUNames.Delete(Index);
  Name2:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix)-Length(name_postfix));
  case Reason of
     0: // превышено максимальное число подключенных пользователей
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXUSERLIMIT)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     1: // имя не соответствует требованиям (содержит служебные символы, либо превышает максимальную длину в 40 символов)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BADNICK)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     2: // бан
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BANNED)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     3: // имя содержит запрещенные слова (не проходит фильтр плохих слов)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKDENIED)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     4: // схожее по написанию имя уже зарегистрировано
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKLEXISTS)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     5: // превышено максимальное число учетных записей с данного IP-адреса
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXNICKLIMIT)+TextToStr(Name2); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     7: // неверный пароль
       begin
         if PCorePlugin^.AskRight(BOT_NAME, 1, '')=1 then
         begin
           PCorePlugin^.AddPassword(BOT_NAME, Name, 0, IniUsers.ReadString('Users',CheckStr(Name2),''));
           DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(Name2); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
         end
         else
         begin
           DataToSend:=WordToStr(LNK_CODE_SERVICE_CANNOTCONNECT)+TextToStr(Name2); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
           Ini:=TIniFile.Create(file_config);
           StrList:=TStringList.Create;
           Ini.ReadSection('Admins', StrList);
           for I := 0 to StrList.Count - 1 do
             //PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, StrList.Names[I],  'У меня не хватает права управления учетными записями! Не могу удалить учетную запись [i]'+Name+'[/i]!');
             PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, UnCheckStr(StrList.Names[I]),  'Не могу удалить учетную запись [i]'+Name+'[/i], удалите её!');
           StrList.Free;
           Ini.Free;
         end;
       end;
     6,8: // заявка на активацию успешно отправлена; заявка на активацию не обработана, либо отклонена
       begin
         if PCorePlugin^.AskRight(BOT_NAME, 2, '')=1 then
         begin
           PCorePlugin^.ActivateUser(BOT_NAME, Name);
           DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(Name2); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
         end
         else
         begin
           DataToSend:=WordToStr(LNK_CODE_SERVICE_CANNOTCONNECT)+TextToStr(Name2); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
           Ini:=TIniFile.Create(file_config);
           StrList:=TStringList.Create;
           Ini.ReadSection('Admins', StrList);
           for I := 0 to StrList.Count - 1 do
             PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, UnCheckStr(StrList.Names[I]),  'У меня не хватает права управления активацией учетных записей! Не могу активировать учетную запись [i]'+Name+'[/i]!');
           StrList.Free;
           Ini.Free;
         end;
       end;
  end;
end;

procedure onMsg(User: TUser; Channel, Text: String; Regime: Integer);
var
  DataToSend: String;
  I,Index: LongInt;
  Num: Word;
begin
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index],DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Num:=0;
  for I := 1 to 16 do
    if ChannelList[I].Name=Channel then
      Num:=I;
  if Num>0 then
  begin
    DataToSend:=WordToStr(LNK_CODE_CMSG)+TextToStr(User.Name)+WordToStr(Num)+TextToStr(Text)+WordToStr(Regime);
    Sock.SendText(DataToSend);
  end;
end;

procedure onImg(User: TUser; Channel: String; Image: TJpegImage);
var
  DataToSend: String;
  I,Index: LongInt;
  Num: Word;
begin
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index],DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Num:=0;
  for I := 1 to 16 do
    if ChannelList[I].Name=Channel then
      Num:=I;
  if Num>0 then
  begin
    DataToSend:=WordToStr(LNK_CODE_CIMG)+TextToStr(User.Name)+WordToStr(Num)+ImgToStr(Image);
    Sock.SendText(DataToSend);
  end;
end;

procedure onPrivate(Name: String; User: TUser; Text: String; Regime: Integer);
var
  DataToSend: String;
  Index: Integer;
  StrList: TStringList;
begin
  if Name=BOT_NAME then
  begin

  	if Copy(Text, 1, 15) = '//createchannel' then
  	begin
  		StrList := TStringList.Create;
    	StrList.Delimiter := ' ';
    	StrList.DelimitedText := Text;
  		if (StrList.Count > 2) then
      begin
      	Index := StrToIntDef(StrList[1], 0);
        if (Index > 0) and (Index <= 16) and not (ChannelList[Index].Permanent) and (ChannelList[Index].Owner = User.Name) then
        begin
        	CheckAndCloseChan(Index);
        	StrList.Delete(0);
        	StrList.Delete(0);
          ChannelList[Index].Name := StrList.DelimitedText;
          CreateChannel(ChannelList[Index].Name, 0, 0);
          DataToSend:=WordToStr(LNK_CODE_SERVICE_CREATECHANNEL)+WordToStr(Index)+TextToStr(ChannelList[Index].Name);
  				Sock.SendText(DataToSend);
        end;
      end;
      StrList.Free;
    	Exit;
  	end;

  	if Copy(Text, 1, 14) = '//closechannel' then
  	begin
  		StrList := TStringList.Create;
    	StrList.Delimiter := ' ';
    	StrList.DelimitedText := Text;
  		if (StrList.Count > 1) then
      begin
      	Index := StrToIntDef(StrList[1], 0);
        if (Index > 0) and (Index <= 16) and (ChannelList[Index].Owner = User.Name) then
        begin
          CheckAndCloseChan(Index);
        end;
      end;
      StrList.Free;
    	Exit;
  	end;

    onPersonalMsg(Name, User, Text);
    Exit;
  end;
  if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=0) then
  begin
    PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Для написания приватных сообщений пользователям сервера '+SERVER_REMOTE+' необходимо подключиться к серверу. Напишите мне "connect" для подключения.');
    Exit;
  end;
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index], DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix)-Length(name_postfix));
  DataToSend:=WordToStr(LNK_CODE_PRIV)+TextToStr(User.Name)+TextToStr(Name)+TextToStr(Text)+WordToStr(Regime);
  Sock.SendText(DataToSend);
end;

procedure onPrivateImg(Name: String; User: TUser; Image: TJpegImage);
var
  DataToSend: String;
  Index: LongInt;
begin
  if Name=BOT_NAME then
  begin
    Exit;
  end;
  if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=0) then
  begin
    PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Для написания приватных сообщений пользователям сервера '+SERVER_REMOTE+' необходимо подключиться к серверу. Напишите мне "connect" для подключения.');
    Exit;
  end;
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index], DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix)-Length(name_postfix));
  DataToSend:=WordToStr(LNK_CODE_PRIVIMG)+TextToStr(User.Name)+TextToStr(Name)+ImgToStr(Image);
  Sock.SendText(DataToSend);
end;

procedure onPersonalMsg(Name: String; User: TUser; Text: String);
var
  DataToSend: String;
  Index: LongInt;
begin
  if Name=BOT_NAME then
  begin
    if Text='connect' then
    begin
      ConnectUser(User);
      Exit;
    end;
    if Text='disconnect' then
    begin
      if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=1) then
      begin
        DataToSend:=WordToStr(LNK_CODE_LEFT)+TextToStr(User.Name);
        Sock.SendText(DataToSend);
        KickUser(User.Name);
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Для подключения к серверу снова напишите мне "connect"');
      end;
      IniUsers.WriteInteger('Connect', CheckStr(User.Name), 0);
      Exit;
    end;
    if Text='status' then
    begin
      if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=0) or not Connected or (SU.ListNames.IndexOf(Name)<>-1) then
      begin
        if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=0) then
          DataToSend:='Вы не подключены к серверу '+SERVER_REMOTE+'.'
        else if not Connected then
          DataToSend:='Вы не подключены к серверу, но будете подключены автоматически, когда он станет доступен.'
        else if (SU.ListNames.IndexOf(Name)<>-1) then
          DataToSend:='Вы не подключены к серверу '+SERVER_REMOTE+', потому что еще не завершена синхронизация списка пользователей на серверах. При любом Вашем сообщении произойдет автоматическое подключение к серверу.';
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, DataToSend);
      end
      else
      begin
        DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSCHECK)+TextToStr(User.Name);
        Sock.SendText(DataToSend);
      end;
      Exit;
    end;
    if Text='silent' then
    begin
      IniUsers.WriteInteger('Message', CheckStr(User.Name), 2);
      Exit;
    end;
    if (Text='остановись') and IsAdmin(User.Name) then
    begin
      PCorePlugin^.StopPlugin;
      Exit;
    end;
    Exit;
  end;
  if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=0) then
  begin
    PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Для написания личных сообщений пользователям сервера '+SERVER_REMOTE+' необходимо подключиться к серверу. Напишите мне "connect" для подключения.');
    Exit;
  end;
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index], DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix)-Length(name_postfix));
  DataToSend:=WordToStr(LNK_CODE_PMSG)+TextToStr(User.Name)+TextToStr(Name)+TextToStr(Text);
  Sock.SendText(DataToSend);
end;

procedure onUserJoinChannel(User: TUser; Channel: String);
var
  DataToSend: String;
  Num, I: Word;
begin
  Num:=0;
  for I := 1 to 16 do
    if (ChannelList[I].Name=Channel) and (Channel<>'') then
      Num:=I;
  if Num>0 then
  begin
  	ConnectUser(User);
    if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=1) then
    begin
      DataToSend:=WordToStr(LNK_CODE_JOINCHAN)+TextToStr(User.Name)+WordToStr(Num);
      Sock.SendText(DataToSend);
    end
    else
    begin
      PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, User.Name, Channel, 'Для доступа к каналу необходимо подключиться к серверу '+SERVER_REMOTE);
      PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Для подключения к серверу напишите мне "connect"');
    end;
  end;
end;

procedure onUserLeftChannel(User: TUser; Channel: String);
var
  DataToSend: String;
  Num, I: Word;
begin
  Num:=0;
  for I := 1 to 16 do
    if (ChannelList[I].Name=Channel) and (Channel<>'') then
      Num:=I;
  if Num>0 then
  begin
    DataToSend:=WordToStr(LNK_CODE_LEFTCHAN)+TextToStr(User.Name)+WordToStr(Num);
    Sock.SendText(DataToSend);
  end;
end;


procedure onUserJoinChat(User: TUser);
var
  DataToSend: String;
  Channels: TChannels;
  I,K,Len: Cardinal;
  Flag: Boolean;
begin

  //if Assigned(VUNames) and (VUNames.IndexOf(User.Name)<>-1) then
  if StrStartsWith(User.Name, name_prefix) and StrEndsWith(User.Name, name_postfix) then
  begin
    VUNames.Add(User.Name);
    Len:=PCorePlugin^.AskUserChannels(User.Name,Channels);
    for I := 1 to Len do
    begin
      Flag:=False;
      for K := 1 to 16 do
        if Channels[I].Name=ChannelList[K].Name then
          Flag:=True;
      if not Flag then
        PCorePlugin^.LeaveChannel(User.Name, Channels[I].Name);
    end;
    DataToSend:=WordToStr(LNK_CODE_SERVICE_CONNECTION_OK)+TextToStr(Copy(User.Name, Length(name_prefix)+1,Length(User.Name)-Length(name_prefix)-Length(name_postfix)));
    Sock.SendText(DataToSend);
  end
  else if User.Name=BOT_NAME then
  begin
    Len:=PCorePlugin^.AskUserChannels(User.Name,Channels);
    for I := 1 to Len do
    begin
      Flag:=False;
      for K := 1 to 16 do
        if Channels[I].Name=ChannelList[K].Name then
          Flag:=True;
      if not Flag then
        PCorePlugin^.LeaveChannel(User.Name, Channels[I].Name);
    end;
    for K := 1 to 16 do
      if ChannelList[K].Name<>'' then
        PCorePlugin^.AddChannel(BOT_NAME, ChannelList[K].Name, 0, 0);
  end
  else
  begin
    if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=1) then
    begin
      User:=PCorePlugin^.AskUserInfo(User.Name, DataToSend);
      DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
      Sock.SendText(DataToSend);
    end;
  end;

  Channels:=nil;
end;

procedure onUserLeftChat(User: TUser);
var
  DataToSend: String;
  Index: LongInt;
begin
  if not (Assigned(VUNames) and (VUNames.IndexOf(User.Name)<>-1)) and
  	not (StrStartsWith(User.Name, name_prefix) and StrEndsWith(User.Name, name_postfix)) then
  begin
    if IniUsers.ReadInteger('Message', CheckStr(User.Name), 2)<>2 then
      IniUsers.DeleteKey('Message', CheckStr(User.Name));
    Index:=SU.ListNames.IndexOf(User.Name);
    if Index=-1 then
    begin
      DataToSend:=WordToStr(LNK_CODE_LEFT)+TextToStr(User.Name);
      Sock.SendText(DataToSend);
    end
    else
      SU.ListNames.Delete(Index);
  end
  {else if Assigned(VUNames) and (VUNames.IndexOf(User.Name)<>-1) and Connected then
  begin
    DataToSend:=WordToStr(LNK_CODE_BAN)+TextToStr(Copy(User.Name, Length(name_prefix)+1, Length(User.Name)-Length(name_prefix)));
    Sock.SendText(DataToSend);
    VUNames.Delete(VUNames.IndexOf(User.Name));
  end;}
end;

procedure onUserStatusChanged(User: TUser; Text: String);
var
  DataToSend: String;
begin
  if not (Assigned(VUNames) and (VUNames.IndexOf(User.Name)<>-1)) then
  begin
    if (IniUsers.ReadInteger('Connect', CheckStr(User.Name), 0)=1) then
    begin
      DataToSend:=WordToStr(LNK_CODE_STATUSCHNG)+TextToStr(User.Name)+TextToStr(Text);
      Sock.SendText(DataToSend);
    end;
  end;
end;

procedure onRestrictionAdd(Restriction: TRestriction);
var
  K: Word;
  DataToSend: String;
  Flag:Boolean;
begin
  DataToSend:='';
  if StrStartsWith(Restriction.Name, name_prefix) and StrEndsWith(Restriction.Name, name_postfix) and (Restriction.ident>2) then
      if Restriction.banType>=2 then
      begin
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K].Name=Restriction.channel) and (ChannelList[K].Name<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=TextToStr(Copy(Restriction.Name, Length(name_prefix)+1, Length(Restriction.Name)-Length(name_prefix)-Length(name_postfix)))+DoubleToStr(Restriction.Remain)+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(Restriction.moder)+TextToStr(Restriction.Reason);
        end;
      end;
      {else
      begin
        DataToSend:=DataToSend+TextToStr(Copy(Restrictions[I].Name, Length(name_prefix)+1, Length(Restrictions[I].Name)-Length(name_prefix)))+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(0)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
      end;}
  if DataToSend<>'' then
  begin
  	DataToSend:=WordToStr(LNK_CODE_BANLIST)+DWordToStr(1)+WordToStr(1)+DataToSend;
  	Sock.SendText(DataToSend);
  end;
end;

procedure onRestrictionRemove(Restriction: TRestriction; UnBanModerName: String);
var
  K: Word;
  DataToSend: String;
  Flag:Boolean;
begin
  DataToSend:='';
  if StrStartsWith(Restriction.Name, name_prefix) and StrEndsWith(Restriction.Name, name_postfix) and (Restriction.ident>2) then
  		if Restriction.banType>=2 then
      begin
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K].Name=Restriction.channel) and (ChannelList[K].Name<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=TextToStr(Copy(Restriction.Name, Length(name_prefix)+1, Length(Restriction.Name)-Length(name_prefix)-Length(name_postfix)))+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(UnBanModerName)+TextToStr(Restriction.Reason);
        end;
      end;
      {else
      begin
        DataToSend:=DataToSend+TextToStr(Copy(Restrictions[I].Name, Length(name_prefix)+1, Length(Restrictions[I].Name)-Length(name_prefix)))+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(0)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
      end;}
  if DataToSend<>'' then
  begin
  	DataToSend:=WordToStr(LNK_CODE_UNBAN)+DataToSend;
  	Sock.SendText(DataToSend);
  end;
end;

procedure LoadSettings(Ini:TIniFile);
var
  I: Byte;
  S: String;
begin
  if Assigned(IgnorePrefixList) then
    FreeAndNil(IgnorePrefixList);
  if Assigned(WhiteStartList) then
    FreeAndNil(WhiteStartList);
  
  IgnorePrefixList:=TStringList.Create;
  IgnorePrefixList.Clear;
  WhiteStartList := TStringList.Create;
  WhiteStartList.Clear;
  for I := 1 to 16 do
  begin
    ChannelList[I].Name:=Ini.ReadString('Channels', 'Channel'+IntToStr(I), '');
    ChannelList[I].Permanent := (ChannelList[I].Name <> '');
    ChannelList[I].Owner:=Ini.ReadString('Channels', 'Owner'+IntToStr(I), '');

    S:=Ini.ReadString('IgnorePrefixes', 'Prefix'+IntToStr(I), '');
    if S<>'' then
      IgnorePrefixList.Add(S);

    S:=Ini.ReadString('Channels', 'WhiteStart'+IntToStr(I), '');
    if S<>'' then
      WhiteStartList.Add(S);
  end;
  CONNECT_IP:=Ini.ReadString('Linker', 'ConnectIP', '127.0.0.1');
  CONNECT_PORT:=Ini.ReadInteger('Linker', 'ConnectPort', 6538);
  SERVER_LOCAL:=Ini.ReadString('Linker', 'ServerName', 'MyServer');
  name_prefix:=Ini.ReadString('Linker', 'NamePrefix', '');
  name_postfix:=Ini.ReadString('Linker', 'NamePostfix', '[Lk]');
  if (name_prefix='') and (name_postfix='') then
  	name_postfix := '[Lk]';
  StartKey:=Ini.ReadInteger('Keys', 'MainKey', 0);
  if (StartKey = 0) then
  begin
  	StartKey :=  Random(2000000000) + 10;
  	Ini.WriteInteger('Keys', 'MainKey', StartKey);
  end;
end;

function Init():Integer;
var
  Channels: TChannels;
  I,K,Len: Cardinal;
  Flag: Boolean;
begin
  Connected:=False;
  IniUsers:=TIniFile.Create(file_users);
  VUNames:=TStringList.Create;
  Sock:=TLinkSocket.Create;
  {$IFNDEF Server}
  RC:=TReconnect.Create(Sock);
  {$ENDIF}
  SU:=TSendData.Create(Sock);
  Result:=1;
  AddKey:=0;
  //IniUsers.EraseSection('Message');
  Len:=PCorePlugin^.AskUserChannels(BOT_NAME,Channels);
  for I := 1 to Len do
  begin
  	Flag:=False;
    for K := 1 to 16 do
    	if Channels[I].Name=ChannelList[K].Name then
      	Flag:=True;
    if not Flag then
    	PCorePlugin^.LeaveChannel(BOT_NAME, Channels[I].Name);
  end;
  for K := 1 to 16 do
  	if ChannelList[K].Name<>'' then
    	PCorePlugin^.AddChannel(BOT_NAME, ChannelList[K].Name, 0, 0);
end;

procedure Destroy();
var
	I: Integer;
begin
	if Connected then
  	for I := 1 to 16 do
    begin
    	CheckAndCloseChan(I);
    end;
  SU.Free;
  {$IFNDEF Server}
  RC.Free;
  {$ENDIF}
  Sock.Free;
  IniUsers.Free;
  Connected:=False;
end;

end.
