{$DEFINE Server}
unit link;

interface

uses
  IniFiles,
  Windows, WinInet, SysUtils, Classes, SyncObjs,
  Math, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  comm_info, comm_data, libfunc, libqueue, ScktComp, LongDataTransfer, JPEG;

type
  TLinkSocket = class
    public
      {$IFDEF Server}
      S: TServerSocket;
      {$ELSE}
      S: TClientSocket;
      {$ENDIF}
      ConnS: TCustomWinSocket;

      procedure SendText(Str: string; EncType:Char='E');

      constructor Create();
      destructor Destroy(); override;
      procedure SocketRead(Sender: TObject; Socket: TCustomWinSocket);
      {$IFDEF Server}
      procedure ServerSocketAccept(Sender: TObject; Socket: TCustomWinSocket);
      procedure ServerSocketClientConnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ServerSocketClientDisconnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ServerSocketClientError(Sender: TObject;
        Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
        var ErrorCode: Integer);
      {$ELSE}
      procedure OnKeysReceive();
      procedure ClientSocketConnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ClientSocketDisconnect(Sender: TObject;
        Socket: TCustomWinSocket);
      procedure ClientSocketError(Sender: TObject;
        Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
        var ErrorCode: Integer);
      {$ENDIF}
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

  ChannelList: array [1..16] of String;
  IgnorePrefixList: TStringList;

  Connected: Boolean;

implementation

constructor TLinkSocket.Create();
begin
  try
    {$IFDEF Server}
    S:=TServerSocket.Create(nil);
    S.OnAccept:=ServerSocketAccept;
    S.OnClientConnect:=ServerSocketClientConnect;
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
    ConnS:=nil;
  end;
end;

procedure TLinkSocket.SendText(Str: string; EncType:Char='E');
begin
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
  ConnS:=Socket;
  VUNames.Clear;
  SU.TimerBans.Enabled:=False;
  SU.Timer.Enabled:=False;
  SU.ListNames.Clear;
  Count:=PCorePlugin^.AskUsersInChat(Users);
  for I := 1 to Count do
    if (Users[I].Name<>BOT_NAME) and (VUNames.IndexOf(Users[I].Name)=-1) and  (IniUsers.ReadInteger('Connect', CheckStr(Users[I].Name), 0)=1)  then
      SU.ListNames.Add(Users[I].Name);
  SU.Timer.Enabled:=True;
  AddKey:=Random(2000000000);
  Self.SendText(IntToStr(AddKey),'K');
  DataToSend:=WordToStr(LNK_CODE_SERVICE_SERVERNAME)+TextToStr(SERVER_LOCAL);
  Self.SendText(DataToSend);
  SU.LastUpdate:=0;
  SU.TimerBans.Enabled:=True;
  Connected:=True;
end;
{$ENDIF}

{$IFDEF Server}
procedure TLinkSocket.ServerSocketClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  if Socket.RemoteAddress<>CONNECT_IP then
  begin
  	PCorePlugin^.WriteLog(file_log, 'Denied '+Socket.RemoteAddress);
    Socket.Close;
  end;
  //Memo1.Lines.Add('Client Connected! '+Socket.RemoteAddress);
end;

{$ELSE}
procedure TLinkSocket.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  SU.LastUpdate:=0;
  SU.TimerBans.Enabled:=True;
end;

procedure TLinkSocket.OnKeysReceive();
var
  Users: TUsers;
  Count, I: DWord;
  DataToSend: String;
begin
  ConnS:=S.Socket;
  SU.Timer.Enabled:=False;
  SU.ListNames.Clear;
  VUNames.Clear;
  DataToSend:=WordToStr(LNK_CODE_SERVICE_SERVERNAME)+TextToStr(SERVER_LOCAL);
  Self.SendText(DataToSend);
  Count:=PCorePlugin^.AskUsersInChat(Users);
  for I := 1 to Count do
    if (Users[I].Name<>BOT_NAME) and (VUNames.IndexOf(Users[I].Name)=-1) and (IniUsers.ReadInteger('Connect', CheckStr(Users[I].Name), 0)=1) then
      SU.ListNames.Add(Users[I].Name);
  SU.Timer.Enabled:=True;
  Connected:=True;
end;
{$ENDIF}

{$IFDEF Server}
procedure TLinkSocket.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
var I: LongInt;
begin
  if (ConnS = Socket) then
  begin
  	ConnS:=nil;
  	Connected:=False;
  	SU.Timer.Enabled:=False;
  	SU.ListNames.Clear;
  	for I := 0 to VUNames.Count - 1 do
    	PCorePlugin^.LeaveVirtualUser(VUNames[I]);
  	VUNames.Clear;
  	SU.TimerBans.Enabled:=False;
  	PCorePlugin^.AddState(BOT_NAME, '');
  end;
end;

{$ELSE}
procedure TLinkSocket.ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
var I: LongInt;
begin
  ConnS:=nil;
  Connected:=False;
  SU.Timer.Enabled:=False;
  SU.ListNames.Clear;
  for I := 0 to VUNames.Count - 1 do
    PCorePlugin^.LeaveVirtualUser(VUNames[I]);
  VUNames.Clear;
  SU.TimerBans.Enabled:=False;
  PCorePlugin^.AddState(BOT_NAME, '');
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
  for I := 1 to 16 do
    if ChannelList[I]<>'' then
    begin
      Count:=PCorePlugin^.AskUsersInChannel(BOT_NAME, ChannelList[I], Users);
      Flag:=False;
      Index:=1;
      while not Flag and (Index<=Count) do
      begin
        Flag:=(Users[Index].Name=UserName);
        Inc(Index);
      end;
      if Flag then
        PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, UserName, ChannelList[I], 'Вам был запрещен доступ к каналу в связи с вашим отключением от сервера '+SERVER_REMOTE);
      end;
  if IniUsers.ReadInteger('Message', CheckStr(UserName), 0)<>2 then
    IniUsers.DeleteKey('Message', CheckStr(UserName));
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
begin
  Code:=0;
  Str:=Copy(S, 1, 1);
  if Str='E' then
    Str:=Decrypt(Copy(S,2,Length(S)-1), StartKey, MultKey, AddKey)
  {$IFNDEF Server}
  else if Str='K' then
  begin
    AddKey:=StrToIntDef(Copy(S,2,Length(S)-1), 0);
    Sock.OnKeysReceive();
    Exit;
  end
  {$ENDIF}
  else
    Str:=Copy(S,2,Length(S)-1);
  try
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
          PCorePlugin^.JoinVirtualUser(name_prefix+Name,Text,0,Pass, Icon, Channel);
        end;
      LNK_CODE_LEFT:
        begin
          Name:=StrToText(Str,P);
          Index:=VUNames.IndexOf(name_prefix+Name);
          if Index<>-1 then
            VUNames.Delete(VUNames.IndexOf(name_prefix+Name));
          Count:=PCorePlugin^.AskUserChannels(name_prefix+Name, Channels);
          for I := 1 to Count do
            PCorePlugin^.LeaveChannel(name_prefix+Name, Channels[I].Name);
          PCorePlugin^.LeaveVirtualUser(name_prefix+Name);
        end;
      LNK_CODE_JOINCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I];
          //if VUNames.IndexOf(name_prefix+Name)=-1 then // Забанен?
          //begin
          //  DataToSend:=WordToStr(LNK_CODE_SERVICE_JOINCHANFAIL)+TextToStr(Name)+WordToStr(I); // сервисное сообщение на другой сервер
          //  Sock.SendText(DataToSend);
          //end
          //else
            PCorePlugin^.AddChannel(name_prefix+Name,Channel,0,0);
        end;
      LNK_CODE_LEFTCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I];
          PCorePlugin^.LeaveChannel(name_prefix+Name,Channel);
        end;
      LNK_CODE_CMSG:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I];
          Text:=StrToText(Str,P);
          Icon:=StrToWord(Str, P);
          PCorePlugin^.AddChannel(name_prefix+Name,Channel,0,0);
          PCorePlugin^.AddMessageToChannel(name_prefix+Name, Channel, Icon, Text);
        end;
      LNK_CODE_CIMG:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I];
          Image:=StrToImg(Str,P);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddChannel(name_prefix+Name,Channel,0,0);
          		PCorePlugin^.AddImageToChannel(name_prefix+Name, Channel, Image);
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
          PCorePlugin^.AddPrivateMessage(name_prefix+Name, Icon, Channel, Text);
        end;
      LNK_CODE_PRIVIMG:
        begin
          Name:=StrToText(Str,P);
          Channel:=StrToText(Str,P);
          Image:=StrToImg(Str,P);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddPrivateImage(name_prefix+Name, Channel, Image);
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
          PCorePlugin^.AddPersonalMessage(name_prefix+Name, 0, Channel, Text);
        end;
      LNK_CODE_STATUSCHNG:
        begin
          Name:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          PCorePlugin^.AddState(name_prefix+Name, Text);
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
              Channel:=ChannelList[Icon];
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
          	Channel:=ChannelList[Icon];
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
          if VUNames.IndexOf(name_prefix+Name)=-1 then
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(1)+TextToStr('') // сервисное сообщение на другой сервер
          else
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(0)+TextToStr(name_prefix+Name); // сервисное сообщение на другой сервер
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
          if (IniUsers.ReadInteger('Message', CheckStr(Name), 0)=0) then
          begin
            Text:= 'Вы подключены к серверу '+SERVER_REMOTE+'. Вы можете отключиться в любой момент, написав мне "disconnect"'+Chr(13)+Chr(10)+'Список общих каналов:';
            for I := 1 to 16 do
              if (ChannelList[I]<>'') then
                Text:=Text+Chr(13)+Chr(10)+'[url=/channel: '+ChannelList[I]+']'+ChannelList[I]+'[/url]';
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
          if ChannelList[Icon]='' then
            Exit;
          PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, Name, ChannelList[Icon], 'Вам был запрещен доступ к каналу в связи с вашим отключением от сервера '+SERVER_REMOTE);
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
begin
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
begin
  Count:=PCorePlugin^.AskRestrictions(Restrictions);
  DataToSend:='';
  SendCount:=0;
  for I := 1 to Count do
  begin
    if (Restrictions[I].date>=LastUpdate) and (Copy(Restrictions[I].Name, 1, Length(name_prefix))=name_prefix) and (Restrictions[I].ident>2) then
      if Restrictions[I].banType>=2 then
      begin
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K]=Restrictions[I].channel) and (ChannelList[K]<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=DataToSend+TextToStr(Copy(Restrictions[I].Name, Length(name_prefix)+1, Length(Restrictions[I].Name)-Length(name_prefix)))+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(K-1)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
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
  Name2:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix));
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
           //PCorePlugin^.AddPassword(BOT_NAME, Name, 0, IniUsers.ReadString('Users',CheckStr(Name2),''));
           PCorePlugin^.RemoveUser(BOT_NAME, Name);
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
    if ChannelList[I]=Channel then
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
    if ChannelList[I]=Channel then
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
  Index: LongInt;
begin
  if Name=BOT_NAME then
  begin
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
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix));
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
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix));
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
  Name:=Copy(Name,Length(name_prefix)+1, Length(Name)-Length(name_prefix));
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
    if (ChannelList[I]=Channel) and (Channel<>'') then
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
    if (ChannelList[I]=Channel) and (Channel<>'') then
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
  if Copy(User.Name, 1, Length(name_prefix))=name_prefix then
  begin
    VUNames.Add(User.Name);
    Len:=PCorePlugin^.AskUserChannels(User.Name,Channels);
    for I := 1 to Len do
    begin
      Flag:=False;
      for K := 1 to 16 do
        if Channels[I].Name=ChannelList[K] then
          Flag:=True;
      if not Flag then
        PCorePlugin^.LeaveChannel(User.Name, Channels[I].Name);
    end;
    DataToSend:=WordToStr(LNK_CODE_SERVICE_CONNECTION_OK)+TextToStr(Copy(User.Name, Length(name_prefix)+1,Length(User.Name)-Length(name_prefix)));
    Sock.SendText(DataToSend);
  end
  else if User.Name=BOT_NAME then
  begin
    Len:=PCorePlugin^.AskUserChannels(User.Name,Channels);
    for I := 1 to Len do
    begin
      Flag:=False;
      for K := 1 to 16 do
        if Channels[I].Name=ChannelList[K] then
          Flag:=True;
      if not Flag then
        PCorePlugin^.LeaveChannel(User.Name, Channels[I].Name);
    end;
    for K := 1 to 16 do
      if ChannelList[K]<>'' then
        PCorePlugin^.AddChannel(BOT_NAME, ChannelList[K], 0, 0);
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
  if not (Assigned(VUNames) and (VUNames.IndexOf(User.Name)<>-1)) and (Copy(User.Name, 1, Length(name_prefix))<>name_prefix) then
  begin
    if IniUsers.ReadInteger('Message', CheckStr(User.Name), 0)<>2 then
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
  if (Copy(Restriction.Name, 1, Length(name_prefix))=name_prefix) and (Restriction.ident>2) then
      if Restriction.banType>=2 then
      begin
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K]=Restriction.channel) and (ChannelList[K]<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=TextToStr(Copy(Restriction.Name, Length(name_prefix)+1, Length(Restriction.Name)-Length(name_prefix)))+DoubleToStr(Restriction.Remain)+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(Restriction.moder)+TextToStr(Restriction.Reason);
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
  if (Copy(Restriction.Name, 1, Length(name_prefix))=name_prefix) and (Restriction.ident>2) then
  		if Restriction.banType>=2 then
      begin
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K]=Restriction.channel) and (ChannelList[K]<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=TextToStr(Copy(Restriction.Name, Length(name_prefix)+1, Length(Restriction.Name)-Length(name_prefix)))+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(UnBanModerName)+TextToStr(Restriction.Reason);
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
  IgnorePrefixList:=TStringList.Create;
  IgnorePrefixList.Clear;
  for I := 1 to 16 do
  begin
    ChannelList[I]:=Ini.ReadString('Channels', 'Channel'+IntToStr(I), '');
    S:=Ini.ReadString('IgnorePrefixes', 'Prefix'+IntToStr(I), '');
    if S<>'' then
      IgnorePrefixList.Add(S);
  end;
  CONNECT_IP:=Ini.ReadString('Linker', 'ConnectIP', '127.0.0.1');
  CONNECT_PORT:=Ini.ReadInteger('Linker', 'ConnectPort', 6538);
  SERVER_LOCAL:=Ini.ReadString('Linker', 'ServerName', 'MyServer');
  name_prefix:=Ini.ReadString('Linker', 'NamePrefix', Chr($13DE));
  StartKey:=Ini.ReadInteger('Keys', 'StartKey', Random(2000000000));
  MultKey:=Ini.ReadInteger('Keys', 'MultKey', Random(2000000000));
  Ini.WriteInteger('Keys', 'StartKey', StartKey);
  Ini.WriteInteger('Keys', 'MultKey', MultKey);
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
    	if Channels[I].Name=ChannelList[K] then
      	Flag:=True;
    if not Flag then
    	PCorePlugin^.LeaveChannel(BOT_NAME, Channels[I].Name);
  end;
  for K := 1 to 16 do
  	if ChannelList[K]<>'' then
    	PCorePlugin^.AddChannel(BOT_NAME, ChannelList[K], 0, 0);
end;

procedure Destroy();
begin
  SU.Free;
  {$IFNDEF Server}
  RC.Free;
  {$ENDIF}
  Sock.Free;
  IniUsers.Free;
  Connected:=False;
end;

{  procedure ResetTimerQ();
  var
    Ptr: Pointer;
    Buf: TBytes;
  begin
    SetLength(Buf, 4);
    Ptr:=@ResetTimer;
    CopyMemory(@Buf[0], @Ptr, 4);
    MsgQueue.InsertMsg(QUEUE_MSGTYPE_CALL, @Buf[0], 4);
    Buf:=nil;
  end; }


end.
