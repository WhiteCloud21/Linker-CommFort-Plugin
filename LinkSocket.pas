{$I plugin.inc}
unit LinkSocket;

interface
uses
  Windows, WinInet, SysUtils, Classes, SyncObjs,
  Math, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  comm_data, comm_info, libfunc, linkData, libVirtualUsers,
  libClasses, ScktComp, LongDataTransfer, LbRSA;

type
  TLinkSocketError = procedure (ErrCode: Integer);
  TReceiveHandler = procedure(const S: String);
  TSendData = class;

  TLinkSocket = class
    private
      ReceiveDataCriticalSection: TCriticalSection;
      ReceiveHandler: TReceiveHandler;
      SU: TSendData;
      {$IFNDEF Server}
      ReconnectTimer: TTimer;
      {$ENDIF}
    public
    	Connected: Boolean;
      {$IFDEF Server}
      S: TServerSocket;
      {$ELSE}
      S: TClientSocket;
      {$ENDIF}
      ConnS: TCustomWinSocket;

      LbRSA1: TLbRSA;

      OnDisconnect: TNotifyEvent;

      procedure SendText(Str: string; EncType:Char='E');

      constructor Create(ReceiveHandler: TReceiveHandler);
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
      {$IFNDEF Server}
      procedure Reconnect(Sender: TObject);
      {$ENDIF}
      function IsUserInSendList(UserName: String): Boolean;
      function ForceUserSend(UserName: String): Boolean;
      function ForceUserRemove(UserName: String): Boolean;
  end;

  TSendData = class
      LastUpdate:TDateTime;
      Timer: TTimer;
      TimerBans: TTimer;
      Sock: TLinkSocket;

      ListNames: TStringList;

      constructor Create(Sock: TLinkSocket);
      destructor Destroy(); override;

      procedure Send(Sender: TObject);
      procedure SendBans(Sender: TObject);

  end;

implementation

constructor TLinkSocket.Create(ReceiveHandler: TReceiveHandler);
begin
  try
  	Self.ReceiveHandler := ReceiveHandler;
    SU := TSendData.Create(Self);
  	ReceiveDataCriticalSection := TCriticalSection.Create;
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
    {$IFNDEF Server}
    ReconnectTimer := TTimer.Create(nil);
    ReconnectTimer.Enabled := True;
    ReconnectTimer.Interval := 60000;
    ReconnectTimer.OnTimer := Reconnect;
    {$ENDIF}
  except
  	on e: Exception do
    PCorePlugin^.WriteLog(file_log, 'Error in creating socket: ' + e.Message);
  end;
end;

destructor TLinkSocket.Destroy();
begin
  try
    if S.Active then
      S.Close();
  finally
    {$IFNDEF Server}
  	FreeAndNil(ReconnectTimer);
    {$ENDIF}
  	FreeAndNil(SU);
    FreeAndNil(S);
    FreeAndNil(LbRSA1);
    ConnS:=nil;
    Connected := False;
    ReceiveDataCriticalSection.Free;
  end;
end;

procedure TLinkSocket.SendText(Str: string; EncType:Char='E');
begin
  if EncType='E' then
  begin
    if not Connected then Exit;
    Str:=EncType+Encrypt(Str, StartKey, MultKey, AddKey);
  end
  else
    Str:=EncType+Str;
  SendLongText(ConnS, Str);
end;

procedure TLinkSocket.SocketRead(Sender: TObject;
  Socket: TCustomWinSocket);
begin
	ReceiveDataCriticalSection.Enter;
  try
  	ReceiveLongText(Socket, ReceiveHandler);
  finally
  	ReceiveDataCriticalSection.Leave;
  end;
end;

{$IFDEF Server}
procedure TLinkSocket.ServerSocketAccept(Sender: TObject; Socket: TCustomWinSocket);
var
	DataToSend: String;
begin
	if Socket.RemoteAddress<>CONNECT_IP then
	begin
		PCorePlugin^.WriteLog(file_log, 'Denied '+Socket.RemoteAddress);
		Socket.Close;
	end
	else
	begin
		if ConnS <> nil then
    begin
			ConnS.Close;
      Socket.Close;
      Exit;
    end;
		ConnS:=Socket;
		VirtUsers.Clear;
		SU.TimerBans.Enabled:=False;
		SU.Timer.Enabled:=False;
		SU.ListNames.Clear;
		LbRSA1.GenerateKeyPair;
		DataToSend:=DwordToStr(PROTOCOL_VER)+TextToStr(LbRSA1.PublicKey.ModulusAsString)+TextToStr(LbRSA1.PublicKey.ExponentAsString);
		Self.SendText(DataToSend, 'K');
	end;
end;

{$ELSE}

procedure TLinkSocket.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
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
    VirtUsers.Clear;
  	SU.TimerBans.Enabled:=False;
    SU.Timer.Enabled:=False;
    SU.ListNames.Clear;
    Connected:=True;

    Count:=PCorePlugin^.AskUsersInChat(Users);
    for I := 1 to Count do
      if (Users[I].Name<>BOT_NAME) and not VirtUsers.IsOtherPluginVirtualUser(Users[I].Name) and (not VirtUsers.IsVirtualUser (Users[I].Name))
      	and not StrEndsWith(Users[I].Name, '[Lk]') and not StrEndsWith(Users[I].Name, '[Lk2]')
    		and not StrEndsWith(Users[I].Name, '[Lk3]') and not StrEndsWith(Users[I].Name, '[Lk4]') then
        SU.ListNames.Add(Users[I].Name);
    SU.LastUpdate:=0;
    SU.Timer.Enabled:=True;
    SU.TimerBans.Enabled:=True;

    // Отправка информации о сервере
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
  except
    on e:Exception do
    begin
      Text:='--------------OnKeysReceiveException--------'+Chr(13)+Chr(10);
      PCorePlugin^.onError(PCorePlugin^, e, Text);
      Connected := false;
    end;
  end;
end;

{$IFDEF Server}
procedure TLinkSocket.ServerSocketClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
var
	I: LongInt;
  VUNames: TStrings;
begin
  if (ConnS = Socket) then
  begin
  	try
  		ConnS:=nil;
  		Connected:=False;
			LongDataTransfer.FlushBuffers;
  		SU.Timer.Enabled:=False;
  		SU.ListNames.Clear;
      VUNames := VirtUsers.GetAllUsers();
  		for I := 0 to VUNames.Count - 1 do
    		PCorePlugin^.LeaveVirtualUser(VUNames[I]);
      VUNames.Free;
      VirtUsers.Clear;
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
var
	I: LongInt;
  VUNames: TStrings;
begin
	try
		ConnS:=nil;
		Connected:=False;
		LongDataTransfer.FlushBuffers;
		SU.Timer.Enabled:=False;
		SU.ListNames.Clear;
    VUNames := VirtUsers.GetAllUsers();
    for I := 0 to VUNames.Count - 1 do
      PCorePlugin^.LeaveVirtualUser(VUNames[I]);
    VUNames.Free;
    VirtUsers.Clear;
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

{$IFNDEF Server}
procedure TLinkSocket.Reconnect(Sender: TObject);
begin
  if not S.Active then
  	S.Active:=True;
end;
{$ENDIF}

function TLinkSocket.IsUserInSendList(UserName: String): Boolean;
begin
	Result := SU.ListNames.IndexOf(UserName) >= 0;
end;

function TLinkSocket.ForceUserSend(UserName: String): Boolean;
var
	Index: Integer;
  DataToSend: String;
  User: TUser;
begin
	Result := False;
  Index:=SU.ListNames.IndexOf(UserName);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(UserName, DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    SendText(DataToSend);
		Result := True;
  end;
end;

function TLinkSocket.ForceUserRemove(UserName: String): Boolean;
var
	Index: Integer;
  DataToSend: String;
begin
	Index:=SU.ListNames.IndexOf(UserName);
  if Index=-1 then
  begin
  	DataToSend:=WordToStr(LNK_CODE_LEFT)+TextToStr(UserName);
    SendText(DataToSend);
  end
  else
  	SU.ListNames.Delete(Index);
  Result:= True;
end;

constructor TSendData.Create(Sock: TLinkSocket);
begin
  try
    Timer:=TTimer.Create(nil);
    Timer.OnTimer:=Send;
    Timer.Interval:=users_packet_interval;
    TimerBans:=TTimer.Create(nil);
    TimerBans.OnTimer:=SendBans;
    TimerBans.Interval:=60000;
    ListNames:=TStringList.Create;
    Self.Sock:=Sock;
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
    	if ListNames.Count>users_per_packet then
      	SendCount:= users_per_packet
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
  VirtualUser: TVirtualUser;
  ListVU: TAutoFreeVirtUsersList;
begin
	try
  	Count:=PCorePlugin^.AskRestrictions(Restrictions);
  	DataToSend:='';
  	SendCount:=0;
  	for I := 1 to Count do
  	begin
    	ListVU := UsersDatabase.GetUserInfos(Restrictions[I].Name);
    	if (Restrictions[I].date>=LastUpdate) and
      		(ListVU.Count > 0) and
          (Restrictions[I].ident>2) then
      	if Restrictions[I].banType>=2 then
      	begin
      		VirtualUser := TVirtualUser(ListVU[0]^);
        	K:=1;
        	Flag:=False;
        	while not Flag and (K<=16) do
        	begin
          	Flag:=(ChannelList[K].Name=Restrictions[I].channel) and (ChannelList[K].Name<>'');
          	Inc(K);
          end;
        	if Flag then
        	begin
          	DataToSend:=DataToSend+TextToStr(VirtualUser.Name)+DoubleToStr(Restrictions[I].Remain)+DWordToStr(Restrictions[I].Ident)+DwordToStr(Restrictions[I].banType)+WordToStr(K-1)+TextToStr(Restrictions[I].moder)+TextToStr(Restrictions[I].Reason);
          	Inc(SendCount);
        	end;
      	end;
      ListVU.Free;
  	end;
  	if LastUpdate=0 then
    	DataToSend:=WordToStr(LNK_CODE_BANLIST)+DWordToStr(SendCount)+WordToStr(0)+DataToSend
  	else
    	DataToSend:=WordToStr(LNK_CODE_BANLIST)+DWordToStr(SendCount)+WordToStr(1)+DataToSend;
  	LastUpdate:=Now;
  	TimerBans.Enabled:=False;
  	Sock.SendText(DataToSend);
  except
    on e:Exception do
    begin
      Text:='--------------SendData.SendBans Exception--------'+Chr(13)+Chr(10);
      PCorePlugin^.onError(PCorePlugin^, e, Text);
    end;
  end;
end;

end.
