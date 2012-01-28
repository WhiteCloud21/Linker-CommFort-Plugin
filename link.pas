{$I plugin.inc}

unit link;

interface

uses
  IniFiles,
  Windows, WinInet, SysUtils, Classes, SyncObjs,
  Math, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  comm_info, comm_data, libfunc, libqueue, libmd5, libClasses,
  libVirtualUsers, ScktComp, LongDataTransfer, JPEG, LbRSA;

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
  VirtUsers: TConnectedVirtualUsers;
  UsersDatabase: TUsersDatabase;

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
    Connected := False;
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
  ReceiveLongText(Socket,ProcessData);
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
      if (Users[I].Name<>BOT_NAME) and not VirtUsers.IsOtherPluginVirtualUser(Users[I].Name) and (not VirtUsers.IsVirtualUser (Users[I].Name))then
        SU.ListNames.Add(Users[I].Name);
    SU.LastUpdate:=0;
    SU.Timer.Enabled:=True;
    SU.TimerBans.Enabled:=True;

    // �������� ���������� � �������
    DataToSend:=WordToStr(LNK_CODE_SERVICE_SERVERNAME)+TextToStr(SERVER_LOCAL);
    Self.SendText(DataToSend);

    // �������� ������ ��������� �������
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
        	PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, UserName, ChannelList[I].Name, '��� ��� �������� ������ � ������ � ����� � ����� ����������� �� ������� '+SERVER_REMOTE);
      	end;
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
    VirtualUser: TConnectedVirtualUser;
    SavedVirtUser: TVirtualUser;
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
      PCorePlugin^.WriteLog(file_log, '���������� �� �����������. ������ ���������: '+IntToStr(PROTOCOL_VER)+'. ������ ��������� �������:'+IntToStr(Ident));
      Sock.ConnS.Close;
    	Exit;
    end;
    Count := 1;
    AddKey := StrToDword(Sock.LbRSA1.DecryptStringW(StrToText(Str, P)), Count);
    Count := 1;
    MultKey := StrToDword(Sock.LbRSA1.DecryptStringW(StrToText(Str, P)), Count);
    Sock.OnKeysReceive();
    Exit;
  end
  {$ELSE}
  else if Str='K' then
  begin
    Str:=Copy(S,2,Length(S)-1);
    P:=1;
    Ident := StrToDword(Str, P);
    if Ident <> PROTOCOL_VER then
    begin
      PCorePlugin^.WriteLog(file_log, '���������� �� �����������. ������ ���������: '+IntToStr(PROTOCOL_VER)+'. ������ ��������� �������:'+IntToStr(Ident));
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
          SavedVirtUser.Name:=StrToText(Str,P);
          SavedVirtUser.ServId := server_id;
          if StrEndsWith(SavedVirtUser.Name, '[Lk]') or StrEndsWith(SavedVirtUser.Name, '[Lk2]')
          	or StrEndsWith(SavedVirtUser.Name, '[Lk3]') or StrEndsWith(SavedVirtUser.Name, '[Lk4]') then
            	Exit;
          Text:=StrToText(Str,P); //IP
          Icon:=StrToWord(Str, P);
          Channel:=StrToText(Str, P); //CompID
          SavedVirtUser.VirtName := UsersDatabase.GetVirtualUserName(SavedVirtUser.Name, SavedVirtUser.ServId);
          if SavedVirtUser.VirtName = '' then
          begin
          	// ����� ������������
            Pass := PCorePlugin^.AskPassword(SavedVirtUser.Name);
            // ������� ������ ��� ��������� �� ����������������
            if (Pass = '') then
            	SavedVirtUser.VirtName := SavedVirtUser.Name
            else
            begin
            	SavedVirtUser.VirtName := SavedVirtUser.Name + '['+SavedVirtUser.ServId+']';
              Pass := PCorePlugin^.AskPassword(SavedVirtUser.VirtName);
            end;
            SavedVirtUser.VirtName := SavedVirtUser.VirtName;
            UsersDatabase.Add(SavedVirtUser);
          end
          else
          	Pass := PCorePlugin^.AskPassword(SavedVirtUser.VirtName);
          if Pass='' then
          	for I := 0 to Random(5)+15 do
            	Pass:=Pass+RandomStr[Random(Length(RandomStr))+1];
          VirtUsers.AddTemp(SavedVirtUser.Name, 0, SavedVirtUser.VirtName);
          PCorePlugin^.JoinVirtualUser(SavedVirtUser.VirtName, Text, 1,Pass, Icon, Channel);
        end;
      LNK_CODE_LEFT:
        begin
          Name := StrToText(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          Count:=PCorePlugin^.AskUserChannels(Name, Channels);
          for I := 1 to Count do
            PCorePlugin^.LeaveChannel(Name, Channels[I].Name);
          PCorePlugin^.LeaveVirtualUser(Name);
        end;
      LNK_CODE_JOINCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.AddChannel(Name, Channel, 0, 0);
        end;
      LNK_CODE_LEFTCHAN:
        begin
          Name:=StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.LeaveChannel(Name, Channel);
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
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.AddChannel(Name,Channel,0,0);
          PCorePlugin^.AddMessageToChannel(Name, Channel, Icon, Text);
        end;
      LNK_CODE_CIMG:
        begin
          Name := StrToText(Str,P);
          I:=StrToWord(Str,P);
          if not I in [1..16] then
            Exit;
          Channel:=ChannelList[I].Name;
          Image:=StrToImg(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddChannel(Name,Channel,0,0);
          		PCorePlugin^.AddImageToChannel(Name, Channel, Image);
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
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.AddPrivateMessage(Name, Icon, Channel, Text);
        end;
      LNK_CODE_PRIVIMG:
        begin
          Name:=StrToText(Str,P);
          Channel:=StrToText(Str,P);
          Image:=StrToImg(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          if (Image <> nil) then
          begin
          	try
          		PCorePlugin^.AddPrivateImage(Name, Channel, Image);
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
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.AddPersonalMessage(Name, 0, Channel, Text);
        end;
      LNK_CODE_STATUSCHNG:
        begin
          Name:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          PCorePlugin^.AddState(Name, Text);
        end;

      LNK_CODE_BAN:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� ���� �������� �� ������� '+SERVER_REMOTE);
        end;

      LNK_CODE_OWNERSHIP_CHECK:
      	begin
          Name:=StrToText(Str,P);
          Pass:=StrToText(Str,P);
          if md5(Pass) = PCorePlugin^.AskPassword(Name) then
          begin
            UsersDatabase.Rename(VirtUsers.GetVirtualUserName(Name, 0), Name, server_id);
          end;
        end;

      LNK_CODE_CHANGE_PASS:
      	begin
          Name:=StrToText(Str,P);
          Pass:=StrToText(Str,P);
        	PCorePlugin^.AddPassword(BOT_NAME, VirtUsers.GetVirtualUserName(Name, 0), 0, Pass);
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
            Icon:=StrToWord(Str, P);  // ����� ������
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
          Icon:=StrToWord(Str, P);  // ����� ������
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

      //-------------------------------- ��������� ��������� -------------------
      LNK_CODE_SERVICE_SERVERNAME:
        begin
          SERVER_REMOTE:=StrToText(Str,P);
          PCorePlugin^.AddState(BOT_NAME, '��������� � ������� '+SERVER_REMOTE);
        end;

      LNK_CODE_SERVICE_UCSCHECK:
        begin
          Name := StrToText(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          if Name = '' then
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(1)+TextToStr('') // ��������� ��������� �� ������ ������
          else
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(0)+TextToStr(Name); // ��������� ��������� �� ������ ������
          Sock.SendText(DataToSend);
        end;

      LNK_CODE_SERVICE_UCSREPLY:
        begin
          Name:=StrToText(Str,P);
          Icon:=StrToWord(Str,P);  // ������
          Text:=StrToText(Str,P);
          case Icon of
            0: PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� ���������� � ������� '+SERVER_REMOTE+' ��� ������ [i]'+Text+'[/i].');
            1: PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���������� � ������� '+SERVER_REMOTE+'. ��������� �������: ����������� �� ������ � �������, ��� ��� �� ������������� ����������� ��� ������������ ����� ����� ���������.');
          end;
        end;

      LNK_CODE_SERVICE_MESSAGE:
      	begin
          Name:=StrToText(Str,P);
          Text:=StrToText(Str,P);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, Text);
      	end;

      LNK_CODE_SERVICE_BADNICK:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: ��������, ������� ������� ���.');
        end;

      LNK_CODE_SERVICE_BANNED:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: ��� �� ������ � �������.');
        end;

      LNK_CODE_SERVICE_NICKDENIED:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: � ����� ���� ���������� ����������� ������� ��� �����.');
        end;

      LNK_CODE_SERVICE_NICKLEXISTS:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: ������ ��� ���������������� �� �������.');
        end;

      LNK_CODE_SERVICE_MAXNICKLIMIT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: � ������ IP ���������������� ����������� ��������� ����� ������� �������.');
        end;

      LNK_CODE_SERVICE_MAXUSERLIMIT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. �������: ���������� ������������ ���������� ������������� ������.');
        end;

      LNK_CODE_SERVICE_CANNOTCONNECT:
        begin
          Name:=StrToText(Str,P);
          KickUser(Name);
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, '�� �� ���� ���������� � ������� '+SERVER_REMOTE+'. ������� �������� ��������������. ����������, ���������.');
        end;

      LNK_CODE_SERVICE_CONNECTION_OK:
        begin
          Name:=StrToText(Str,P);
          // ����������� � ������� ������� �������
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
          Icon:=StrToWord(Str,P); // ����� ������
          if not Icon in [1..16] then
            Exit;
          if ChannelList[Icon].Name='' then
            Exit;
          PCorePlugin^.AddRestriction(BOT_NAME, 2, 3, 0, KICK_TIME, Name, ChannelList[Icon].Name, '��� ��� �������� ������ � ������ � ����� � ����� ����������� �� ������� '+SERVER_REMOTE);
        end;

       LNK_CODE_SERVICE_TEMPCHANLIST:
        begin
        	for I := 1 to 16 do
          	ChannelsUpdated[I] := False;
        	Count := StrToWord(Str,P);  // ���������� �������
          for K := 1 to Count do
          begin
          	Icon := StrToWord(Str,P);  // ����� ������
          	Name := StrToText(Str,P);	 // ��� ������
          	if not Icon in [1..16] then
            	Exit;
          	if ChannelList[Icon].Permanent or (ChannelList[Icon].Owner<>'') then
            	Exit;
            // ��� ������
            if ChannelList[Icon].Name = Name then
            	Exit;
            // �������� ������� ������
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
          Icon:=StrToWord(Str,P);  // ����� ������
          Name:=StrToText(Str,P);	 // ��� ������
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
          Icon:=StrToWord(Str,P);  // ����� ������
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
    	if ListNames.Count>20 then
      	SendCount:= 20
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
  VirtualUser: TConnectedVirtualUser;
  Ini: TIniFile;
  StrList: TStringList;
  I: Integer;
begin
	VirtualUser := VirtUsers.GetUserInfo(Name);
  VirtUsers.DeleteTemp(Name);
  case Reason of
     0: // ��������� ������������ ����� ������������ �������������
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXUSERLIMIT)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     1: // ��� �� ������������� ����������� (�������� ��������� �������, ���� ��������� ������������ ����� � 40 ��������)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BADNICK)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     2: // ���
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BANNED)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     3: // ��� �������� ����������� ����� (�� �������� ������ ������ ����)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKDENIED)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     4: // ������ �� ��������� ��� ��� ����������������
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKLEXISTS)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     5: // ��������� ������������ ����� ������� ������� � ������� IP-������
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXNICKLIMIT)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
         Sock.SendText(DataToSend);
       end;
     7: // �������� ������
       begin
       	PCorePlugin^.WriteLog(file_log, '������ ����� ������������ '+VirtualUser.VirtName);
        DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
        Sock.SendText(DataToSend);
       end;
     6,8: // ������ �� ��������� ������� ����������; ������ �� ��������� �� ����������, ���� ���������
       begin
         if PCorePlugin^.AskRight(BOT_NAME, 2, '')=1 then
         begin
           PCorePlugin^.ActivateUser(BOT_NAME, Name);
           DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
           Sock.SendText(DataToSend);
         end
         else
         begin
           DataToSend:=WordToStr(LNK_CODE_SERVICE_CANNOTCONNECT)+TextToStr(VirtualUser.Name); // ��������� ��������� �� ������ ������
           Sock.SendText(DataToSend);
           Ini:=TIniFile.Create(file_config);
           StrList:=TStringList.Create;
           Ini.ReadSection('Admins', StrList);
           for I := 0 to StrList.Count - 1 do
             PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, UnCheckStr(StrList.Names[I]),  '� ���� �� ������� ����� ���������� ���������� ������� �������! �� ���� ������������ ������� ������ [i]'+VirtualUser.VirtName+'[/i]!');
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
  VirtualUser : TConnectedVirtualUser;
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
  Index:=SU.ListNames.IndexOf(User.Name);
  if Index<>-1 then
  begin
    User:=PCorePlugin^.AskUserInfo(SU.ListNames[Index], DataToSend);
    SU.ListNames.Delete(Index);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  VirtualUser := VirtUsers.GetUserInfo(Name);
  DataToSend:=WordToStr(LNK_CODE_PRIV)+TextToStr(User.Name)+TextToStr(VirtualUser.Name)+TextToStr(Text)+WordToStr(Regime);
  Sock.SendText(DataToSend);
end;

procedure onPrivateImg(Name: String; User: TUser; Image: TJpegImage);
var
  DataToSend: String;
  Index: LongInt;
  VirtualUser : TConnectedVirtualUser;
begin
  if Name=BOT_NAME then
  begin
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
  VirtualUser := VirtUsers.GetUserInfo(Name);
  DataToSend:=WordToStr(LNK_CODE_PRIVIMG)+TextToStr(User.Name)+TextToStr(VirtualUser.Name)+ImgToStr(Image);
  Sock.SendText(DataToSend);
end;

procedure onPersonalMsg(Name: String; User: TUser; Text: String);
var
  DataToSend, tempStr: String;
  Index: LongInt;
  VirtualUser: TConnectedVirtualUser;
begin
  if Name=BOT_NAME then
  begin
    if Text='status' then
    begin
      if not Connected or (SU.ListNames.IndexOf(Name)<>-1) then
      begin
        if not Connected then
          DataToSend:='�� �� ���������� � �������, �� ������ ���������� �������������, ����� �� ������ ��������.'
        else if (SU.ListNames.IndexOf(Name)<>-1) then
          DataToSend:='�� �� ���������� � ������� '+SERVER_REMOTE+', ������ ��� ��� �� ��������� ������������� ������ ������������� �� ��������. ��� ����� ����� ��������� ���������� �������������� ����������� � �������.';
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, DataToSend);
      end
      else
      begin
        DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSCHECK)+TextToStr(User.Name);
        Sock.SendText(DataToSend);
      end;
      Exit;
    end;
    if StrStartsWith(Text, '��� ������ ') then
    begin
      tempStr := Copy(Text, 12, MaxInt);
      if (Length(tempStr) > 0) and Connected then
      begin
      	DataToSend := WordToStr(LNK_CODE_OWNERSHIP_CHECK)+TextToStr(User.Name)+TextToStr(tempStr);
        Sock.SendText(DataToSend);
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, '���� ��� ������ ������ ������, �� ��� ��������� ����������� �� ������� �� ������ ������ ��� ����� ��� ���������.');
      end;
      Exit;
    end;
    if StrStartsWith(Text, '������� ������ ') then
    begin
      tempStr := Copy(Text, 16, MaxInt);
      if (Length(tempStr) > 0) and Connected then
      begin
      	DataToSend := WordToStr(LNK_CODE_CHANGE_PASS)+TextToStr(User.Name)+TextToStr(tempStr);
        Sock.SendText(DataToSend);
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, '������ ��� ������� ������ �� ������ ������� ��� �������.');
      end;
      Exit;
    end;
    if (Text='����������') and IsAdmin(User.Name) then
    begin
      PCorePlugin^.StopPlugin;
      Exit;
    end;
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
  VirtualUser := VirtUsers.GetUserInfo(Name);
  DataToSend:=WordToStr(LNK_CODE_PMSG)+TextToStr(User.Name)+TextToStr(VirtualUser.Name)+TextToStr(Text);
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
    DataToSend:=WordToStr(LNK_CODE_JOINCHAN)+TextToStr(User.Name)+WordToStr(Num);
    Sock.SendText(DataToSend);
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
  VirtualUser : TConnectedVirtualUser;
begin
  VirtualUser.VirtName := User.Name;
  VirtualUser.ServId := VirtUsers.GetServIdFromTemp(User.Name);
  if (VirtualUser.ServId >= 0) then
  begin
    VirtualUser.Name := UsersDatabase.GetUserInfo(VirtualUser.VirtName, server_id).Name;
    VirtUsers.DeleteTemp(User.Name);
    VirtUsers.Add(VirtualUser);
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
    DataToSend:=WordToStr(LNK_CODE_SERVICE_CONNECTION_OK)+TextToStr(VirtualUser.Name);
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
  else if not VirtUsers.IsOtherPluginVirtualUser(User.Name) then
  begin
  	if StrEndsWith(User.Name, '[Lk]') or StrEndsWith(User.Name, '[Lk2]')
    	or StrEndsWith(User.Name, '[Lk3]') or StrEndsWith(User.Name, '[Lk4]') then
      	Exit;
  	User:=PCorePlugin^.AskUserInfo(User.Name, DataToSend);
    DataToSend:=WordToStr(LNK_CODE_JOIN)+TextToStr(User.Name)+TextToStr(User.IP)+WordToStr(User.sex)+TextToStr(PCorePlugin^.AskID(User.Name));
    Sock.SendText(DataToSend);
  end;
  Channels:=nil;
end;

procedure onUserLeftChat(User: TUser);
var
  DataToSend: String;
  Index: LongInt;
begin
  if not (Assigned(VirtUsers) and VirtUsers.IsVirtualUser(User.Name)) then
  begin
    Index:=SU.ListNames.IndexOf(User.Name);
    if Index=-1 then
    begin
      DataToSend:=WordToStr(LNK_CODE_LEFT)+TextToStr(User.Name);
      Sock.SendText(DataToSend);
    end
    else
      SU.ListNames.Delete(Index);
  end
  else
  	VirtUsers.Delete(User.Name);
end;

procedure onUserStatusChanged(User: TUser; Text: String);
var
  DataToSend: String;
begin
  if not (Assigned(VirtUsers) and VirtUsers.IsVirtualUser(User.Name)) then
  begin
  	DataToSend:=WordToStr(LNK_CODE_STATUSCHNG)+TextToStr(User.Name)+TextToStr(Text);
    Sock.SendText(DataToSend);
  end;
end;

procedure onRestrictionAdd(Restriction: TRestriction);
var
  K: Word;
  DataToSend: String;
  Flag:Boolean;
  VirtualUser: TVirtualUser;
  ListVU: TAutoFreeVirtUsersList;
begin
  DataToSend:='';
  ListVU := UsersDatabase.GetUserInfos(Restriction.Name);
  if (ListVU.Count > 0) and (Restriction.ident>2) then
      if Restriction.banType>=2 then
      begin
      	VirtualUser := TVirtualUser(ListVU[0]^);
        K:=1;
        Flag:=False;
        while not Flag and (K<=16) do
        begin
          Flag:=(ChannelList[K].Name=Restriction.channel) and (ChannelList[K].Name<>'');
          Inc(K);
        end;
        if Flag then
        begin
          DataToSend:=TextToStr(VirtualUser.Name)+DoubleToStr(Restriction.Remain)+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(Restriction.moder)+TextToStr(Restriction.Reason);
        end;
      end;
  ListVU.Free;
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
  VirtualUser: TVirtualUser;
  ListVU: TAutoFreeVirtUsersList;
begin
  DataToSend:='';
  ListVU := UsersDatabase.GetUserInfos(Restriction.Name);
  if (ListVU.Count > 0) and (Restriction.ident>2) then
  	if Restriction.banType>=2 then
    begin
    	VirtualUser := TVirtualUser(ListVU[0]^);
    	K:=1;
      Flag:=False;
      while not Flag and (K<=16) do
      begin
      	Flag:=(ChannelList[K].Name=Restriction.channel) and (ChannelList[K].Name<>'');
        Inc(K);
      end;
      if Flag then
      begin
      	DataToSend:=TextToStr(VirtualUser.Name)+DWordToStr(Restriction.Ident)+DwordToStr(Restriction.banType)+WordToStr(K-1)+TextToStr(UnBanModerName)+TextToStr(Restriction.Reason);
      end;
    end;
  ListVU.Free;
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
  server_id := Ini.ReadString('Linker', 'ServId', 'Serv');
  if not Length(server_id) in [1..4] then
  	server_id := 'Serv';
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
  UsersDatabase := TUsersDatabase.Create;
  VirtUsers := TConnectedVirtualUsers.Create;
  Sock:=TLinkSocket.Create;
  {$IFNDEF Server}
  RC:=TReconnect.Create(Sock);
  {$ENDIF}
  SU:=TSendData.Create(Sock);
  Result:=1;
  AddKey:=0;
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
  VirtUsers.Free;
  UsersDatabase.Free;
  Connected:=False;
  LongDataTransfer.FlushBuffers;
end;

end.
