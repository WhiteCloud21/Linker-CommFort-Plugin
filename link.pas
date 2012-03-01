{$I plugin.inc}
unit link;

interface

uses
  IniFiles,
  Windows, WinInet, SysUtils, Classes, SyncObjs,
  Math, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  comm_info, comm_data, libfunc, libqueue, libmd5, libClasses, linkData,
  LinkSocket,
  libVirtualUsers, ScktComp, LongDataTransfer, JPEG, LbRSA;

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

  Connected: Boolean;

implementation

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
      PCorePlugin^.WriteLog(file_log, 'Соединение не установлено. Версия протокола: '+IntToStr(PROTOCOL_VER)+'. Версия протокола клиента:'+IntToStr(Ident));
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
      PCorePlugin^.WriteLog(file_log, 'Соединение не установлено. Версия протокола: '+IntToStr(PROTOCOL_VER)+'. Версия протокола сервера:'+IntToStr(Ident));
      Sock.ConnS.Close;
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
					if StrEndsWith(SavedVirtUser.Name, '[Lk]') or StrEndsWith(SavedVirtUser.Name, '[Lk2]')
    				or StrEndsWith(SavedVirtUser.Name, '[Lk3]') or StrEndsWith(SavedVirtUser.Name, '[Lk4]') then
      				Exit;
          SavedVirtUser.ServId := server_id;
          Text:=StrToText(Str,P); //IP
          Icon:=StrToWord(Str, P);
          Channel:=StrToText(Str, P); //CompID
          SavedVirtUser.VirtName := UsersDatabase.GetVirtualUserName(SavedVirtUser.Name, SavedVirtUser.ServId);
          if SavedVirtUser.VirtName = '' then
          begin
          	// Новый пользователь
            Pass := PCorePlugin^.AskPassword(SavedVirtUser.Name);
            // Учетная запись без постфикса не зарегистрирована
            if (Pass = '') then
            	SavedVirtUser.VirtName := SavedVirtUser.Name
            else
            begin
            	SavedVirtUser.VirtName := SavedVirtUser.Name + '['+SavedVirtUser.ServId+']';
              Pass := PCorePlugin^.AskPassword(SavedVirtUser.VirtName);
            end;
            UsersDatabase.Add(SavedVirtUser);
          end
          else
          	Pass := PCorePlugin^.AskPassword(SavedVirtUser.VirtName);
          Ident := 1;
          if Pass='' then
          begin
          	for I := 0 to Random(5)+15 do
            	Pass:=Pass+RandomStr[Random(Length(RandomStr))+1];
            Ident := 0;
          end;
          try
          	VirtUsers.AddTemp(SavedVirtUser.Name, 0, SavedVirtUser.VirtName);
            if (vu_ip <> '') then
          		PCorePlugin^.JoinVirtualUser(SavedVirtUser.VirtName, vu_ip, Ident,Pass, Icon, Channel)
            else
          		PCorePlugin^.JoinVirtualUser(SavedVirtUser.VirtName, Text, Ident,Pass, Icon, Channel);
          except
            // Уже в списке входа, но не вошел
          end;
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
          PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, Name, 'Вы были забанены на сервере '+SERVER_REMOTE);
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
          Name := StrToText(Str,P);
          Name := VirtUsers.GetVirtualUserName(Name, 0);
          if Name = '' then
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(1)+TextToStr('') // сервисное сообщение на другой сервер
          else
            DataToSend:=WordToStr(LNK_CODE_SERVICE_UCSREPLY)+TextToStr(Name)+WordToStr(0)+TextToStr(Name); // сервисное сообщение на другой сервер
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
          // Подключение к другому серверу успешно
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
     0: // превышено максимальное число подключенных пользователей
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXUSERLIMIT)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     1: // имя не соответствует требованиям (содержит служебные символы, либо превышает максимальную длину в 40 символов)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BADNICK)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     2: // бан
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_BANNED)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     3: // имя содержит запрещенные слова (не проходит фильтр плохих слов)
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKDENIED)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     4: // схожее по написанию имя уже зарегистрировано
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_NICKLEXISTS)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     5: // превышено максимальное число учетных записей с данного IP-адреса
       begin
         DataToSend:=WordToStr(LNK_CODE_SERVICE_MAXNICKLIMIT)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
         Sock.SendText(DataToSend);
       end;
     7: // неверный пароль
       begin
       	PCorePlugin^.WriteLog(file_log, 'Ошибка входа пользователя '+VirtualUser.VirtName);
        DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
        Sock.SendText(DataToSend);
       end;
     6, 8: //заявка на активацию не обработана, либо отклонена
       begin
         if PCorePlugin^.AskRight(BOT_NAME, 2, '')=1 then
         begin
           PCorePlugin^.ActivateUser(BOT_NAME, Name);
           DataToSend:=WordToStr(LNK_CODE_SERVICE_RESENDMEUSER)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
         end
         else
         begin
           DataToSend:=WordToStr(LNK_CODE_SERVICE_CANNOTCONNECT)+TextToStr(VirtualUser.Name); // сервисное сообщение на другой сервер
           Sock.SendText(DataToSend);
           Ini:=TIniFile.Create(file_config);
           StrList:=TStringList.Create;
           Ini.ReadSection('Admins', StrList);
           for I := 0 to StrList.Count - 1 do
             PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, UnCheckStr(StrList.Names[I]),  'У меня не хватает права управления активацией учетных записей! Не могу активировать учетную запись [i]'+VirtualUser.VirtName+'[/i]!');
           StrList.Free;
           Ini.Free;
         end;
       end;
  end;
end;

procedure onMsg(User: TUser; Channel, Text: String; Regime: Integer);
var
  DataToSend: String;
  I: LongInt;
  Num: Word;
begin
  Sock.ForceUserSend(User.Name);
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
  I: LongInt;
  Num: Word;
begin
  Sock.ForceUserSend(User.Name);
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
  Sock.ForceUserSend(User.Name);
  VirtualUser := VirtUsers.GetUserInfo(Name);
  DataToSend:=WordToStr(LNK_CODE_PRIV)+TextToStr(User.Name)+TextToStr(VirtualUser.Name)+TextToStr(Text)+WordToStr(Regime);
  Sock.SendText(DataToSend);
end;

procedure onPrivateImg(Name: String; User: TUser; Image: TJpegImage);
var
  DataToSend: String;
  VirtualUser : TConnectedVirtualUser;
begin
  if Name=BOT_NAME then
  begin
    Exit;
  end;
  Sock.ForceUserSend(User.Name);
  VirtualUser := VirtUsers.GetUserInfo(Name);
  DataToSend:=WordToStr(LNK_CODE_PRIVIMG)+TextToStr(User.Name)+TextToStr(VirtualUser.Name)+ImgToStr(Image);
  Sock.SendText(DataToSend);
end;

procedure onPersonalMsg(Name: String; User: TUser; Text: String);
var
  DataToSend, tempStr: String;
  VirtualUser: TConnectedVirtualUser;
begin
  if Name=BOT_NAME then
  begin
    if Text='status' then
    begin
      if not Connected or Sock.IsUserInSendList(Name) then
      begin
        if not Connected then
          DataToSend:='Вы не подключены к серверу, но будете подключены автоматически, когда он станет доступен.'
        else if Sock.IsUserInSendList(Name) then
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
    if StrStartsWith(Text, 'мой пароль ') then
    begin
      tempStr := Copy(Text, 12, MaxInt);
      if (Length(tempStr) > 0) and Connected then
      begin
      	DataToSend := WordToStr(LNK_CODE_OWNERSHIP_CHECK)+TextToStr(User.Name)+TextToStr(tempStr);
        Sock.SendText(DataToSend);
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Если был указан верный пароль, то при следующем подключении вы войдете на другой сервер под ником без постфикса.');
      end;
      Exit;
    end;
    if StrStartsWith(Text, 'сменить пароль ') then
    begin
      tempStr := Copy(Text, 16, MaxInt);
      if (Length(tempStr) > 0) and Connected then
      begin
      	DataToSend := WordToStr(LNK_CODE_CHANGE_PASS)+TextToStr(User.Name)+TextToStr(tempStr);
        Sock.SendText(DataToSend);
        PCorePlugin^.AddPersonalMessage(BOT_NAME, 0, User.Name, 'Пароль для учетной записи на другом сервере был изменен.');
      end;
      Exit;
    end;
    if (Text='остановись') and IsAdmin(User.Name) then
    begin
      PCorePlugin^.StopPlugin;
      Exit;
    end;
    Exit;
  end;
  Sock.ForceUserSend(User.Name);
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
begin
  if not (Assigned(VirtUsers) and VirtUsers.IsVirtualUser(User.Name)) then
  begin
  	Sock.ForceUserRemove(User.Name);
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
  vu_ip := Ini.ReadString('Linker', 'VUIP', '');
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
  Sock:=TLinkSocket.Create(ProcessData);
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
  Sock.Free;
  VirtUsers.Free;
  UsersDatabase.Free;
  Connected:=False;
  LongDataTransfer.FlushBuffers;
end;

end.
