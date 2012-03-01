unit comm_info;

interface

uses Windows, Classes, Sysutils, comm_data, libqueue, JPEG, Graphics;

type

  TCommPluginC = class
    private
      dwPluginID      : DWORD;
      CommFortProcess : TtypeCommFortProcess;
      CommFortGetData : TtypeCommFortGetData;
      FonError        : TError;
      FonAuthFail     : TAuthFail;
      FonJoinChannelFail : TJoinChannelFail;
      FonPrivMsg      : TPrivMsg;
      FonPrivImg      : TPrivImg;
      FonPMsg         : TPMsg;
      FonJoinBot      : TJoinBot;
      FonUserJoinChannel: TUsrJoin;
      FonUserLeftChannel: TUsrLeft;
      FonPubMsg       : TPubMsg;
      FonPubImg       : TPubImg;
      FonChatUsrJoin  : TChatUsrJoin;
      FonChatUsrLeft  : TChatUsrLeft;
      FonRestAdded  	: TRestAdded;
      FonRestRemoved  : TRestRemoved;
      FonStatusChanged: TChnStt;

      function FixImage(channel: string; var img: TJpegImage): Boolean;
    public
      constructor Create(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
      destructor Destroy; override;

      procedure WriteLog(fileName: String; Text: String);

      procedure Process(dwMessageID : DWORD; bMessage : TBytes; dwMessageLength : DWORD);
      procedure JoinVirtualUser(Name, IP: String; PassType: DWord; Pass: String; Icon: DWord; CompID: String='');
      procedure LeaveVirtualUser(Name: String);
      procedure AddMessageToChannel(Name, channel : string; regime : DWord; text : string);
      procedure AddPrivateMessage(Name: String; regime : DWord; User : string; text : string);
      procedure AddPersonalMessage(Name: String; Importance: DWord; User, Text : string);
      procedure AddImageToChannel(Name, channel : string; image : TJpegImage); overload;
      procedure AddImageToChannel(Name, channel : string; filename : String); overload;
      procedure AddPrivateImage(Name, User : string; image : TJpegImage); overload;
      procedure AddPrivateImage(Name, User : string; filename : String); overload;
      procedure AddTheme(Name:String; channel : string; Theme : string);
      procedure AddGreeting(Name:String; channel : string; Text : string);
      procedure AddState(Name, newstate : string);
      procedure AddChannel(Name, channel : string; visibility, regime : DWord);
      procedure LeaveChannel(Name, channel : string);
      procedure AddRestriction(Name: String; restrictiontype, identificationtype, anonymitytype : DWord; time : Double; ident : string; Channel : string; reason : string);
      procedure RemoveRestriction(Name: String; restrictionid : DWORD; reason : string);
      procedure RemoveChannel(Name, channel : string);
      procedure ActivateUser(Name, User : string);
      procedure RemoveUser(Name, User : string);
      procedure AddPassword(Name, User : string; PassType: DWord; Password: String);
      procedure StopPlugin();

      function AskProgramType():DWord;
      function AskProgramVersion():String;
      function AskPluginTempPath():String;
      function AskUserChannels(Name: String; var ChannelList: TChannels):DWord;
      function AskUsersInChannel(Name, Channel: String; var UserList: TUsers):DWord;
      function AskUsersInChat(var UserList: TUsers):DWord;
      function AskRestrictions(var RestList: TRestrictions):DWord;
      function AskIPState(Name: String):DWord;
      function AskPassword(Name: String):String;
      function AskIP(Name: String):String;
      function AskID(Name: String):String;
      procedure AskMaxImageSize(channel: String; var ByteSize: DWord; var PixelSize: DWord);

      function AskUserInfo(Name: String; var Status: String):TUser;

      function AskRight(Name: String; RightType: DWord; Channel: String):DWord;

      property PluginID: DWORD read dwPluginID;
      property onError: TError read FonError write FonError;
      property onAuthFail: TAuthFail read FonAuthFail write FonAuthFail;
      property onJoinChannelFail: TJoinChannelFail read FonJoinChannelFail write FonJoinChannelFail;
      property onPrivateMessage: TPrivMsg read FonPrivMsg write FonPrivMsg;
      property onPrivateImage: TPrivImg read FonPrivImg write FonPrivImg;
      property onPersonalMessage: TPMsg read FonPMsg write FonPMsg;
      property onBotJoin: TJoinBot read FonJoinBot write FonJoinBot;
      property onChatUserJoin: TChatUsrJoin read FonChatUsrJoin write FonChatUsrJoin;
      property onChatUserLeft: TChatUsrLeft read FonChatUsrLeft write FonChatUsrLeft;
      property onPublicMessage: TPubMsg read FonPubMsg write FonPubMsg;
      property onPublicImage: TPubImg read FonPubImg write FonPubImg;
      property onUserJoinChannel: TUsrJoin read FonUserJoinChannel write FonUserJoinChannel;
      property onUserLeftChannel: TUsrLeft read FonUserLeftChannel write FonUserLeftChannel;
      property onUserStatusChanged: TChnStt read FonStatusChanged write FonStatusChanged;
      property onRestrictionAdded: TRestAdded read FonRestAdded write FonRestAdded;
      property onRestrictionRemoved: TRestRemoved read FonRestRemoved write FonRestRemoved;
    end;
  PCommPluginC = ^TCommPluginC;

  ICommPlugin = class
    public
      CorePlugin: TCommPluginC;
      constructor Create(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
      destructor Destroy; override;
  end;

  function GetJPEGFromFile(filename: string) : TJPEGImage;

var
  PCorePlugin: PCommPluginC;
  MsgQueue: TMsgQueue;

implementation

constructor ICommPlugin.Create(dwThisPluginID : DWORD;func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
begin
  CorePlugin := TCommPluginC.Create(dwThisPluginID, @func1, @func2);
end;

destructor ICommPlugin.Destroy;
begin
  CorePlugin.Free;
  inherited;
end;

constructor TCommPluginC.Create(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
begin
  dwPluginID := dwThisPluginID;
  CommFortProcess := func1;
  CommFortGetData := func2;
  MsgQueue:=TMsgQueue.Create(dwPluginID, CommFortProcess);
end;

destructor TCommPluginC.Destroy;
begin
  MsgQueue.Free;
  inherited;
end;

procedure TCommPluginC.WriteLog(fileName: String; Text: String);
var
  LogFile: TStringList;
begin
  LogFile:=TStringList.Create;
  LogFile.LoadFromFile(fileName, TEncoding.Unicode);
  LogFile.Add(DateTimeToStr(Now)+': '+Text);
  LogFile.SaveToFile(fileName, TEncoding.Unicode);
  LogFile.Free;
end;

procedure TCommPluginC.Process(dwMessageID : DWORD; bMessage : TBytes; dwMessageLength : DWORD);
var
    user: TUser;
    name, text, channel, theme: string;
    regime: integer;
    I, K: Cardinal;
    msg: TMemoryStream;
    img: TJpegImage;
    restriction: TRestriction;
begin
  case dwMessageID of
    PM_PLUGIN_AUTH_FAIL:
    if Assigned(FonAuthFail) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        FonAuthFail(Self, Name, K);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_JOINCHANNEL_FAIL:
    if Assigned(FonJoinChannelFail) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        Channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        FonJoinChannelFail(Self, name, channel, K);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;
    PM_PLUGIN_MSG_PRIV:
    if Assigned(FonPrivMsg) or Assigned(FonPrivImg) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex := K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        regime := K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        Text :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        if (K > 0) then
        begin
          msg := TMemoryStream.Create;
          img := TJPEGImage.Create;
          try
          	msg.WriteBuffer(bMessage[I+4], K);
          	msg.Seek(0, soFromBeginning);
          	img.LoadFromStream(msg);
        		if Assigned(FonPrivImg) then
        			FonPrivImg(Self, name, user, img);
          finally
            img.Free;
            msg.Free;
          end;
        end
        else
        	if Assigned(FonPrivMsg) then
        		FonPrivMsg(Self, name, user, regime, text);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_MSG_PM:
      if Assigned(FonPMsg) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex := K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        Text :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        FonPMsg(Self, name, user, text);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;
    PM_PLUGIN_JOIN_BOT:
      if Assigned(FonJoinBot) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        theme :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        text :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        FonJoinBot(Self, Name, channel, theme, text);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_MSG_PUB:
    if Assigned(FonPubMsg) or Assigned(FonPubImg) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        if Name<>BOT_NAME then Exit;

        CopyMemory(@K, @bMessage[I], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:= K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        Channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        regime := K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        Text :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        if (K > 0) then
        begin
          msg := TMemoryStream.Create;
          img := TJPEGImage.Create;
          try
           	msg.WriteBuffer(bMessage[I+4], K);
          	msg.Seek(0, soFromBeginning);
          	img.LoadFromStream(msg);
            if Assigned(FonPubImg) then
        			FonPubImg(Self, name, user, channel, img);
          finally
            img.Free;
          	msg.Free;
          end;
        end
        else
        	if Assigned(FonPubMsg) then
        		FonPubMsg(Self, name, user, channel, regime, text);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_USER_JOINEDCHANNEL:
    if Assigned(FonUserJoinChannel) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        if Name<>BOT_NAME then Exit;

        CopyMemory(@K, @bMessage[I], 4);
        Channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:= K;
        FonUserJoinChannel(Self, name, user, channel);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_USER_LEAVEDCHANNEL:
    if Assigned(FonUserLeftChannel) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        if Name<>BOT_NAME then Exit;

        CopyMemory(@K, @bMessage[I], 4);
        Channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, I+4,K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:= K;
        FonUserLeftChannel(Self, name, user, channel);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_USER_JOINED:
    if Assigned(FonChatUsrJoin) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:= K;
        FonChatUsrJoin(Self, user);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_USER_LEAVED:
    if Assigned(FonChatUsrLeft) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:=K;
        FonChatUsrLeft(Self, user);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_STATUS_CHANGED:
    if Assigned(FonStatusChanged) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        user.Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        user.sex:= K;
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        Text :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        FonStatusChanged(Self, user, Text);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

    PM_PLUGIN_RESTRICT_ADDED:
    if Assigned(FonRestAdded) then try
      begin
        CopyMemory(@restriction.restID, @bMessage[0], 4);
        I:=4;
        CopyMemory(@restriction.remain, @bMessage[I], 8);
        I:=I+8;
        CopyMemory(@restriction.ident, @bMessage[I], 4);
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.IPrange :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.compID :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@restriction.banType, @bMessage[I], 4);
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.moder :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.reason :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        //I:=I+K*2+4;
        restriction.date := Now;
        FonRestAdded(Self, restriction);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;


    PM_PLUGIN_RESTRICT_DELETED:
    if Assigned(FonRestRemoved) then try
      begin
        CopyMemory(@K, @bMessage[0], 4);
        Name :=TEncoding.Unicode.GetString(bMessage, 4, K*2);
        I:=K*2+4;
        CopyMemory(@restriction.restID, @bMessage[I], 4);
        I:=I+4;
        CopyMemory(@restriction.ident, @bMessage[I], 4);
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.Name :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.IP :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.IPrange :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.compID :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@restriction.banType, @bMessage[I], 4);
        I:=I+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.channel :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.moder :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        I:=I+K*2+4;
        CopyMemory(@K, @bMessage[I], 4);
        restriction.reason :=TEncoding.Unicode.GetString(bMessage, I+4, K*2);
        //I:=I+K*2+4;
        restriction.date := Now;
        FonRestRemoved(Self, restriction, Name);
      end except
      on e: exception do
        if Assigned(FOnError) then
          FOnError(Self, e);
      end;

  end;
end;

//--------------------------------------------- CommfortProcess-----------------

procedure TCommPluginC.JoinVirtualUser(Name, IP: String; PassType: DWord; Pass: String; Icon: DWord; CompID: String='');
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  len:=Length(IP);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(IP)^, len*2);
  msg.WriteBuffer(PassType, 4);
  len:=Length(Pass);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Pass)^, len*2);
  msg.WriteBuffer(Icon, 4);
	if CompId<>'' then
  begin
    len:=Length(CompID);
    msg.WriteBuffer(len, 4);
    msg.WriteBuffer(PChar(CompID)^, len*2);
  end;
  MsgQueue.InsertMsg(PM_PLUGIN_JOIN_VIRTUAL_USER, msg);
  msg.Free;
end;

procedure TCommPluginC.LeaveVirtualUser(Name: String);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_LEAVE_VIRTUAL_USER, msg);
  msg.Free;
end;

procedure TCommPluginC.AddMessageToChannel(Name, channel : string; regime : DWord; text : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	msg.WriteBuffer(regime, 4);
	len:=Length(Channel);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Channel)^, len*2);
	len:=Length(Text);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Text)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_SNDMSG_PUB, msg);
	msg.Free;
end;

procedure TCommPluginC.AddPrivateMessage(Name: String; regime : DWord; user : string; text : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	msg.WriteBuffer(regime, 4);
	len:=Length(User);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(User)^, len*2);
	len:=Length(Text);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Text)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_SNDMSG_PRIV, msg);
	msg.Free;
end;

procedure TCommPluginC.AddPersonalMessage(Name: String; Importance: DWord; User, Text : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	msg.WriteBuffer(Importance, 4);
	len:=Length(User);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(User)^, len*2);
	len:=Length(Text);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Text)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_SNDMSG_PM, msg);
	msg.Free;
end;

procedure TCommPluginC.AddImageToChannel(Name, channel : string; image : TJpegImage);
var
  msg: TMemoryStream;
  stream: TMemoryStream;
  len: Integer;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	len:=Length(Channel);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Channel)^, len*2);
	stream:=TMemoryStream.Create;
	image.SaveToStream(stream);
	len:=stream.Size;
	msg.WriteBuffer(len, 4);
	image.SaveToStream(msg);
	stream.Free;
	MsgQueue.InsertMsg(PM_PLUGIN_SNDIMG_PUB, msg);
  msg.Free;
end;

procedure TCommPluginC.AddImageToChannel(Name, channel : string; filename : String);
var
  img: TJpegImage;
begin
	img := nil;
	try
  	try
  		img := GetJPEGFromFile(filename);
      if FixImage(channel, img) then
  			AddImageToChannel(Name, channel, img);
  	except
    	on e: Exception do
      	onError(self, e, ' File: '+filename+Chr(13)+Chr(10));
  	end;
  finally
    img.Free;
  end;
end;

procedure TCommPluginC.AddPrivateImage(Name, User : string; image : TJpegImage);
var
  msg: TMemoryStream;
  stream: TMemoryStream;
  len: Integer;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	len:=Length(User);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(User)^, len*2);
	stream:=TMemoryStream.Create;
	image.SaveToStream(stream);
	len:=stream.Size;
	msg.WriteBuffer(len, 4);
	image.SaveToStream(msg);
	stream.Free;
	MsgQueue.InsertMsg(PM_PLUGIN_SNDIMG_PRIV, msg);
  msg.Free;
end;

procedure TCommPluginC.AddPrivateImage(Name, User : string; filename : String);
var
  img: TJpegImage;
begin
	img := nil;
	try
  	try
  		img := GetJPEGFromFile(filename);
      if FixImage('&priv', img) then
  			AddPrivateImage(Name, User, img);
  	except
    	on e: Exception do
      	onError(self, e, ' File: '+filename+Chr(13)+Chr(10));
  	end;
  finally
    img.Free;
  end;
end;

procedure TCommPluginC.AddTheme(Name:String; channel : string; Theme : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	len:=Length(Channel);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Channel)^, len*2);
	len:=Length(Theme);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Theme)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_THEME_CHANGE, msg);
	msg.Free;
end;

procedure TCommPluginC.AddGreeting(Name:String; channel : string; Text : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	len:=Length(Channel);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Channel)^, len*2);
	len:=Length(Text);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Text)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_GREETING_CHANGE, msg);
	msg.Free;
end;

procedure TCommPluginC.AddState(Name, newstate : String);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len * 2);
  len:=Length(newstate);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(newstate)^, len * 2);
  MsgQueue.InsertMsg(PM_PLUGIN_STATUS_CHANGE, msg);
  msg.Free;
end;

procedure TCommPluginC.AddChannel(Name, channel : string; visibility, regime : DWord);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len * 2);
  len:=Length(Channel);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Channel)^, len * 2);
  msg.WriteBuffer(visibility, 4);
  msg.WriteBuffer(regime, 4);
  MsgQueue.InsertMsg(PM_PLUGIN_CHANNEL_JOIN, msg);
  msg.Free;
end;

procedure TCommPluginC.LeaveChannel(Name, channel : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
	msg := TMemoryStream.Create;
	len:=Length(Name);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Name)^, len*2);
	len:=Length(Channel);
	msg.WriteBuffer(len, 4);
	msg.WriteBuffer(PChar(Channel)^, len*2);
	MsgQueue.InsertMsg(PM_PLUGIN_CHANNEL_LEAVE, msg);
	msg.Free;
end;

procedure TCommPluginC.AddRestriction(Name: String; restrictiontype, identificationtype, anonymitytype : DWord; time : Double; ident : string; Channel : string; reason : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  msg.WriteBuffer(identificationtype, 4);
  len:=Length(ident);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(ident)^, len*2);
  msg.WriteBuffer(restrictiontype, 4);
  len:=Length(Channel);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Channel)^, len*2);
  msg.WriteBuffer(time, 8);
  len:=Length(Reason);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Reason)^, len*2);
  msg.WriteBuffer(anonymitytype, 4);
  MsgQueue.InsertMsg(PM_PLUGIN_RESTRICT_SET, msg);
  msg.Free;
end;

procedure TCommPluginC.RemoveRestriction(Name: String; restrictionid : DWORD; reason : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  msg.WriteBuffer(restrictionid, 4);
  len:=Length(Reason);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Reason)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_RESTRICT_DEL, msg);
  msg.Free;
end;

procedure TCommPluginC.RemoveChannel(Name, channel : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  len:=Length(Channel);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Channel)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_CHANNEL_DEL, msg);
  msg.Free;
end;

procedure TCommPluginC.ActivateUser(Name, User : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  len:=Length(User);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(User)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_ACCOUNT_AGREE, msg);
  msg.Free;
end;

procedure TCommPluginC.RemoveUser(Name, User : string);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  len:=Length(User);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(User)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_ACCOUNT_DEL, msg);
  msg.Free;
end;

procedure TCommPluginC.AddPassword(Name, User : string; PassType: DWord; Password: String);
var
  msg: TMemoryStream;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  len:=Length(User);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(User)^, len*2);
  msg.WriteBuffer(PassType, 4);
  len:=Length(Password);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Password)^, len*2);
  MsgQueue.InsertMsg(PM_PLUGIN_PASSWORD_CHANGE, msg);
  msg.Free;
end;

procedure TCommPluginC.StopPlugin;
begin
  CommFortProcess(dwPluginID, PM_PLUGIN_STOP, '', 0);
end;

//--------------------------------------------- CommfortGetData-----------------

function TCommPluginC.AskProgramType():DWord;
var
  Buf: TBytes;
  iSize: DWord;
begin
  iSize:=CommFortGetData(dwPluginID, GD_PROGRAM_TYPE, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_PROGRAM_TYPE, Buf, iSize, nil, 0);
  CopyMemory(@Result, @Buf[0], 4);
end;

function TCommPluginC.AskProgramVersion():String;
var
  Buf: TBytes;
  iSize: DWord;
begin
  iSize := CommFortGetData(dwPluginID, GD_PROGRAM_VERSION, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_PROGRAM_VERSION, Buf, iSize, nil, 0);
  CopyMemory(@iSize, @Buf[0], 4);
  Result:=TEncoding.Unicode.GetString(Buf, 4, iSize*2);
end;

function TCommPluginC.AskPluginTempPath():String;
var
  Buf: TBytes;
  iSize: DWord;
begin
  iSize := CommFortGetData(dwPluginID, GD_PLUGIN_TEMPPATH, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_PLUGIN_TEMPPATH, Buf, iSize, nil, 0);
  CopyMemory(@iSize, @Buf[0], 4);
  Result:=TEncoding.Unicode.GetString(Buf, 4, iSize*2);
  TEMP_PATH := Result;
end;

function TCommPluginC.AskUserChannels(Name: String; var ChannelList: TChannels):DWord;
var
  Buf, msg: TBytes;
  iSize, len: DWord;
  I: DWord;
  J: Word;
begin
	if (Length(Name) = 0) then
  begin
  	Result := 0;
    Exit;
  end;
	len:=Length(Name)*2+4;
  SetLength(msg, len);
  len:=Length(Name);
  CopyMemory(@msg[0], @len, 4);
  CopyMemory(@msg[4], PChar(Name), len*2);
  i:=4+len*2;
  iSize := CommFortGetData(dwPluginID, GD_USERCHANNELS_GET, nil, 0, @msg[0], i);
  SetLength(Buf, iSize);
  if iSize=0 then
  begin
  	Result:=0;
    Exit;
  end;
  CommFortGetData(dwPluginID, GD_USERCHANNELS_GET, Buf, iSize, @msg[0], i);
  CopyMemory(@Result, @Buf[0], 4);
  I:=4;
  setLength(ChannelList, Result + 1);
  for J := 1 to Result do
  begin
    CopyMemory(@iSize, @Buf[I],4);
    ChannelList[J].Name:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@ChannelList[J].Users, @Buf[I],4);
    I:=I+4;
    CopyMemory(@iSize, @Buf[I],4);
    ChannelList[J].Theme:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
  end;
end;

function TCommPluginC.AskUsersInChannel(Name, Channel: String; var UserList: TUsers):DWord;
var
  Buf, msg: TBytes;
  iSize, len: DWord;
  I: DWord;
  J: Word;
begin
      len:=Length(Name)*2+Length(Channel)*2+8;
      SetLength(msg, len);
      len:=Length(Name);
      CopyMemory(@msg[0], @len, 4);
      CopyMemory(@msg[4], PChar(Name), len*2);
      i:=4+len*2;
      len:=Length(Channel);
      CopyMemory(@msg[i], @len, 4);
      CopyMemory(@msg[i+4], PChar(Channel), len*2);
      i:=i+len*2+4;
      iSize := CommFortGetData(dwPluginID, GD_CHANNELUSERS_GET, nil, 0, @msg[0], i);
      SetLength(Buf, iSize);
      if iSize=0 then
      begin
        Result:=0;
        Exit;
      end;
      CommFortGetData(dwPluginID, GD_CHANNELUSERS_GET, Buf, iSize, @msg[0], i);
  CopyMemory(@Result, @Buf[0], 4);
  I:=4;
  setLength(UserList, Result + 1);
  for J := 1 to Result do
  begin
    CopyMemory(@iSize, @Buf[I],4);
    UserList[J].Name:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I],4);
    UserList[J].IP:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@UserList[J].sex, @Buf[I],4);
    I:=I+4;
  end;
end;

function TCommPluginC.AskUsersInChat(var UserList: TUsers):DWord;
var
  Buf: TBytes;
  iSize: DWord;
  I: DWord;
  J: Word;
begin
  iSize := CommFortGetData(dwPluginID, GD_USERS_GET, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  if iSize=0 then
  begin
    Result:=0;
    Exit;
  end;
  CommFortGetData(dwPluginID, GD_USERS_GET, Buf, iSize, nil, 0);
  CopyMemory(@Result, @Buf[0], 4);
  I:=4;
  setLength(UserList, Result + 1);
  for J := 1 to Result do
  begin
    CopyMemory(@iSize, @Buf[I],4);
    UserList[J].Name:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I],4);
    UserList[J].IP:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@UserList[J].sex, @Buf[I],4);
    I:=I+4;
  end;
end;

function TCommPluginC.AskRestrictions(var RestList: TRestrictions):DWord;
var
  Buf: TBytes;
  iSize: DWord;
  I: DWord;
  J: Word;
begin
  iSize := CommFortGetData(dwPluginID, GD_RESTRICTIONS_GET, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_RESTRICTIONS_GET, Buf, iSize, nil, 0);
  Result:=Dword(Buf[0]);
  I:=4;
  setLength(RestList, Result + 1);
  for J := 1 to Result do
  begin
    CopyMemory(@RestList[J].restID, @Buf[I], 4);
    I:=I+4;
    CopyMemory(@RestList[J].Date, @Buf[I], 8);
    I:=I+8;
    CopyMemory(@RestList[J].Remain, @Buf[I], 8);
    I:=I+8;
    CopyMemory(@RestList[J].ident, @Buf[I], 4);
    I:=I+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].Name:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].IP:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].IPRange:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].compID:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@RestList[J].banType, @Buf[I], 4);
    I:=I+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].channel:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].moder:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
    CopyMemory(@iSize, @Buf[I], 4);
    RestList[J].reason:=TEncoding.Unicode.GetString(Buf, I+4, iSize*2);
    I:=I+iSize*2+4;
  end;
end;

function TCommPluginC.AskIPState(Name: String):DWord;
var
  Buf: TBytes;
  iSize: DWord;
begin
  iSize := CommFortGetData(dwPluginID, GD_RESTRICTIONS_GET, nil, 0, nil, 0);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_RESTRICTIONS_GET, Buf, iSize, nil, 0);
  CopyMemory(@Result, @Buf[0], 4);
end;

function TCommPluginC.AskPassword(Name: String):String;
var
  msg: TMemoryStream;
  Buf: TBytes;
  iSize: DWord;
begin
  msg := TMemoryStream.Create;
  iSize:= Length (Name);
  msg.WriteBuffer(iSize, 4);
  msg.WriteBuffer(PChar(Name)^, iSize*2);

  iSize := CommFortGetData(dwPluginID, GD_PASSWORD_GET, nil, 0, msg.Memory, msg.Size);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_PASSWORD_GET, Buf, iSize, msg.Memory, msg.Size);
  Result:=TEncoding.Unicode.GetString(Buf, 4, Dword(Buf[0])*2);
  msg.Free;
end;

function TCommPluginC.AskIP(Name: String):String;
var
  msg: TStringStream;
  Buf: TBytes;
  iSize: DWord;
begin
  msg := TStringStream.Create('', TEncoding.Unicode);
  msg.Position := 0;
  iSize:= Length (Name);
  msg.Write(iSize, 4);
  msg.WriteString(Name);
  iSize := CommFortGetData(dwPluginID, GD_IP_GET, nil, 0, PChar(msg.DataString), 4+Length(Name)*2);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_IP_GET, Buf, iSize, PChar(msg.DataString), 4+Length(Name)*2);
  Result:=TEncoding.Unicode.GetString(Buf, 4, Dword(Buf[0])*2);
  msg.Free;
end;

function TCommPluginC.AskID(Name: String):String;
var
  msg: TStringStream;
  Buf: TBytes;
  iSize: DWord;
begin
  msg := TStringStream.Create('', TEncoding.Unicode);
  msg.Position := 0;
  iSize:= Length (Name);
  msg.Write(iSize, 4);
  msg.WriteString(Name);
  iSize := CommFortGetData(dwPluginID, GD_ID_GET, nil, 0, PChar(msg.DataString), 4+Length(Name)*2);
  SetLength(Buf, iSize);
  CommFortGetData(dwPluginID, GD_ID_GET, Buf, iSize, PChar(msg.DataString), 4+Length(Name)*2);
  Result:=TEncoding.Unicode.GetString(Buf, 4, Dword(Buf[0])*2);
  msg.Free;
end;

function TCommPluginC.AskUserInfo(Name: string; var Status: String): TUser;
var
  msg: TStringStream;
  Buf: TBytes;
  iSize: DWord;
begin
  msg := TStringStream.Create('', TEncoding.Unicode);
  msg.Position := 0;
  iSize:= Length (Name);
  msg.Write(iSize, 4);
  msg.WriteString(Name);
  iSize := CommFortGetData(dwPluginID, GD_USERINFO_GET, nil, 0, PChar(msg.DataString), 4+Length(Name)*2);
  Result.IP:='UNCONNECTED';
  Result.Name:=Name;
  if iSize>0 then
  begin
    SetLength(Buf, iSize);
    CommFortGetData(dwPluginID, GD_USERINFO_GET, Buf, iSize, PChar(msg.DataString), 4+Length(Name)*2);
    iSize:=4+Dword(Buf[0])*2;
    Result.IP:=TEncoding.Unicode.GetString(Buf, iSize+4, Dword(Buf[iSize])*2);
    iSize:=iSize+4+Dword(Buf[iSize])*2;
    iSize:=iSize+4+Dword(Buf[iSize])*2;
    CopyMemory(@Result.sex, @Buf[iSize],4);
    iSize:=iSize+4;
    Status:=TEncoding.Unicode.GetString(Buf, iSize+4, Dword(Buf[iSize])*2);
  end;
  msg.Free;
end;

function TCommPluginC.AskRight(Name: String; RightType: DWord; Channel: String):DWord;
var
  msg: TMemoryStream;
  Buf: TBytes;
  len: DWord;
begin
  msg := TMemoryStream.Create;
  len:=Length(Name);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Name)^, len*2);
  msg.WriteBuffer(RightType, 4);
  len:=Length(Channel);
  msg.WriteBuffer(len, 4);
  msg.WriteBuffer(PChar(Channel)^, len*2);
  len := CommFortGetData(dwPluginID, GD_RIGHT_GET, nil, 0, msg.Memory, msg.Size);
  SetLength(Buf, len);
  CommFortGetData(dwPluginID, GD_RIGHT_GET, Buf, len, msg.Memory, msg.Size);
  CopyMemory(@Result, @Buf[0], 4);
  msg.Free;
end;

procedure TCommPluginC.AskMaxImageSize(Channel: String; var ByteSize: DWord; var PixelSize: DWord);
var
	outmsg: TMemoryStream;
	inmsg: TMemoryStream;
	iSize: DWord;
begin
	outmsg := TMemoryStream.Create;
	iSize := Length(channel);
	outmsg.WriteBuffer(iSize, 4);
	outmsg.WriteBuffer(PChar(Channel)^, iSize * 2);
	outmsg.Seek(0, soBeginning);
	iSize := CommFortGetData(dwPluginID, GD_MAXIMAGESIZE, nil, 0, outmsg.Memory, outmsg.Size);
	inmsg := TMemoryStream.Create;
	inmsg.SetSize(iSize);
	iSize := CommFortGetData(dwPluginID, GD_MAXIMAGESIZE, inmsg.Memory, inmsg.Size, outmsg.Memory, outmsg.Size);
	inmsg.ReadBuffer(ByteSize, 4);
	inmsg.ReadBuffer(PixelSize, 4);
	outmsg.Free;
	inmsg.Free;
end;

// -------------------------------- Other --------------------------------------
function TCommPluginC.FixImage(channel: string; var img: TJpegImage): Boolean;
var
	bmp: TBitmap;
  byteSize, pixelSize: DWord;
  aspect: Double;
begin
	bmp := TBitmap.Create;
	try
  	Result := False;
		// ѕроверка соответстви€ требовани€м канала
		AskMaxImageSize(channel, byteSize, pixelSize);
    if (byteSize > 0) then
    begin
    	if (img.Width * img.Height > Int(pixelSize)) then
      begin
      	aspect := img.Width / img.Height;
        bmp.SetSize(Trunc(Sqrt(pixelSize * aspect)), Trunc(Sqrt(pixelSize / aspect)));
        bmp.Canvas.StretchDraw(bmp.Canvas.Cliprect, img);
        img.Assign(bmp);
				bmp.Dormant;
				bmp.FreeImage;
      end;
    end;
    Result := True;
  finally
    bmp.Free;
  end;
end;

function GetJPEGFromFile(filename: string) : TJPEGImage;
var
	bmp: TBitmap;
begin
	Result := TJPEGImage.Create;
	bmp := TBitmap.Create;
	try
    if (Length(filename) > 4) and (Copy(filename, Length(filename) - 3, 4) = '.bmp') then
    begin
      bmp.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Plugins\'+PLUGIN_FILENAME+'\img\' + filename);
      Result.Assign(bmp);
    end
    else
      Result.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Plugins\'+PLUGIN_FILENAME+'\img\' + filename);
  finally
    bmp.Free;
  end;
end;

end.
