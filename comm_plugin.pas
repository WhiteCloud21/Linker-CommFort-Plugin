unit comm_plugin;

interface

uses Windows,
      comm_info,
      comm_data,
      IniFiles, WinInet,
      SysUtils, Classes, Math, ShellAPI, JPEG,
      linkData, link,
      libfunc;
  
type
  TCommPlugin = class(ICommPlugin)
  private
  
  public
    constructor Create(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
    destructor Destroy; override;
    procedure Error(Sender: TObject; e: Exception; Extratext: String='');
    procedure PublicMessage(Sender: TObject; Name: string; User : TUser; channel: string; regime : integer; bMessage : string);
    procedure PublicImage(Sender: TObject; Name: string; User : TUser; channel: string; image: TJpegImage);
    procedure BotAuthFail(Sender: TObject; Name: string; Reason: Word);
    procedure BotJoin(Sender: TObject; Name: string; channel : string; theme : string; greeting: string);
    procedure PrivateMessage(Sender: TObject; Name: string; User : TUser; regime : integer; bMessage : string);
    procedure PrivateImage(Sender: TObject; Name: string; User : TUser; image: TJpegImage);
    procedure PersonalMessage(Sender: TObject; Name: string; User : TUser; bMessage : string);
    procedure UserJoinChannel(Sender: TObject; Name: string; User : TUser; Channel : string);
    procedure UserLeftChannel(Sender: TObject; Name: string; User : TUser; Channel : string);
    procedure UserJoinChat(Sender: TObject; User : TUser);
    procedure UserLeftChat(Sender: TObject; User : TUser);
    procedure UserStatusChanged(Sender: TObject; User : TUser; Text: String);
    procedure RestrictionAdded(Sender: TObject; Restriction : TRestriction);
    procedure RestrictionRemoved(Sender: TObject; Restriction : TRestriction; UnbanModer: String);
  end;

var
  ThisPlugin: TCommPlugin;
  Loaded: Boolean;
  
implementation

constructor TCommPlugin.Create(dwThisPluginID : DWORD; func1 : TtypeCommFortProcess; func2: TtypeCommFortGetData);
var
  Ini: TIniFile;
  StrList: TStringList;
begin
  inherited;
  try
    Randomize;
    config_dir:=CorePlugin.AskPluginTempPath+PLUGIN_FILENAME;
    if not DirectoryExists(config_dir) then
      CreateDir(config_dir);
		
    StrList:=TStringList.Create;
    StrList.Add(';Файл автоматически создан '+DateTimeToStr(Now));
		
    file_config := GetConfigFullName('config.ini');
		
    file_users:=config_dir+'\users.ini';
    if not FileExists(file_users) then
      StrList.SaveToFile(file_users, TEncoding.Unicode);
  
	  file_log:=config_dir+'\error.log';
    if not FileExists(file_log) then
      StrList.SaveToFile(file_log, TEncoding.Unicode);

	  file_debug:=config_dir+'\debug.log';
    if not FileExists(file_debug) then
      StrList.SaveToFile(file_debug, TEncoding.Unicode);

    PCorePlugin:= @CorePlugin;

    // Указываем метод, который будет вызываться при ошибке
    CorePlugin.onError                      := Error;

    CorePlugin.onAuthFail                   := BotAuthFail;

    // Указываем метод, который будет вызываться при сообщении в канале
    CorePlugin.onPublicMessage              := PublicMessage;
    CorePlugin.onPublicImage              	:= PublicImage;

    // Указываем метод, который будет вызываться при подключении бота
    CorePlugin.onBotJoin                    := BotJoin;

    // Указываем метод, который будет вызываться при получении приватного сообщения
    CorePlugin.onPrivateMessage             := PrivateMessage;
    CorePlugin.onPrivateImage              	:= PrivateImage;

    // Указываем метод, который будет вызываться при получении персонального сообщения
    CorePlugin.onPersonalMessage            := PersonalMessage;


    // Указываем метод, который будет вызываться при подключении пользователя к каналу
    CorePlugin.onUserJoinChannel            := UserJoinChannel;
    CorePlugin.onUserLeftChannel            := UserLeftChannel;

    CorePlugin.onChatUserJoin               := UserJoinChat;
    CorePlugin.onChatUserLeft               := UserLeftChat;

    CorePlugin.onUserStatusChanged          := UserStatusChanged;

    CorePlugin.onRestrictionAdded						:= RestrictionAdded;
    CorePlugin.onRestrictionRemoved					:= RestrictionRemoved;


    // Создаем виртуального пользователя
    Loaded:=False;
    if not FileExists(file_config) then
    begin
      MessageBox(0, 'Отсутствуют один или несколько файлов, необходимых для работы плагина. При нажатии ОК откроется директория, в которую необходимо скопировать файлы настроек.', 'Ошибка при запуске плагина "Linker"', MB_ICONEXCLAMATION);
      ShellExecute(0, 'open', PChar('"'+config_dir+'"'), nil, nil, SW_SHOWNORMAL);
      CorePlugin.StopPlugin;
    end
    else
      begin
        Ini := TIniFile.Create(file_config);
        link.LoadSettings(Ini);
        BOT_NAME:= Ini.ReadString('Linker', 'BotName', 'Linker');
        BOT_PASS:= Ini.ReadString('Linker', 'BotPass', '');
        BOT_IP := Ini.ReadString('Linker', 'BotIP', 'N/A');
        if Ini.ReadInteger('Linker', 'BotIsFemale', 0)=1 then
          BOT_ISFEMALE:=1
        else
          BOT_ISFEMALE:=0;
        Ini.Free;
        if (BOT_PASS='') then
        begin
          MessageBox(0, PChar('Смените пароль учётной записи плагина в настройках! (Параметр BotPass в файле "' + file_config + '")'), 'Ошибка при запуске плагина "Линковщик"', MB_ICONEXCLAMATION);
          CorePlugin.StopPlugin;
        end
        else
          CorePlugin.JoinVirtualUser(BOT_NAME, BOT_IP, 0, BOT_PASS, BOT_ISFEMALE);
      end;
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, '----onInit Exception----'+Chr(13)+Chr(10));
  end;
end;

destructor TCommPlugin.Destroy;
begin
  //данная функция вызывается при завершении работы программы
  if Loaded then
  begin
    Link.Destroy();
    Loaded := false;
  end;
  inherited;
end;

// Метод, вызываемый при ошибке
procedure TCommPlugin.Error(Sender: TObject; e: Exception; Extratext: String='');
var
  Msg, Stack: String;
  Inner: Exception;
begin
  Inner := E;
  Msg := '';
  while Inner <> nil do
  begin
    if Msg <> '' then
      Msg := Msg + sLineBreak;
    Msg := Msg + Inner.Message;
    if (Msg <> '') and (Msg[Length(Msg)] > '.') then
      Msg := Msg + '.';

    Stack := Inner.StackTrace;
    if Stack <> '' then
    begin
      if Msg <> '' then
        Msg := Msg + sLineBreak + sLineBreak;
      Msg := Msg + Stack + sLineBreak;
    end;

    Inner := Inner.InnerException;
  end;
  CorePlugin.WriteLog(file_log, Msg + Extratext + sLineBreak);
end;

// Метод, вызываемый при невозможности подключения виртуального пользователя
{
  Name - имя виртуального пользователя
  Reason - причина
}
procedure TCommPlugin.BotAuthFail(Sender: TObject; Name: string; Reason: Word);
var
  Str: String;
begin
  if Name=BOT_NAME then exit;
  try
    Str:='----onAuthFail Exception----'+Chr(13)+Chr(10);
    Link.onAuthFail(Name, Reason);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

// Метод, вызываемый при подключении виртуального пользователя к каналу
{
  Name - имя виртуального пользователя
  channel - канал
  theme - тема канала
  greeting - приветствие канала
}
procedure TCommPlugin.BotJoin(Sender: TObject; Name: String; channel : string; theme : string; greeting: string);
var
  Str: String;
begin
	try
  	Str:='----onBotJoin Exception----'+Chr(13)+Chr(10);
		if not Loaded then begin
    	Link.Init();
    	Loaded:=True;
  	end;
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

// Метод, вызываемый при сообщении в канал
{
  Name - имя виртуального пользователя
  User - пользователь(Name, IP, sex)
  channel - канал
  regime - режим сообщения[0-обычно, 1-как состояние]
  bMessage - текст сообщения
}
procedure TCommPlugin.PublicMessage(Sender: TObject; Name: String; User : TUser; channel: string; regime : integer; bMessage : string);
var
  Str:String;
  I: Integer;
begin
  if not Loaded then Exit;
  if User.Name=BOT_NAME then exit;
  try
  	if VirtUsers.IsVirtualUser(User.Name) then exit;
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
      	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        	Exit;
    Str:='----onMsg Exception----'+Chr(13)+Chr(10);
    Link.OnMsg(User, Channel, bMessage, regime);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

// Метод, вызываемый при изображении в канал
{
  Name - имя виртуального пользователя
  User - пользователь(Name, IP, sex)
  channel - канал
  image - изображение
}
procedure TCommPlugin.PublicImage(Sender: TObject; Name: string; User : TUser; channel: string; image: TJpegImage);
var
  Str:String;
  I: Integer;
begin
  if not Loaded then Exit;
  if User.Name=BOT_NAME then exit;
  try
  	if VirtUsers.IsVirtualUser(User.Name) then exit;
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
      	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        	Exit;
    Str:='----onImage Exception----'+Chr(13)+Chr(10);
    Link.OnImg(User, Channel, image);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

// Метод, вызываемый при получении приватного сообщения
{
  Name - имя виртуального пользователя, который присутствует в данном канале
  User - пользователь(Name, IP, sex)
  regime - режим сообщения[0-обычно, 1-как состояние]
  bMessage - текст сообщения
}
procedure TCommPlugin.PrivateMessage(Sender: TObject; Name: String; User : TUser; regime : integer; bMessage : string);
var
  Str: String;
begin
  if not Loaded then Exit;
  if User.Name=BOT_NAME then exit;
  if VirtUsers.IsVirtualUser(User.Name) then exit;
  try
    Str:='----onPrivate Exception----'+Chr(13)+Chr(10);
    Link.onPrivate(Name, User, bMessage, regime);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

// Метод, вызываемый при получении изображения в приват
{
  Name - имя виртуального пользователя, который присутствует в данном канале
  User - пользователь(Name, IP, sex)
  image - изображение
}
procedure TCommPlugin.PrivateImage(Sender: TObject; Name: string; User : TUser; image: TJpegImage);
var
  Str: String;
begin
  if not Loaded then Exit;
  if User.Name=BOT_NAME then exit;
  if VirtUsers.IsVirtualUser(User.Name) then exit;
  try
    Str:='----onPrivateImage Exception----'+Chr(13)+Chr(10);
    Link.onPrivateImg(Name, User, image);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.PersonalMessage(Sender: TObject; Name: string; User : TUser; bMessage : string);
var
  Str: String;
begin
  if not Loaded then Exit;
  if VirtUsers.IsVirtualUser(User.Name) then exit;
  try
    Str:='----onPrivate Exception----'+Chr(13)+Chr(10);
    Link.onPersonalMsg(Name, User, bMessage);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.UserJoinChannel(Sender: TObject; Name: string; User : TUser; Channel : string);
var
  Str: String;
  I: Integer;
begin
  if not Loaded then Exit;
  try
  	if VirtUsers.IsVirtualUser(User.Name) then exit;
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
      	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        	Exit;
    Str:='----onUserJoinChannel Exception----'+Chr(13)+Chr(10);
    Link.onUserJoinChannel(User, Channel);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.UserLeftChannel(Sender: TObject; Name: string; User : TUser; Channel : string);
var
  Str: String;
  I: Integer;
begin
  if not Loaded then Exit;
  try
  	if VirtUsers.IsVirtualUser(User.Name) then exit;
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
     	 	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
       	 	Exit;
    Str:='----onUserLeftChannel Exception----'+Chr(13)+Chr(10);
    Link.onUserLeftChannel(User, Channel);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.UserJoinChat(Sender: TObject; User : TUser);
var
  Str: String;
  I: Integer;
begin
  //if not Loaded then Exit;
  try
    Str:='----onUserJoinChat Exception----'+Chr(13)+Chr(10);
  	if not Loaded then begin
    	Link.Init();
    	Loaded:=True;
  	end;
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
      	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        	Exit;
    Link.onUserJoinChat(User);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.UserLeftChat(Sender: TObject; User : TUser);
var
  Str: String;
  I: Integer;
begin
	if not Loaded then Exit;
  try
    Str:='----onUserLeftChat Exception----'+Chr(13)+Chr(10);
  	if Assigned(IgnorePrefixList) then
    	for I := 0 to IgnorePrefixList.Count - 1 do
      	if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        	Exit;
    Link.onUserLeftChat(User);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.UserStatusChanged(Sender: TObject; User : TUser; Text: String);
var
  Str: String;
  I: Integer;
begin
  if not Loaded then Exit;
  if Assigned(IgnorePrefixList) then
    for I := 0 to IgnorePrefixList.Count - 1 do
      if Copy(User.Name, 1, Length(IgnorePrefixList.Strings[I]))=IgnorePrefixList.Strings[I] then
        Exit;
  try
    Str:='----onUserStatusChanged Exception----'+Chr(13)+Chr(10);
    Link.onUserStatusChanged(User, Text);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.RestrictionAdded(Sender: TObject; Restriction : TRestriction);
var
	Str: String;
begin
  if not Loaded then Exit;
  if Restriction.Moder = BOT_NAME then Exit;
  try
    Str:='----onRestrictionAdded Exception----'+Chr(13)+Chr(10);
    Link.onRestrictionAdd(Restriction);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

procedure TCommPlugin.RestrictionRemoved(Sender: TObject; Restriction : TRestriction; UnbanModer: String);
var
	Str: String;
begin
  if not Loaded then Exit;
  if Restriction.Moder = BOT_NAME then Exit;
  try
    Str:='----onRestrictionRemoved Exception----'+Chr(13)+Chr(10);
    Link.onRestrictionRemove(Restriction, UnbanModer);
  except
    on e: exception do
      CorePlugin.onError(CorePlugin, e, Str);
  end;
end;

begin
	PLUGIN_FILENAME := ExtractFileNameEx(GetDllPath, false);

end.
