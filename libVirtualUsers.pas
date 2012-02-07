unit libVirtualUsers;

interface

uses Windows, SysUtils, SQLiteWrap, Classes, libClasses, comm_data, libSync;

type
	TConnectedVirtualUser = record
      Name : String;
      ServId: Integer;
      VirtName : String;
  end;

	TConnectedVirtualUsers = class
  private
    database: TSQLiteDatabase;
    sync: TPluginSync;
  public
  	constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Add(User : TConnectedVirtualUser);
    procedure AddTemp(name: String; servId: Integer; virtName: String);
    procedure DeleteTemp(virtName : String);
    function GetServIdFromTemp(virtName : String): Integer;
    procedure Delete(Name: String; serverId: Integer); overload;
    procedure Delete(VirtName: String); overload;
    function GetVirtualUserName(Name: String; serverId: Integer) : String;
    function IsVirtualUser(Name: String) : Boolean;
    function IsOtherPluginVirtualUser(Name: String) : Boolean;
    function GetUserInfo(VirtName: String) : TConnectedVirtualUser;
    function GetAllUsers : TStrings;
  end;

	TVirtualUser = record
      Name : String;
      ServId: String;
      VirtName : String;
  end;
  PVirtualUser = ^TVirtualUser;

	TAutoFreeVirtUsersList = class(TList)
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

	TUsersDatabase = class
  private
    database: TSQLiteDatabase;
    databasePath: String;
  public
  	constructor Create();
    destructor Destroy(); override;
    procedure Add(User : TVirtualUser);
    procedure Rename(oldName: String; newName: String; servId: String);
    function GetVirtualUserName(Name: String; serverId: String) : String;
    function IsVirtualUser(Name: String) : Boolean;
    function GetUserInfos(VirtName: String) : TAutoFreeVirtUsersList;
    function GetUserInfo(VirtName: String; ServId: String) : TVirtualUser;
  end;

implementation

constructor TConnectedVirtualUsers.Create();
begin
	inherited;
	database := TSQLiteDatabase.Create(':memory:');
  database.ExecSQL('CREATE TABLE "virtUser" ("name" VARCHAR NOT NULL , "servId" INTEGER NOT NULL , "virtName" VARCHAR NOT NULL  UNIQUE , PRIMARY KEY ("name", "servId"))');
  database.ExecSQL('CREATE TABLE "tempVU" ("name" VARCHAR NOT NULL , "servId" INTEGER NOT NULL , "virtName" VARCHAR NOT NULL  UNIQUE , PRIMARY KEY ("name", "servId"))');
  sync := TPluginSync.Create(TEMP_PATH+'LinkerSync.sqlite', PLUGIN_FILENAME);
end;

destructor TConnectedVirtualUsers.Destroy();
begin
	sync.Free;
  database.Free;
  inherited;
end;

procedure TConnectedVirtualUsers.Clear();
begin
	database.ExecSQL('DELETE FROM virtUser');
	database.ExecSQL('DELETE FROM tempVU');
  sync.Clear;
end;

procedure TConnectedVirtualUsers.Add(User : TConnectedVirtualUser);
begin
  database.AddParamText(':Name', User.Name);
  database.AddParamInt(':ServId', User.ServId);
  database.AddParamText(':VirtName', User.VirtName);
	database.ExecSQL('INSERT INTO virtUser (name, servId, virtName) VALUES (:Name, :ServId, :VirtName)');
  sync.Add(User.VirtName);
end;

procedure TConnectedVirtualUsers.AddTemp(name: String; servId: Integer; virtName: String);
begin
  database.AddParamText(':Name', name);
  database.AddParamInt(':ServId', servId);
  database.AddParamText(':VirtName', virtName);
	database.ExecSQL('INSERT INTO tempVU (name, servId, virtName) VALUES (:Name, :ServId, :VirtName)');
  sync.Add(virtName);
end;

procedure TConnectedVirtualUsers.DeleteTemp(virtName : String);
begin
  database.AddParamText(':VirtName', virtName);
	database.ExecSQL('DELETE FROM tempVU WHERE virtName = :VirtName');
  sync.Delete(virtName);
end;

function TConnectedVirtualUsers.GetServIdFromTemp(virtName : String): Integer;
var
  tempTable: TSQLiteTable;
begin
	Result := -1;
  database.AddParamText(':VirtName', virtName);
  tempTable := database.GetTable('SELECT servId FROM tempVU WHERE virtName = :VirtName LIMIT 1');
  if not tempTable.EOF then
  begin
    Result := tempTable.FieldAsInteger(0);
  end;
  tempTable.Free;
end;

procedure TConnectedVirtualUsers.Delete(Name: String; serverId: Integer);
begin
  sync.Delete(GetVirtualUserName(Name, serverId));
  database.AddParamText(':Name', Name);
  database.AddParamInt(':ServId', serverId);
	database.ExecSQL('DELETE FROM virtUser WHERE name = :Name AND servId = :ServId');
end;

procedure TConnectedVirtualUsers.Delete(VirtName: String);
begin
  database.AddParamText(':VirtName', VirtName);
	database.ExecSQL('DELETE FROM virtUser WHERE virtName = :VirtName');
  sync.Delete(VirtName);
end;

function TConnectedVirtualUsers.GetVirtualUserName(Name: String; serverId: Integer) : String;
begin
  database.AddParamText(':Name', Name);
  database.AddParamInt(':ServId', serverId);
  Result := database.GetTableString('SELECT virtName FROM virtUser WHERE name = :Name AND servId = :ServId '+
  	'UNION SELECT virtName FROM tempVU WHERE name = :Name AND servId = :ServId LIMIT 1');
end;

function TConnectedVirtualUsers.IsVirtualUser(Name: String) : Boolean;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', Name);
  tempTable := database.GetTable('SELECT virtName FROM virtUser WHERE virtName = :VirtName '+
  	'UNION SELECT virtName FROM tempVU WHERE virtName = :VirtName LIMIT 1');
  Result := not tempTable.EOF;
  tempTable.Free;
end;

function TConnectedVirtualUsers.IsOtherPluginVirtualUser(Name: String) : Boolean;
begin
	Result := sync.IsItemOwned(Name);
end;

function TConnectedVirtualUsers.GetUserInfo(VirtName: String) : TConnectedVirtualUser;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', VirtName);
  Result.Name := '';
  Result.ServId := -1;
  Result.VirtName := '';
  tempTable := database.GetTable('SELECT name, servId FROM virtUser WHERE virtName = :VirtName '+
  	'UNION SELECT name, servId FROM tempVU WHERE virtName = :VirtName LIMIT 1');
  if not tempTable.EOF then
  begin
    Result.Name := tempTable.FieldAsString(0);
    Result.ServId := tempTable.FieldAsInteger(1);
    Result.VirtName := VirtName;
  end;
  tempTable.Free;
end;

function TConnectedVirtualUsers.GetAllUsers : TStrings;
begin
	Result := TStringList.Create;
  database.GetTableStrings('SELECT virtName FROM virtUser', Result);
end;

constructor TUsersDatabase.Create();
var
  existsFlag: Boolean;
begin
	inherited;
	databasePath := config_dir + '\main.sqlite';
  existsFlag := FileExists(databasePath);
	database := TSQLiteDatabase.Create(databasePath);
  if not existsFlag then
  begin
		database.ExecSQL('CREATE TABLE IF NOT EXISTS "virtUser" ("name" VARCHAR NOT NULL , "servId" VARCHAR NOT NULL , "virtName" VARCHAR NOT NULL , PRIMARY KEY ("name", "servId"));');
    database.ExecSQL('CREATE INDEX IF NOT EXISTS "index_virtUser_virtName" ON "virtUser" ("virtName" ASC);');
  end;
end;

destructor TUsersDatabase.Destroy();
begin
  database.Free;
  inherited;
end;

procedure TUsersDatabase.Add(User : TVirtualUser);
begin
  database.AddParamText(':Name', User.Name);
  database.AddParamText(':ServId', User.ServId);
  database.AddParamText(':VirtName', User.VirtName);
	database.ExecSQL('INSERT INTO virtUser (name, servId, virtName) VALUES (:Name, :ServId, :VirtName)');
end;

procedure TUsersDatabase.Rename(oldName: String; newName: String; servId: String);
begin
  database.AddParamText(':Name', oldName);
  database.AddParamText(':ServId', servId);
  database.AddParamText(':VirtName', newName);
	database.ExecSQL('UPDATE virtUser SET virtName = :VirtName WHERE virtName = :Name AND servId = :ServId');
end;

function TUsersDatabase.GetVirtualUserName(Name: String; serverId: String) : String;
begin
  database.AddParamText(':Name', Name);
  database.AddParamText(':ServId', serverId);
  Result := database.GetTableString('SELECT virtName FROM virtUser WHERE name = :Name AND servId = :ServId LIMIT 1');
end;

function TUsersDatabase.IsVirtualUser(Name: String) : Boolean;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', Name);
  tempTable := database.GetTable('SELECT virtName FROM virtUser WHERE virtName = :VirtName LIMIT 1');
  Result := not tempTable.EOF;
  tempTable.Free;
end;

function TUsersDatabase.GetUserInfos(VirtName: String) : TAutoFreeVirtUsersList;
var
  tempTable: TSQLiteTable;
  VirtualUser: PVirtualUser;
begin
  Result := TAutoFreeVirtUsersList.Create;
  database.AddParamText(':VirtName', VirtName);
  tempTable := database.GetTable('SELECT name, servId FROM virtUser WHERE virtName = :VirtName');
  while not tempTable.EOF do
  begin
  	New(VirtualUser);
    VirtualUser^.Name := tempTable.FieldAsString(0);
    VirtualUser^.ServId := tempTable.FieldAsString(1);
    VirtualUser^.VirtName := VirtName;
    Result.Add(VirtualUser);
    tempTable.Next;
  end;
  tempTable.Free;
end;

function TUsersDatabase.GetUserInfo(VirtName: String; ServId: String) : TVirtualUser;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', VirtName);
  database.AddParamText(':ServId', ServId);
  Result.Name := '';
  Result.ServId := '';
  Result.VirtName := '';
  tempTable := database.GetTable('SELECT name, servId FROM virtUser WHERE virtName = :VirtName AND servId = :ServId LIMIT 1');
  if not tempTable.EOF then
  begin
    Result.Name := tempTable.FieldAsString(0);
    Result.ServId := tempTable.FieldAsString(1);
    Result.VirtName := VirtName;
  end;
  tempTable.Free;
end;

procedure TAutoFreeVirtUsersList.Notify(Ptr: Pointer; Action: TListNotification);
begin
	if Action = lnDeleted then
		Dispose(PVirtualUser(Ptr));
end;

end.
