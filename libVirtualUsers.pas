unit libVirtualUsers;

interface

uses Windows, SysUtils, SQLiteWrap, Classes, comm_data;

type
	TConnectedVirtualUser = record
      Name : String;
      ServId: Integer;
      VirtName : String;
  end;

	TConnectedVirtualUsers = class
  private
    database: TSQLiteDatabase;
  public
  	constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Add(User : TConnectedVirtualUser);
    procedure Delete(Name: String; serverId: Integer); overload;
    procedure Delete(VirtName: String); overload;
    function GetVirtualUserName(Name: String; serverId: Integer) : String;
    function IsVirtualUser(Name: String) : Boolean;
    function GetUserInfo(VirtName: String) : TConnectedVirtualUser;
    function GetAllUsers : TStrings;
  end;

	TVirtualUser = record
      Name : String;
      ServId: String;
      VirtName : String;
  end;


	TVirtualUsers = class
  private
    database: TSQLiteDatabase;
    databasePath: String;
  public
  	constructor Create();
    destructor Destroy(); override;
    procedure Add(User : TVirtualUser);
    function GetVirtualUserName(Name: String; serverId: String) : String;
    function IsVirtualUser(Name: String) : Boolean;
    function GetUserInfo(VirtName: String) : TVirtualUser;
  end;

implementation

constructor TConnectedVirtualUsers.Create();
begin
	database := TSQLiteDatabase.Create(':memory:');
  database.ExecSQL('CREATE TABLE "virtUser" ("name" VARCHAR NOT NULL , "servId" INTEGER NOT NULL , "virtName" VARCHAR NOT NULL  UNIQUE , PRIMARY KEY ("name", "servId"))');
end;

destructor TConnectedVirtualUsers.Destroy();
begin
  database.Free;
  inherited;
end;

procedure TConnectedVirtualUsers.Clear();
begin
	database.ExecSQL('DELETE FROM virtUser');
end;

procedure TConnectedVirtualUsers.Add(User : TConnectedVirtualUser);
begin
  database.AddParamText(':Name', User.Name);
  database.AddParamInt(':ServId', User.ServId);
  database.AddParamText(':VirtName', User.VirtName);
	database.ExecSQL('INSERT INTO virtUser (name, servId, virtName) VALUES (:Name, :ServId, :VirtName)');
end;

procedure TConnectedVirtualUsers.Delete(Name: String; serverId: Integer);
begin
  database.AddParamText(':Name', Name);
  database.AddParamInt(':ServId', serverId);
	database.ExecSQL('DELETE FROM virtUser WHERE name = :Name AND servId = :ServId');
end;

procedure TConnectedVirtualUsers.Delete(VirtName: String);
begin
  database.AddParamText(':VirtName', VirtName);
	database.ExecSQL('DELETE FROM virtUser WHERE virtName = :VirtName');
end;

function TConnectedVirtualUsers.GetVirtualUserName(Name: String; serverId: Integer) : String;
begin
  database.AddParamText(':Name', Name);
  database.AddParamInt(':ServId', serverId);
  Result := database.GetTableString('SELECT virtName FROM virtUser WHERE name = :Name AND servId = :ServId LIMIT 1');
end;

function TConnectedVirtualUsers.IsVirtualUser(Name: String) : Boolean;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', Name);
  tempTable := database.GetTable('SELECT virtName FROM virtUser WHERE virtName = :VirtName LIMIT 1');
  Result := not tempTable.EOF;
  tempTable.Free;
end;

function TConnectedVirtualUsers.GetUserInfo(VirtName: String) : TConnectedVirtualUser;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', VirtName);
  Result.Name := '';
  Result.ServId := -1;
  Result.VirtName := '';
  tempTable := database.GetTable('SELECT name, servId FROM virtUser WHERE virtName = :VirtName LIMIT 1');
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

constructor TVirtualUsers.Create();
var
  existsFlag: Boolean;
begin
	databasePath := config_dir + '\main.sqlite';
  existsFlag := FileExists(databasePath);
  if not existsFlag then
  	FileCreate(databasePath);
	database := TSQLiteDatabase.Create(databasePath);
  if not existsFlag then
  begin
  	database.ExecSQL('CREATE TABLE "user" ("id" INTEGER PRIMARY KEY  AUTOINCREMENT  NOT NULL , "name" VARCHAR NOT NULL  UNIQUE , "connected" BOOL NOT NULL  DEFAULT 0);' +
			'CREATE TABLE "virtUser" ("name" VARCHAR NOT NULL , "servId" VARCHAR NOT NULL , "virtName" VARCHAR NOT NULL  UNIQUE , PRIMARY KEY ("name", "servId"));');
  end;
end;

destructor TVirtualUsers.Destroy();
begin
  database.Free;
  inherited;
end;

procedure TVirtualUsers.Add(User : TVirtualUser);
begin
  database.AddParamText(':Name', User.Name);
  database.AddParamText(':ServId', User.ServId);
  database.AddParamText(':VirtName', User.VirtName);
	database.ExecSQL('INSERT INTO virtUser (name, servId, virtName) VALUES (:Name, :ServId, :VirtName)');
end;

function TVirtualUsers.GetVirtualUserName(Name: String; serverId: String) : String;
begin
  database.AddParamText(':Name', Name);
  database.AddParamText(':ServId', serverId);
  Result := database.GetTableString('SELECT virtName FROM virtUser WHERE name = :Name AND servId = :ServId LIMIT 1');
end;

function TVirtualUsers.IsVirtualUser(Name: String) : Boolean;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', Name);
  tempTable := database.GetTable('SELECT virtName FROM virtUser WHERE virtName = :VirtName LIMIT 1');
  Result := not tempTable.EOF;
  tempTable.Free;
end;

function TVirtualUsers.GetUserInfo(VirtName: String) : TVirtualUser;
var
  tempTable: TSQLiteTable;
begin
  database.AddParamText(':VirtName', VirtName);
  Result.Name := '';
  Result.ServId := '';
  Result.VirtName := '';
  tempTable := database.GetTable('SELECT name, servId FROM virtUser WHERE virtName = :VirtName LIMIT 1');
  if not tempTable.EOF then
  begin
    Result.Name := tempTable.FieldAsString(0);
    Result.ServId := tempTable.FieldAsString(1);
    Result.VirtName := VirtName;
  end;
  tempTable.Free;
end;

end.
