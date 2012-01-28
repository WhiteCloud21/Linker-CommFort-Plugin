unit libSync;

interface

uses Windows, SysUtils, SQLiteWrap, Classes;

type
	TPluginSync = class
  private
    database: TSQLiteDatabase;
    pluginName: String;
  public
  	constructor Create(databasePath: String; pluginName: String);
    destructor Destroy(); override;
    procedure Add(item: String);
    procedure Delete(item: String);
    procedure Clear();
    function  IsItemOwned(item: String; checkOnlyOtherPlugins: Boolean = true): Boolean;
  end;

implementation

constructor TPluginSync.Create(databasePath: String; pluginName: String);
begin
	inherited Create();
  Self.pluginName := pluginName;
	database := TSQLiteDatabase.Create(databasePath);
  database.ExecSQL('CREATE TABLE IF NOT EXISTS "sync" ("pluginName" VARCHAR NOT NULL , "item" VARCHAR NOT NULL UNIQUE , PRIMARY KEY ("item"))');
	Clear();
end;

destructor TPluginSync.Destroy();
begin
	Clear();
  database.Free;
  inherited;
end;

procedure TPluginSync.Add(item: String);
begin
	database.AddParamText(':Name', pluginName);
	database.AddParamText(':Item', item);
  database.ExecSQL('INSERT INTO sync (pluginName, item) VALUES (:Name, :Item)');
end;

procedure TPluginSync.Delete(item: String);
begin
	database.AddParamText(':Name', pluginName);
	database.AddParamText(':Item', item);
  database.ExecSQL('DELETE FROM sync WHERE pluginName = :Name AND item = :Item');
end;

procedure TPluginSync.Clear();
begin
	database.AddParamText(':Name', pluginName);
  database.ExecSQL('DELETE FROM sync WHERE pluginName = :Name');
end;

function TPluginSync.IsItemOwned(item: String; checkOnlyOtherPlugins: Boolean = true): Boolean;
begin
	database.AddParamText(':Item', item);
  if checkOnlyOtherPlugins then
  begin
		database.AddParamText(':Name', pluginName);
  	Result := database.GetTableString('SELECT pluginName FROM sync WHERE item = :Item AND pluginName != :Name') <> '';
  end
  else
  begin
  	Result := database.GetTableString('SELECT pluginName FROM sync WHERE item = :Item') <> '';
  end;
end;

end.
