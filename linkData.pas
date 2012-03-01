unit linkData;

interface
uses Classes, libVirtualUsers;

type
  TChannelInfo = record
    Name: String;
    Permanent: Boolean;
    Owner: String;
  end;

var
  ChannelList: array [1..16] of TChannelInfo;
  WhiteStartList: TStringList;
  IgnorePrefixList: TStringList;

  VirtUsers: TConnectedVirtualUsers;
  UsersDatabase: TUsersDatabase;

  server_id: String;
  vu_ip: String;

implementation

end.
