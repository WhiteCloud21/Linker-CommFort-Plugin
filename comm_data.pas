{$I plugin.inc}

unit comm_data;

interface

uses Windows, SysUtils, IniFiles, JPEG;

type
  TUser = record
    Name : string;
    IP   : string;
    sex  : DWord;
  end;
  TUsers = array of TUser;

  TChannel = record
    Name  : string;
    Users : DWord;
    Theme : string;
  end;
  TChannels = array of TChannel;

  TRegUser = record
    Name : string;
    IP   : string;
  end;
  TRegUsers = array of TRegUser;

  TWaitUser = record
    Status: Word;
    Name  : string;
    IP    : string;
    ID    : string;
    date  : Double;
    msg   : string;
    moder : string;
    reason: string;
  end;
  TWaitUsers = array of TWaitUser;

  TRestriction = record
    restID : DWORD;
    date   : Double;
    remain : Double;
    ident  : DWord;
    Name   : string;
    IP     : string;
    IPrange: string;
    compID : string;
    banType: DWord;
    channel: string;
    moder  : string;
    reason : string;
  end;
  TRestrictions = array of TRestriction;

  TtypeCommFortProcess = procedure(dwPluginID : DWORD; dwMessageID : DWORD; bMessage : PChar; dwMessageLength : DWORD); stdcall;
  TtypeCommFortGetData = function(dwPluginID : DWORD; dwMessageID : DWORD; bInBuffer : TBytes; inBufferLength : DWORD; bOutBuffer : PChar; outBufferLength : DWORD) : DWORD; stdcall;
  TError = procedure(Sender: TObject; Error: Exception; Extratext: String='') of object;
  TPreMsg = function(Sender: TObject; bMessage : PCHAR; dwMessageLength : PDWORD): boolean of object;
  TAuthFail = procedure(Sender: TObject; Name : string; Reason: Word) of object;
  TJoinChannelFail = procedure(Sender: TObject; Name, Channel : string; Reason: Word) of object;
  TPrivMsg = procedure(Sender: TObject; Name: String; User : TUser; regime : integer; bMessage : string) of object;
  TPrivImg = procedure(Sender: TObject; Name: String; User : TUser; Image : TJpegImage) of object;
  TPMsg = procedure(Sender: TObject; Name: String; User : TUser; bMessage : string) of object;
  TJoinBot = procedure(Sender: TObject; Name: string; channel : string; theme : string; greeting: string) of object;
  TPubMsg = procedure(Sender: TObject; Name: String; User : TUser; channel: string; regime : integer; bMessage : string) of object;
  TPubImg = procedure(Sender: TObject; Name: String; User : TUser; channel: string; Image : TJpegImage) of object;
  TChnTheme = procedure(Sender: TObject; Name: String; User : TUser; channel: string; newtheme : string) of object;
  TUsrJoin = procedure(Sender: TObject; Name: String; User : TUser; channel: string) of object;
  TUsrLeft = procedure(Sender: TObject; Name: String; User : TUser; channel: string) of object;
  TChnName = procedure(Sender: TObject; User : TUser; newname: string; newicon: integer) of object;
  TChnIcon = procedure(Sender: TObject; User : TUser; newicon: integer) of object;
  TChnStt = procedure(Sender: TObject; User : TUser; newstate: string) of object;
  TChatUsrJoin = procedure(Sender: TObject; User : TUser) of object;
  TChatUsrLeft = procedure(Sender: TObject; User : TUser) of object;
  TRestAdded = procedure(Sender: TObject; Restriction: TRestriction) of object;
  TRestRemoved = procedure(Sender: TObject; Restriction: TRestriction; UnBanModer: String) of object;

const

  PM_PLUGIN_JOIN_VIRTUAL_USER   = 1001; //plugin -> commfort: ���������� ������������ ������������: �����(���) + �����(IP-�����) + �����(��� ������) + �����(������) + �����(������)
  PM_PLUGIN_LEAVE_VIRTUAL_USER  = 1002; //plugin -> commfort: ��������� ������������ ������������: �����(���)
  PM_PLUGIN_SNDMSG_PUB          = 1020; //plugin -> commfort: ������������ ��������� � �����: �����(��� ������������ ������������) + �����(�����) + �����(�����) + �����(���������) ������: 0 - ����� ����������� ������ 1 - ����� ����������� ���������� (F9)
  PM_PLUGIN_SNDMSG_PRIV         = 1021; //plugin -> commfort: ������������ ��������� � ������: �����(��� ������������ ������������) + �����(�����)+�����(��� ������������)+�����(���������) ������: 0 - ����� ����������� ������ 1 - ����� ����������� ���������� (F9)
  PM_PLUGIN_SNDMSG_PM           = 1022; //plugin -> commfort: ��������� ������ ���������:  �����(��� ������������ ������������) + �����(��� ��������) + �����(��� ������������)+�����(���������)
	PM_PLUGIN_SNDIMG_PUB          = 1080; //plugin -> commfort: ������������ ����������� � �����: �����(��� ������������ ������������) + �����(�����) + ������(������ ����������� � ������� jpg)
  PM_PLUGIN_SNDIMG_PRIV         = 1081; //plugin -> commfort: ������������ ����������� � ������: �����(��� ������������ ������������) + �����(��� ������������) + ������(������ ����������� � ������� jpg)
  PM_PLUGIN_THEME_CHANGE        = 1023; //plugin -> commfort: �������� ���� ������: �����(��� ������������ ������������) + �����(�����)+�����(����� ����)
  PM_PLUGIN_GREETING_CHANGE     = 1024; //plugin -> commfort: �������� ����������� ������: �����(�����)+�����(����� �����������)

  PM_PLUGIN_STATUS_CHANGE       = 1025; //plugin -> commfort: �������� ���������: �����(��� ������������ ������������) + �����(����� ���������)
  PM_PLUGIN_RENAME_CHANNEL      = 1029; //plugin -> commfort: ������������ ����� : �����(��� ������������ ������������) + �����(�����) + �����(����� �������� ������)

  PM_PLUGIN_RESTRICT_SET        = 1040; //plugin -> commfort: �������� �����������: �����(��� ������������ ������������) + �����(��� �������������) + �����(������ �������������) + ����� (��� �����������) + �����(����� �����������) + ����() + �����(������� �����������) + �����(��� �����������)
  PM_PLUGIN_RESTRICT_DEL        = 1041; //plugin -> commfort: ����� �����������: �����(��� ������������ ������������) + �����(ID �����������) + �����(�������)
  PM_PLUGIN_CHANNEL_DEL         = 1028; //plugin -> commfort: ������� (�������) �����: �����(��� ������������ ������������) + �����(�����)
  PM_PLUGIN_CHANGE_ICON         = 1026; //plugin -> commfort: �������� ������: �����(��� ������������ ������������) + �����(����� ������)
  PM_PLUGIN_ANNOUNCMENT_ADD     = 1050; //plugin -> commfort: ������������ ����������: �����(��� ������������ ������������) + �����(ID �������) + �����(���������) + �����(����� ����������) + �����(��� ��������) + �����(��� ������� ������������) + ����(���� �������� ����������)
  PM_PLUGIN_ANNOUNCMENT_DEL     = 1051; //plugin -> commfort: ������� ����������: �����(��� ������������ ������������) + �����(ID ����������)
  PM_PLUGIN_COMMENT_ADD         = 1055; //plugin -> commfort: ������������ �����������: �����(��� ������������ ������������) + �����(ID ����������) + �����(����� �����������)
  PM_PLUGIN_COMMENT_DEL         = 1056; //plugin -> commfort: ������� �����������: �����(��� ������������ ������������) + �����(ID �����������)
  PM_PLUGIN_PASSWORD_CHANGE     = 1070; //plugin -> commfort: �������� ������: �����(��� ������������ ������������) + �����(��� ������������, ������� ���������� �������� ������) + �����(��� ������) + �����(����� ������)
  PM_PLUGIN_ACCOUNT_DEL         = 1071; //plugin -> commfort: ������� ������� ������ � ������� : �����(��� ������������ ������������) + �����(��� ���������� ������������)
  PM_PLUGIN_ACCOUNT_AGREE       = 1033; //plugin -> commfort: ������� ��������� ������� ������: �����(��� ������������ ������������) + �����(����������� ������� ������)
  PM_PLUGIN_ACCOUNT_DISAGREE    = 1034; //plugin -> commfort: ��������� ��������� ������� ������: �����(��� ������������ ������������) + �����(����������� ������� ������) + �����(�������)

  PM_PLUGIN_STOP                = 2100; //plugin -> commfort: ���������� ������
  PM_PLUGIN_CHANNEL_JOIN        = 1026; //plugin -> commfort: �������/������������ � ������ ������: �����(��� ������������ ������������) + �����(�����)+�����(���������)+�����(����� �����) ���������: 0 - ����� ������� � ������ ������� 1 - ����� �������� � ������ ������� ����� �����: 0 - ���� �������� ���� ������������� 1 - ���� �������� ������ �� ����������� ��������! ���� ������������ ����� ������������ ���������� �� ����� ��� � 16 ����� �������.
  PM_PLUGIN_CHANNEL_LEAVE       = 1027; //plugin -> commfort: �������� ����� �����

  PM_PLUGIN_AUTH_FAIL           = 1090; //commfort -> plugin: ����������� ������������ ������������ ����������: �����(��� ������������ ������������) + �����(��� �������)
  PM_PLUGIN_JOINCHANNEL_FAIL    = 1091; //commfort -> plugin: ����������� � ������ ������������ ������������ ����������:�����(��� ������������ ������������) + �����(�����) + �����(��� �������)
  PM_PLUGIN_MSG_PRIV            = 1060; //commfort -> plugin: ��������� � ������: �����(��� ������������ ������������) + ������������()+�����(�����)+�����(���������) ������: 0 - ����� ����������� ������ 1 - ����� ����������� ���������� (F9)
  PM_PLUGIN_MSG_PM              = 1061; //commfort -> plugin: ������ ���������: �����(��� ������������ ������������) + ������������()+�����(���������)
  PM_PLUGIN_JOIN_BOT            = 1062; //commfort -> plugin: ����������� � ������ ����: �����(��� ������������ ������������) + �����(�����)+�����(����)+�����(�����������)
  PM_PLUGIN_MSG_PUB             = 1070; //commfort -> plugin: ���������� ��������� � �����: �����(��� ������������ ������������) + ������������()+�����(�����)+�����(�����)+�����(���������) ������: 0 - ����� ����������� ������ 1 - ����� ����������� ���������� (F9)
  PM_PLUGIN_THEME_CHANGED       = 1071; //commfort -> plugin: ����� ���� ������: �����(��� ������������ ������������, ������� ������������ � ������ ������) + ������������()+�����(�����)+�����(����� ����)
  PM_PLUGIN_USER_JOINEDCHANNEL  = 1072; //commfort -> plugin: ����������� � ������ ������� ������������: �����(��� ������������ ������������, ������� ������������ � ������ ������) + ������������()+�����(�����)
  PM_PLUGIN_USER_LEAVEDCHANNEL  = 1073; //commfort -> plugin: ����� �� ������ ������� ������������: �����(��� ������������ ������������, ������� ������������ � ������ ������) + ������������()+�����(�����)
  PM_PLUGIN_ICON_CHANGED        = 1076; //commfort -> plugin: ����� ������: ������������()+�����(����� ����� ������)
  PM_PLUGIN_STATUS_CHANGED      = 1077; //commfort -> plugin: ����� ���������: ������������()+�����(����� ���������)
  PM_PLUGIN_USER_JOINED         = 1078; //commfort -> plugin: ������������ ������������� � ����: ������������()
  PM_PLUGIN_USER_LEAVED         = 1079; //commfort -> plugin: ������������ ������� ���: ������������()
  PM_PLUGIN_RESTRICT_ADDED	    = 1106; //commfort -> plugin: ����� ����������� (���):  �����(ID �����������) + ����(���������� �� ��������� �����������) + �����(��� �������������) + �����(������� ������) + �����(IP-�����) + �����(�������� IP-�������) + �����(ID ����������) + �����(��� �����������) + �����(�����) + �����(������� ������ ����������) + �����(�������)
  PM_PLUGIN_RESTRICT_DELETED    = 1107; //commfort -> plugin: ������ ����������� (����): �����(������� ������ ����������, �������� �����������) + �����(ID �����������) + �����(��� �������������) + �����(������� ������) + �����(IP-�����) + �����(�������� IP-�������) + �����(ID ����������) + �����(��� �����������) + �����(�����) + �����(������� ������ ����������, ����������� �����������) + �����(�������)

  GD_PROGRAM_TYPE               = 2000; //plugin -> commfort: ��� ���������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(��� ���������)
  GD_PROGRAM_VERSION            = 2001; //plugin -> commfort: ������ ���������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(������ ���������)
  GD_PLUGIN_TEMPPATH            = 2010; //plugin -> commfort: ������������� ���� ��� ��������� ������ ��������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����)
  GD_CHANNELS_GET               = 1040; //plugin -> commfort: ������ ����� �������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + (�����(�������� ������) + �����(���������� ������������� � ������) + �����(���� ������))*����������
  GD_USERS_GET                  = 1041; //plugin -> commfort: ������ ������������� � ����. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + ������������()*����������
  GD_USERCHANNELS_GET           = 1080; //plugin -> commfort: ������ �������, � ������� ��������� ����������� ������������. ���� ������ (���������): �����(��� ������������ ������������). ���� ������ (��������): �����(����������) + (�����(�������� ������) + �����(���������� ������������� � ������) + �����(���� ������))*����������
  GD_CHANNELUSERS_GET           = 1081; //plugin -> commfort: ������ ������������� � ������, � �������� ��������� ����������� ������������. ���� ������ (���������): �����(��� ������������ ������������) + �����(�����). ���� ������ (��������): �����(����������) + ������������()*����������
  GD_REGUSERS_GET               = 1042; //plugin -> commfort: ������ ������������������ �������������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + ������������()*����������
  GD_WAITUSERS_GET              = 1043; //plugin -> commfort: ������ ������ �� ���������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + (�����(������) + ����_�_�����() + �����(���) + �����(IP-�����) + �����(ID ����������) + �����(���������) + �����(������� ������ ����������, ������������� ������) + �����(������� ����������))*����������
  GD_RESTRICTIONS_GET           = 1044; //plugin -> commfort: ������ �����������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + (�����(ID �����������) + ����_�_�����(�������� ������) + ����(���������� �� ��������� �����������) + �����(��� �������������) + �����(������� ������) + �����(IP-�����) + �����(�������� IP-�������) + �����(ID ����������) + �����(��� �����������) + �����(�����) + �����(������� ������ ����������) + �����(�������))*����������
  GD_IPSTATE_GET                = 1050; //plugin -> commfort: ��������� ������� IP-������. ���� ������ (���������): �����(��� ������������). ���� ������ (��������): �����(��������� ������� IP-������)
  GD_PASSWORD_GET               = 1060; //plugin -> commfort: ������ ������� ������. ���� ������ (���������): �����(��� ������������). ���� ������ (��������): �����(32 ���������� MD5 ���-��� ������)
  GD_IP_GET                     = 1061; //plugin -> commfort: IP-����� ������������. ���� ������ (���������): �����(��� ������������). ���� ������ (��������): �����(IP-�����)
  GD_ID_GET                     = 1062; //plugin -> commfort: ID ���������� ������������. ���� ������ (���������): �����(��� ������������). ���� ������ (��������): �����(ID ����������)
  GD_USERINFO_GET               = 1063; //plugin -> commfort: ���������� � ������������. ���� ������ (���������): �����(��� ������������). ���� ������ (��������): �����(IP-�����) + �����(�������� ������������ IP-�����) + �����(ID ����������) + �����(������ ������������) + �����(���������) + �����(������ �������) + �����(��������� ���������� ���� ���������) + �����(����� ������� � ��������) + �����(�������� �������)
  GD_RIGHT_GET                  = 1090; //plugin -> commfort: ����� ������� ������. ���� ������ (���������): �����(������� ������) + �����(��� �����) + �����(�����). ���� ������ (��������): �����(��� ���������� �����)
  GD_MAXIMAGESIZE               = 1030; //plugin -> commfort: ������������ ������ ����������� � ������. ���� ������ (���������): [������� ��������]. ���� ������ (��������): �����(����������) + (�����(�������� ������) + �����(���������� ������������� � ������) + �����(���� ������))*����������

  GD_PLUGIN_SERVER_OR_CLIENT    = 2800; //plugin -> commfort: �������������� �������
  GD_PLUGIN_NAME                = 2810; //plugin -> commfort: �������� �������

  PRE_PLUGIN_MSG                = 0;  //commfort -> plugin: ������������ ���������
  PRE_PLUGIN_THEME              = 1;  //commfort -> plugin: ������������ ���
  PRE_PLUGIN_ANNOUNCMENT        = 2;  //commfort -> plugin: ������������ ����������

  LNK_CODE_JOIN                 = 1;
  LNK_CODE_LEFT                 = 2;
  LNK_CODE_JOINCHAN             = 3;
  LNK_CODE_LEFTCHAN             = 4;
  LNK_CODE_CMSG                 = 10;
  LNK_CODE_CIMG                 = 11;
  LNK_CODE_PRIV                 = 20;
  LNK_CODE_PRIVIMG              = 21;
  LNK_CODE_PMSG                 = 30;
  LNK_CODE_STATUSCHNG           = 40;

  LNK_CODE_BAN                  = 100;

  LNK_CODE_BANLIST              = 1001;
  LNK_CODE_UNBAN 	 	            = 1010;

  LNK_CODE_SERVICE_SERVERNAME   = 15001;

  LNK_CODE_SERVICE_UCSCHECK     = 15011;
  LNK_CODE_SERVICE_UCSREPLY     = 15012;

  LNK_CODE_SERVICE_BADNICK      = 20001;
  LNK_CODE_SERVICE_BANNED       = 20002;
  LNK_CODE_SERVICE_NICKDENIED   = 20003;
  LNK_CODE_SERVICE_NICKLEXISTS  = 20004;
  LNK_CODE_SERVICE_MAXNICKLIMIT = 20005;
  LNK_CODE_SERVICE_CANNOTCONNECT= 20007;
  LNK_CODE_SERVICE_MAXUSERLIMIT = 20008;

  LNK_CODE_SERVICE_CONNECTION_OK= 20051;

  LNK_CODE_SERVICE_RESENDMEUSER = 20101;

  LNK_CODE_SERVICE_JOINCHANFAIL = 20201;

  LNK_CODE_SERVICE_TEMPCHANLIST = 20500;
  LNK_CODE_SERVICE_CREATECHANNEL= 20510;
  LNK_CODE_SERVICE_CLOSECHANNEL = 20511;

  PLU_VER  = '0.1.0';
  PROTOCOL_VER : DWord = 3;

  {$IFDEF Server}
  PLU_NAME = '��������� �������� '+PLU_VER+' (������)';
  {$ELSE}
  PLU_NAME = '��������� �������� '+PLU_VER+' (������)';
  {$ENDIF}
  RandomStr ='qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890';

var
  BOT_NAME: String;
  BOT_PASS: String;
  BOT_ISFEMALE: Byte;
  BOT_IP: String;

  CONNECT_IP: String;
  CONNECT_PORT: Word;

  SERVER_LOCAL: String;
  SERVER_REMOTE: String;

  PLUGIN_FILENAME: String;

  StartKey, MultKey, AddKey: Integer;

  name_prefix: String;
  name_postfix: String;
  config_dir:String;

  file_debug: String;
  file_log: String;
  file_config: String;
  file_users: String;

  IniUsers: TIniFile;

implementation

end.
