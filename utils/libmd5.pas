unit libmd5;

interface
function md5(s:string):string;

implementation

uses Windows, Types, SysUtils, MessageDigest_5;
{$R-}
{$Q-}
function md5(s:string):string;
 var
  _MD5: IMD5;
begin
  _MD5 := GetMD5;
  _MD5.Init;
  _MD5.Update(TByteDynArray(RawByteString(s)), Length(s));
  Result := _MD5.AsString;
end;
{$R+}
{$Q+}

end.
