unit WSHtmlCommon;

interface
uses IdBaseComponent,IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdIOHandler,
     IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, System.Classes,WSCommon, Sysutils, Generics.Collections, Strutils;

    type

      TWStableRows  = TList<TStringList>;
      TWShtmlTable = record
       id   : string;
       name : string;
       content : TWStableRows;
      end;

      TWShtmlLink = record
       id   : string;
       name : string;
       href : string;
       LinkClass: string;
       content: string;
      end;

      TWShtmlImage = record
       id   : string;
       name : string;
       src  : string;
      end;

      TWShtmlLinks  = TList<TWShtmlLink>;
      TWShtmlTables = TList<TWShtmlTable>;
      TWShtmlImages = TList<TWShtmlImage>;

      TWSHtml = class(TWSCommon)
       private
        _ID_client : TIdHTTP;
         L1,L2, T1,T2, I1,I2: integer;
        _links  : TWShtmlLinks;
        _images : TWShtmlimages;
        _tables : TWShtmlTables;
        _HTML, _HTML_T   : WideString;
        function GetNextLink(var Link: TWShtmlLink):Boolean;
        function CleanHtmlFormatting(Html:string):string;
        function GetTagPairContent(Tag, TagEnd, Html:string; var LastEnderPos:integer; var TagProperties: string):string;
        procedure GetAllLinks;
        function getAttrib(Atag, Attrib : string):string;
        function GetTables(var Tables: TWShtmlTables):Boolean;
       public
        constructor create;
        function get(AUrl:string):string;

        property Tables : TWShtmlTables read _tables;
        property Links  : TWShtmlLinks read _links;

      end;


implementation

{ TOhttp }

function TWSHtml.CleanHtmlFormatting(Html: string): string;
begin
  Result := StringReplace( Html, '&amp;', '&', [rfReplaceAll,rfIgnoreCase] );
end;

constructor TWSHtml.create;
begin
  inherited;
 _ID_client := TIdHTTP.Create();
 T1 :=1;
 T2 :=1;
 L1 :=1;
 L2 :=1;
 I1 :=1;
 I1 :=0;
end;

function TWSHtml.get(AUrl: string):string;
begin
 try
  Log('Getting URL: '+AUrl );
  _HTML := _ID_client.Get(AUrl);
  GetAllLinks;
  GetTables(_tables);
  Result := _HTML;
  Log('Success!');
 except
  on E : Exception  do
  begin
   Log('HTTP : ' + E.Message,LEXCEPT);
  end;
 end;
end;

procedure TWSHtml.GetAllLinks;
var Link : TWShtmlLink;
begin
  L1 := 1;
  L2 := 1;
  if not Assigned(_links) then
    _links := TList<TWShtmlLink>.Create
  else
    _links.Clear;
  while GetNextLink(Link) do
  begin
    _links.Add(Link);
    Log('Link found: ' + Link.content + '" -> HREF =  ' + Link.href, LDUMPINFO );
  end;
end;

function TWSHtml.getAttrib(Atag, Attrib: string): string;
var ab,ae : integer;
      att : string;
begin
 att    := Attrib+'="';
 ab     := Posex(att, ATag );
 if ab = 0 then
 begin
  Result := '';
 end
 else
 begin
  ae     := Posex('"',Atag, ab+length(att)+1 );
  result := Copy(ATag, ab+length(att), ae - (ab+length(att))  );
 end;
end;


function TWSHtml.GetNextLink(var Link: TWShtmlLink):Boolean;

  function getTagRaw():string;
  begin
    L1  := Posex('<a', _HTML, L2 );
    if L1 = 0 then
    begin
      L1     := 1;
      L2     := 1;
      result := '';
    end
    else
    begin
     L2  := Posex('>', _HTML, L1);
     Result := Copy(_HTML, L1, (L2-L1)+ 1);
    end;
  end;

  function getTagCont():string;
  var te : integer;
  begin
   te := Posex('</a>', _HTML, L2);
   result := Copy( _HTML, L2 + 1 , (te - L2) - 1);
  end;

  function getAttrib(Atag, Attrib : string):string;
  var ab,ae : integer;
      att : string;
  begin
   att    := Attrib+'="';
   ab     := Posex(att, ATag );
   if ab = 0 then
   begin
    Result := '';
   end
   else
   begin
    ae     := Posex('"',Atag, ab+length(att)+1 );
    result := Copy(ATag, ab+length(att), ae - (ab+length(att))  );
   end;
  end;

var raw, LinkTag, LinkCont, href, name: string;
begin
 Result       := False;
 LinkTag      := getTagRaw;
 if LinkTag = '' then
  exit;

 Link.href     := CleanHtmlFormatting(getAttrib(LinkTag, 'href'));
 Link.Content  := CleanHtmlFormatting(getTagCont);
 Link.LinkClass:= CleanHtmlFormatting(getAttrib(LinkTag, 'class'));
 Link.LinkClass:= CleanHtmlFormatting(getAttrib(LinkTag, 'id'));
 Link.LinkClass:= CleanHtmlFormatting(getAttrib(LinkTag, 'name'));
 Result := True;
end;

function TWSHtml.GetTables(var Tables: TWShtmlTables):Boolean;
var Body, Row, Cell  : string ;
   Trows : TWStableRows;
   Table : TWShtmlTable;
   Properties: string;
   lastpos, LastCell, LastRow:integer;
begin
 Tables  := TWShtmlTables.Create;
 lastPos := 1;
 Result  := False;
  repeat
   Body    := getTagPairContent('table','/table', _HTML, lastpos, Properties);
   if Body <> '' then
   begin
      try
       LastRow  := 1;
       LastCell := 1;
       Trows   := TWStableRows.Create;
       Log('Table found: "' + Properties + '"  ', LDUMPINFO );
       repeat
         Row := getTagPairContent('tr','/tr', Body, LastRow, Properties);
         if Row <> '' then
         begin
           LastCell := 1;
           Trows.Add(TStringList.Create);
           repeat
            Cell := getTagPairContent('td','/td', Row, LastCell, Properties);
              if Cell <> '' then
              begin
                Trows.Last.Add(Cell);
                Log('ROW #'+ IntToStr(Trows.Count) + ' DATA ADDED: ' + Cell,LDUMPINFO  );
              end;
           until (Cell = '');
          end;
       until (Row = '');
       Result := True;
     Except
       Result := False;
     end;
    Table.id := '';
    Table.name := '';
    Table.content := Trows;
    Tables.Add(Table);
   end;
   until (Body = '');

end;

function TWSHtml.GetTagPairContent(Tag, TagEnd, Html: string;
  var LastEnderPos: integer; var TagProperties: string ): string;
  var Opener, Ender, PropOpener, PropEnder: integer;
begin
  TagProperties := '';
  Result := '';
  Ender := LastEnderPos;
  PropOpener := Posex('<' + Tag, Html, Ender) + 1;
    if PropOpener < LastenderPos then
      exit;

  PropEnder := Posex('>', Html, PropOpener);
  TagProperties := Copy(Html, PropOpener + Length(Tag), (PropEnder - PropOpener) - Length(Tag) );
  Ender := Posex(TagEnd, Html, PropEnder) + Length(Tagend);
  LastEnderPos := Ender;
  Result := StringReplace(Copy(Html, PropEnder + 1, (Ender - (PropEnder + 1) ) - (Length(TagEnd) + 1)), #13#10, '', [rfReplaceAll]) ;
end;

end.
