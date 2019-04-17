unit WSWCUploaderThread;

interface
uses Classes, System.SysUtils, System.Generics.Collections,
     WSOCPRoduct, WSProductCommon, WSCommon, WCRestApi, WSPCunionMain, Vcl.Forms;

     type
     TWSUploaderThread = class(TThread)

     procedure Execute; override;

     end;


implementation

{ TWSUploaderThread }

procedure TWSUploaderThread.Execute;
begin

end;

end.
