-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Ada Server for NP++ Elephant Plugin",
              Version     => (45, 1, 0, 21),
              Kind        => Windows,
              Compiler    => "GNATPRO\22.1");

with Command;

procedure Ada_Server is
begin
  Command.Execute;
end Ada_Server;
