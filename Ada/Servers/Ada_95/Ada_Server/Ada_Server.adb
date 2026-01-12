-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Ada Server for NP++ Elephant Plugin",
              Version     => (45, 3, 0, 3),
              Kind        => Windows,
              Compiler    => "GNATPRO\23.0");

with Command;

procedure Ada_Server is
begin
  Command.Execute;
end Ada_Server;
