-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Ada Renamer",
              Version     => (45, 1, 0, 5),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\23.0");

with Work;

procedure Renamer is
begin
  Work.Start;
end Renamer;

