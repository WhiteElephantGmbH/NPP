-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

pragma Build (Description => "Ada Renamer",
              Version     => (45, 1, 0 ,4),
              Kind        => Console,
              Icon        => False,
              Compiler    => "GNATPRO\21.2");

with Work;

procedure Renamer is
begin
  Work.Start;
end Renamer;

