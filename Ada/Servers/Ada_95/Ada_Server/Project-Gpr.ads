-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Project.Gpr is

  File_Extension : constant String := ".gpr";

  function File_Is_Generated return Boolean;

  function Filename return String;

  type Information is record
    Project_Name : Strings.Element;
    Source_Path  : Strings.Element;
  end record;

  function Information_Of (The_Filename : String) return Information;

end Project.Gpr;
