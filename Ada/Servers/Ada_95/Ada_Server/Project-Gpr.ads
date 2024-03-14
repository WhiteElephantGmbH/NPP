-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Project.Gpr is

  File_Extension : constant String := ".gpr";

  function File_Is_Generated return Boolean;

  function Filename return String;

  type Information is record
    Project_Name : Text.String;
    Source_Path  : Text.String;
  end record;

  function Information_Of (The_Filename : String) return Information;

end Project.Gpr;
