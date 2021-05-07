-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with String_List;

package Project is

  type Case_Modification is (No_Change, Keywords, Change_All);

  function Confirmation_Message return String;

  function Initialized (Filename : String) return Boolean;

  procedure Change_To (Filename : String);

  function Is_Source (Filename : String) return Boolean;

  function Binary_Folder return String;

  function Source_Folder return String;

  function Product_Directory return String;

  function Product_Sub_Path return String;

  function Is_In_Reference_Area (Filename : String) return Boolean;

  function Run return Boolean;

  procedure Finalize;

  function Is_Defined return Boolean;

  function Program_Unit return String;

  function Is_Program_Unit (Filename : String) return Boolean;

  function Case_Handling_Style return Case_Modification;

  function Full_Name_Of (Filename : String) return String;

  function Name return String;

  function Actual return String;

  function Folder return String;

  function Directory return String;

  function Language_Directory return String;

  function Defined_Environment return Boolean;

  function Gpr_Filename return String;

  function Resource_Object return String;

  function Gpr_File_Is_Generated return Boolean;

  function Promotion_Areas return String;

  function Promotion_List return String_List.Item;

  function Must_Be_Build_First (Filename : String) return Boolean;

  function Created_Target_Folder return String;

  function Tools_Folder return String;

  function Environment return String;

end Project;
