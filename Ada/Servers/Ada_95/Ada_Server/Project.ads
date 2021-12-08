-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Containers.Indefinite_Ordered_Maps;
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

  function Has_New_Resource return Boolean;

  function Promotion_Areas return String;

  function Promotion_List return String_List.Item;

  function Must_Be_Build_First (Filename : String) return Boolean;

  function Created_Target_Folder return String;

  function Tools_Defined return Boolean;

  function Tools_Folder return String;

  function Tools_Kind return String;

  function Has_Second_Compiler return Boolean;

  procedure Set_Back_To_First_Compiler;

  procedure Define_Environment;

  function Environment return String;

private

  Object_Area : constant String := "objects";

  function Object_Folder return String;

  function Program_Unit_Name return String;

  function Target_Directory return String;

  function Target_Folder return String;

  function Tools_Directory return String;

  procedure Set_Error (Message : String) with No_Return;

  package Names is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                               Element_Type => String);
  The_Library_Directories : Names.Map;
  The_Library_Names       : Names.Map;
  The_Library_Sources     : Names.Map;

  The_Libraries          : String_List.Item;
  The_Source_Directories : String_List.Item;

end Project;
