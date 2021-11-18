-- *********************************************************************************************************************
-- *                       (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                                  *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with String_List;

package Ada_95.Build is

  type Kind is (Console_Application, Windows_Application, Dll);

  subtype Application_Kind is Kind range Console_Application .. Windows_Application;

  type Version_Number is range 0 .. 255;

  type Version is record
    Major    : Version_Number := 255;
    Minor    : Version_Number := 255;
    Variant  : Version_Number := 255;
    Revision : Version_Number := 255;
  end record;

  type Library_Check_Completion is
    (Library_Ok, Library_Id_Ok, Ada_Project_Path_Missing, Library_Not_Found, Library_Id_Not_Found);

  type Library_Check_Function is access function (Library : String) return Library_Check_Completion;


  procedure Initialize (Filename   : String;
                        Check      : Library_Check_Function;
                        Is_Startup : Boolean);

  procedure Define;

  function Is_Defined return Boolean;

  function Defined_Kind (Item : String) return Boolean;

  procedure Set_Console_Application;

  function Application_Kind_Image return String;

  function Is_Dll return Boolean;

  procedure Define_Interface (Item : String);

  function Actual_Interface return String;

  procedure Define (Item : Version);

  function Actual_Version return Version;

  function Defined_Compiler (Item : String) return Boolean;

  procedure Define_Tools_Directories (Item : String);

  procedure Set_Tools_Default;

  function Tools_Directories return String;

  function Tools_Defined return Boolean;

  function Tools_Default_Set return Boolean;

  procedure Define_Description (Item : String);

  function Actual_Description return String;

  procedure Define_Company (Item : String);

  function Actual_Company return String;

  function Check_Of (Library : String) return Library_Check_Completion;

  procedure Define_Libraries (Item : String_List.Item);

  function Actual_Libraries return String_List.Item;

  function Defined_Resource (Item : String) return Boolean;

  function Has_Resource return Boolean;

  function Actual_Resource return String;

  function Defined_Icon (Item : String) return Boolean;

  function Has_Icon return Boolean;

end Ada_95.Build;
