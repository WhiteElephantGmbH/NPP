-- *********************************************************************************************************************
-- *                   (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with String_List;
with Strings;

package Ada_95.Build is

  Sub_Directory_32 : constant String := "32";
  Sub_Directory_64 : constant String := "64";

  use type Strings.Item;
  Sub_Directories : constant Strings.Item := Sub_Directory_32 + Sub_Directory_64;

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

  procedure Define_Interface (Item : String_List.Item);

  function Actual_Interface return String_List.Item;

  procedure Define (Item : Version);

  function Actual_Version return Version;

  function Exists (Compiler : String) return Boolean;

  function Defined_Compiler (Item : String) return Boolean;

  function Defined_Compilers (First  : String;
                              Second : String) return Boolean;

  function Tools_Defined return Boolean;

  procedure Define_Global_Tools_Directory (Item : String);

  function Global_Tools_Used return Boolean;

  procedure Define_Tools_Directory (Item : String);

  function Tools_Directory return String;

  function Tools_Kind_Image return String;

  function Compiler_Area return String;

  function Directories_Area return String;

  function Has_Second_Tools_Directory return Boolean;

  procedure Set_Back_To_First;

  function Is_Maching (Filename : String) return Boolean;

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
