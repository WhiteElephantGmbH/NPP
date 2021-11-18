-- *********************************************************************************************************************
-- *                       (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                                  *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Environment_Variables;
with Ada_95.File;
with Files;
with Log;
with Text;

package body Ada_95.Build is

  The_Project_Folder    : Text.String;
  Build_Defined         : Boolean;
  Use_Icon              : Boolean;
  The_Version           : Version;
  The_Kind              : Kind;
  The_Company           : Text.String;
  The_Description       : Text.String;
  The_Tools_Directories : Text.String;
  Tools_Have_Default    : Boolean;
  The_Libraries         : String_List.Item;
  The_Resource          : Text.String;
  The_Interface         : Text.String;
  Library_Check         : Library_Check_Function;


  procedure Initialize (Filename : String;
                        Check    : Library_Check_Function) is
  begin
    Library_Check := Check;
    The_Project_Folder := Text.String_Of (Files.Normalized_Folder(Files.Directory_Of (Filename)));
    Build_Defined := False;
    Use_Icon := True;
    The_Kind := Windows_Application;
    The_Version := (others => <>);
    Text.Clear (The_Tools_Directories);
    Text.Clear (The_Company);
    Text.Clear (The_Description);
    Text.Clear (The_Resource);
    Text.Clear (The_Interface);
    String_List.Clear (The_Libraries);
    Tools_Have_Default := False;
  end Initialize;


  function Project_Folder return String is
  begin
    return Text.String_Of (The_Project_Folder);
  end Project_Folder;


  procedure Define is
  begin
    Build_Defined := True;
  end Define;

  function Is_Defined return Boolean is (Build_Defined);


  procedure Set_Console_Application is
  begin
    The_Kind := Console_Application;
  end Set_Console_Application;


  function Defined_Kind (Item : String) return Boolean is
  begin
    if Item = "Console" then
      The_Kind := Console_Application;
    elsif Item = "Windows" then
      The_Kind := Windows_Application;
    elsif Item = "Dll" then
      The_Kind := Dll;
      Use_Icon := False;
    else
      return False;
    end if;
    return True;
  end Defined_Kind;

  function Is_Dll return Boolean is (The_Kind = Dll);


  function Application_Kind_Image return String is
  begin
    case Application_Kind(The_Kind) is
    when Windows_Application =>
      return "windows";
    when Console_Application =>
      return "console";
    end case;
  end Application_Kind_Image;


  procedure Define_Interface (Item : String) is
  begin
    The_Interface := Text.String_Of (Item);
  end Define_Interface;

  function Actual_Interface return String is (Text.String_Of (The_Interface));


  procedure Define (Item : Version) is
  begin
    The_Version := Item;
  end Define;

  function Actual_Version return Version is (The_Version);


  procedure Define_Company (Item : String) is
  begin
    The_Company := Text.String_Of (Item);
  end Define_Company;

  function Actual_Company return String is (Text.String_Of (The_Company));


  procedure Define_Description (Item : String) is
  begin
    The_Description := Text.String_Of (Item);
  end Define_Description;

  function Actual_Description return String is (Text.String_Of (The_Description));


  function System_Drive return String is
    System_Drive_Variable : constant String := "SYSTEMDRIVE";
  begin
    if Ada.Environment_Variables.Exists (System_Drive_Variable) then
      return Ada.Environment_Variables.Value (System_Drive_Variable);
    else
      return "C:";
    end if;
  end System_Drive;


  function Defined_Compiler (Item : String) return Boolean is
    Tools_Directory : constant String := System_Drive & Files.Separator & Item & Files.Separator  & "bin";
  begin
    if Files.Directory_Exists (Tools_Directory) then
      if not Tools_Have_Default then
        Define_Tools_Directories (Tools_Directory);
      end if;
      return True;
    end if;
    return False;
  end Defined_Compiler;


  procedure Define_Tools_Directories (Item : String) is
  begin
    Log.Write ("||| Tools Directories: " & Item);
    The_Tools_Directories := Text.String_Of (File.Normalized (Item));
  end Define_Tools_Directories;


  procedure Set_Tools_Default is
  begin
    Log.Write ("||| Tools have default");
    Tools_Have_Default := True;
  end Set_Tools_Default;


  function Tools_Directories return String is (Text.String_Of (The_Tools_Directories));

  function Tools_Defined return Boolean is (not Text.Is_Null (The_Tools_Directories));

  function Tools_Default_Set return Boolean is (Tools_Have_Default);

  function Check_Of (Library : String) return Library_Check_Completion is
  begin
    return Library_Check (Library);
  end Check_Of;

  procedure Define_Libraries (Item : String_List.Item) is

  begin
    The_Libraries := Item;
  end Define_Libraries;

  function Actual_Libraries return String_List.Item is (The_Libraries);


  function Defined_Resource (Item : String) return Boolean is
    Filename : constant String := Project_Folder & Item & ".rc";
  begin
    if Files.Exists (Filename) then
      The_Resource := Text.String_Of (Filename);
      return True;
    end if;
    return False;
  end Defined_Resource;

  function Has_Resource return Boolean is (not Text.Is_Null (The_Resource));

  function Actual_Resource return String is (Text.String_Of (The_Resource));


  function Defined_Icon (Item : String) return Boolean is
  begin
    if Item = "False" then
      Use_Icon := False;
    elsif Item = "True" then
      Use_Icon := True;
    else
      return False;
    end if;
    return True;
  end Defined_Icon;

  function Has_Icon return Boolean is (Use_Icon);

end Ada_95.Build;
