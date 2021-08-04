-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada_95.File;
with Ada_95.Project;
with Configuration;
with File;
with Files;
with Log;
with Promotion;
with Server;
with Strings;
with Target;
with Text;

package body Project is

  package Names is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                               Element_Type => String);
  Language : constant String := "Ada";

  procedure Set_Confirmation (Message : String := "") is
  begin
    Promotion.Set_Message (Server.Confirmation & "%b" & Message);
  end Set_Confirmation;


  function Confirmation_Message return String is
  begin
    if Promotion.Message_Ready then
      return Promotion.Message;
    end if;
    return "";
  end Confirmation_Message;


  The_Actual_Project     : Text.String;
  The_Project_Name       : Text.String;
  The_Project_Directory  : Text.String;
  The_Language_Directory : Text.String;
  The_Source_Root        : Text.String;
  The_Binary_Root        : Text.String;
  The_Product_Directory  : Text.String;
  The_Product_Sub_Path   : Text.String;
  The_Promotion_Areas    : Text.String;
  The_Library_Path       : Text.String;

  The_Ignore_Areas       : String_List.Item;
  The_Implied_Areas      : String_List.Item;
  The_Reference_Areas    : String_List.Item;
  The_Libraries          : String_List.Item;
  The_Base_Path          : String_List.Item;
  The_Source_Directories : String_List.Item;
  The_Promotion_List     : String_List.Item;

  The_Library_Directories : Names.Map;
  The_Library_Names       : Names.Map;
  The_Library_Sources     : Names.Map;

  Project_Is_Defined      : Boolean := False;
  The_Case_Handling_Style : Case_Modification;

  type Gnat_Compiler is (GPL, GNATPRO, Unknown);
  type Compiler_Year is range 2016 .. 2099;

  Ada_Project_Path       : constant String := "ADA_PROJECT_PATH";
  Project_File_Extension : constant String := ".gpr";

  The_Gnat_Compiler : Gnat_Compiler;
  The_Compiler_Year : Compiler_Year;


  procedure Set_Project_Undefined is
  begin
    Text.Clear (The_Actual_Project);
    Text.Clear (The_Project_Name);
    Text.Clear (The_Project_Directory);
    Text.Clear (The_Language_Directory);
    Text.Clear (The_Source_Root);
    Text.Clear (The_Binary_Root);
    Text.Clear (The_Product_Directory);
    Text.Clear (The_Product_Sub_Path);
    Text.Clear (The_Promotion_Areas);
    Text.Clear (The_Library_Path);
    The_Ignore_Areas.Clear;
    The_Implied_Areas.Clear;
    The_Reference_Areas.Clear;
    The_Libraries.Clear;
    The_Base_Path.Clear;
    The_Source_Directories.Clear;
    The_Promotion_List.Clear;
    The_Library_Directories.Clear;
    The_Library_Names.Clear;
    The_Library_Sources.Clear;
    Project_Is_Defined := False;
  end Set_Project_Undefined;


  function Language_Directory return String is (Text.String_Of (The_Language_Directory));

  function Language_Folder return String is (Language_Directory & Files.Separator);

  function Definition_File return String is (Language_Folder & Server.Project_File);

  function Directory return String is (Text.String_Of (The_Project_Directory));

  function Folder return String is (Directory & Files.Separator);

  function Actual return String is (Text.String_Of (The_Actual_Project));

  function Name return String is (Text.String_Of (The_Project_Name));

  function Binary_Folder return String is (Text.String_Of (The_Binary_Root) & Files.Separator);

  function Source_Folder return String is (Text.String_Of (The_Source_Root) & Files.Separator);

  function Product_Directory return String is (Text.String_Of (The_Product_Directory));

  function Product_Sub_Path return String is (Text.String_Of (The_Product_Sub_Path));


  function Is_Defined return Boolean is
  begin
    Log.Write ("||| Project.Is_Defined: " & Project_Is_Defined'img);
    return Project_Is_Defined;
  end Is_Defined;


  The_Configuration_Handle : access Configuration.File_Handle;

  function Element_For (Key         : String;
                        Application : String) return String is

    Handle  : constant Configuration.Section_Handle := Configuration.Handle_For (The_Configuration_Handle.all,
                                                                                 Application);
    Element : constant String := Configuration.Value_Of (Handle, Key);

  begin
    return Element;
  end Element_For;


  function Is_Base_Area (Item : String) return Boolean is
  begin
    for Area of The_Implied_Areas loop
      if Item = Area then
        return True;
      end if;
    end loop;
    for Area of The_Reference_Areas loop
      if Item = Area then
        return True;
      end if;
    end loop;
    return False;
  end Is_Base_Area;


  function Is_To_Ignore (The_Directory : String) return Boolean is
  begin
    for Area of The_Ignore_Areas loop
      if Text.Is_Equal (The_Directory, Source_Folder & Area) then
        return True;
      end if;
    end loop;
    return False;
  end Is_To_Ignore;


  function Reference_Areas return String_List.Item is
  begin
    return The_Reference_Areas;
  end Reference_Areas;


  function Part_Of (Filename : String;
                    After    : String) return String is
  begin
    if Filename(Filename'first .. Filename'first + After'length - 1) = After then
      return Filename(Filename'first + After'length .. Filename'last);
    end if;
    return "";
  end Part_Of;


  function Target_Directory return String is
    Project_Area : constant String := Part_Of (Directory, After => Source_Folder);
  begin
    return Binary_Folder & Project_Area;
  end Target_Directory;


  function Target_Folder return String is (Target_Directory & Files.Separator);


  function Created_Target_Folder return String is
    The_Folder : constant String := Target_Folder;
  begin
    Files.Create_Folder (The_Folder);
    return The_Folder;
  end Created_Target_Folder;


  Object_Area : constant String := "objects";

  function Object_Directory return String is
  begin
    return Target_Folder & Object_Area;
  end Object_Directory;


  function Object_Folder return String is
  begin
    return Object_Directory & Files.Separator;
  end Object_Folder;


  Resource_Object_Name : constant String := "resources.o";

  function Resource_Object return String is (Object_Folder & Resource_Object_Name);

  function Resource_Filename return String is (Directory & Files.Separator & Name & ".rc");


  type Phase is (Unknown, Initializing, Promoting);

  The_Phase : Phase := Unknown;

  Error_Set : exception;

  procedure Set_Error (Message : String) with No_Return is
  begin
    case The_Phase is
    when Initializing =>
      Promotion.Set_Message (Server.Not_Confirmed & Message);
      raise Error_Set;
    when Promoting =>
      Promotion.Set_Error (Message);
    when Unknown =>
      raise Program_Error;
    end case;
  end Set_Error;


  function System_Drive return String is
    System_Drive_Variable : constant String := "SYSTEMDRIVE";
  begin
    if Ada.Environment_Variables.Exists (System_Drive_Variable) then
      return Ada.Environment_Variables.Value (System_Drive_Variable);
    else
      return "C:";
    end if;
  end System_Drive;


  function Tools_Directory return String is
  begin
    case The_Gnat_Compiler is
    when GPL =>
      return System_Drive & "\GNAT\" & Strings.Trimmed (The_Compiler_Year'img) & "\bin";
    when GNATPRO =>
      return System_Drive & "\GNATPRO\7.3.1\bin";
    when Unknown =>
      return System_Drive & "\GNAT\2017\bin";
    end case;
  end Tools_Directory;


  function Resource_Info return String is
  begin
    return " (resource: " & Resource_Filename & ")";
  end Resource_Info;


  procedure Check_Name (Library : String) is
    Gpr_File : constant String := Library & Project_File_Extension;
  begin
    if not Ada.Environment_Variables.Exists (Ada_Project_Path) then
      Set_Error (Ada_Project_Path & " missing for " & Gpr_File & Resource_Info);
    end if;
    declare
      Path_Value : constant String := Ada.Environment_Variables.Value (Ada_Project_Path);
      Path_List  : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Path_Value, Separator => ';'));
    begin
      for Path of Path_List loop
        if File.Exists (Path & Files.Separator & Gpr_File) then
          return;
        end if;
      end loop;
      Set_Error (Gpr_File & " not found in " & Ada_Project_Path & "=" & Path_Value & Resource_Info);
    end;
  end Check_Name;


  procedure Check_Compiler is
  begin
    if not File.Directory_Exists (Tools_Directory) then
      Set_Error ("Compiler tools directory """ & Tools_Directory & """ not found" & Resource_Info);
    end if;
  end Check_Compiler;


  procedure Define_Environment is
    Filename : constant String := Resource_Filename;
    The_File : Ada.Text_IO.File_Type;
  begin
    The_Gnat_Compiler := Unknown;
    The_Libraries.Clear;
    if File.Exists (Filename) then
      begin
        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
      exception
      when Item: others =>
        Log.Write (Item);
        return;
      end;
      begin
        while not Ada.Text_IO.End_Of_File (The_File) loop
          declare
            Line  : constant String := Ada.Text_IO.Get_Line (The_File);
            Items : constant Strings.Item
              := Strings.Purge_Of (Strings.Item_Of (Line, Separator => ' ', Symbols => ",;""\"));
          begin
            if Items.Count >= 7 then
              if Items(Strings.First_Index) = "VALUE" then
                declare
                  The_Index : Natural := Strings.First_Index;

                  function Next_Item return String is
                  begin
                    if The_Index >= Strings.First_Index + Items.Count - 1 then
                      return "";
                    end if;
                    The_Index := The_Index + 1;
                    return Items(The_Index);
                  end Next_Item;

                  procedure Get_Next (Item : String) is
                  begin
                    if Next_Item /= Item then
                      Set_Error (Item & " expected in " & Filename & " at line " & Strings.Trimmed (Line));
                    end if;
                  end Get_Next;

                  function Next_Resource return String is
                  begin
                    Get_Next ("""");
                    declare
                      Resource : constant String := Next_Item;
                    begin
                      Get_Next ("""");
                      return Resource;
                    end;
                  end Next_Resource;

                  procedure Adjust_Index is
                  begin
                    Get_Next (",");
                    Get_Next ("""");
                  end Adjust_Index;

                  Resource : constant String := Next_Resource;

                begin
                  if Resource = "ProductName" then
                    Adjust_Index;
                    declare
                      Compiler : constant String := Next_Item;
                    begin
                      if Compiler = "GPL" then
                        The_Gnat_Compiler := GPL;
                        declare
                          Year : constant String := Next_Item;
                        begin
                          The_Compiler_Year := Compiler_Year'value(Year);
                        exception
                        when others =>
                          Set_Error ("Unknown compiler: " & Compiler & " " & (if Year = "\"
                                                                             then "(year missing)"
                                                                             else Year) & " in " & Filename);
                        end;
                      elsif Compiler = "GNATPRO" then
                        The_Gnat_Compiler := GNATPRO;
                      else
                        Set_Error ("Unknown compiler: " & Compiler & " in " & Filename);
                      end if;
                    end;
                  elsif Resource = "Libraries" then
                    Adjust_Index;
                    The_Libraries.Clear;
                    loop
                      declare
                        Item : constant String := Next_Item;
                      begin
                        exit when Item in "\" | "";
                        if Item /= ";" then
                          The_Libraries.Append (Item);
                        end if;
                      end;
                    end loop;
                  end if;
                end;
              end if;
            end if;
          end;
        end loop;
        Ada.Text_IO.Close (The_File);
      exception
      when others =>
        Ada.Text_IO.Close (The_File);
        raise;
      end;
    end if;
    Check_Compiler;
    Text.Clear (The_Library_Path);
    for Library of The_Libraries loop
      if The_Library_Names.Is_Empty then
        Check_Name (Library);
      elsif not The_Library_Names.Contains (Library) then
        Set_Error ("Library id <" & Library & "> from " & Filename & " not found in " & Definition_File);
      else
        Text.Append_To (The_Library_Path, The_Library_Directories.Element(Library) & ";");
      end if;
      Log.Write ("||| Resource Library Id: " & Library);
    end loop;
  end Define_Environment;


  function Defined_Environment return Boolean is

    procedure Create_Object_Path is
    begin
      Files.Create_Folder (Object_Directory);
    exception
    when others =>
      null;
    end Create_Object_Path;

    Filename : constant String := Resource_Filename;

  begin -- Defined_Environment
    if File.Exists (Filename) then
      if not File.Exists (Resource_Object) or else File.Is_Newer (Filename, Resource_Object) then
        The_Phase := Promoting;
        Define_Environment;
        The_Phase := Unknown;
        begin
          File.Delete_Directory (Target_Directory);
        exception
        when others =>
          null; -- target directory was locked
        end;
        Create_Object_Path;
        return True;
      end if;
    end if;
    Create_Object_Path;
    return False;
  exception
  when others =>
    The_Phase := Unknown;
    raise;
  end Defined_Environment;


  procedure Create_Work_Area_For (Project_Parts :     Strings.Item;
                                  The_Work_Path : out String_List.Item) is

    function Part_Of (Index : Positive) return String is
    begin
      return Project_Parts(Index);
    end Part_Of;

    use type String_List.Item;

  begin -- Create_Work_Area_For
    The_Work_Path := The_Base_Path;
    String_List.Clear (The_Source_Directories);
    The_Project_Directory := The_Language_Directory;
    for Index in Strings.First_Index .. Project_Parts.Count - 1 loop
      declare
        Path : constant String := Ada_95.File.Normalized (Folder & Part_Of (Index));
      begin
        for System_Path of reverse The_Implied_Areas loop
          if Path = Source_Folder & System_Path then
            return;
          end if;
        end loop;
        for System_Path of reverse Reference_Areas loop
          if Path = Source_Folder & System_Path then
            return;
          end if;
        end loop;
        The_Project_Directory := Text.String_Of (Path);
        The_Source_Directories := Path + The_Source_Directories;
        The_Work_Path := Folder + The_Work_Path;
      end;
    end loop;
    for Area of The_Implied_Areas loop
      The_Source_Directories := The_Source_Directories + (Source_Folder & Area);
    end loop;
    The_Project_Name := Text.String_Of (Part_Of (Project_Parts.Count - 1));
    Text.Clear (The_Product_Sub_Path);
    for Index in Strings.First_Index .. Project_Parts.Count - 2 loop
      declare
        The_Part : constant String := Part_Of (Index);
      begin
        if Text.Is_Equal (The_Part, The_Project_Name) then
          declare
            The_Items : constant Strings.Item := Strings.Item_Of (List => Project_Parts,
                                                                  Selection => (First => Index,
                                                                                Last  => Project_Parts.Count - 2));
            The_Text : constant String := "\" & Strings.Data_Of (The_Items, Separator => "\");
          begin
            The_Product_Sub_Path := Text.String_Of (Ada_95.File.Normalized_Folder (The_Text));
          end;
        end if;
      end;
    end loop;
    Define_Environment;
    for Library of The_Libraries loop
      declare
        Source_Directory : constant String := The_Library_Sources.Element(Library);
      begin
        The_Reference_Areas.Append (Source_Directory);
        The_Work_Path.Append (Source_Directory & Files.Separator);
      end;
    end loop;

  end Create_Work_Area_For;


  procedure Initialize_Project (The_Work_Path : String_List.Item) is
  begin
    Ada_95.Project.Initialize (The_Work_Path);
  end Initialize_Project;


  function Application_From (The_Directory : String) return String is
    Module : constant String := Files.Module_Of (The_Directory);
  begin
    return The_Directory & Files.Separator & Module & ".adb";
  end Application_From;


  function Initialized (Filename : String) return Boolean is

    function Element_Of (Key         : String;
                         Application : String;
                         Must_Exist  : Boolean := True) return String is

      Element : constant String := Element_For (Key, Application);

    begin
      if Must_Exist and Element = "" then
        Set_Error ("<[" & Application & "] " & Key & "> undefined in " & Definition_File);
      end if;
      return Element;
    end Element_Of;


    procedure Define_Location (The_Item    : out Text.String;
                               Key         :     String;
                               Application :     String;
                               Root        :     String := "") is

      Location      : constant String := Element_Of (Application => Application, Key => Key);
      The_Directory : constant String := Root & Location;

    begin
      if not File.Directory_Exists (The_Directory) then
        Set_Error (Application & " " & Key & " <" & The_Directory & "> unknown");
      end if;
      The_Item := Text.String_Of (Ada_95.File.Normalized (Location));
    end Define_Location;


    procedure Define_Source_Path (Path_Name  :        String;
                                  The_Areas  : out    String_List.Item;
                                  The_Path   : in out String_List.Item;
                                  Must_Exist :        Boolean := True) is

      Source_Path : constant String := Element_Of (Key         => Path_Name,
                                                   Application => "Source",
                                                   Must_Exist  => Must_Exist);
      Areas : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Source_Path, ';'));

      use type String_List.Item;

    begin
      for Index in Strings.First_Index .. Areas.Count loop
        declare
          Area          : constant String := Strings.Trimmed (Areas(Index));
          The_Directory : constant String := Source_Folder & Area;
        begin
          if not File.Directory_Exists (The_Directory) then
            Set_Error ("Unknown source directory <" & The_Directory & ">");
          end if;
          The_Areas := The_Areas + Ada_95.File.Normalized (Area);
          The_Path := The_Path + Ada_95.File.Normalized_Folder (The_Directory);
        end;
      end loop;
    end Define_Source_Path;


    type Gpr_Information is record
      Project_Name : Text.String;
      Source_Path  : Text.String;
    end record;

    function Gpr_Information_Of (The_Gpr_Filename : String) return Gpr_Information is

      The_File : Ada.Text_IO.File_Type;
      The_Gpr  : Gpr_Information;

      Gpr_Directory : constant String := File.Containing_Directory_Of (The_Gpr_Filename);

      procedure Parse_Gpr is
        The_Tokens : String_List.Item;

        function Next_Token return String is
        begin
          while The_Tokens.Is_Empty loop
            if Ada.Text_IO.End_Of_File (The_File) then
              return "";
            end if;
            declare
              Line   : constant String := Ada.Text_IO.Get_Line (The_File);
              Tokens : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Line,
                                                                                   Separator => ' ',
                                                                                   Symbols=>")(;"));
            begin
              The_Tokens := Strings.List_Of (Tokens);
            end;
          end loop;
          return Unused : constant String := The_Tokens.First_Element do
            The_Tokens.Delete_First;
          end return;
        exception
        when Item: others =>
          Log.Write (Item);
          return "";
        end Next_Token;

      begin -- Parse_Gpr
        loop
          declare
            Token : constant String := Next_Token;
          begin
            exit when Token = "";
            if (Token = "library" and then Next_Token = "project") or Token = "project" then
              The_Gpr.Project_Name := Text.String_Of (Next_Token);
            elsif Token = "for" and then Next_Token = "Source_Dirs" and then
              Next_Token = "use" and then Next_Token = "("
            then
              declare
                Source_Path : constant String := Text.Trimmed (Next_Token, '"');
              begin
                The_Gpr.Source_Path := Text.String_Of (File.Full_Name_Of (Name_Or_Directory => Source_Path,
                                                                          Current_Directory => Gpr_Directory));
              exception
              when others =>
                The_Gpr.Source_Path := Text.String_Of (Source_Path); -- use original to show in error message
              end;
              exit;
            end if;
          end;
        end loop;
      end Parse_Gpr;

    begin -- Gpr_Information_Of
      begin
        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, The_Gpr_Filename);
      exception
      when Item: others =>
        Log.Write (Item);
        return The_Gpr;
      end;
      Parse_Gpr;
      Ada.Text_IO.Close (The_File);
      return The_Gpr;
    end Gpr_Information_Of;


    procedure Define_Libraries is
      List : constant String := Element_Of (Application => "Library", Key => "List", Must_Exist => False);
    begin
      if List /= "" then
        declare
          Library_Ids : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Strings.Purge_Of (List), ','));
        begin
          for Library of Library_Ids loop
            declare
              Gpr_Name : constant String := Element_Of (Application => "Library", Key => Library, Must_Exist => True);
              Gpr_File : constant String := File.Name_Of (Gpr_Name, Project_File_Extension);
            begin
              if not File.Exists(Gpr_File) then
                Set_Error ("File " & Gpr_File & " not found for " & Library & " in " & Definition_File);
              end if;
              declare
                Gpr              : constant Gpr_Information := Gpr_Information_Of (Gpr_File);
                Gpr_Directory    : constant String := File.Containing_Directory_Of (Gpr_File);
                Gpr_Project_Name : constant String := Text.String_Of (Gpr.Project_Name);
                Gpr_Source_Path  : constant String := Text.String_Of (Gpr.Source_Path);
              begin
                if Gpr_Project_Name = "" then
                  Set_Error ("Library project name for " & Library & " not found in " & Gpr_File);
                end if;
                if Gpr_Source_Path = "" then
                  Set_Error ("Library project source directory for " & Library & " not found in " & Gpr_File);
                elsif not File.Directory_Exists (Gpr_Source_Path) then
                  Set_Error (Gpr_Source_Path & " for " & Library & " not found in " & Gpr_File);
                end if;
                if The_Library_Names.Contains (Library) then
                  Set_Error ("Library " & Library & " defined twice in " & Definition_File);
                end if;
                The_Library_Names.Insert (Key => Library, New_Item => Gpr_Project_Name);
                The_Library_Directories.Insert (Key => Library, New_Item => Gpr_Directory);
                The_Library_Sources.Insert (Key => Library, New_Item => Gpr_Source_Path);
                Log.Write ("||| Library " & Library & " - Location: " & Gpr_Directory
                                                    & " - Name: " & Gpr_Project_Name
                                                    & " - Source: " & Gpr_Source_Path);
              end;
            end;
          end loop;
        end;
      end if;
    end Define_Libraries;

    Project_Parts : constant Strings.Item := Files.Project_Parts_Of (Filename, Language, The_Language_Directory);

    The_Work_Path : String_List.Item;

  begin -- Initialized
    Log.Write ("||| Project.Initialize: " & Filename);
    The_Actual_Project := Text.String_Of (Filename);
    The_Phase := Initializing;
    if The_Configuration_Handle = null then -- only first time because language directory does not change
      The_Configuration_Handle := new Configuration.File_Handle'(Configuration.Handle_For (Definition_File));
    end if;
    if Text.Is_Null (The_Language_Directory) then
      Set_Error ("Unknown " & Language & " Project for " & Filename);
    elsif not File.Exists (Definition_File) then
      Set_Error ("Project definition file missing: " & Definition_File);
    elsif Project_Parts.Count < 2 then
      Set_Error ("Unknown project for " & Filename);
    else
      declare
        Leaf_Directory : constant String := Files.Directory_Of (Filename);
      begin
        if not File.Is_Leaf_Directory (Leaf_Directory) then
          Set_Error ("Not in project area " & Filename);
        else
          if not File.Exists (Application_From (Leaf_Directory)) then
            Set_Error ("No application in " & Leaf_Directory);
          end if;
        end if;
      end;
    end if;
    Define_Location (The_Source_Root, Key => "Root", Application => "Source");
    Define_Source_Path ("Ignore", The_Ignore_Areas, The_Path => The_Base_Path, Must_Exist => False);
    Define_Source_Path ("Path", The_Implied_Areas, The_Path => The_Base_Path);
    Define_Source_Path ("Reference", The_Reference_Areas, The_Path => The_Base_Path);
    Define_Libraries;
    Create_Work_Area_For (Project_Parts, The_Work_Path);
    Define_Location (The_Binary_Root, Key => "Root", Application => "Binary");
    Define_Location (The_Product_Directory, Key => "Location", Application => "Product");
    declare
      Case_Update : constant String := Element_Of (Key => "Case_Update", Application => "Style", Must_Exist => False);
      Token_Kind  : constant String := Strings.Legible_Of (Case_Update);
    begin
      if Token_Kind = "None" then
        The_Case_Handling_Style := No_Change;
      elsif Token_Kind = "Keywords" then
        The_Case_Handling_Style := Keywords;
      else
        The_Case_Handling_Style := Change_All;
      end if;
    end;
    ----------------------------------------
    for The_Folder of The_Work_Path loop
      Log.Write ("||| Folder: " & The_Folder);
    end loop;
    for Area of The_Implied_Areas loop
      Log.Write ("||| Implied: " & Area);
    end loop;
    for Area of The_Reference_Areas loop
      Log.Write ("||| Reference: " & Area);
    end loop;
    for Area of The_Ignore_Areas loop
      Log.Write ("||| Ignore: " & Area);
    end loop;
    ----------------------------------------
    Initialize_Project (The_Work_Path);
    Set_Confirmation (Language & " project " & Name & " opened");
    Project_Is_Defined := True;
    The_Phase := Unknown;
    return True;
  exception
  when Error_Set =>
    Set_Project_Undefined;
    The_Phase := Unknown;
    return False;
  end Initialized;


  procedure Change_To (Filename : String) is

    The_Directory : Text.String;

    Project_Parts : constant Strings.Item := Files.Project_Parts_Of (Filename, Language, The_Directory);

    The_Work_Path : String_List.Item;

  begin
    if not Text.Is_Equal (The_Directory, The_Language_Directory) then
      raise Program_Error;
    end if;
    The_Phase := Promoting;
    Create_Work_Area_For (Project_Parts, The_Work_Path);
    -----------------------------------------------
    Log.Write ("||| Project.Change_To: " & Filename);
    for The_Folder of The_Work_Path loop
      Log.Write ("||| Folder: " & The_Folder);
    end loop;
    -----------------------------------------------
    Ada_95.Project.Finalize;
    Initialize_Project (The_Work_Path);
    Promotion.Set_Message ("Project changed to " & Filename);
    The_Phase := Unknown;
  end Change_To;


  function Is_Source (Filename : String) return Boolean is
    Is_In_Project : constant Boolean := Ada_95.File.Is_In_Project (Filename);
  begin
    Log.Write ("||| Project.Is_Source " & Filename & " => " & Is_In_Project'img);
    return Is_In_Project;
  end Is_Source;


  function Is_In_Reference_Area (Filename : String) return Boolean is
    The_Directory : constant String := Files.Directory_Of (Filename);
  begin
    for Area of Reference_Areas loop
      if Text.Is_Equal (The_Directory, Source_Folder & Area) then
        return True;
      elsif Text.Is_Equal (The_Directory, Area) then
        return True;
      end if;
    end loop;
    return False;
  end Is_In_Reference_Area;


  function Program_Unit_Name return String is (Name & ".adb");


  function Program_Unit return String is (Folder & Program_Unit_Name);


  function Is_Program_Unit (Filename : String) return Boolean is
  begin
    return Text.Is_Equal (Filename, Program_Unit);
  end Is_Program_Unit;


  function Case_Handling_Style return Case_Modification is (The_Case_Handling_Style);


  function Tools_Folder return String is
  begin
    return Tools_Directory & Files.Separator;
  end Tools_Folder;


  function Environment return String is

    The_Environment : Text.String;

    function New_Path return String is
    begin
      return Tools_Directory & ";" & Ada.Environment_Variables.Value ("Path");
    end New_Path;

    procedure Add (Id    : String;
                   Value : String) is
    begin
      if Text.Is_Equal (Id, "path") then
        Text.Append_To (The_Environment, Id & '=' & New_Path & Ascii.Nul);
      elsif Text.Is_Null (The_Library_Path) then
        Text.Append_To (The_Environment, Id & '=' & Value & Ascii.Nul);
      elsif Text.Is_Equal (Id, "gpr_project_path") then
        Log.Write ("||| " & Id & " ignored");
      elsif Text.Is_Equal (Id, Ada_Project_Path) then
        null;
      else
        Text.Append_To (The_Environment, Id & '=' & Value & Ascii.Nul);
      end if;
    end Add;

  begin -- Environment
    Ada.Environment_Variables.Iterate (Add'access);
    if not Text.Is_Null (The_Library_Path) then
      declare
        Ada_Path : constant String := Ada_Project_Path & "=" & Text.String_Of (The_Library_Path);
      begin
        Log.Write ("||| Environment: " & Ada_Path);
        Text.Append_To (The_Environment, Ada_Path & Ascii.Nul);
      end;
    end if;
    return Text.String_Of (The_Environment);
  end Environment;


  Gpr_Is_Generated : Boolean := False;

  function Gpr_File_Is_Generated return Boolean is (Gpr_Is_Generated);


  function Gpr_Filename return String is

    Project_Name   : constant String := Name;
    Interface_Name : constant String := Name & "_Interface";
    Gpr_Name       : constant String := Project_Name & Project_File_Extension;


    function Has_Interface_In (The_Path : String) return Boolean is
      Interface_Source     : constant String := The_Path & Files.Separator & Interface_Name & ".ads";
      Containing_Directory : constant String := File.Containing_Directory_Of (The_Path);
    begin
      if Containing_Directory /= Language_Directory then
        if File.Exists (Interface_Source) then
          return True;
        end if;
        return Has_Interface_In (Containing_Directory);
      end if;
      return False;
    end Has_Interface_In;

    Is_Dll : constant Boolean := Has_Interface_In (Folder);

    Source   : constant String := Folder & Gpr_Name;
    Filename : constant String := Created_Target_Folder & Gpr_Name;

    The_File : Ada.Text_IO.File_Type;

    procedure Put (Line : String) is
    begin
      Ada.Text_IO.Put_Line (The_File, Line);
    end Put;

    function Name_Of (The_Library : String) return String is
    begin
      if The_Library_Names.Is_Empty then
        return The_Library;
      else
        return The_Library_Names.Element (The_Library);
      end if;
    end Name_Of;

  begin -- Gpr_Filename
    Gpr_Is_Generated := False;
    if Project_Name = "" then
      return "";
    end if;
    if File.Exists (Source) then
      File.Delete (Filename);
      return Source;
    elsif not File.Exists (Filename) then
      Gpr_Is_Generated := True;
      Ada.Text_IO.Create (The_File, Mode => Ada.Text_IO.Out_File, Name => Filename);
      if not Is_Dll and then not The_Libraries.Is_Empty then
        for Library of The_Libraries loop
          Put ("with """ & Name_Of (Library) & """;");
        end loop;
        Put ("");
      end if;
      Put ("project " & Project_Name & " is");
      Put ("");
      Put ("   package Naming is");
      Put ("      for Casing use ""mixedcase"";");
      Put ("   end Naming;");
      Put ("");
      if Is_Dll then
        Put ("   for Library_Name use """ & Project_Name & """;");
        Put ("   for Shared_Library_Prefix use """";");
        Put ("");
      end if;
      for The_Directory of The_Source_Directories loop
        if The_Directory = The_Source_Directories.First_Element then
          Put ("   for Source_Dirs use (""" & The_Directory & """,");
        elsif The_Directory = The_Source_Directories.Last_Element then
          Put ("                        """ & The_Directory & """);");
        else
          Put ("                        """ & The_Directory & """,");
        end if;
      end loop;
      Put ("");
      if Is_Dll then
        Put ("   for Library_Interface use (""" & Interface_Name & """);");
        Put ("");
      end if;
      Put ("   for Object_Dir use """ & Object_Area & """;");
      Put ("");
      if Is_Dll then
        Put ("   for Library_Options use (""-L" & Product_Directory & """, ""resources.o"");");
        Put ("   for Library_Dir use """ & Product_Directory & Product_Sub_Path & """;");
        Put ("   for Library_Ali_Dir use """ & Target_Directory & """;");
        Put ("   for Library_Kind use ""dynamic"";");
        Put ("   for Library_Standalone use ""encapsulated"";");
      else
        Put ("   for Exec_Dir use """ & Product_Directory & Product_Sub_Path & """;");
        Put ("   for Main use (""" & Program_Unit_Name & """);");
      end if;
      Put ("");
      Put ("   package Pretty_Printer is");
      Put ("      for Default_Switches (""ada"") use (""-i2"", ""-M0"", ""-aL"", ""-A1"", ""-A4"");");
      Put ("   end Pretty_Printer;");
      Put ("");
      Put ("   package Builder is");
      Put ("      for Default_Switches (""ada"") use (""-s"", ""-g"");");
      if not Is_Dll then
        Put ("      for Executable (""" & Program_Unit_Name & """) use """ & Project_Name & """;");
      end if;
      Put ("   end Builder;");
      Put ("");
      Put ("   package Compiler is");
      Put ("      for Default_Switches (""ada"") use");
      Put ("         (""-O1"", ""-gnatQ"", ""-gnata"", ""-gnato"", ""-g"", ""-gnat12"",");
      Put ("          ""-gnatwceGhijkmopruvz.c.N.p.t.w.x"", ""-gnatykmpM120"");");
      Put ("   end Compiler;");
      Put ("");
      Put ("   package Binder is");
      Put ("      for Default_Switches (""ada"") use (""-E"");");
      Put ("   end Binder;");
      Put ("");
      if not Is_Dll then
        Put ("   package Linker is");
        Put ("      for Linker_Options use ();");
        Put ("      for Default_Switches (""ada"") use");
        Put ("         (""-g"", ""-L" & Product_Directory & """,");
        Put ("          """ & Resource_Object & """,""-m" & Ada_95.Project.Application_Kind_Image &""");");
        Put ("   end Linker;");
        Put ("");
      end if;
      Put ("end " & Project_Name & ";");
      Ada.Text_IO.Close (The_File);
    end if;
    return Filename;
  end Gpr_Filename;


  function Full_Name_Of (Filename : String) return String  is
  begin
    return Ada_95.File.Full_Name_Of (Filename);
  end Full_Name_Of;


  function Found_In_Directory (The_Directory : String;
                               Area          : String) return String is

    package FS renames Ada.Directories;

    Not_Found : exception;

    The_Handle : FS.Search_Type;
    The_Entry  : FS.Directory_Entry_Type;

  begin
    FS.Start_Search (Search    => The_Handle,
                     Directory => The_Directory,
                     Pattern   => "",
                     Filter    => (FS.Directory => True, others => False));
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Entry_Name     : constant String := FS.Simple_Name (The_Entry);
        Directory_Name : constant String := FS.Full_Name (The_Entry);
      begin
        if Strings.Is_Equal (Entry_Name, Area) then
          return Directory_Name;
        end if;
      end;
    end loop;
    FS.Start_Search (Search    => The_Handle,
                     Directory => The_Directory,
                     Pattern   => "",
                     Filter    => (FS.Directory => True, others => False));
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Entry_Name     : constant String := FS.Simple_Name (The_Entry);
        Directory_Name : constant String := FS.Full_Name (The_Entry);
      begin
        if Entry_Name(Entry_Name'first) = '.' then
          null;
        else
          begin
            return Found_In_Directory (The_Directory => Directory_Name,
                                       Area          => Area);
          exception
          when others =>
            null;
          end;
        end if;
      end;
    end loop;
    raise Not_Found;
  end Found_In_Directory;


  function Promotion_Areas return String is (Text.String_Of (The_Promotion_Areas));

  function Promotion_List return String_List.Item is

    use type String_List.Item;

    Area_List : constant String := Element_For (Key         => "All",
                                                Application => "Promote");

    Areas : constant Strings.Item := Strings.Purge_Of (Strings.Item_Of (Area_List, '+', "-"));

    Remove_Area : Boolean := False;

    Header : constant String := "[Promote] All = " & Area_List & " -> ";

  begin -- Define_Promotion_List
    Text.Clear (The_Promotion_Areas);
    The_Promotion_List.Clear;
    for Index in Strings.First_Index .. Areas.Count loop
      declare
        Item : constant String := Strings.Trimmed (Areas(Index));
      begin
        if Item = "-" then
          Remove_Area := True;
        else
          declare
            Area : constant String := Item;

            function Location return String is
            begin
              return Found_In_Directory (Language_Directory, Area);
            end Location;

            procedure Append_Project (The_Directory : String) is
            begin
              if not Is_To_Ignore (The_Directory) then
                declare
                  Application : constant String := Application_From (The_Directory);
                begin
                  if Application /= "" and then File.Exists (Application) then
                    if Remove_Area then
                      begin
                        The_Promotion_List := The_Promotion_List - Application;
                      exception
                      when others =>
                        Promotion.Set_Error (Header & Application & " NOT removable!");
                      end;
                    else
                      The_Promotion_List := The_Promotion_List + Application;
                    end if;
                  end if;
                end;
              end if;
            end Append_Project;

          begin
            if Is_Base_Area (Area) then
              Promotion.Set_Error (Header & Area & " NOT allowed!");
            end if;
            begin
              File.Iterate_Over_Leaf_Directories (Location, Append_Project'access);
            exception
            when others =>
              Promotion.Set_Error (Header & Area & " unknown!");
            end;
          end;
          Remove_Area := False;
        end if;
      end;
    end loop;
    for The_Project of The_Promotion_List loop
      Log.Write ("||| Project: " & The_Project);
    end loop;
    The_Promotion_Areas := Text.String_Of (Ada_95.File.Normalized (Area_List));
    return The_Promotion_List;
  end Promotion_List;


  function Must_Be_Build_First (Filename : String) return Boolean is
    The_Filename : constant String := Files.Folder_Of (Filename) & Files.Module_Of (Filename) & Project_File_Extension;
  begin
    return File.Exists (The_Filename);
  end Must_Be_Build_First;


  function Run return Boolean is
  begin
    return Target.Run (Product_Directory & "\" & Name & ".exe", "");
  end Run;


  procedure Finalize is
  begin
    Log.Write ("||| Project.Finalize");
    Ada_95.Project.Finalize;
    Set_Project_Undefined;
  end Finalize;

end Project;
