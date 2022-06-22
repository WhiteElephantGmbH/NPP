-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Project;
with Ada_95.Token;
with Build;
with File;
with Files;
with Log;
with Os.Process;
with Project.Gpr;
with Project.Resource;
with Promotion;
with Server;
with Strings;
with Text;

package body Target is

  procedure Log_Execution (Item : String) is
  begin
    Log.Write ("    -> " & Item);
  end Log_Execution;


  procedure Log_Error (Item : String) is
  begin
    Log.Write ("   ERROR OUTPUT: <" & Item & ">");
  end Log_Error;


  procedure Check_For_Parser_Errors_In (Filename : String) is

    The_Error_Token : Ada_95.Token.Handle;

    function Error_Image return String is
    begin
      return Strings.Legible_Of (Ada_95.Project.Error_Kind'image(Ada_95.Project.Error_Kind_Of(The_Error_Token)));
    end Error_Image;

    use type Ada_95.Token.Handle;

  begin -- Check_For_Parser_Errors_In
    The_Error_Token := Ada_95.Project.First_Error_Of (Filename);
    if The_Error_Token /= null then
      Promotion.Set_Error (Item      => Error_Image,
                           File      => Ada_95.Token.Filename_Of (The_Error_Token),
                           At_Line   => Ada_95.Token.Line_Number_Of (The_Error_Token),
                           At_Column => Ada_95.Token.Column_Position_Of (The_Error_Token));
    end if;
  end Check_For_Parser_Errors_In;


  procedure Generate_Resource_Object is

    Windres : constant String := Project.Tools_Folder & "windres.exe";

    The_Resource_Name : Text.String := Text.String_Of (Project.Name);

    The_Source_Directory : Text.String := Text.String_Of (Project.Directory);

    function Resource_Name return String is
    begin
      return Text.String_Of (The_Resource_Name);
    end Resource_Name;

    function Source_Directory return String is
    begin
      return Text.String_Of (The_Source_Directory);
    end Source_Directory;

    function Resource_Filename return String is
    begin
      if Build.Is_Defined then
        return Project.Resource.Filename;
      else
        return Source_Directory & Files.Separator & Resource_Name & Project.Resource.Extension;
      end if;
    end Resource_Filename;

    function Parameters return String is
    begin
      return "-i " & Resource_Filename & " --codepage=65001 --output-format=coff -o " & Project.Resource.Object;
    end Parameters;

    function Changed_To_Parent_Resource return Boolean is
      Parent_Directory : constant String := Files.Directory_Of (Source_Directory);
    begin
      if Text.Is_Equal (Parent_Directory, Project.Language_Directory) or else
        not File.Directory_Exists (Parent_Directory)
      then
        if Project.Gpr.File_Is_Generated then
          Log.Write ("Target.Generate_Resource_Object - no shared resource <" & Resource_Filename & ">");
          Promotion.Set_Error ("Unknown Resource File");
        else
          return False;
        end if;
      end if;
      The_Source_Directory := Text.String_Of (Parent_Directory);
      The_Resource_Name := Text.String_Of (Files.Name_Of (Source_Directory));
      return False;
    end Changed_To_Parent_Resource;

  begin -- Generate_Resource_Object
    if not Build.Is_Defined then
      while not File.Exists (Resource_Filename) loop
        if not Changed_To_Parent_Resource then
          return;
        end if;
      end loop;
    end if;
    Log_Execution (Windres & " " & Parameters);
    declare
      Result : constant String := Os.Process.Execution_Of (Executable     => Windres,
                                                           Parameters     => Parameters,
                                                           Current_Folder => Source_Directory);
      Error_Text : constant String := Strings.Trimmed (Result);
    begin
      if Error_Text /= "" then
        Log_Error (Error_Text);
        Promotion.Set_Error (Error_Text);
        return;
      end if;
    end;
  exception
  when Promotion.Error =>
    raise;
  when Os.Process.Execution_Failed =>
    Promotion.Set_Error ("Resource generation not executed");
  when Item: others =>
    Log.Write ("Target.Generate_Resource_Object", Item);
    Promotion.Set_Error ("Resource generation failed");
  end Generate_Resource_Object;


  procedure Gpr_Execute (Parameters : String) is

    Gpr_Build : constant String := Project.Tools_Folder & "gprbuild";

    Product_Location : constant String := Project.Product_Directory & Project.Product_Sub_Path;

    Gpr_Parameters : constant String := " -j0 -P" & Project.Gpr.Filename &
                                        " -XBinary_Root=" & Project.Binary_Folder &
                                        " -XProduct_Location=" & Product_Location &
                                        " -XSource_Root=" & Project.Source_Folder &
                                        " " & Parameters;

    End_Of_Text : constant String := "" & Ascii.Nul;

  begin -- Gpr_Execute
    if not Project.Tools_Defined then
      Promotion.Set_Error ("GNAT tools directory undefined");
    end if;
    Log_Execution (Gpr_Build & Gpr_Parameters);
    File.Create_Directory (Product_Location);
    declare
      Result : constant String := Os.Process.Execution_Of (Executable     => Gpr_Build,
                                                           Parameters     => Gpr_Parameters,
                                                           Environment    => Project.Environment,
                                                           Current_Folder => Project.Created_Target_Folder);
      Result_Text : constant String := Strings.Trimmed (Result);
    begin
      if Result_Text = "" then
        return;
      end if;
      Log.Write ("   OUTPUT: <" & Result_Text & ">");
      if Text.Location_Of ("Memory region", Result_Text, Result_Text'first) = Result_Text'first then
        return;
      end if;
      declare
        First_Index : Natural := Result_Text'first;
        The_Index   : Natural := Result_Text'first;

        function Next_String_Until (Separator : String) return String is
        begin
          if Separator = End_Of_Text then
            The_Index := Result_Text'last + 1;
          else
            The_Index := Text.Location_Of (Separator, Result_Text, First_Index);
          end if;
          declare
            The_Text : constant String := Result_Text(First_Index .. The_Index - 1);
          begin
            First_Index := The_Index + Separator'length;
            return The_Text;
          end;
        end Next_String_Until;

        Separator : constant String := ":";
        Filename  : constant String := Project.Full_Name_Of (Next_String_Until (Separator));
      begin
        if File.Exists (Filename) then
          declare
            Line   : constant Server.Line_Number  := Server.Line_Number'value(Next_String_Until (Separator));
            Column : constant Server.Column_Range := Server.Column_Range'value(Next_String_Until (Separator));
          begin
            Promotion.Set_Error (Item      => Next_String_Until (End_Of_Text),
                                 File      => Filename,
                                 At_Line   => Line,
                                 At_Column => Column);
          end;
        else
          Promotion.Set_Error (Result_Text);
        end if;
      exception
      when Promotion.Error =>
        raise;
      when Item: others =>
        Log.Write ("!!! Gnat.Gpr_Execute - Error Parser", Item);
      end;
    end;
  exception
  when Promotion.Error =>
    raise;
  when Os.Process.Execution_Failed =>
    Promotion.Set_Error ("GNAT GPR not executed");
  when Item: others =>
    Log.Write ("Gnat.Gpr_Execute", Item);
    Promotion.Set_Error ("GNAT GPR execution failed");
  end Gpr_Execute;


  procedure Compile (Filename : String) is
  begin
    if Strings.Lowercase_Of(Filename(Filename'last - 2 .. Filename'last)) = "ads" then
      Promotion.Set_Message ("Check " & Filename);
      --TEST------------------------------------
      Log_Execution ("Check: " & Filename);
      ------------------------------------------
      Gpr_Execute ("-ws -c -gnatc -u -q -eS " & Filename);
      Check_For_Parser_Errors_In (Filename);
      Promotion.Set_Message ("Semantic check successfully completed");
    else
      Promotion.Set_Message ("Compile " & Filename);
      --TEST--------------------------------------
      Log_Execution ("Compile: " & Filename);
      --------------------------------------------
      Gpr_Execute ("-ws -c -u -q -eS " & Filename);
      Check_For_Parser_Errors_In (Filename);
    end if;
  end Compile;


  procedure Build (Filename : String) is

  begin
    Promotion.Set_Message ("Build " & Project.Tools_Kind & Filename);
    Check_For_Parser_Errors_In (Filename);
    Gpr_Execute ("-XLIBRARY_TYPE=static -eS -q");
  exception
  when Promotion.Error =>
    raise;
  when Os.Process.Execution_Failed =>
    Promotion.Set_Error ("Gnat Builder not executed");
  when Item: others =>
    Log.Write ("Gnat.Build", Item);
    Promotion.Set_Error ("Unknown Build result", Filename);
  end Build;


  procedure Modifier_Handling is
    Modifier : constant String := Project.Modifier_Tool;
  begin
    if Modifier /= "" then
      Log_Execution ("Modify: " & Project.Product);
      declare
        Result : constant String := Os.Process.Execution_Of (Executable => Modifier,
                                                             Parameters => Project.Modifier_Parameters);
      begin
        if Result /= "" then
          Log_Execution ("Modify Result: " & Result);
        end if;
      end;
    end if;
  end Modifier_Handling;


  procedure Promote (Filename : String) is

    procedure Promote is
    begin
      if Project.Is_Program_Unit (Filename) then
        if Project.Has_New_Resource then
          Generate_Resource_Object;
        else
          Project.Define_Environment;
        end if;
        Build (Filename);
        Modifier_Handling;
      else
        if Project.Is_Maching (Filename) then
          Compile (Filename);
        end if;
      end if;
    end Promote;

  begin
    --TEST--------------------------------
    Log.Write ("&&& Promote " & Filename);
    --------------------------------------
    Promote;
    if Project.Has_Second_Compiler then
      Promote;
      Project.Set_Back_To_First_Compiler;
    end if;
    if Project.Is_Program_Unit (Filename) then
      Promotion.Define_Next_Message_Color (Promotion.Green);
    end if;
  end Promote;


  function Run (Executable : String;
                Parameters : String) return Boolean is
  begin
    --TEST---------------------------------------------------
    Log.Write ("&&& Run " & Executable & " " & Parameters);
    ---------------------------------------------------------
    Os.Process.Create (Executable => Executable,
                       Parameters => Parameters,
                       Console    => Os.Process.Invisible);
    return True;
  exception
  when others =>
    return False;
  end Run;

end Target;
