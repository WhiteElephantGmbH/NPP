-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Build;
with File;
with Files;
with Log;
with Project.Resource;
with Strings;

package body Project.Gpr is

  Is_Generated : Boolean := False;

  function File_Is_Generated return Boolean is (Is_Generated);


  function Filename return String is

    Project_Name : constant String := Name;
    Gpr_Name     : constant String := Project_Name & File_Extension;

    Legacy_Interface_Name : constant String := Name & "_Interface";

    function Interface_Name return String is
    begin
      if Build.Is_Defined then
        return Build.Actual_Interface;
      else
        return Legacy_Interface_Name;
      end if;
    end Interface_Name;

    function Has_Legacy_Interface_In (The_Path : String) return Boolean is
      Interface_Source     : constant String := The_Path & Files.Separator & Legacy_Interface_Name & ".ads";
      Containing_Directory : constant String := File.Containing_Directory_Of (The_Path);
    begin
      if Containing_Directory /= Language_Directory then
        if File.Exists (Interface_Source) then
          return True;
        end if;
        return Has_Legacy_Interface_In (Containing_Directory);
      end if;
      return False;
    end Has_Legacy_Interface_In;

    function Project_Is_Dll return Boolean is
    begin
      if Build.Is_Defined then
        return Build.Is_Dll;
      else
        return Has_Legacy_Interface_In (Folder);
      end if;
    end Project_Is_Dll;

    Is_Dll : constant Boolean := Project_Is_Dll;

    Source       : constant String := Folder & Gpr_Name;
    Gpr_Filename : constant String := Created_Target_Folder & Gpr_Name;

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

  begin -- Filename
    Is_Generated := False;
    if Project_Name = "" then
      return "";
    end if;
    if File.Exists (Source) then
      File.Delete (Gpr_Filename);
      return Source;
    elsif not File.Exists (Gpr_Filename) then
      Is_Generated := True;
      Ada.Text_IO.Create (The_File, Mode => Ada.Text_IO.Out_File, Name => Gpr_Filename);
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
        Put ("          """ & Resource.Object & """,""-m" & Build.Application_Kind_Image &""");");
        Put ("   end Linker;");
        Put ("");
      end if;
      Put ("end " & Project_Name & ";");
      Ada.Text_IO.Close (The_File);
    end if;
    return Gpr_Filename;
  end Filename;


  function Information_Of (The_Filename : String) return Information is

    The_File : Ada.Text_IO.File_Type;
    The_Gpr  : Information;

    Gpr_Directory : constant String := File.Containing_Directory_Of (The_Filename);

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

  begin -- Information_Of
    begin
      Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, The_Filename);
    exception
    when Item: others =>
      Log.Write (Item);
      return The_Gpr;
    end;
    Parse_Gpr;
    Ada.Text_IO.Close (The_File);
    return The_Gpr;
  end Information_Of;

end Project.Gpr;