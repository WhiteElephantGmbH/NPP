-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Build;
with File;
with Log;
with Project.Resource;

package body Project.Gpr is

  Is_Generated : Boolean := False;

  function File_Is_Generated return Boolean is (Is_Generated);


  function Filename return String is

    Project_Name  : constant String := Name;
    Gpr_Name      : constant String := Project_Name & File_Extension;
    Configuration : constant String := Language_Folder & "Gnat.adc";

    function Interface_Name return String is
    begin
      if Build.Is_Defined then
        return '"' & Build.Actual_Interface.To_Data (Separator => """, """) & '"';
      else
        return '"' & Legacy_Interface_Name & '"';
      end if;
    end Interface_Name;

    Product_Is_Dll : constant Boolean := Project.Is_Dll;

    Source       : constant String := Gpr_Source_Folder & Gpr_Name;
    Gpr_Filename : constant String := Created_Target_Folder & Gpr_Name;

    The_File : Ada.Text_IO.File_Type;

    procedure Put (Line : String) is
    begin
      Ada.Text_IO.Put_Line (The_File, Line);
    end Put;

  begin -- Filename
    Is_Generated := False;
    if Project_Name = "" then
      return "";
    end if;
    File.Delete (Gpr_Filename);
    if File.Exists (Source) then
      return Source;
    end if;
    Is_Generated := True;
    Ada.Text_IO.Create (The_File, Mode => Ada.Text_IO.Out_File, Name => Gpr_Filename);
    if not Is_Dll and then not The_Libraries.Is_Empty then
      for Library of The_Libraries loop
        if The_Library_Names.Is_Empty then
          Put ("with """ & Library & """;");
        else
          for Library_Name of The_Library_Names.Element (Library) loop
            Put ("with """ & Library_Name & """;");
          end loop;
        end if;
      end loop;
      Put ("");
    end if;
    Put ("project " & Project_Name & " is");
    Put ("");
    Put ("   package Naming is");
    Put ("      for Casing use ""mixedcase"";");
    Put ("   end Naming;");
    Put ("");
    Put ("   for Languages use (""Ada"", ""Winres"");");
    Put ("");
    if Product_Is_Dll then
      Put ("   for Library_Name use """ & Project_Name & """;");
      Put ("   for Shared_Library_Prefix use """";");
      Put ("");
    end if;
    for The_Directory of Source_Directories loop
      if The_Directory = Source_Directories.First_Element then
        Put ("   for Source_Dirs use (""" & The_Directory & """,");
      else
        Put ("                        """ & The_Directory & """,");
      end if;
    end loop;
    Put ("                        """ & Target_Directory & """);");
    Put ("");
    if Product_Is_Dll then
      Put ("   for Library_Interface use (" & Interface_Name & ");");
      Put ("");
    end if;
    Put ("   for Object_Dir use """ & Object_Directory & """;");
    Put ("");
    if Product_Is_Dll then
      Put ("   for Library_Options use (""-L" & Product_Directory & """);");
      Put ("   for Library_Dir use """ & Product_Directory & Product_Sub_Path & """;");
      Put ("   for Library_Ali_Dir use """ & Ali_Directory & """;");
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
    if File.Exists (Configuration) then
      Put ("      for Global_Configuration_Pragmas use """ & Configuration & """;");
    end if;
    if not Product_Is_Dll then
      Put ("      for Executable (""" & Program_Unit_Name & """) use """ & Project_Name & """;");
    end if;
    Put ("   end Builder;");
    Put ("");
    Put ("   package Compiler is");
    Put ("      for Default_Switches (""ada"") use");
    Put ("         (""-O1"", ""-gnatQ"", ""-gnata"", ""-gnato"", ""-g"", ""-" & Ada_Version & """,");
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
      if Is_Legacy_Compiler then
        Put ("          """ & Resource.Common_Object_File & """,");
      end if;
      Put ("          ""-m" & Build.Application_Kind_Image & """);");
      Put ("   end Linker;");
      Put ("");
    end if;
    Put ("end " & Project_Name & ";");
    Ada.Text_IO.Close (The_File);
    return Gpr_Filename;
  end Filename;


  function Information_Of (The_Filename : String) return Information is

    Gpr_Directory : constant String := File.Containing_Directory_Of (The_Filename);

    The_Gpr : Information;

    procedure Append (Gpr_Name : String) is

      Gpr_Filename : constant String := Gpr_Directory & File.Folder_Separator & Gpr_Name & File_Extension;

      The_With_Projects : Text.List;

      The_File : Ada.Text_IO.File_Type;

      procedure Parse_Gpr is
        The_Tokens : Text.List;

        function Next_Token return String is
        begin
          while The_Tokens.Is_Empty loop
            if Ada.Text_IO.End_Of_File (The_File) then
              return "";
            end if;
            declare
              Line   : constant String := Ada.Text_IO.Get_Line (The_File);
              Tokens : constant Text.Strings := Text.Strings_Of (Line, Separator => ' ', Symbols=>")(;");
            begin
              The_Tokens := Tokens.To_List;
            end;
            if not The_Tokens.Is_Empty then
              if Text.Location_Of ("--", The_Tokens.First_Element) = Text.Start_Of_String then
                The_Tokens.Clear;
              end if;
            end if;
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
              The_With_Projects.Clear;
              declare
                Project_Name : constant String := Next_Token;
              begin
                if Project_Name = "GNATCOLL_Core" then
                  Append ("GNATCOLL_Minimal"); -- to get access to root package GNATCOLL
                end if;
                The_Gpr.Project_Names.Append (Project_Name);
              end;
            elsif Token = "abstract" and then Next_Token = "project" then
              for Project_Name of The_With_Projects loop
                Append (Project_Name);
              end loop;
              exit;
            elsif Token = "with" then
              declare
                Project_Name : constant String := Text.Trimmed (Next_Token, '"');
              begin
                The_With_Projects.Append (Project_Name);
              end;
            elsif Token = "for" and then Next_Token = "Source_Dirs" and then
              Next_Token = "use" and then Next_Token = "("
            then
              declare
                Source_Path : constant String := Text.Trimmed (Next_Token, '"');
              begin
                The_Gpr.Source_Path.Append (File.Full_Name_Of (Name_Or_Directory => Source_Path,
                                                               Current_Directory => Gpr_Directory));
              exception
              when others =>
                The_Gpr.Source_Path.Append (Source_Path); -- use original to show in error message
              end;
              exit;
            end if;
          end;
        end loop;
      end Parse_Gpr;

    begin -- Append
      Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Gpr_Filename);
      Parse_Gpr;
      Ada.Text_IO.Close (The_File);
    end Append;

  begin -- Information_Of
    Append (File.Base_Name_Of (The_Filename));
    return The_Gpr;
  exception
  when Item: others =>
    Log.Write (Item);
    return ([],[]);
  end Information_Of;

end Project.Gpr;
