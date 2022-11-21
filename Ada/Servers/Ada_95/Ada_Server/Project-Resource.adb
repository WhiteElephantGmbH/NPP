-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Calendar;
with Ada.Text_IO;
with Build;
with File;
with Files;
with Log;
with Strings;

package body Project.Resource is

  Object_Name : constant String := "resources.o";


  function Filename return String is
    Legacy_Source    : constant String := Folder & Name & Extension;
    Generated_Source : constant String := Target_Folder & Name & Extension;
  begin
    if Build.Is_Defined then
      if File.Exists (Legacy_Source) then
        Set_Error ("Resource file <" & Legacy_Source & "> is obsolescent");
      end if;
      return Generated_Source;
    else
      return Legacy_Source;
    end if;
  end Filename;


  function Object return String is (Object_Folder & Object_Name);


  function Information return String is
  begin
    if Build.Is_Defined then
      return "";
    else
      return " (resource: " & Filename & ")";
    end if;
  end Information;


  procedure Generate is

    Resource_Filename : constant String := Filename;

    The_File : Ada.Text_IO.File_Type;

    procedure Put (Line : String) is
    begin
      Ada.Text_IO.Put_Line (The_File, Line);
    end Put;

    function File_Version_Image return String is
      function Image_Of is new Strings.Image_Of (Build.Version_Number);
      Version : constant Build.Version := Build.Actual_Version;
    begin
      return Image_Of (Version.Major) & ','
           & Image_Of (Version.Minor) & ','
           & Image_Of (Version.Variant) & ','
           & Image_Of (Version.Revision);
    end File_Version_Image;

    function Unix_Style_Of (Item : String) return String is
      The_Item : String := Item;
    begin
      for The_Character of The_Item loop
        if The_Character = Files.Separator then
          The_Character := Files.Other_Separator;
        end if;
      end loop;
      return The_Item;
    end Unix_Style_Of;

    function Icon_Name return String is
    begin
      return Unix_Style_Of (Folder & Name & ".ico");
    end Icon_Name;

    Actual_Year : constant Ada.Calendar.Year_Number := Ada.Calendar.Year (Ada.Calendar.Clock);

  begin -- Generate
    if File.Exists (Resource_Filename) then
      return;
    end if;
    Log.Write ("||| Generate Resource " & Resource_Filename);
    begin
      Ada.Text_IO.Create (The_File, Mode => Ada.Text_IO.Out_File, Name => Resource_Filename);
    exception
    when others =>
      Set_Error ("Can't create Resource " & Resource_Filename);
    end;
    if Build.Has_Icon then
      Put ("1 ICON """ & Icon_Name & """");
    end if;
    Put ("1 VERSIONINFO");
    Put ("  FILEVERSION " & File_Version_Image);
    Put ("  FILEFLAGSMASK 0x3FL");
    Put ("  FILEFLAGS 0x0L");
    Put ("  FILEOS 0x4L");
    Put ("  FILETYPE 0x1L");
    Put ("  FILESUBTYPE 0x0L");
    Put ("BEGIN");
    Put ("  BLOCK ""StringFileInfo""");
    Put ("  BEGIN");
    Put ("    BLOCK ""040904E4""");
    Put ("    BEGIN");
    Put ("      VALUE ""LegalCopyright"", ""Copyright © " & Build.Actual_Company & Actual_Year'img & "\0""");
    Put ("      VALUE ""FileDescription"", """ & Build.Actual_Description & "\0""");
    Put ("      VALUE ""OriginalFilename"", """ & Name & (if Build.Is_Dll then ".dll" else ".exe") & "\0""");
    Put ("      VALUE ""InternalName"", """ & Name & "\0""");
    Put ("      VALUE ""ProductName"", """ & Product_Name & "\0""");
    Put ("      VALUE ""ProductVersion"", """ & Product_Version & "\0""");
    Put ("    END");
    Put ("  END");
    Put ("  BLOCK ""VarFileInfo""");
    Put ("  BEGIN");
    Put ("    VALUE ""Translation"", 0x409, 1252");
    Put ("  END");
    Put ("END");
    if Build.Has_Resource then
      Put ("#include """ & Unix_Style_Of(Build.Actual_Resource) & """");
    end if;
    Ada.Text_IO.Close (The_File);
  end Generate;


  procedure Evaluate_Legacy is
    Resource_Filename : constant String := Filename;
    The_File          : Ada.Text_IO.File_Type;
  begin
    Log.Write ("||| Evaluate Legacy");
    The_Libraries.Clear;
    if File.Exists (Resource_Filename) then
      begin
        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Resource_Filename);
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
                      Resource_Item : constant String := Next_Item;
                    begin
                      Get_Next ("""");
                      return Resource_Item;
                    end;
                  end Next_Resource;

                  procedure Adjust_Index is
                  begin
                    Get_Next (",");
                    Get_Next ("""");
                  end Adjust_Index;

                  Resource_Item : constant String := Next_Resource;

                begin
                  if Resource_Item = "ProductName" then
                    if not Tools_Defined then
                      Adjust_Index;
                      declare
                        Compiler : constant String := Next_Item;
                        Defined  : Boolean := False;
                      begin
                        if Compiler = "GPL" then
                          declare
                            type Compiler_Year is range 2016 .. 2099;
                            Year     : constant String := Next_Item;
                            The_Year : Compiler_Year;
                          begin
                            begin
                              The_Year := Compiler_Year'value(Year);
                            exception
                            when others =>
                              Set_Error ("Unknown compiler: " & Compiler & " " & (if Year = "\"
                                                                                 then "(year missing)"
                                                                                 else Year) & " in " & Filename);
                            end;
                            Defined := Build.Defined_Compiler ("GNAT\" & Strings.Trimmed (The_Year'img));
                          end;
                        elsif Compiler = "GNATPRO" then
                          Defined := Build.Defined_Compiler ("GNATPRO\" & Legacy_Product_Version);
                        end if;
                        if not Defined then
                          Set_Error ("Unknown compiler: " & Compiler & " in " & Filename);
                        end if;
                      end;
                    end if;
                  elsif Resource_Item = "Libraries" then
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
  end Evaluate_Legacy;

end Project.Resource;
