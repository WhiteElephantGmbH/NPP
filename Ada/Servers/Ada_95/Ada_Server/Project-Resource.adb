-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with File;
with Files;
with Log;
with Strings;

package body Project.Resource is

  Object_Name : constant String := "resources.o";

  function Is_Generated return Boolean is
  begin
    return False;
  end Is_Generated;


  function Filename return String is (Directory & Files.Separator & Name & Extension);


  function Object return String is (Object_Folder & Object_Name);


  function Information return String is
  begin
    if Is_Legacy then
      return " (resource: " & Filename & ")";
    else
      return "";
    end if;
  end Information;


  procedure Evaluate_Legacy is
    Resource_Filename : constant String := Filename;
    The_File          : Ada.Text_IO.File_Type;
  begin
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
                    if Text.Is_Null (The_Tools_Directories) then
                      Adjust_Index;
                      declare
                        Compiler : constant String := Next_Item;
                      begin
                        if Compiler = "GPL" then
                          declare
                            type Compiler_Year is range 2016 .. 2099;
                            Year : constant String := Next_Item;
                          begin
                            The_Tools_Directories := Text.String_Of
                              (System_Drive & "\GNAT\" & Strings.Trimmed (Compiler_Year'value(Year)'img) & "\bin");
                          exception
                          when others =>
                            Set_Error ("Unknown compiler: " & Compiler & " " & (if Year = "\"
                                                                               then "(year missing)"
                                                                               else Year) & " in " & Filename);
                          end;
                        elsif Compiler = "GNATPRO" then
                          The_Tools_Directories := Text.String_Of (System_Drive & "\GNATPRO\7.3.1\bin");
                        else
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
