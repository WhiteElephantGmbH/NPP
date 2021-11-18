-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Build;
with File;
with Log;
with Project;
with String_List;

package body Build_Parser is

  procedure Evaluate is

    End_Detected : exception;

    The_Line   : String(1..255);
    The_Index  : Natural := 0;
    Last_Index : Natural;

    The_File : Ada.Text_IO.File_Type;

    function Next_Token return String is

      function Next_Character return Character is
      begin
        loop
          if The_Index = 0 then
            if Ada.Text_IO.End_Of_File (The_File) then
              raise End_Detected;
            end if;
            Ada.Text_IO.Get_Line (The_File, The_Line, Last_Index);
            if Ada.Text_IO.End_Of_File (The_File) then
              raise End_Detected;
            end if;
            Last_Index := Last_Index + 1;
            The_Line(Last_Index) := ' ';
            The_Index := 1;
          end if;
          if The_Index < Last_Index and then The_Line(The_Index .. The_Index + 1) = "--" then
            The_Index := 0;
          elsif The_Index <= Last_Index then
            The_Index := The_Index + 1;
            return The_Line(The_Index - 1);
          else
            The_Index := 0;
          end if;
        end loop;
      end Next_Character;

      Start_Index : Natural;

    begin -- Next_Token
      loop
        case Next_Character is
        when ' ' | '(' | ',' | ';' =>
          null;
        when ')' =>
          return ")";
        when others =>
          Start_Index := The_Index - 1;
          exit;
        end case;
      end loop;
      loop
        case The_Line(Start_Index) is
        when ')' =>
          return ")";
        when '=' =>
          case Next_Character is
          when '>' =>
            return "=>";
          when others =>
            null;
          end case;
        when '"' =>
          loop
            case Next_Character is
            when '"' =>
              return The_Line(Start_Index + 1 .. The_Index - 2);
            when others =>
              null;
            end case;
          end loop;
        when others =>
          case Next_Character is
          when ')' => -- keep as token
            The_Index := The_Index - 1;
            return The_Line(Start_Index .. The_Index - 1);
          when ' ' | '(' | ',' | ';' =>
            return The_Line(Start_Index .. The_Index - 2);
          when others =>
            null;
          end case;
        end case;
      end loop;
    end Next_Token;

    function Found (Token : String) return Boolean is
    begin
      while Token /= Next_Token loop
        null;
      end loop;
      return True;
    end Found;

    Filename : constant String := Project.Program_Unit;

  begin -- Evaluate
    Log.Write ("||| Evaluate build information from " & Filename);
    if File.Exists (Filename) then
      begin
        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
      exception
      when Item: others =>
        Log.Write (Item);
        return;
      end;
      while Found ("pragma") loop
        declare
          Pragma_Name : constant String := Next_Token;
        begin
          if Pragma_Name = "Build" then
            Build.Define;
            loop
              declare
                Attribute_Name : constant String := Next_Token;
                Association    : constant String := Next_Token;
                Attribute      : constant String := Next_Token;

                procedure Define_Compiler is
                begin
                  if not Build.Defined_Compiler (Attribute) then
                    Log.Write ("!!! Build - Compiler <" & Attribute & "> unknown");
                  end if;
                end Define_Compiler;

                procedure Define_Kind is
                begin
                  if not Build.Defined_Kind (Attribute) then
                    Log.Write ("!!! Build - Kind <" & Attribute & "> unknown");
                  end if;
                end Define_Kind;

                procedure Define_Libraries is
                  The_Libraries : String_List.Item;
                begin
                  The_Libraries.Append (Attribute);
                  loop
                    declare
                      Token : constant String := Next_Token;
                    begin
                      exit when Token = ")";
                      The_Libraries.Append (Token);
                    end;
                  end loop;
                  Build.Define_Libraries (The_Libraries);
                end Define_Libraries;

                procedure Define_Interface is
                begin
                  if not Build.Defined_Interface (Attribute) then
                    Log.Write ("!!! Build - Interface <" & Attribute & "> unknown");
                  end if;
                end Define_Interface;

                procedure Define_Resource is
                begin
                  if not Build.Defined_Resource (Attribute) then
                    Log.Write ("!!! Build - Resource <" & Attribute & "> unknown");
                  end if;
                end Define_Resource;

                procedure Define_Version is
                  The_Version : Build.Version;
                begin
                  The_Version.Major := Build.Version_Number'value(Attribute);
                  The_Version.Minor := Build.Version_Number'value(Next_Token);
                  The_Version.Variant := Build.Version_Number'value(Next_Token);
                  The_Version.Revision := Build.Version_Number'value(Next_Token);
                  if Next_Token = ")" then
                    Build.Define (The_Version);
                  end if;
                exception
                when others =>
                  Log.Write ("!!! Build - Incorrect version number");
                end Define_Version;

                procedure Define_Icon is
                begin
                  if not Build.Defined_Icon (Attribute) then
                    Log.Write ("!!! Build - Icon => " & Attribute & " unknown");
                  end if;
                end Define_Icon;

              begin
                exit  when Attribute_Name = ")" or else Association /= "=>";
                if Attribute_Name = "Compiler" then
                  Define_Compiler;
                elsif Attribute_Name = "Desciption" then
                  Build.Define_Description (Attribute);
                elsif Attribute_Name = "Kind" then
                  Define_Kind;
                elsif Attribute_Name = "Libraries" then
                  Define_Libraries;
                elsif Attribute_Name = "Interface" then
                  Define_Interface;
                elsif Attribute_Name = "Resource" then
                  Define_Resource;
                elsif Attribute_Name = "Version" then
                  Define_Version;
                elsif Attribute_Name = "Icon" then
                  Define_Icon;
                end if;
              end;
            end loop;
          elsif Pragma_Name = "Style_White_Elephant" then
            Build.Define_Company ("White Elephant GmbH");
          elsif Pragma_Name = "Style_Soudronic" then
            Build.Define_Company ("Soudronic AG");
          end if;
        end;
      end loop;
      Ada.Text_IO.Close (The_File);
    end if;
  exception
  when End_Detected =>
    Ada.Text_IO.Close (The_File);
  end Evaluate;

end Build_Parser;
