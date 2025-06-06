-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body File is

  package FS renames Ada.Directories;

  The_Folder_Separator : constant Character
    with
      Import        => True,
      Convention    => C,
      External_Name => "__gnat_dir_separator";


  function Folder_Separator return Character is
  begin
    return The_Folder_Separator;
  end Folder_Separator;


  function "+" (Directory : String) return Folder is
  begin
    return Folder(Directory);
  end "+";


  function "+" (Left, Right : String) return Folder is
  begin
    return Folder(Left & The_Folder_Separator & Right);
  end "+";


  function "+" (Left  : Folder;
                Right : String) return Folder is
  begin
    return Folder(String(Left) & The_Folder_Separator & Right);
  end "+";


  function Composure (Directory : String;
                      Filename  : String;
                      Extension : String) return String renames FS.Compose;

  function Composure (Directory : Folder;
                      Filename  : String;
                      Extension : String) return String is
  begin
    return Composure (String(Directory), Filename, Extension);
  end Composure;


  function Name_Of (Name      : String;
                    Extension : String) return String is
  begin
    begin
      if Extension_Of (Name) = "" then
        return Name & Extension;
      end if;
    exception
    when others =>
      null;
    end;
    return Name;
  end Name_Of;


  function Full_Name_Of (Name_Or_Directory : String;
                         Current_Directory : String) return String is
  begin
    if Name_Or_Directory'length > 2 and then Name_Or_Directory(Name_Or_Directory'first + 1) = ':' then
      return Ada.Directories.Full_Name (Name_Or_Directory);
    end if;
    return Ada.Directories.Full_Name (Current_Directory & '/' & Name_Or_Directory);
  end Full_Name_Of;


  function Is_Legal (Name_Or_Directory : String) return Boolean is
  begin
    declare
      Unused : constant String := Ada.Directories.Full_Name (Name_Or_Directory);
    begin
      return True;
    end;
  exception
  when others =>
    return False;
  end Is_Legal;


  function Exists (Name : String) return Boolean is
    use type FS.File_Kind;
  begin
    return FS.Kind (Name) = FS.Ordinary_File;
  exception
  when others =>
    return False;
  end Exists;


  function Directory_Exists (Name : String) return Boolean is
    use type FS.File_Kind;
  begin
    return FS.Kind (Name) = FS.Directory;
  exception
  when others =>
    return False;
  end Directory_Exists;


  function Is_Newer (The_Name  : String;
                     Than_Name : String) return Boolean is
    use type Ada.Calendar.Time;
  begin
    return Modification_Time_Of (The_Name) > Modification_Time_Of (Than_Name);
  end Is_Newer;


  procedure Delete (Name : String) is
  begin
    FS.Delete_File (Name);
  exception
  when Name_Error =>
    null;
  end Delete;


  procedure Create_Directory (Path : String) is
  begin
    Ada.Directories.Create_Path (Path);
  end Create_Directory;


  procedure Delete_Directory (Name : String) is
  begin
    FS.Delete_Tree (Name);
  exception
  when Name_Error =>
    null;
  end Delete_Directory;


  procedure X_Copy (Source      : String;
                    Destination : String;
                    Pattern     : String := "*") is

    procedure Copy_Entry (Directory_Entry : FS.Directory_Entry_Type) is
      Full_Name      : constant String := FS.Full_Name(Directory_Entry);
      Dest_Full_Name : constant String := Destination & Full_Name(Full_Name'first + Source'length .. Full_Name'last);
    begin
      case FS.Kind (Directory_Entry) is
      when FS.Directory =>
        if FS.Simple_Name(Directory_Entry) /= "." and FS.Simple_Name(Directory_Entry) /= ".." then
          FS.Create_Path (Dest_Full_Name);
          FS.Search (Directory => Full_Name,
                     Pattern   => Pattern,
                     Process   => Copy_Entry'access);
        end if;
      when FS.Ordinary_File =>
        FS.Copy_File (Full_Name, Dest_Full_Name);
      when others =>
        null;
      end case;
    end Copy_Entry;

  begin -- X_Copy
    FS.Create_Path (Destination);
    FS.Search (Directory => Source,
               Pattern   => Pattern,
               Process   => Copy_Entry'access);
  end X_Copy;


  function Is_Leaf_Directory (Directory  : String;
                              Exceptions : Text.Strings := Text.None) return Boolean is

    The_Count : Natural := 0;

    procedure Iterate_For (Directory_Entry : FS.Directory_Entry_Type) is
    begin
      declare
        Name : constant String := FS.Simple_Name(Directory_Entry);
      begin
        if not (Name(Name'first) = '.') and then not Exceptions.Contains (Name) then
          The_Count := The_Count + 1;
        end if;
      end;
    exception
    when others =>
      null;
    end Iterate_For;

  begin -- Is_Leaf_Directory
    FS.Search (Directory => Directory,
               Pattern   => "",
               Filter    => [FS.Directory => True, others => False],
               Process   => Iterate_For'access);
    return The_Count = 0;
  exception
  when others =>
    return False;
  end Is_Leaf_Directory;


  procedure Iterate_Over_Leaf_Directories (From_Directory : String;
                                           Iterator       : access procedure (Leaf_Directory : String);
                                           Exceptions     : Text.Strings := Text.None) is
    The_Count : Natural := 0;

    procedure Iterate_For (Directory_Entry : FS.Directory_Entry_Type) is
    begin
      declare
        Name      : constant String := FS.Simple_Name(Directory_Entry);
        Directory : constant String := FS.Full_Name(Directory_Entry);
      begin
        if not (Name(Name'first) in '.') and then not Exceptions.Contains (Name) then
          The_Count := The_Count + 1;
          Iterate_Over_Leaf_Directories (Directory, Iterator, Exceptions);
        end if;
      end;
    exception
    when others =>
      null;
    end Iterate_For;

  begin -- Iterate_Over_Leaf_Directories
    FS.Search (Directory => From_Directory,
               Pattern   => "",
               Filter    => [FS.Directory => True, others => False],
               Process   => Iterate_For'access);
    if The_Count = 0 then
      Iterator (From_Directory);
    end if;
  end Iterate_Over_Leaf_Directories;


  function Found_Directory (Simple_Name  : String;
                            In_Directory : String) return String is

    The_Handle : FS.Search_Type;
    The_Entry  : FS.Directory_Entry_Type;

  begin
    FS.Start_Search (Search    => The_Handle,
                     Directory => In_Directory,
                     Pattern   => "",
                     Filter    => [FS.Directory => True, others => False]);
    while FS.More_Entries (The_Handle) loop
      FS.Get_Next_Entry (The_Handle, The_Entry);
      declare
        Name      : constant String := FS.Simple_Name (The_Entry);
        Directory : constant String := FS.Full_Name (The_Entry);
      begin
        if Text.Matches (Name, Simple_Name) then
          return Directory;
        elsif Name(Name'first) = '.' then
          null;
        else
          begin
            return Found_Directory (Simple_Name  => Simple_Name,
                                    In_Directory => Directory);
          exception
          when others =>
            null;
          end;
        end if;
      end;
    end loop;
    raise Not_Found;
  end Found_Directory;


  ------------------------
  -- File Iterator Loop --
  ------------------------

  function Iterator_For (Name : String) return Item is
  begin
    return (Name_Length => Name'length,
            Name        => Name,
            others      => <>);
  end Iterator_For;


  function Has_More (Data : Cursor) return Boolean is
  begin
    if Data.Has_More then
      return True;
    else
      FS.End_Search (Data.Position);
      return False;
    end if;
  end Has_More;


  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String is
  begin
    return FS.Full_Name (The_Item.Actual);
  end Constant_Reference;


  type Item_Access is access all Item;
  for Item_Access'storage_size use 0;

  type Iterator is new List_Iterator_Interfaces.Forward_Iterator with record
    Container : Item_Access;
  end record;

  overriding
  function First (Object : Iterator) return Cursor;

  overriding
  function Next (Object       : Iterator;
                 Unused_Index : Cursor) return Cursor;


  function Iterate (The_Item : Item) return List_Iterator_Interfaces.Forward_Iterator'class is
  begin
    return Iterator'(Container => The_Item'unrestricted_access);
  end Iterate;


  function First (Object : Iterator) return Cursor is
  begin
    FS.Start_Search (Search    => Object.Container.Data.Position,
                     Directory => Object.Container.Name,
                     Pattern   => "",
                     Filter    => [FS.Ordinary_File => True, others => False]);
    return Next (Object, null);
  end First;


  function Next (Object       : Iterator;
                 Unused_Index : Cursor) return Cursor is
  begin
    Object.Container.Data.Has_More := FS.More_Entries (Object.Container.Data.Position);
    if Object.Container.Data.Has_More then
      FS.Get_Next_Entry (Object.Container.Data.Position, Object.Container.Actual);
    end if;
    return Object.Container.Data'access;
  end Next;

end File;
