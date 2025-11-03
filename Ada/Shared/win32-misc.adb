-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                           www.white-elephant.ch                                                   *
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

with Ada.Unchecked_Conversion;

package body Win32.Misc is

  package Nt renames Win32.Winnt;

  function Current_Process return Nt.HANDLE
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "GetCurrentProcess";

  type Handle_Array is array (Positive range <>) of Nt.HANDLE;

  type Module_Information is record
    Base_Address : Win32.LPVOID;
    Module_Size  : Win32.DWORD;
    Entry_Point  : Win32.LPVOID with Unreferenced;
  end record
  with Convention => C;

  function Get_Module_Information (Handle      :        Nt.HANDLE;
                                   Module      :        Nt.HANDLE;
                                   Module_Info : access Module_Information;
                                   Cb          :        Win32.DWORD)
    return Win32.BOOL
  with
    Import        => Standard.True,
    Convention    => Stdcall,
    External_Name => "GetModuleInformation";


  function Module_From_Address (The_Address : System.Address) return Win32.Winnt.HANDLE is
    The_Count : aliased Win32.DWORD;
    Unused    :         Win32.BOOL;
    use type Nt.HANDLE;
    Max_Int_Bit : constant := Standard'address_size - 1;
    type Int is range -2 ** (Max_Int_Bit) .. 2 ** Max_Int_Bit - 1;
    function Convert is new Ada.Unchecked_Conversion (System.Address, Int);

    function Get_Modules_Count (Handle    :        Nt.HANDLE;
                                Hmodules  : access Nt.HANDLE; -- dummy
                                Cb        :        Win32.DWORD;
                                Cb_Needed : access Win32.DWORD)
      return Win32.BOOL
    with
      Import        => Standard.True,
      Convention    => Stdcall,
      External_Name => "EnumProcessModules";
  begin
    if Get_Modules_Count (Current_Process,
                          null,
                          0,
                          The_Count'access) /= Win32.FALSE
    then
      declare
        use type Win32.DWORD;
        type Modules is new Handle_Array(1 .. Positive(The_Count * 8 / Nt.HANDLE'size));

        function Enum_Process_Modules (Handle    :        Nt.HANDLE;
                                       Hmodules  : access Modules;
                                       Cb        :        Win32.DWORD;
                                       Cb_Needed : access Win32.DWORD)
          return Win32.BOOL
        with
          Import        => Standard.True,
          Convention    => Stdcall,
          External_Name => "EnumProcessModules";

        The_Modules     : aliased Modules;
        Actual_Count    : aliased Win32.DWORD;
        The_Information : aliased Module_Information;
        Info_Size       : constant Win32.DWORD := Module_Information'size / 8;
      begin
        if Enum_Process_Modules (Current_Process,
                                 The_Modules'access,
                                 The_Count,
                                 Actual_Count'access) /= Win32.FALSE
        then
          if Actual_Count /= The_Count then
            raise No_Information;
          end if;
          for The_Module of The_Modules loop
            if Get_Module_Information (Current_Process,
                                       The_Module,
                                       The_Information'access,
                                       Info_Size) /= Win32.FALSE
            then
              if (The_Address >= The_Information.Base_Address) and then
                (Convert(The_Address) <= Convert(The_Information.Base_Address) + Int(The_Information.Module_Size))
              then
                return The_Module;
              end if;
            end if;
          end loop;
        end if;
      end;
    end if;
    raise No_Information;
  end Module_From_Address;


  function Get_Base_Address_Of (The_Module : Win32.Winnt.HANDLE) return Win32.LPVOID is
    The_Information : aliased Module_Information;
    use type Win32.DWORD;
    Info_Size       : constant Win32.DWORD := Module_Information'size / 8;
  begin
    if Get_Module_Information (Current_Process,
                               The_Module,
                               The_Information'access,
                               Info_Size) /= Win32.FALSE
    then
      return The_Information.Base_Address;
    end if;
    raise No_Information;
  end Get_Base_Address_Of;


end Win32.Misc;
