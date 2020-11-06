-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Win.Dll is

  The_Instance : HINSTANCE := System.Null_Address;

  function Main (Hinst               : HINSTANCE;
                 Unused_Fdw_Reason   : DWORD;
                 Unused_Ppv_Reserved : LPVOID) return BOOL
  with
    Export,
    Convention    => Stdcall,
    External_Name => "DllMain";
  pragma Unreferenced (Main);

  function Main (Hinst               : in HINSTANCE;
                 Unused_Fdw_Reason   : in DWORD;
                 Unused_Ppv_Reserved : in LPVOID) return BOOL is
  begin
    The_Instance := Hinst;
    return TRUE;
  end Main;


  function Instance return HINSTANCE is
  begin
    return The_Instance;
  end Instance;

end Win.Dll;
