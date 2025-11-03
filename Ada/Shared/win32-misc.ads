-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Win32.Winnt;

package Win32.Misc is
--
-- This package contains miscellaneous routines to help interface with Microsoft Windows.
--

  No_Information : exception;

  function Module_From_Address (The_Address : System.Address) return Win32.Winnt.HANDLE;
  
  function Get_Base_Address_Of (The_Module : Win32.Winnt.HANDLE) return Win32.LPVOID;
 

end Win32.Misc;
