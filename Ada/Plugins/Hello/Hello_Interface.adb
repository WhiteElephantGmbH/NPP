-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
-->Style: White_Elephant

with Npp.Message;
with Npp.Plugin;
with Win.Dll;
with Scintilla;
with System.Address_To_Access_Conversions;

package body Hello_Interface is

  Wide_Nul : constant Wide_Character := Wide_Character'first;

  Promote_Toolbar : aliased Npp.Toolbar_Icons;


  procedure Load_Resources is
    Bitmap : aliased constant String := "PromoteBmp" & Ascii.Nul;
  begin
    Promote_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Bitmap);
  end Load_Resources;


  Magenta : constant Win.COLORREF := Win.RGB (192, 0, 192);

  procedure Promote is
  begin
    Npp.Message.Write_Line ("Hello World", Magenta);
  end Promote;


  procedure About is
  begin
    Npp.Message.Write_Line ("N++ plugin demo written in Ada.");
  end About;


  procedure Initialize is
  begin
    Load_Resources;
    Npp.Plugin.Add_Function ("Promote", Promote'access, Promote_Toolbar);
    Npp.Plugin.Add_Function ("About", About'access);
  end Initialize;


  ---------------
  -- Interface --
  ---------------

  Name : aliased constant Wide_String := "Ada Plugin" & Wide_Nul;

  function Get_Name return System.Address is
  begin
    return Name'address;
  end Get_Name;


  function Is_Unicode return Win.BOOL is
  begin
    return Npp.Plugin.Is_Unicode;
  end Is_Unicode;


  procedure Set_Info (Data : Info) is
  begin
    Npp.Plugin.Set_Info ((Data.Npp_Handle, Data.Scintilla_Main_Handle, Data.Scintilla_Second_Handle));
  end Set_Info;


  function Get_Funcs_Array (Count : access Win.INT) return System.Address is
  begin
    return Npp.Plugin.Get_Funcs_Array (Count);
  end Get_Funcs_Array;


  package Convertion is new System.Address_To_Access_Conversions (Scintilla.Notification);

  procedure Be_Notified (Notify : System.Address) is
  begin
    Npp.Plugin.Be_Notified (Convertion.To_Pointer(Notify));
  end Be_Notified;


  function Message_Proc (Unused_Message : UINT;
                         Unused_Wpara   : WPARAM;
                         Unused_Lpara   : LPARAM) return LRESULT is
  begin
    return Signed(Win.OK);
  end Message_Proc;

begin
  Npp.Plugin.Define ("Npp_Hello");
  Npp.Plugin.Install (Set_Info_Call => Initialize'access);
  Npp.Message.Install;
end Hello_Interface;
