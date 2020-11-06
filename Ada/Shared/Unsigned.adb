-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

package body Unsigned is

  Space : constant Character := ' ';


  function Digit_Of (The_Byte : Byte) return Character is
    Hex_Digits : constant String := "0123456789ABCDEF";
  begin
    return Hex_Digits (Natural(The_Byte) + 1);
  end Digit_Of;


  function Byte_Of (The_Address : System.Address) return Byte is

    type Byte_Access is access all Byte;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Byte_Access);

  begin
    return Convert (The_Address).all;
  end Byte_Of;


  function String_Of (The_Byte : Byte) return Byte_String is
    The_Byte_String : constant Byte_String (1..1) := (others => The_Byte);
  begin
    return The_Byte_String;
  end String_Of;


  function String_Of (The_Address : System.Address;
                      The_Size    : Natural) return Byte_String is

    subtype Bytes is Byte_String (1..The_Size);

    type Bytes_Access is access all Bytes;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Bytes_Access);

  begin
    if The_Size = 0 then
      return Byte_Null_String;
    else
      return Convert (The_Address).all;
    end if;
  end String_Of;


  function String_Of (The_String : String) return Byte_String is
  begin
    return String_Of (The_String'address, The_String'length);
  end String_Of;


  function String_Of (The_String : Byte_String) return String is

    subtype Characters is String (1..The_String'length);

    type String_Access is access all Characters;

    function Convert is new Ada.Unchecked_Conversion (System.Address, String_Access);

  begin
    return Convert (The_String'address).all;
  end String_Of;


  function Image_Of (The_Byte : Byte) return String is
    Image : constant String := Byte'image(The_Byte);
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;


  function Hex_Image_Of (The_Byte : Byte) return String is
  begin
    return Digit_Of (The_Byte / 16) & Digit_Of (The_Byte mod 16);
  end Hex_Image_Of;


  function Hex_Image_Of (The_String : Byte_String) return String is
  begin
    if The_String'length = 0 then
      return "";
    else
      declare
        The_Result : String (1.. The_String'length * 3 - 1) := (others => Space);
      begin
        for The_Index in The_String'range loop
          Ada.Strings.Fixed.Overwrite (The_Result,
                                       (The_Index + 1 - The_String'first) * 3 - 2,
                                       Hex_Image_Of (The_String(The_Index)));
        end loop;
        return The_Result;
      end;
    end if;
  end Hex_Image_Of;


  function Image_Of (The_Word : Word) return String is
    Image : constant String := Word'image(The_Word);
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;


  function Hex_Image_Of (The_Word : Word) return String is
  begin
    return Hex_Image_Of (Byte(The_Word / 2**8)) & Hex_Image_Of(Byte(The_Word mod 2**8));
  end Hex_Image_Of;


  function Hex_Image_Of (The_String : Word_String) return String is
  begin
    if The_String'length = 0 then
      return "";
    else
      declare
        The_Result : String (1.. The_String'length * 5 -1) := (others => Space);
      begin
        for The_Index in The_String'range loop
          Ada.Strings.Fixed.Overwrite (The_Result, (The_Index * 5 - 4), Hex_Image_Of (The_String(The_Index)));
        end loop;
        return The_Result;
      end;
    end if;
  end Hex_Image_Of;


  function Image_Of (The_Longword : Longword) return String is
    Image : constant String := Longword'image(The_Longword);
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;


  function Image_Of (The_Value : Natural) return String is
    Image : constant String := Natural'image(The_Value);
  begin
    return Image(Image'first + 1 .. Image'last);
  end Image_Of;


  function Hex_Image_Of (The_Longword : Longword) return String is
  begin
    return Hex_Image_Of (Word(The_Longword / 2**16)) & Hex_Image_Of(Word(The_Longword mod 2**16));
  end Hex_Image_Of;


  function Hex_Image_Of (The_String : Longword_String) return String is
  begin
    if The_String'length = 0 then
      return "";
    else
      declare
        The_Result : String (1.. The_String'length * 9 -1) := (others => Space);
      begin
        for The_Index in The_String'range loop
          Ada.Strings.Fixed.Overwrite (The_Result, (The_Index * 9 - 8), Hex_Image_Of (The_String(The_Index)));
        end loop;
        return The_Result;
      end;
    end if;
  end Hex_Image_Of;


  function Hex_Value_Of (The_String : String) return Byte is
  begin
    return Byte'value("16#" & The_String & "#");
  end Hex_Value_Of;


  function Hex_Value_Of (The_String : String) return Byte_String is
    The_Values : Byte_String (1 .. (The_String'length + 1) / 2);
    The_Index  : Natural := The_Values'first;
    The_First  : Natural := 0;
  begin
    for Index in The_String'range loop
      if The_String(Index) = ' ' then
        if The_First /= 0 then
          The_Values(The_Index) := Hex_Value_Of (The_String(The_First .. Index - 1));
          The_Index := The_Index + 1;
          The_First := 0;
        end if;
      else
        if The_First = 0 then
          The_First := Index;
        end if;
      end if;
    end loop;
    if The_First /= 0 then
      The_Values(The_Index) := Hex_Value_Of (The_String(The_First .. The_String'last));
      The_Index := The_Index + 1;
    end if;
    return The_Values (The_Values'first .. The_Index - 1);
  exception
  when others =>
    raise String_Image_Error;
  end Hex_Value_Of;


  function From_Base64 (Base64_String : String) return Byte_String is

    subtype Six_Bits is Byte range 0 .. 16#3F#;

    function Sextet_Of (The_Character : Character) return Six_Bits is
    begin
      case The_Character is
      when 'A' .. 'Z' =>
        return Character'pos(The_Character) - Character'pos('A');
      when 'a' .. 'z' =>
        return Character'pos(The_Character) - Character'pos('a') + 26;
      when '0' .. '9' =>
        return Character'pos(The_Character) - Character'pos('0') + 52;
      when '+' =>
        return 62;
      when '/' =>
        return 63;
      when others =>
        raise Bad_Encoding;
      end case;
    end Sextet_Of;

    type Position is (First, Second, Third, Fourth, Padding);
    The_Position : Position := First;
    Expansion    : Byte_String (1 .. ((Base64_String'length + 3) / 4) * 3);
    The_Index    : Positive := Expansion'first;

  begin -- From_Base64
    for The_Byte of Base64_String loop
      if The_Byte = '=' then
        The_Position := Padding;
      else
        case The_Position is
        when First =>
          Expansion (The_Index) := Sextet_Of (The_Byte) * 4;
          The_Position := Second;
        when Second =>
          Expansion (The_Index) := Expansion (The_Index) + Sextet_Of (The_Byte) / 16#10#;
          The_Index := The_Index + 1;
          Expansion (The_Index) := (Sextet_Of (The_Byte) mod 16#10#) * 16#10#;
          The_Position := Third;
        when Third =>
          Expansion (The_Index) := Expansion (The_Index) + Sextet_Of (The_Byte) / 4;
          The_Index := The_Index + 1;
          Expansion (The_Index) := (Sextet_Of (The_Byte) mod 4) * 16#40#;
          The_Position := Fourth;
        when Fourth =>
          Expansion (The_Index) := Expansion (The_Index) + Sextet_Of (The_Byte);
          The_Index := The_Index + 1;
          The_Position := First;
        when Padding =>
          raise Bad_Encoding;
        end case;
      end if;
    end loop;
    return Expansion (Expansion'first .. The_Index - 1);
  end From_Base64;


  function To_Base64 (The_String : Byte_String) return String is

    subtype Six_Bits is Byte range 0 .. 16#3F#;

    function Base_Of (The_Sextet : Six_Bits) return Character is
    begin
      case The_Sextet is
      when 0 .. 25 =>
        return Character'val(The_Sextet + Character'pos('A'));
      when 26 .. 51 =>
        return Character'val(The_Sextet - 26 + Character'pos('a'));
      when 52 .. 61 =>
        return Character'val(The_Sextet - 52 + Character'pos('0'));
      when 62 =>
        return '+';
      when 63 =>
        return '/';
      end case;
    end Base_Of;

    type Position is (First, Second, Third);
    The_Position : Position := First;
    The_Base64   : String (1 .. ((The_String'length + 2) / 3) * 4);
    The_Index    : Positive := The_Base64'first;
    Remainder    : Byte;
  begin
    for The_Byte of The_String loop
      case The_Position is
      when First =>
        The_Base64 (The_Index) := Base_Of (The_Byte / 4);
        The_Index := The_Index + 1;
        Remainder := The_Byte mod 4;
        The_Position := Second;
      when Second =>
        The_Base64 (The_Index) := Base_Of (Remainder * 16#10# + The_Byte / 16#10#);
        The_Index := The_Index + 1;
        Remainder := The_Byte mod 16#10#;
        The_Position := Third;
      when Third =>
        The_Base64 (The_Index) := Base_Of (Remainder * 4 + The_Byte / 16#40#);
        The_Index := The_Index + 1;
        The_Base64 (The_Index) := Base_Of (The_Byte mod 16#40#);
        The_Index := The_Index + 1;
        The_Position := First;
      end case;
    end loop;
    case The_Position is
      when First =>
        null;
      when Second =>
        The_Base64 (The_Index) := Base_Of (Remainder * 16#10#);
        The_Base64 (The_Index + 1 .. The_Base64'last) := "==";
      when Third =>
        The_Base64 (The_Index) := Base_Of (Remainder * 4);
        The_Base64 (The_Base64'last) := '=';
    end case;
    return The_Base64;
  end To_Base64;


  function Hex_Value_Of (The_String : String) return Word is
  begin
    return Word'value("16#" & The_String & "#");
  end Hex_Value_Of;


  function Hex_Value_Of (The_String : String) return Longword is
  begin
    return Longword'value("16#" & The_String & "#");
  end Hex_Value_Of;


  function Hex_Value_Of (The_String : String) return Quadword is
  begin
    return Quadword'value("16#" & The_String & "#");
  end Hex_Value_Of;


  function Word_Of (The_String : Byte_String) return Word is
    The_Value : Word := 0;
  begin
    for The_Index in reverse The_String'range loop
      The_Value := The_Value * 2**8 + Word(The_String(The_Index));
    end loop;
    return The_Value;
  end Word_Of;


  function Word_Of_Big_Endian (The_String : Byte_String) return Word is
    The_Value : Word := 0;
  begin
    for The_Index in The_String'range loop
      The_Value := The_Value * 2**8 + Word(The_String(The_Index));
    end loop;
    return The_Value;
  end Word_Of_Big_Endian;


  function Longword_Of (The_String : Byte_String) return Longword is
    The_Value : Longword := 0;
  begin
    for The_Index in reverse The_String'range loop
      The_Value := The_Value * 2**8 + Longword(The_String(The_Index));
    end loop;
    return The_Value;
  end Longword_Of;


  function Longword_Of_Big_Endian (The_String : Byte_String) return Longword is
    The_Value : Longword := 0;
  begin
    for The_Index in The_String'range loop
      The_Value := The_Value * 2**8 + Longword(The_String(The_Index));
    end loop;
    return The_Value;
  end Longword_Of_Big_Endian;


  function Longword_Of (The_String : Word_String) return Longword is
    The_Value : Longword := 0;
  begin
    for The_Index in reverse The_String'range loop
      The_Value := The_Value * 2**16 + Longword(The_String(The_Index));
    end loop;
    return The_Value;
  end Longword_Of;


  function String_Of (The_Word : Word) return Byte_String is
  begin
    return (Byte(The_Word mod 2**8), Byte(The_Word / 2**8));
  end String_Of;


  function String_Of (The_Longword : Longword) return Byte_String is
  begin
    return String_Of (Word(The_Longword mod 2**16)) & String_Of (Word(The_Longword / 2**16));
  end String_Of;


  function String_Of (The_String : Longword_String) return Byte_String is
    The_Value : Byte_String (1..The_String'length * 4);
  begin
    for Index in The_String'range loop
      The_Value (The_Value'first + Index * 4 - 4 .. The_Value'first + Index * 4 - 1) := String_Of (The_String(Index));
    end loop;
    return The_Value;
  end String_Of;


  function String_Of (The_Longword : Longword) return Word_String is
  begin
    return (Word(The_Longword mod 2**16), Word(The_Longword / 2**16));
  end String_Of;


  function Longword_Of (The_Integer : Integer) return Longword is
    function To_Longword is new Ada.Unchecked_Conversion (Integer, Longword);
  begin
    return To_Longword (The_Integer);
  end Longword_Of;


  function To_Integer (The_Longword : Longword) return Integer is
    function To_Integer is new Ada.Unchecked_Conversion (Longword, Integer);
  begin
    return To_Integer (The_Longword);
  end To_Integer;


  function Quadword_Of (The_String : Byte_String) return Quadword is
    The_Value : Quadword := 0;
  begin
    for The_Index in reverse The_String'range loop
      The_Value := The_Value * 2**8 + Quadword(The_String(The_Index));
    end loop;
    return The_Value;
  end Quadword_Of;


  function Quadword_Of_Big_Endian (The_String : Byte_String) return Quadword is
    The_Value : Quadword := 0;
  begin
    for The_Index in The_String'range loop
      The_Value := The_Value * 2**8 + Quadword(The_String(The_Index));
    end loop;
    return The_Value;
  end Quadword_Of_Big_Endian;


  function String_Of (The_Quadword : Quadword) return Byte_String is
  begin
    return String_Of (Least_Significant_Longword_Of (The_Quadword)) &
           String_Of (Most_Significant_Longword_Of (The_Quadword));
  end String_Of;


  function Quadword_Of (The_String : Word_String) return Quadword is
    The_Value : Quadword := 0;
  begin
    for The_Index in reverse The_String'range loop
      The_Value := The_Value * 2**16 + Quadword(The_String(The_Index));
    end loop;
    return The_Value;
  end Quadword_Of;


  function String_Of (The_Quadword : Quadword) return Word_String is
  begin
    return String_Of (Least_Significant_Longword_Of (The_Quadword)) &
           String_Of (Most_Significant_Longword_Of (The_Quadword));
  end String_Of;


  function Quadword_Of (The_String : Longword_String) return Quadword is
  begin
    if The_String'length > 2 then
      raise Constraint_Error;
    end if;
    return Quadword_Of (Least_Significant => The_String(The_String'first),
                        Most_Significant  => The_String(The_String'last));
  end Quadword_Of;


  function String_Of (The_Quadword : Quadword) return Longword_String is
  begin
    return (Least_Significant_Longword_Of (The_Quadword), Most_Significant_Longword_Of (The_Quadword));
  end String_Of;


  function Quadword_Of (Most_Significant  : Longword;
                        Least_Significant : Longword) return Quadword is
  begin
    return Quadword(Most_Significant) * 2**32 + Quadword(Least_Significant);
  end Quadword_Of;


  function Most_Significant_Longword_Of (The_Quadword : Quadword) return Longword is
  begin
    return Longword (The_Quadword / 2**32);
  end Most_Significant_Longword_Of;


  function Least_Significant_Longword_Of (The_Quadword : Quadword) return Longword is
  begin
    return Longword(The_Quadword and 16#FFFF_FFFF#);
  end Least_Significant_Longword_Of;


  function Least_Significant_Word_Of (The_Quadword : Quadword) return Word is
  begin
    return Word(The_Quadword and 16#FFFF#);
  end Least_Significant_Word_Of;


  function Image_Of (The_Quadword : Quadword) return String is
    End_Of_Buffer : constant := 30;
    The_Remainder : Natural;
    The_Index     : Natural := End_Of_Buffer + 1;
    The_Buffer    : String (1..End_Of_Buffer);
    The_Number    : Quadword := The_Quadword;
  begin
    if The_Number = 0 then
      return "0";
    else
      while The_Number > 0 loop
        if The_Index <= The_Buffer'first then
          return "*** Overflow ***";
        else
           The_Index := The_Index - 1;
        end if;
        The_Remainder := Natural(The_Number mod 10);
        The_Number := The_Number / 10;
        The_Buffer (The_Index) := Character'val(Character'pos('0') + The_Remainder);
      end loop;
      return The_Buffer (The_Index..End_Of_Buffer);
    end if;
  end Image_Of;


  function Hex_Image_Of (The_Quadword : Quadword) return String is
  begin
    return Hex_Image_Of (Most_Significant_Longword_Of(The_Quadword)) &
           Hex_Image_Of (Least_Significant_Longword_Of(The_Quadword));
  end Hex_Image_Of;


  function Swap (The_Word : Word) return Word is
    The_Bytes : constant Byte_String := String_Of (The_Word);
  begin
    return Word_Of (Byte_String'(The_Bytes(The_Bytes'first+1),
                                 The_Bytes(The_Bytes'first)));
  end Swap;


  function Swap (The_Value : Big_Endian_Word) return Word is
  begin
    return Swap (Word(The_Value));
  end Swap;


  function Swap (The_Value : Word) return Big_Endian_Word is
  begin
    return Big_Endian_Word(Word'(Swap (The_Value)));
  end Swap;


  procedure Swap (The_Word : in out Word) is
  begin
    The_Word := Swap (The_Word);
  end Swap;


  function Swap (The_Longword : Longword) return Longword is
    The_Bytes : constant Byte_String := String_Of (The_Longword);
  begin
    return Longword_Of (Byte_String'(The_Bytes(The_Bytes'first + 3),
                                     The_Bytes(The_Bytes'first + 2),
                                     The_Bytes(The_Bytes'first + 1),
                                     The_Bytes(The_Bytes'first)));
  end Swap;


  function Swap (The_Value : Big_Endian_Longword) return Longword is
  begin
    return Swap (Longword(The_Value));
  end Swap;


  function Swap (The_Value : Longword) return Big_Endian_Longword is
  begin
    return Big_Endian_Longword(Longword'(Swap (The_Value)));
  end Swap;


  procedure Swap (The_Longword : in out Longword) is
  begin
    The_Longword := Swap (The_Longword);
  end Swap;


  function Swap (The_Quadword : Quadword) return Quadword is
    The_Bytes : constant Byte_String := String_Of (The_Quadword);
  begin
    return Quadword_Of (Byte_String'(The_Bytes(The_Bytes'first + 7),
                                     The_Bytes(The_Bytes'first + 6),
                                     The_Bytes(The_Bytes'first + 5),
                                     The_Bytes(The_Bytes'first + 4),
                                     The_Bytes(The_Bytes'first + 3),
                                     The_Bytes(The_Bytes'first + 2),
                                     The_Bytes(The_Bytes'first + 1),
                                     The_Bytes(The_Bytes'first)));
  end Swap;


  function Swap (The_Value : Big_Endian_Quadword) return Quadword is
  begin
    return Swap (Quadword(The_Value));
  end Swap;


  function Swap (The_Value : Quadword) return Big_Endian_Quadword is
  begin
    return Big_Endian_Quadword(Quadword'(Swap (The_Value)));
  end Swap;


  procedure Swap (The_Quadword : in out Quadword) is
  begin
    The_Quadword := Swap (The_Quadword);
  end Swap;


  procedure Split (The_Time        :     Duration;
                   The_Seconds     : out Quadword;
                   The_Nanoseconds : out Longword) is
  begin
    if The_Time < 0.5 then
      The_Seconds := Quadword (The_Time);
    else
      The_Seconds := Quadword (The_Time - 0.5);
    end if;
    The_Nanoseconds := Longword((The_Time - Duration(The_Seconds)) * 1_000_000_000);
  end Split;

end Unsigned;
