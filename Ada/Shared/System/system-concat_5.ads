------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 5                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2008-2020, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains a procedure for runtime concatenation of five string
--  operands. It is used when we want to save space in the generated code.

pragma Compiler_Unit_Warning;

package System.Concat_5 is

   procedure Str_Concat_5 (R : out String; S1, S2, S3, S4, S5 : String);
   --  Performs the operation R := S1 & S2 & S3 & S4 & S5. The bounds
   --  of R are known to be correct (usually set by a call to the
   --  Str_Concat_Bounds_5 procedure below), so no bounds checks are required,
   --  and it is known that none of the input operands overlaps R. No
   --  assumptions can be made about the lower bounds of any of the operands.

   procedure Str_Concat_Bounds_5
     (Lo, Hi             : out Natural;
      S1, S2, S3, S4, S5 : String);
   --  Assigns to Lo..Hi the bounds of the result of concatenating the five
   --  given strings, following the rules in the RM regarding null operands.

end System.Concat_5;
