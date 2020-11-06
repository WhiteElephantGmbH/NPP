------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . W I D E _ W I D E _ S T R I N G _ S P L I T          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Useful wide_string-manipulation routines: given a set of separators, split
--  a wide_string wherever the separators appear, and provide direct access
--  to the resulting slices. See GNAT.Array_Split for full documentation.

with Ada.Strings.Wide_Wide_Maps; use Ada.Strings;
with GNAT.Array_Split;

package GNAT.Wide_Wide_String_Split is new GNAT.Array_Split
  (Element          => Wide_Wide_Character,
   Element_Sequence => Wide_Wide_String,
   Element_Set      => Wide_Wide_Maps.Wide_Wide_Character_Set,
   To_Set           => Wide_Wide_Maps.To_Set,
   Is_In            => Wide_Wide_Maps.Is_In);
