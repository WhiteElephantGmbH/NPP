------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                    S Y S T E M . S T R I N G _ O P S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This package contains functions for runtime operations on strings
--  (other than runtime comparison, found in s-strcom.ads).

--  NOTE: This package is obsolescent. It is no longer used by the compiler
--  which now generates concatenation inline. It is retained only because
--  it may be used during bootstrapping using old versions of the compiler.

pragma Compiler_Unit_Warning;

package System.String_Ops is
   pragma Pure;

   function Str_Concat (X, Y : String) return String;
   --  Concatenate two strings and return resulting string

   function Str_Concat_SC (X : String; Y : Character) return String;
   --  Concatenate string and character

   function Str_Concat_CS (X : Character; Y : String) return String;
   --  Concatenate character and string

   function Str_Concat_CC (X, Y : Character) return String;
   --  Concatenate two characters

end System.String_Ops;
