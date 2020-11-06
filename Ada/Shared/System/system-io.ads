------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M _ I O                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  A simple text I/O package, used for diagnostic output in the runtime,
--  This package is also preelaborated, unlike Text_Io, and can thus be
--  with'ed by preelaborated library units. It includes only Put routines
--  for character, integer, string and a new line function

package System.IO is
   pragma Preelaborate;

   procedure Put (X : Integer);

   procedure Put (C : Character);

   procedure Put (S : String);
   procedure Put_Line (S : String);

   procedure New_Line (Spacing : Positive := 1);

   type File_Type is limited private;

   function Standard_Error return File_Type;
   function Standard_Output return File_Type;

   procedure Set_Output (File : File_Type);

private

   type File_Type is (Stdout, Stderr);
   --  Stdout = Standard_Output, Stderr = Standard_Error

   pragma Inline (Standard_Error);
   pragma Inline (Standard_Output);

end System.IO;
