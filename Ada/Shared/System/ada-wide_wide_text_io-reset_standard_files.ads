------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                ADA.WIDE_WIDE_TEXT_IO.RESET_STANDARD_FILES                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2009, Free Software Foundation, Inc.            --
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

--  This package provides a reset routine that resets the standard files used
--  by Ada.Wide_Wide_Text_IO. This is useful in systems such as VxWorks where
--  Ada.Wide_Wide_Text_IO is elaborated at the program start, but a system
--  restart may alter the status of these files, resulting in incorrect
--  operation of Wide_Wide_Text_IO (in particular if the standard input file
--  is changed to be interactive, then Get_Line may hang looking for an extra
--  character after the end of the line.

procedure Ada.Wide_Wide_Text_IO.Reset_Standard_Files;
--  Reset standard Wide_Wide_Text_IO files as described above
