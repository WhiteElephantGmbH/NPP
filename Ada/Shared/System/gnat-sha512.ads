------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . S H A 5 1 2                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2009-2011, Free Software Foundation, Inc.           --
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

--  This package implements the SHA-512 secure hash function as described in
--  FIPS PUB 180-3. The complete text of FIPS PUB 180-3 can be found at:
--    http://csrc.nist.gov/publications/fips/fips180-3/fips180-3_final.pdf

--  See the declaration of GNAT.Secure_Hashes.H in g-sechas.ads for complete
--  documentation.

with GNAT.Secure_Hashes.SHA2_Common;
with GNAT.Secure_Hashes.SHA2_64;
with System;

package GNAT.SHA512 is new GNAT.Secure_Hashes.H
  (Block_Words    => GNAT.Secure_Hashes.SHA2_Common.Block_Words,
   State_Words    => 8,
   Hash_Words     => 8,
   Hash_Bit_Order => System.High_Order_First,
   Hash_State     => GNAT.Secure_Hashes.SHA2_64.Hash_State,
   Initial_State  => GNAT.Secure_Hashes.SHA2_64.SHA512_Init_State,
   Transform      => GNAT.Secure_Hashes.SHA2_64.Transform);
