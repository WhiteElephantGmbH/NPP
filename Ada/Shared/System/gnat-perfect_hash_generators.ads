------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          G N A T . P E R F E C T _ H A S H _ G E N E R A T O R S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2002-2022, AdaCore                     --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a generator of static minimal perfect hash functions.
--  To understand what a perfect hash function is, we define several notions.
--  These definitions are inspired from the following paper:

--    Zbigniew J. Czech, George Havas, and Bohdan S. Majewski ``An Optimal
--    Algorithm for Generating Minimal Perfect Hash Functions'', Information
--    Processing Letters, 43(1992) pp.257-264, Oct.1992

--  Let W be a set of m words. A hash function h is a function that maps the
--  set of words W into some given interval I of integers [0, k-1], where k is
--  an integer, usually k >= m. h (w) where w is a word in W computes an
--  address or an integer from I for the storage or the retrieval of that
--  item. The storage area used to store items is known as a hash table. Words
--  for which the same address is computed are called synonyms. Due to the
--  existence of synonyms a situation called collision may arise in which two
--  items w1 and w2 have the same address. Several schemes for resolving
--  collisions are known. A perfect hash function is an injection from the word
--  set W to the integer interval I with k >= m.  If k = m, then h is a minimal
--  perfect hash function. A hash function is order preserving if it puts
--  entries into the hash table in a prespecified order.

--  A minimal perfect hash function is defined by two properties:

--    Since no collisions occur each item can be retrieved from the table in
--    *one* probe. This represents the "perfect" property.

--    The hash table size corresponds to the exact size of W and *no larger*.
--    This represents the "minimal" property.

--  The functions generated by this package require the words to be known in
--  advance (they are "static" hash functions). The hash functions are also
--  order preserving. If w2 is inserted after w1 in the generator, then h (w1)
--  < h (w2). These hashing functions are convenient for use with realtime
--  applications.

with System.Perfect_Hash_Generators;

package GNAT.Perfect_Hash_Generators is

   package SPHG renames System.Perfect_Hash_Generators;

   Default_K_To_V : constant Float  := 2.05;
   --  Default ratio for the algorithm. When K is the number of keys, V =
   --  (K_To_V) * K is the size of the main table of the hash function. To
   --  converge, the algorithm requires K_To_V to be strictly greater than 2.0.

   Default_Pkg_Name : constant String := "Perfect_Hash";
   --  Default package name in which the hash function is defined

   Default_Position : constant String := "";
   --  The generator allows selection of the character positions used in the
   --  hash function. By default, all positions are selected.

   Default_Tries : constant Positive := 20;
   --  This algorithm may not succeed to find a possible mapping on the first
   --  try and may have to iterate a number of times. This constant bounds the
   --  number of tries.

   type Optimization is new SPHG.Optimization;
   --  Optimize either the memory space or the execution time. Note: in
   --  practice, the optimization mode has little effect on speed. The tables
   --  are somewhat smaller with Memory_Space.

   Verbose : Boolean renames SPHG.Verbose;
   --  Output the status of the algorithm. For instance, the tables, the random
   --  graph (edges, vertices) and selected char positions are output between
   --  two iterations.

   procedure Initialize
     (Seed   : Natural;
      K_To_V : Float        := Default_K_To_V;
      Optim  : Optimization := Memory_Space;
      Tries  : Positive     := Default_Tries);
   --  Initialize the generator and its internal structures. Set the ratio of
   --  vertices over keys in the random graphs. This value has to be greater
   --  than 2.0 in order for the algorithm to succeed. The word set is not
   --  modified (in particular when it is already set). For instance, it is
   --  possible to run several times the generator with different settings on
   --  the same words.
   --
   --  A classical way of doing is to Insert all the words and then to invoke
   --  Initialize and Compute. If this fails to find a perfect hash function,
   --  invoke Initialize again with other configuration parameters (probably
   --  with a greater K_To_V ratio). Once successful, invoke Produce and then
   --  Finalize.

   procedure Finalize;
   --  Deallocate the internal structures and the words table

   procedure Insert (Value : String);
   --  Insert a new word into the table. ASCII.NUL characters are not allowed.

   Too_Many_Tries : exception renames SPHG.Too_Many_Tries;
   --  Raised after Tries unsuccessful runs

   procedure Compute (Position : String := Default_Position);
   --  Compute the hash function. Position allows the definition of selection
   --  of character positions used in the word hash function. Positions can be
   --  separated by commas and ranges like x-y may be used. Character '$'
   --  represents the final character of a word. With an empty position, the
   --  generator automatically produces positions to reduce the memory usage.
   --  Raise Too_Many_Tries if the algorithm does not succeed within Tries
   --  attempts (see Initialize).

   procedure Produce
     (Pkg_Name   : String  := Default_Pkg_Name;
      Use_Stdout : Boolean := False);
   --  Generate the hash function package Pkg_Name. This package includes the
   --  minimal perfect Hash function. The output is normally placed in the
   --  current directory, in files X.ads and X.adb, where X is the standard
   --  GNAT file name for a package named Pkg_Name. If Use_Stdout is True, the
   --  output goes to standard output, and no files are written.

end GNAT.Perfect_Hash_Generators;
