------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  The language-defined package Strings.Fixed provides string-handling
--  subprograms for fixed-length strings; that is, for values of type
--  Standard.String. Several of these subprograms are procedures that modify
--  the contents of a String that is passed as an out or an in out parameter;
--  each has additional parameters to control the effect when the logical
--  length of the result differs from the parameter's length.
--
--  For each function that returns a String, the lower bound of the returned
--  value is 1.
--
--  The basic model embodied in the package is that a fixed-length string
--  comprises significant characters and possibly padding (with space
--  characters) on either or both ends. When a shorter string is copied to a
--  longer string, padding is inserted, and when a longer string is copied to a
--  shorter one, padding is stripped. The Move procedure in Strings.Fixed,
--  which takes a String as an out parameter, allows the programmer to control
--  these effects. Similar control is provided by the string transformation
--  procedures.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Ada.Strings.Maps; use type Ada.Strings.Maps.Character_Mapping_Function;
with Ada.Strings.Search;

package Ada.Strings.Fixed with SPARK_Mode is
   pragma Preelaborate;

   --------------------------------------------------------------
   -- Copy Procedure for Strings of Possibly Different Lengths --
   --------------------------------------------------------------

   procedure Move
     (Source  : String;
      Target  : out String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space)
   with

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  The Move procedure copies characters from Source to Target. If Source
   --  has the same length as Target, then the effect is to assign Source to
   --  Target. If Source is shorter than Target then:
   --
   --  * If Justify=Left, then Source is copied into the first Source'Length
   --    characters of Target.
   --
   --  * If Justify=Right, then Source is copied into the last Source'Length
   --    characters of Target.
   --
   --  * If Justify=Center, then Source is copied into the middle Source'Length
   --    characters of Target. In this case, if the difference in length
   --    between Target and Source is odd, then the extra Pad character is on
   --    the right.
   --
   --  * Pad is copied to each Target character not otherwise assigned.
   --
   --  If Source is longer than Target, then the effect is based on Drop.
   --
   --  * If Drop=Left, then the rightmost Target'Length characters of Source
   --    are copied into Target.
   --
   --  * If Drop=Right, then the leftmost Target'Length characters of Source
   --    are copied into Target.
   --
   --  * If Drop=Error, then the effect depends on the value of the Justify
   --    parameter and also on whether any characters in Source other than Pad
   --    would fail to be copied:
   --
   --    * If Justify=Left, and if each of the rightmost
   --      Source'Length-Target'Length characters in Source is Pad, then the
   --      leftmost Target'Length characters of Source are copied to Target.
   --
   --    * If Justify=Right, and if each of the leftmost
   --      Source'Length-Target'Length characters in Source is Pad, then the
   --      rightmost Target'Length characters of Source are copied to Target.
   --
   --    * Otherwise, Length_Error is propagated.

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            => Pattern'length > 0
       and then Mapping /= null
       and then (if Source'length > 0 then From in Source'range),

     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'length = 0
        =>
          Index'result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'length > 0
          and then
            (for some J in
              (if Going = Forward then From else Source'first)
               .. (if Going = Forward then Source'last else From)
                - (Pattern'length - 1) =>
              Ada.Strings.Search.Match (Source, Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Index'result in
            (if Going = Forward then From else Source'first)
            .. (if Going = Forward then Source'last else From)
             - (Pattern'length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then
              Ada.Strings.Search.Match (Source, Pattern, Mapping, Index'result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'range =>
                 (if (if Going = Forward
                      then J in From .. Index'result - 1
                      else J - 1 in Index'result
                                    .. From - Pattern'length)
                  then not (Ada.Strings.Search.Match
                              (Source, Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'result = 0),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   pragma Ada_05 (Index);

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            => Pattern'length > 0
       and then (if Source'length > 0 then From in Source'range),

     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'length = 0
        =>
          Index'result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'length > 0
          and then
            (for some J in
              (if Going = Forward then From else Source'first)
              .. (if Going = Forward then Source'last else From)
               - (Pattern'length - 1) =>
              Ada.Strings.Search.Match (Source, Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Index'result in
            (if Going = Forward then From else Source'first)
            .. (if Going = Forward then Source'last else From)
             - (Pattern'length - 1)

          --  The slice beginning at the returned index matches Pattern

          and then
            Ada.Strings.Search.Match (Source, Pattern, Mapping, Index'result)

            --  The result is the smallest or largest index which satisfies the
            --  matching, respectively when Going = Forward and
            --  Going = Backward.

            and then
              (for all J in Source'range =>
                 (if (if Going = Forward
                      then J in From .. Index'result - 1
                      else J - 1 in Index'result
                                    .. From - Pattern'length)
                  then not (Ada.Strings.Search.Match
                              (Source, Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'result = 0),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   pragma Ada_05 (Index);

   --  Each Index function searches, starting from From, for a slice of
   --  Source, with length Pattern'Length, that matches Pattern with respect to
   --  Mapping; the parameter Going indicates the direction of the lookup. If
   --  Source is the null string, Index returns 0; otherwise, if From is not in
   --  Source'Range, then Index_Error is propagated. If Going = Forward, then
   --  Index returns the smallest index I which is greater than or equal to
   --  From such that the slice of Source starting at I matches Pattern. If
   --  Going = Backward, then Index returns the largest index I such that the
   --  slice of Source starting at I matches Pattern and has an upper bound
   --  less than or equal to From. If there is no such slice, then 0 is
   --  returned. If Pattern is the null string, then Pattern_Error is
   --  propagated.

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre            => Pattern'length > 0,

     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'length = 0
        =>
          Index'result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'length > 0
          and then
            (for some J in
              Source'first .. Source'last - (Pattern'length - 1) =>
                Ada.Strings.Search.Match (Source, Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Index'result in Source'first .. Source'last - (Pattern'length - 1)

            --  The slice beginning at the returned index matches Pattern

            and then
              Ada.Strings.Search.Match (Source, Pattern, Mapping, Index'result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'range =>
                 (if (if Going = Forward
                      then J <= Index'result - 1
                      else J - 1 in Index'result
                                    .. Source'last - Pattern'length)
                  then not (Ada.Strings.Search.Match
                              (Source, Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'result = 0),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Direction := Forward;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre            => Pattern'length > 0 and then Mapping /= null,

     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

       --  If Source is the empty string, then 0 is returned

       (Source'length = 0
        =>
          Index'result = 0,

        --  If some slice of Source matches Pattern, then a valid index is
        --  returned.

        Source'length > 0
          and then
            (for some J in
              Source'first .. Source'last - (Pattern'length - 1) =>
                Ada.Strings.Search.Match (Source, Pattern, Mapping, J))
        =>
          --  The result is in the considered range of Source

          Index'result in Source'first .. Source'last - (Pattern'length - 1)

          --  The slice beginning at the returned index matches Pattern

          and then
            Ada.Strings.Search.Match (Source, Pattern, Mapping, Index'result)

            --  The result is the smallest or largest index which satisfies
            --  the matching, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'range =>
                 (if (if Going = Forward
                      then J <= Index'result - 1
                      else J - 1 in Index'result
                                    .. Source'last - Pattern'length)
                  then not (Ada.Strings.Search.Match
                              (Source, Pattern, Mapping, J)))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'result = 0),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);

   --  If Going = Forward, returns:
   --
   --     Index (Source, Pattern, Source'First, Forward, Mapping)
   --
   --  otherwise, returns:
   --
   --     Index (Source, Pattern, Source'Last, Backward, Mapping).

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   with
     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

        --  If no character of Source satisfies the property Test on Set, then
        --  0 is returned.

       ((for all C of Source =>
           (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          Index'result = 0,

        --  Otherwise, an index in the range of Source is returned

        others
        =>

          --  The result is in the range of Source

          Index'result in Source'range

            --  The character at the returned index satisfies the property
            --  Test on Set.

            and then
              (Test = Inside)
              = Ada.Strings.Maps.Is_In (Source (Index'result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'range =>
                 (if J /= Index'result
                       and then (J < Index'result) = (Going = Forward)
                  then (Test = Inside)
                       /= Ada.Strings.Maps.Is_In (Source (J), Set)))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   with
     Pre            => (if Source'length > 0 then From in Source'range),
     Post           => Index'result in 0 | Source'range,
     Contract_Cases =>

        --  If Source is the empty string, or no character of the considered
        --  slice of Source satisfies the property Test on Set, then 0 is
        --  returned.

        (Source'length = 0
          or else
            (for all J in Source'range =>
               (if J = From or else (J > From) = (Going = Forward) then
                  (Test = Inside) /= Ada.Strings.Maps.Is_In (Source (J), Set)))
        =>
          Index'result = 0,

        --  Otherwise, an index in the considered range of Source is returned

        others
        =>

          --  The result is in the considered range of Source

          Index'result in Source'range
            and then (Index'result = From
                       or else
                         (Index'result > From) = (Going = Forward))

            --  The character at the returned index satisfies the property
            --  Test on Set.

            and then
              (Test = Inside)
              = Ada.Strings.Maps.Is_In (Source (Index'result), Set)

            --  The result is the smallest or largest index which satisfies
            --  the property, respectively when Going = Forward and Going =
            --  Backward.

            and then
              (for all J in Source'range =>
                 (if J /= Index'result
                    and then (J < Index'result) = (Going = Forward)
                    and then (J = From
                                or else (J > From) = (Going = Forward))
                  then (Test = Inside)
                       /= Ada.Strings.Maps.Is_In (Source (J), Set)))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   pragma Ada_05 (Index);
   --  Index searches for the first or last occurrence of any of a set of
   --  characters (when Test=Inside), or any of the complement of a set of
   --  characters (when Test=Outside). If Source is the null string, Index
   --  returns 0; otherwise, if From is not in Source'Range, then Index_Error
   --  is propagated. Otherwise, it returns the smallest index I >= From (if
   --  Going=Forward) or the largest index I <= From (if Going=Backward) such
   --  that Source(I) satisfies the Test condition with respect to Set; it
   --  returns 0 if there is no such Character in Source.

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   with
     Pre            => (if Source'length /= 0 then From in Source'range),

     Post           => Index_Non_Blank'result in 0 | Source'range,
     Contract_Cases =>

        --  If Source is the empty string, or all characters in the considered
        --  slice of Source are Space characters, then 0 is returned.

        (Source'length = 0
          or else
            (for all J in Source'range =>
               (if J = From or else (J > From) = (Going = Forward) then
                  Source (J) = ' '))
        =>
          Index_Non_Blank'result = 0,

        --  Otherwise, a valid index is returned

        others
        =>

          --  The result is in the considered range of Source

          Index_Non_Blank'result in Source'range
            and then (Index_Non_Blank'result = From
                        or else (Index_Non_Blank'result > From)
                                = (Going = Forward))

            --  The character at the returned index is not a Space character

            and then Source (Index_Non_Blank'result) /= ' '

            --  The result is the smallest or largest index which is not a
            --  Space character, respectively when Going = Forward and
            --  Going = Backward.

            and then
              (for all J in Source'range =>
                 (if J /= Index_Non_Blank'result
                       and then (J < Index_Non_Blank'result)
                                = (Going = Forward)
                       and then (J = From or else (J > From)
                                = (Going = Forward))
                  then Source (J) = ' '))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   pragma Ada_05 (Index_Non_Blank);
   --  Returns Index (Source, Maps.To_Set(Space), From, Outside, Going)

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   with
     Post           => Index_Non_Blank'result in 0 | Source'range,
     Contract_Cases =>

        --  If all characters of Source are Space characters, then 0 is
        --  returned.

       ((for all C of Source => C = ' ') => Index_Non_Blank'result = 0,

        --  Otherwise, a valid index is returned

        others                           =>

          --  The result is in the range of Source

          Index_Non_Blank'result in Source'range

            --  The character at the returned index is not a Space character

            and then Source (Index_Non_Blank'result) /= ' '

            --  The result is the smallest or largest index which is not a
            --  Space character, respectively when Going = Forward and Going
            --  = Backward.

            and then
              (for all J in Source'range =>
                 (if J /= Index_Non_Blank'result
                       and then (J < Index_Non_Blank'result)
                              = (Going = Forward)
                  then Source (J) = ' '))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  Returns Index (Source, Maps.To_Set(Space), Outside, Going)

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre      => Pattern'length /= 0,
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   function Count
     (Source  : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre      => Pattern'length /= 0 and then Mapping /= null,
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   --  Returns the maximum number of nonoverlapping slices of Source that match
   --  Pattern with respect to Mapping. If Pattern is the null string then
   --  Pattern_Error is propagated.

   function Count
     (Source : String;
      Set    : Maps.Character_Set) return Natural
   with
     Global   => null,
     Annotate => (GNATprove, Always_Return);
   --  Returns the number of occurrences in Source of characters that are in
   --  Set.

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre            => (if Source'length /= 0 then From in Source'range),
     Contract_Cases =>

        --  If Source is the empty string, or if no character of the considered
        --  slice of Source satisfies the property Test on Set, then First is
        --  set to From and Last is set to 0.

       (Source'length = 0
          or else
            (for all C of Source (From .. Source'last) =>
               (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          First = From and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others
        =>

          --  First and Last are in the considered range of Source

          First in From .. Source'last
            and then Last in First .. Source'last

            --  No character between From and First satisfies the property Test
            --  on Set.

            and then
              (for all C of Source (From .. First - 1) =>
                 (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))

            --  All characters between First and Last satisfy the property Test
            --  on Set.

            and then
              (for all C of Source (First .. Last) =>
                 (Test = Inside) = Ada.Strings.Maps.Is_In (C, Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Source'last
               then
                 (Test = Inside)
                 /= Ada.Strings.Maps.Is_In (Source (Last + 1), Set))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   pragma Ada_2012 (Find_Token);
   --  If Source is not the null string and From is not in Source'Range, then
   --  Index_Error is raised. Otherwise, First is set to the index of the first
   --  character in Source(From .. Source'Last) that satisfies the Test
   --  condition. Last is set to the largest index such that all characters in
   --  Source(First .. Last) satisfy the Test condition. If no characters in
   --  Source(From .. Source'Last) satisfy the Test condition, First is set to
   --  From, and Last is set to 0.

   procedure Find_Token
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural)
   with
     Pre            => Source'first > 0,
     Contract_Cases =>

        --  If Source is the empty string, or if no character of Source
        --  satisfies the property Test on Set, then First is set to From and
        --  Last is set to 0.

       (Source'length = 0
          or else
            (for all C of Source =>
               (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))
        =>
          First = Source'first and then Last = 0,

        --  Otherwise, First and Last are set to valid indexes

        others
        =>

          --  First and Last are in the considered range of Source

          First in Source'range
            and then Last in First .. Source'last

            --  No character before First satisfies the property Test on Set

            and then
              (for all C of Source (Source'first .. First - 1) =>
                 (Test = Inside) /= Ada.Strings.Maps.Is_In (C, Set))

            --  All characters between First and Last satisfy the property Test
            --  on Set.

            and then
              (for all C of Source (First .. Last) =>
                 (Test = Inside) = Ada.Strings.Maps.Is_In (C, Set))

            --  If Last is not Source'Last, then the character at position
            --  Last + 1 does not satify the property Test on Set.

            and then
              (if Last < Source'last
               then
                 (Test = Inside)
                 /= Ada.Strings.Maps.Is_In (Source (Last + 1), Set))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  Equivalent to Find_Token (Source, Set, Source'First, Test, First, Last)

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping_Function) return String
   with
     Pre    => Mapping /= null,
     Post   =>

       --  Lower bound of the returned string is 1

       Translate'result'first = 1

         --  The returned string has the same length as Source

         and then Translate'result'last = Source'length

         --  Each character in the returned string is the translation of the
         --  character at the same position in Source through Mapping.

         and then
           (for all J in Source'range =>
              Translate'result (J - Source'first + 1)
              = Mapping (Source (J))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);

   function Translate
     (Source  : String;
      Mapping : Maps.Character_Mapping) return String
   with
     Post   =>

       --  Lower bound of the returned string is 1

       Translate'result'first = 1

         --  The returned string has the same length as Source

         and then Translate'result'last = Source'length

         --  Each character in the returned string is the translation of the
         --  character at the same position in Source through Mapping.

         and then
           (for all J in Source'range =>
              Translate'result (J - Source'first + 1)
              = Ada.Strings.Maps.Value (Mapping, Source (J))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);

   --  Returns the string S whose length is Source'Length and such that S (I)
   --  is the character to which Mapping maps the corresponding element of
   --  Source, for I in 1 .. Source'Length.

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping_Function)
   with
     Pre      => Mapping /= null,
     Post     =>

       --  Each character in Source after the call is the translation of the
       --  character at the same position before the call, through Mapping.

       (for all J in Source'range => Source (J) = Mapping (Source'old (J))),
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   procedure Translate
     (Source  : in out String;
      Mapping : Maps.Character_Mapping)
   with
     Post     =>

       --  Each character in Source after the call is the translation of the
       --  character at the same position before the call, through Mapping.

       (for all J in Source'range =>
          Source (J) = Ada.Strings.Maps.Value (Mapping, Source'old (J))),
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   --  Equivalent to Source := Translate(Source, Mapping)

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : String;
      Low    : Positive;
      High   : Natural;
      By     : String) return String
   with
     Pre            =>
       Low - 1 <= Source'last
         and then High >= Source'first - 1
         and then (if High >= Low
                   then Natural'max (0, Low - Source'first)
                        <= Natural'last
                           - By'length
                           - Natural'max (Source'last - High, 0)
                   else Source'length <= Natural'last - By'length),

     --  Lower bound of the returned string is 1

     Post           => Replace_Slice'result'first = 1,
     Contract_Cases =>

        --  If High >= Low, then the returned string comprises
        --  Source (Source'First .. Low - 1) & By
        --  & Source(High + 1 .. Source'Last).

       (High >= Low =>

          --  Length of the returned string

          Replace_Slice'result'length
          = Integer'max (0, Low - Source'first)
            + By'length
            + Integer'max (Source'last - High, 0)

            --  Elements starting at Low are replaced by elements of By

            and then
              Replace_Slice'result (1 .. Integer'max (0, Low - Source'first))
              = Source (Source'first .. Low - 1)
            and then
              Replace_Slice'result
                (Integer'max (0, Low - Source'first) + 1
                 .. Integer'max (0, Low - Source'first) + By'length)
              = By

            --  When there are remaining characters after the replaced slice,
            --  they are appended to the result.

            and then
              (if High < Source'last
               then
                 Replace_Slice'result
                   (Integer'max (0, Low - Source'first) + By'length + 1
                    .. Replace_Slice'result'last)
                 = Source (High + 1 .. Source'last)),

        --  If High < Low, then the returned string is
        --  Insert (Source, Before => Low, New_Item => By).

        others      =>

          --  Length of the returned string

          Replace_Slice'result'length = Source'length + By'length

            --  Elements of By are inserted after the element at Low

            and then
              Replace_Slice'result (1 .. Low - Source'first)
              = Source (Source'first .. Low - 1)
            and then
              Replace_Slice'result
                (Low - Source'first + 1 .. Low - Source'first + By'length)
              = By

            --  When there are remaining characters after Low in Source, they
            --  are appended to the result.

            and then
              (if Low < Source'last
               then
                Replace_Slice'result
                  (Low - Source'first + By'length + 1
                   .. Replace_Slice'result'last)
                = Source (Low .. Source'last))),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  If Low > Source'Last + 1, or High < Source'First - 1, then Index_Error
   --  is propagated. Otherwise:
   --
   --  * If High >= Low, then the returned string comprises
   --    Source (Source'First .. Low - 1)
   --    & By & Source(High + 1 .. Source'Last), but with lower bound 1.
   --
   --  * If High < Low, then the returned string is
   --    Insert (Source, Before => Low, New_Item => By).

   procedure Replace_Slice
     (Source  : in out String;
      Low     : Positive;
      High    : Natural;
      By      : String;
      Drop    : Truncation := Error;
      Justify : Alignment  := Left;
      Pad     : Character  := Space)
   with
     Pre      =>
       Low - 1 <= Source'last
         and then High >= Source'first - 1
         and then (if High >= Low
                   then Natural'max (0, Low - Source'first)
                        <= Natural'last
                           - By'length
                           - Natural'max (Source'last - High, 0)
                   else Source'length <= Natural'last - By'length),

   --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --    Move (Replace_Slice (Source, Low, High, By),
   --          Source, Drop, Justify, Pad).

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String
   with
     Pre    =>
       Before - 1 in Source'first - 1 .. Source'last
         and then Source'length <= Natural'last - New_Item'length,

     Post   =>

       --  Lower bound of the returned string is 1

       Insert'result'first = 1

         --  Length of the returned string

         and then Insert'result'length = Source'length + New_Item'length

         --  Elements of New_Item are inserted after element at Before

         and then
           Insert'result (1 .. Before - Source'first)
           = Source (Source'first .. Before - 1)
         and then
           Insert'result
             (Before - Source'first + 1
              .. Before - Source'first + New_Item'length)
           = New_Item

         --  When there are remaining characters after Before in Source, they
         --  are appended to the returned string.

         and then
           (if Before <= Source'last
            then
              Insert'result
                (Before - Source'first + New_Item'length + 1
                 .. Insert'result'last)
              = Source (Before .. Source'last)),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  Propagates Index_Error if Before is not in
   --  Source'First .. Source'Last + 1; otherwise, returns
   --  Source (Source'First .. Before - 1)
   --  & New_Item & Source(Before..Source'Last), but with lower bound 1.

   procedure Insert
     (Source   : in out String;
      Before   : Positive;
      New_Item : String;
      Drop     : Truncation := Error)
   with
     Pre      =>
       Before - 1 in Source'first - 1 .. Source'last
         and then Source'length <= Natural'last - New_Item'length,

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to Move (Insert (Source, Before, New_Item), Source, Drop)

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String
   with
     Pre      =>
       Position - 1 in Source'first - 1 .. Source'last
         and then
           (if Position - Source'first >= Source'length - New_Item'length
            then Position - Source'first <= Natural'last - New_Item'length),

     Post     =>

       --  Lower bound of the returned string is 1

       Overwrite'result'first = 1

         --  Length of the returned string

         and then
           Overwrite'result'length
           = Integer'max (Source'length,
                          Position - Source'first + New_Item'length)

         --  Elements after Position are replaced by elements of New_Item

         and then
           Overwrite'result (1 .. Position - Source'first)
           = Source (Source'first .. Position - 1)
         and then
           Overwrite'result
             (Position - Source'first + 1
              .. Position - Source'first + New_Item'length)
           = New_Item

         --  If the end of Source is reached before the characters in New_Item
         --  are exhausted, the remaining characters from New_Item are appended
         --  to the string.

         and then
           (if Position <= Source'last - New_Item'length
            then
              Overwrite'result
                (Position - Source'first + New_Item'length + 1
                 .. Overwrite'result'last)
              = Source (Position + New_Item'length .. Source'last)),
     Global   => null,
     Annotate => (GNATprove, Always_Return);
   --  Propagates Index_Error if Position is not in
   --  Source'First .. Source'Last + 1; otherwise, returns the string obtained
   --  from Source by consecutively replacing characters starting at Position
   --  with corresponding characters from New_Item. If the end of Source is
   --  reached before the characters in New_Item are exhausted, the remaining
   --  characters from New_Item are appended to the string.

   procedure Overwrite
     (Source   : in out String;
      Position : Positive;
      New_Item : String;
      Drop     : Truncation := Right)
   with
     Pre      =>
       Position - 1 in Source'first - 1 .. Source'last
         and then
           (if Position - Source'first >= Source'length - New_Item'length
            then Position - Source'first <= Natural'last - New_Item'length),

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to Move(Overwrite(Source, Position, New_Item), Source, Drop)

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String
   with
     Pre            => (if From <= Through
                        then (From in Source'range
                                and then Through <= Source'last)),

     --  Lower bound of the returned string is 1

     Post           =>
       Delete'result'first = 1,

     Contract_Cases =>

        --  If From <= Through, the characters between From and Through are
        --  removed.

       (From <= Through =>

          --  Length of the returned string

          Delete'result'length = Source'length - (Through - From + 1)

            --  Elements before From are preserved

            and then
              Delete'result (1 .. From - Source'first)
              = Source (Source'first .. From - 1)

            --  If there are remaining characters after Through, they are
            --  appended to the returned string.

            and then
              (if Through < Source'last
               then Delete'result
                      (From - Source'first + 1 .. Delete'result'last)
                    = Source (Through + 1 .. Source'last)),

        --  Otherwise, the returned string is Source with lower bound 1

        others          =>
          Delete'result'length = Source'length
            and then Delete'result = Source),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  If From <= Through, the returned string is
   --  Replace_Slice(Source, From, Through, ""); otherwise, it is Source with
   --  lower bound 1.

   procedure Delete
     (Source  : in out String;
      From    : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   with
     Pre      => (if From <= Through
                  then (From in Source'range
                          and then Through <= Source'last)),

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --     Move (Delete (Source, From, Through),
   --           Source, Justify => Justify, Pad => Pad).

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : String;
      Side   : Trim_End) return String
   with
     Post     =>

       --  Lower bound of the returned string is 1

       (Trim'result'first = 1)

       --  If all characters in Source are Space, the returned string is
       --  empty.

         and then
           (if (for all J in Source'range => Source (J) = ' ')
            then Trim'result = ""

            --  Otherwise, the returned string is a slice of Source

            else
              (declare
                 Low  : constant Positive :=
                   (if Side = Right then Source'first
                    else Index_Non_Blank (Source, Forward));
                 High : constant Positive :=
                   (if Side = Left then Source'last
                    else Index_Non_Blank (Source, Backward));
               begin
                 Trim'result = Source (Low .. High)
              )
           ),
     Global   => null,
     Annotate => (GNATprove, Always_Return);
   --  Returns the string obtained by removing from Source all leading Space
   --  characters (if Side = Left), all trailing Space characters (if
   --  Side = Right), or all leading and trailing Space characters (if
   --  Side = Both).

   procedure Trim
     (Source  : in out String;
      Side    : Trim_End;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   with

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --     Move (Trim (Source, Side), Source, Justify=>Justify, Pad=>Pad).

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String
   with
     Post   =>

       --  Lower bound of the returned string is 1

       Trim'result'first = 1

         --  If all characters are contained in one of the sets Left and Right,
         --  then the returned string is empty.

         and then
           (if (for all K in Source'range =>
                  Ada.Strings.Maps.Is_In (Source (K), Left))
                 or
                   (for all K in Source'range =>
                      Ada.Strings.Maps.Is_In (Source (K), Right))
            then Trim'result = ""

        --  Otherwise, the returned string is a slice of Source

        else
           (declare
              Low  : constant Positive :=
                Index (Source, Left, Outside, Forward);
              High : constant Positive :=
                Index (Source, Right, Outside, Backward);
            begin
              Trim'result = Source (Low .. High))),
     Global   => null,
     Annotate => (GNATprove, Always_Return);
   --  Returns the string obtained by removing from Source all leading
   --  characters in Left and all trailing characters in Right.

   procedure Trim
     (Source  : in out String;
      Left    : Maps.Character_Set;
      Right   : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad     : Character := Space)
   with

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --     Move (Trim (Source, Left, Right),
   --           Source, Justify => Justify, Pad=>Pad).

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   with
     Post           =>

       --  Lower bound of the returned string is 1

       Head'result'first = 1

         --  Length of the returned string is Count.

         and then Head'result'length = Count,

     Contract_Cases =>

        --  If Count <= Source'Length, then the first Count characters of
        --  Source are returned.

       (Count <= Source'length =>
          Head'result = Source (Source'first .. Source'first - 1 + Count),

        --  Otherwise, the returned string is Source concatenated with
        --  Count - Source'Length Pad characters.

        others                 =>
          Head'result (1 .. Source'length) = Source
            and then
              Head'result (Source'length + 1 .. Count)
              = [1 .. Count - Source'length => Pad]),
     Global         => null,
     Annotate       => (GNATprove, Always_Return);
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the first Count characters of Source. Otherwise, its contents
   --  are Source concatenated with Count - Source'Length Pad characters.

   procedure Head
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   with

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --     Move (Head (Source, Count, Pad),
   --           Source, Drop => Error, Justify => Justify, Pad => Pad).

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   with
     Post   =>

       --  Lower bound of the returned string is 1

       Tail'result'first = 1

         --  Length of the returned string is Count

         and then Tail'result'length = Count,
     Contract_Cases =>

       --  If Count is zero, then the returned string is empty

       (Count = 0                     =>
          Tail'result = "",

        --  Otherwise, if Count <= Source'Length, then the last Count
        --  characters of Source are returned.

        (Count in 1 .. Source'length) =>
          Tail'result = Source (Source'last - Count + 1 .. Source'last),

        --  Otherwise, the returned string is Count - Source'Length Pad
        --  characters concatenated with Source.

        others                        =>

           --  If Source is empty, then the returned string is Count Pad
           --  characters.

          (if Source'length = 0
           then Tail'result = [1 .. Count => Pad]
           else
             Tail'result (1 .. Count - Source'length)
             = [1 .. Count - Source'length => Pad]
               and then
                 Tail'result (Count - Source'length + 1 .. Tail'result'last)
                 = Source)),
     Global   => null,
     Annotate => (GNATprove, Always_Return);
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the last Count characters of Source. Otherwise, its contents
   --  are Count-Source'Length Pad characters concatenated with Source.

   procedure Tail
     (Source  : in out String;
      Count   : Natural;
      Justify : Alignment := Left;
      Pad     : Character := Space)
   with

     --  Incomplete contract

     Global   => null,
     Annotate => (GNATprove, Might_Not_Return);
   --  Equivalent to:
   --
   --     Move (Tail (Source, Count, Pad),
   --           Source, Drop => Error, Justify => Justify, Pad => Pad).

   ----------------------------------
   -- String Constructor Functions --
   ----------------------------------

   function "*"
     (Left  : Natural;
      Right : Character) return String
   with
     Post     =>

       --  Lower bound of the returned string is 1

       "*"'result'first = 1

         --  Length of the returned string

         and then "*"'result'length = Left

         --  All characters of the returned string are Right

         and then (for all C of "*"'result => C = Right),
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   function "*"
     (Left  : Natural;
      Right : String) return String
   with
     Pre    => (if Right'length /= 0 then Left <= Natural'last / Right'length),

     Post   =>

       --  Lower bound of the returned string is 1

       "*"'result'first = 1

         --  Length of the returned string

         and then "*"'result'length = Left * Right'length

         --  Content of the string is Right concatenated with itself Left times

         and then
           (for all K in "*"'result'range =>
              "*"'result (K) = Right (Right'first + (K - 1) mod Right'length)),
     Global   => null,
     Annotate => (GNATprove, Always_Return);

   --  These functions replicate a character or string a specified number of
   --  times. The first function returns a string whose length is Left and each
   --  of whose elements is Right. The second function returns a string whose
   --  length is Left * Right'Length and whose value is the null string if
   --  Left = 0 and otherwise is (Left - 1)*Right & Right with lower bound 1.

end Ada.Strings.Fixed;
