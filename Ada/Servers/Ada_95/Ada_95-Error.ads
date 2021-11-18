-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Source;
with Memory;

package Ada_95.Error is

  subtype Column_Range is Source.Column_Range;

  type Kind is
    (Base_Delimiter_Not_Expected,
     Base_Missing,
     Based_Number_Ends_With_Underscore,
     Based_Number_Not_Terminated,
     Based_Number_Starts_With_Radix_Point,
     Based_Number_Starts_With_Underscore,
     Based_Number_With_Double_Underscore,
     Designator_Expected,
     Digit_Expected,
     Digit_Not_Expected,
     Duplicate_Radix_Point,
     End_Of_File_Error,
     End_Of_File_Expected,
     Exponent_Ends_With_Underscore,
     Exponent_Digit_Expected,
     Exponent_Digit_Not_Expected,
     Exponent_Number_With_Double_Underscore,
     Exponent_Value_Expected,
     Extended_Digit_Expected,
     Extended_Digit_Not_Expected,
     Identifier_Starts_With_Underscore,
     Identifier_Ends_With_Underscore,
     Identifier_With_Double_Underscore,
     Negative_Exponent_Not_Allowed,
     Number_Ends_With_Underscore,
     Number_Starts_With_Radix_Point,
     Number_With_Double_Underscore,
     Real_Base_Not_Allowed,

     Goto_Not_Allowed, -- Incorrect_Style'first
     Incorrect_Identifier,
     Missing_Exception_Handler,
     Restricted_Function_Expression,
     Suspicious_Form_Of_Entry_Call, --Incorrect_Style'last

     Ada_Project_Path_Missing, -- In_Semantic'first
     Already_Defined,
     Compiler_Not_Defined,
     Icon_Not_Allowed_For_Dlls,
     Interface_Not_Defined,
     Interface_Specification_Expected,
     Kind_Not_Defined,
     Library_Not_Found,
     Library_Id_Not_Found,
     Obsolescent_Pragma_Call,
     Only_For_Dlls,
     Parent_Unit_Name_Error,
     Resource_File_Not_Found,
     Special_Comment_In_Use,
     Style_Already_Set,
     Unit_Kind_Error,
     Unit_Name_Error,
     Unknown_Boolean_Value,
     Unknown_Project_Kind,
     Unknown_Specification,
     Unknown_Tools_Directory,
     Version_Not_Defined,
     Version_Number_Out_Of_Range, -- In_Semantic'last

     String_Not_Terminated,
     Syntax_Error,
     Underscore_Not_Expected,
     Unknown_Character,

     Not_Implemented);

  subtype Incorrect_Style is Kind range Goto_Not_Allowed .. Suspicious_Form_Of_Entry_Call;

  subtype In_Semantic is Kind range Ada_Project_Path_Missing .. Version_Number_Out_Of_Range;


  procedure Add (Message  : Kind;
                 Item     : String;
                 Position : Column_Range);

  type Handle is private;

  function Handle_Of (Item : String) return Handle with Inline;

  function Image_Of (Item : Handle) return String with Inline;

  function Length_Of (Item : Handle) return Natural with Inline;

private
  type Handle is access String;
  for Handle'storage_pool use Memory.Pool.all;
end Ada_95.Error;
