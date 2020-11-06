-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token.Data;

package Ada_95.Token.Checker is

  procedure Define_Block_Label_Usage (Unit      : Data.Unit_Handle;
                                      The_Style : Lexical.Style_Pragma) with Inline;

  function Is_Ok (Item      : Identifier_Handle;
                  The_Style : Lexical.Style_Pragma) return Boolean with Inline;

  function Obsolescent_Pragma_Check (The_Style : Lexical.Style_Pragma) return Boolean;

  function Is_Restricted  (The_Style : Lexical.Style_Pragma) return Boolean;

end Ada_95.Token.Checker;
