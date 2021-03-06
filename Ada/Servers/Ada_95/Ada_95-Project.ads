-- *********************************************************************************************************************
-- *                   (c) 2008 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Error;
with Ada_95.Library;
with Ada_95.Token;
with Server;
with String_List;

package Ada_95.Project is

  type Application_Kind is (Console_Application, Windows_Application);

  type Error_Kind is new Error.Kind;

  subtype Token_Kind is Standard.Server.Token_Kind;

  procedure Initialize (Work_Path : String_List.Item);

  procedure Set_Console_Application;

  function Application_Kind_Image return String;

  procedure Finalize;

  function Token_Handle_Of (Filename : String;
                            Content  : String := "") return Token.Handle;

  function Token_Handle_Of (Filename : String;
                            Line     : Token.Line_Number;
                            Column   : Token.Column_Position;
                            Content  : String) return Token.Handle;

  function Full_Name_Of (Item : Token.Handle) return String;

  function Local_Unit return Library.Handle;

  function Unit_Of (Filename : String;
                    Content  : String) return Library.Handle;

  function Reference_Of (Filename : String;
                         Line     : Token.Line_Number;
                         Column   : Token.Column_Position;
                         Content  : String := "") return Token.Handle;


  function Token_Kind_Of (Item : Token.Handle) return Token_Kind;


  function First_Error_Of (Filename : String;
                           Content  : String := "") return Token.Handle;

  function Next_Error return Token.Handle;

  function Error_Kind_Of (Item : Token.Handle) return Error_Kind;

end Ada_95.Project;
