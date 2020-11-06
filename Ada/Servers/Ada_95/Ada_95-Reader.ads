-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package Ada_95.Reader is

  type Element is record
    Tokens : Token.Sequence;
  end record;

  procedure Start;

  procedure Add (Filename : String);

  generic
    with procedure Visit (Item : Element);
  procedure Iterator;

  procedure Finalize;

end Ada_95.Reader;
