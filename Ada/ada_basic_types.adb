-------------------------------------------------------------------------------
--  IDENTIFIERS
--  Ada identifiers are case insensitive 
--  e.g. lowercase, UPPERCASE, snake_case, PascalCase, Mixed_Case, camelCase
--  Identifiers have to start with a letter and end with a letter or a number,
--  they must not end with an underscore
--  They may contain non-consecutive underscores

--  While Ada is case-insensitive, the community standard (Best Practice) is 
--  to use Mixed_Case_With_Underscores for almost everything 
--  (variables, subprograms, and packages).
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  RESERVED WORDS
--  Ada has a set of reserved words (keywords) that cannot be used as 
--  identifiers for variables, constants, or subprograms.
--
--  - They are case-insensitive (BEGIN = begin = Begin).
--  - Best Practice: Use lowercase for reserved words and Mixed_Case for 
--    your own identifiers to improve readability.
-------------------------------------------------------------------------------

-- Example of Best Practice formatting:
procedure Hello_World is -- 'procedure' and 'is' are reserved words
begin                    -- 'begin' is a reserved word
   Put_Line("Hello!");
end Hello_World;         -- 'end' is a reserved word


-------------------------------------------------------------------------------
--  NUMBERS
--  Ada provides unique ways to represent numbers for better readability.
--
--  - Readability: Use underscores to separate groups (e.g., 1_000_000).
--  - Based Literals: Represent numbers in bases 2 to 16.
--    Syntax: Base#Value# -> 16#FF# is 255.
--  - Strict Floats: Floats must have a digit before and after the dot.
--    Valid: 1.0, 0.5 | Invalid: 1. , .5
-------------------------------------------------------------------------------

-- Code Examples:
Population : Integer := 8_000_000;   -- Easy to read
Hex_Value  : Integer := 16#A0#;      -- 160 in decimal
Pi         : Float   := 3.141_59;    -- Note the underscore for clarity!


-------------------------------------------------------------------------------
--  VARIABLE DECLARATION
--  Defined by one (or several) names, 
--  followed by :
--  followed by type reference
--  and possibly an initial value
-------------------------------------------------------------------------------

A : Integer;
B : Integer := 7;
C : constant Integer := 42;
D,E : Integer := F(5);
-- F is a function being called here.
-- Note: F(5) is executed TWICE (once for D and once for E).
-- This is important if F returns different values on each call!

--  Elaboration is done sequentially!
--  Variables must be declared before they are used.
A : Integer := 7;
B : Integer := A;
C : Integer := D; -- COMPILATION ERROR!!! D Value is not yet known.
D : Integer := 9;

--  Initialization is called for each variable individually
A, B : Float := New_Float_Number;
--  is equivalent to:
A : Float := New_Float_Number;
B : Float := New_Float_Number;

--  Note: ':=' on a declaration is an initialization, not an assignment!