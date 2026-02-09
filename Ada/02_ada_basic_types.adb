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

First_Variable : Integer := 7;
Hello_World_Statement: String :-= "Hello World!"


-------------------------------------------------------------------------------
--  RESERVED WORDS
--  Ada has a set of reserved words (keywords) that cannot be used as 
--  identifiers for variables, constants, or subprograms.
--
--  - They are case-insensitive (BEGIN = begin = Begin).
--  - Best Practice: Use lowercase for reserved words and Mixed_Case for 
--    your own identifiers to improve readability.
-------------------------------------------------------------------------------

procedure Hello_World is -- 'procedure' and 'is' are reserved words
begin                    -- 'begin' is a reserved word
   Put_Line("Hello!");
end Hello_World;         -- 'end' is a reserved word


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

Population : Integer := 8_000_000;   -- Easy to read
Hex_Value  : Integer := 16#A0#;      -- 160 in decimal
Pi         : Float   := 3.141_59;    -- Note the underscore for clarity!



-------------------------------------------------------------------------------
-- CUSTOM TYPES DECLARATION
-- Ada allows you to define types that precisely match your data's logic.
-- This catches errors at compile-time that other languages only see at runtime.
-- Types are named and associated with specific properties (ranges, attributes) 
-- and operators.
-------------------------------------------------------------------------------

procedure Custom_Types_Example is
   -- 1. Integer Types with Ranges
   -- The compiler ensures the value never leaves these bounds.
   type Score      is range 0 .. 20;
   type Percentage is range -100 .. 100;

   -- 2. Enumeration Types
   -- Define a list of possible values (more readable than using integers).
   type Color   is (Red, Green, Blue, Yellow, Black);
   type Ternary is (True, False, Unknown);

   -- 3. Float Types with Precision
   -- You define the minimum number of significant decimal digits.
   type Distance    is digits 10;
   type Temperature is digits 5 range -273.15 .. 1_000_000.0;
   -- Note: Adding a range to a float can decrease performance but adds safety!

   ----------------------------------------------------------------------------
   -- CREATING TYPES FROM EXISTING TYPES (Derived Types)
   ----------------------------------------------------------------------------
   -- You can create a new type based on an existing one.
   -- They will be incompatible even if they have the same representation!
   type Math_Score is new Score;
   
   -- New types can also further restrict the range.
   type Exam_Score   is new Score range 0 .. 10;
   type Primary_Color is new Color range Red .. Blue;

begin
   -- SEMANTIC /= REPRESENTATION
   -- Even though Score and Math_Score are both integers:
   -- Score_Var := Math_Score_Var; -- COMPILER ERROR! due to the different range
   -- This prevents logical errors. 
   null;
end Custom_Types_Example;


-------------------------------------------------------------------------------
-- TYPE CONVERSION
-- In Ada, conversion between types must be EXPLICIT. 
-- This ensures that the programmer is aware of potential data loss or checks.
-------------------------------------------------------------------------------

procedure Conversion_Demo is
   -- Types with similar structure can be converted
   type T1 is range 0 .. 10;
   type T2 is range 1 .. 10;

   V1 : T1 := 0;
   V2 : T2;
   
   V_Float   : Float := 1.0;
   V_Integer : Integer;

begin
   -- 1. Explicit Cast for Basic Types
   V_Integer := Integer(V_Float); -- You must explicitly call Integer()

   -- 2. Conversion and Verification
   -- Conversion is possible if types have the same structure or are derived.
   -- BUT: Ada performs a verification (check) during conversion!
   
   -- V2 := T2(V1); 
   -- This compiles, but results in a RUN-TIME ERROR (Constraint_Error)
   -- because V1 is '0', which is outside the valid range of T2 (1..10).

   null;
end Conversion_Demo;



-------------------------------------------------------------------------------
-- TYPE CONVERSION PRACTICAL EXAMPLE: Water Tank Safety Systems
-- This demonstrates why explicit conversion and runtime checks are crucial.
-------------------------------------------------------------------------------

procedure Tank_Safety_System is
   -- A sensor might measure a wide physical range
   type Raw_Sensor_Reading is range 0 .. 5000;
   
   -- Our specific tank only allows a safe operating range
   type Safe_Level is range 1 .. 100;

   Sensor_Value : Raw_Sensor_Reading;
   Current_Safe : Safe_Level;

begin
   -- SCENARIO 1: Everything is fine
   Sensor_Value := 50; 
   -- Explicit conversion: Programmer says "I know I am converting this"
   Current_Safe := Safe_Level(Sensor_Value);
   -- Success: 50 is within 1 .. 100

   -- SCENARIO 2: Dangerous levels (Runtime Error)
   Sensor_Value := 500; 
   
   -- In other languages, Current_Safe might just become 100 (clamped) 
   -- or overflow silently. In Ada:
   Current_Safe := Safe_Level(Sensor_Value);
   -- RUNTIME ERROR: Constraint_Error is raised!
   -- The system stops immediately instead of operating with a "wrong" 100.
end Tank_Safety_System;



-------------------------------------------------------------------------------
--  STRINGS
--  Strings in Ada are fixed-length arrays of characters.
-------------------------------------------------------------------------------

-- 1. Standard String (Length is fixed at initialization)
Message : String := "Ada is powerful";

-- Declaration with direct assignment (length automatically becomes 5)
Greeting : String := "Hello"; 

-- Fixed-length declaration (must be exactly 10 characters)
Name : String (1 .. 10) := "Ada Lovela";

-- 2. Concatenation using '&'
Full_Greeting : String := "Hello " & "World";

-- 3. 'Image Attribute
-- Converts a scalar value (like Integer) to a String.
-- Note: It often adds a leading space for positive numbers!
Result_Text : String := Integer'Image(100); -- Result: " 100"

--  A quick preview: If you need strings in Ada that can change their length 
--  (as in other languages), there is a package for this called Ada.Strings.Unbounded.
