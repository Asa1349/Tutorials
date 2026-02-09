-------------------------------------------------------------------------------
-- WHAT IS ADA USED FOR?
-- Ada is a highly structured, statically typed language designed for 
-- safety-critical systems (Aerospace, Defense, Railway, Medical Devices).
--
-- WHY ADA?
-- The focus is on reliability. The compiler is extremely strict and catches 
-- logical errors that in other languages might only cause a crash at runtime. 
-- Ada code is optimized for readability ("Read many times, write once").
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--  The following example introduces the fundamentals of Ada through a simple 
--  console application. The program prompts the user for two integers, 
--  calculates their sum, and categorizes the result as zero, positive, or 
--  negative.

--  Key concepts covered:
--       - Basic console I/O using Ada.Text_IO.
--       - Strong typing and explicit type conversion using attributes like 
--         'Value and 'Image.
--       - Ada’s unique syntax for control structures (if-elsif-else).
--       - The mandatory separation of declaration and execution blocks.
-------------------------------------------------------------------------------


with Ada.Text_IO; -- Import the standard package for Input/Output

procedure Ada_Introduction is -- The procedure must have the same name as the file itself.
   -- DECLARATION ZONE: Variables must be defined before the 'begin' keyword.
   -- Syntax: Name : Type;
   First_Number  : Integer;
   Second_Number : Integer;
   Result       : Integer;

begin
   -- User Input 1
   Ada.Text_IO.Put_Line ("First number: "); 
   -- 'Value is an attribute that converts a String (from Get_Line) to an Integer.
   First_Number := Integer'Value (Ada.Text_IO.Get_Line);

   -- User Input 2
   Ada.Text_IO.Put_Line ("Second number: ");
   Second_Number := Integer'Value (Ada.Text_IO.Get_Line);

   -- Calculation
   Result := First_Number + Second_Number;

   -- CONTROL STRUCTURE: if-elsif-else
   -- 'Image is an attribute that converts a numeric value back into a String.
   if Result = 0 then
      Ada.Text_IO.Put_Line ("The result is 0");
   elsif Result > 0 then
      Ada.Text_IO.Put_Line ("Positive result: " & Integer'Image (Result));
   else
      Ada.Text_IO.Put_Line ("Negative result: " & Integer'Image (Result));
   end if;

end Ada_Introduction;



-------------------------------------------------------------------------------
-- ADA CONCEPTS: STRONG TYPING & RANGES
--  Ada is a "Strongly Typed" language. 
--  You cannot mix Integer and Float in calculations without explicit conversion.
--
--  - Valid:   Float(10) * 2.5
--  - Invalid: 10 * 2.5 (Compiler Error!)
--  - Division: 5 / 2 is 2 (Integer) | 5.0 / 2.0 is 2.5 (Float)
-------------------------------------------------------------------------------
with Ada.Text_IO;

procedure Ada_Introduction is
   -- Custom types: This prevents logical errors!
   type Age_Type is range 0 .. 120;
   
   My_Age   : Age_Type := 25;
   My_Score : Integer  := 100;
   
   -- Floating point
   Pi       : Float := 3.14159;
   Radius   : Float := 5.0;
   Area     : Float;

begin
   -- 1. STRIKT TYPING (No implicit casting)
   -- Area := Pi * Radius * Radius; -- This works (all Float)
   -- Area := Pi * 5;              -- ERROR! Cannot multiply Float and Integer
   
   -- 2. EXPLICIT CONVERSION
   Area := Pi * Float(My_Score); -- You must explicitly tell Ada to convert
   
   -- 3. RANGE SAFETY
   -- My_Age := 150; -- The COMPILER or RUNTIME will throw an error here!
   -- My_Age := -5   -- The COMPILER or RUNTIME will throw an error here!

   Ada.Text_IO.Put_Line ("Area calculated with explicit cast: " & Float'Image(Area));

end Ada_Introduction;



-------------------------------------------------------------------------------
-- SEMANTIC SAFETY: Miles vs. Kilometers
-- Even though both use the same numeric representation (Float),
-- Ada prevents you from accidentally mixing them up.
-------------------------------------------------------------------------------

procedure Navigation_Example is
   -- Define distinct types for different units
   type Miles      is new Float;
   type Kilometers is new Float;

   Distance_US : Miles      := 10.0;
   Distance_EU : Kilometers := 16.0;
   
   Total_Distance : Kilometers;
begin
   -- 1. COMPILER ERROR:
   Total_Distance := Distance_US + Distance_EU;
   -- Ada says: "invalid operand types for operator "+""
   -- You cannot add Miles to Kilometers!

   -- 2. CORRECT WAY: Explicit Conversion and Calculation
   -- You must consciously convert the value:
   Total_Distance := Distance_EU + Kilometers(Distance_US * 1.609);
   
end Navigation_Example;



-------------------------------------------------------------------------------
-- ADA BEST PRACTICE: NUMERIC I/O
-- This version uses specialized packages for numeric input and output.
--
-- WHY USE THIS?
-- 1. Type Safety: It directly reads the expected type (Integer).
-- 2. Formatting: It allows precise control over the output (e.g., width).
-- 3. Efficiency: No manual string-to-integer conversion needed.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO; -- Specialized package for Integer I/O

procedure Numeric_IO is 
   -- Renaming packages is a common best practice to keep code readable 
   -- while avoiding name collisions from global 'use' clauses.
   package TIO renames Ada.Text_IO;
   package ITIO renames Ada.Integer_Text_IO;

   First_Number  : Integer;
   Second_Number : Integer;
   Result        : Integer;

begin
   -- User Input using ITIO.Get
   TIO.Put ("Enter first number: "); 
   ITIO.Get (First_Number); -- Reads an integer directly from input

   TIO.Put ("Enter second number: ");
   ITIO.Get (Second_Number);

   -- Calculation
   Result := First_Number + Second_Number;

   -- Output using ITIO.Put
   TIO.Put ("The result is: ");
   -- Width => 0 ensures the number is printed without leading spaces.
   ITIO.Put (Item => Result, Width => 0);
   -- Note: TIO.Put does not start a new line. 
   -- Even if the code is on separate lines, the output will appear on the same line.
   -- Use TIO.New_Line or TIO.Put_Line to move the cursor to the next line.
   TIO.New_Line;

   -- Logic for result classification
   if Result = 0 then
      TIO.Put_Line ("Classification: The result is exactly zero.");
   elsif Result > 0 then
      TIO.Put_Line ("Classification: The result is positive.");
   else
      TIO.Put_Line ("Classification: The result is negative.");
   end if;

end Numeric_IO;
