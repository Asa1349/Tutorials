-------------------------------------------------------------------------------
-- IF - ELSIF - ELSE
-- Ada uses 'elsif' (with only one 'e')
-- Every 'if' block must be closed with 'end if' 
-------------------------------------------------------------------------------

procedure Temperature_Monitor is
   Temperature : Integer := 105;

begin
   if Temperature < 0 then
      Ada.Text_IO.Put_Line ("System status: Frozen.");

   elsif Temperature >= 0 and Temperature < 50 then
      Ada.Text_IO.Put_Line ("Sytsem status: Optimal");

   elsif Temperature >= 50 and Temperature < 100 then
      Ada.Text_IO.Put_Line ("System status: Warning, temperature rising!");

   elsif Temperature >= 100 then
      Ada.Text_IO.Put_Line ("System status: CRITICAL! Shutting down.");
   
   else
      -- Technically, this code block will never be reached, 
      -- but it's a good plac efor error handling.
      Ada.Text_IO.Put_Line ("System status: Unknown.");
   end if;
end Temperature_Monitor;

-- 1. ELSIF
--    Use 'elsif' instead of 'else if'. This is the most common mistake 
--    made by people switching over. In Ada, it is always 'elsif'.
--
-- 2. THEN
--    Then is mandatory: The word 'then' must follow every condition (except 'else').
--
-- 3. END IF
--    End if: Every if structure must be explicitly terminated with 'end if'.
--
-- 4. LOGICAL OPERATORS 
--    As in the example, you can combine conditions using 
--    'and', 'or', or 'not'.
--
-- 5. INDENTATION
--    It is good practice to indent the code within the blocks 
--    to make it more readable.
--  
-- Why is this better than using many small if statements?
--   • In an if-elseif-else chain, only the first block whose condition is true 
--     is executed. 
--   • As soon as a condition is true, the program jumps directly to the end 
--     of the if statement. 
--   • This is more efficient and prevents logical errors that would result in 
--     multiple warnings being issued simultaneously.


-------------------------------------------------------------------------------
-- CASE STATEMENTS: THE RULES OF PRECISION
-- Unlike 'if-elsif', a 'case' statement in Ada is strictly checked by the 
-- compiler for coverage and overlap.
-------------------------------------------------------------------------------

-- Example from above with case instead of if-statements
procedure Temperature_Monitor_Case is
   Temperature : Integer := 105;

begin
   case Temperature is
      -- For “less than 0,” we use the lower limit of the integer type.
      when Integer'First .. -1 =>
         Ada.Text_IO.Put_Line ("System status: Frozen.");
      
      when 0 .. 49 =>
         Ada.Text_IO.Put_Line ("Sytsem status: Optimal");

      when 50 .. 99 =>
         Ada.Text_IO.Put_Line ("System status: Warning, temperature rising!");

      -- For “greater than or equal to 100,” we use the upper limit of the type
      when 100 .. Integer'Last =>
         Ada.Text_IO.Put_Line ("System status: CRITICAL! Shutting down.");

      -- In Ada, ‘others’ is almost always mandatory to ensure that EVERY possible value is covered.
      when others =>
         Ada.Text_IO.Put_Line ("System status: Unknown.");
   end case;
end Temperature_Monitor_Case;


--  Example with Enumeration
procedure Case_Study is
   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   Today : Day := Mon;
   Score : Integer := 85;
begin
   case Today is
      when Mon .. Fri =>
         Ada.Text_IO.Put_Line ("Back to work.");
      when Sat | Sun =>
         Ada.Text_IO.Put_Line ("Weekend!");
   end case;
end Case_Study;


-- 1. DISCRETE TYPES ONLY
--    Case statements only work with discrete types: Integers, Characters, 
--    and Enumerations. You CANNOT use 'case' for Floating Point numbers.
--
-- 2. FULL COVERAGE (Total Exhaustiveness)
--    The compiler ensures that EVERY possible value of the type is handled.
--    If you forget a single number or value, the code will not compile.
--    Use 'when others =>' as a catch-all if you don't list every value.
--
-- 3. NO OVERLAP
--    Values or ranges cannot overlap. Each input must have exactly one 
--    matching branch. This prevents ambiguous logic.
--
-- 4. RANGES AND CHOICES
--    - Use '..' to define a range (e.g., 1 .. 10).
--    - Use '|' to separate multiple individual choices (e.g., 1 | 3 | 5).
--    - Use attributes like 'First' and 'Last' to cover extremes.


-------------------------------------------------------------------------------
-- FOR LOOP
-- A for loop allows iteration through a discrete range.
-- The loop parameter (e.g., 'Hour') is automatically declared and local 
-- to the loop. You cannot modify it manually inside the loop!
-------------------------------------------------------------------------------

procedure Measurement_Loop is
begin
   -- Standard loop
   for Hour in 1 .. 10 loop
      Ada.Text_IO.Put_Line ("Taking measurment for hour" & Integer'Image(Hour));
   end loop;

   -- Reverse loop: counting down
   for Countdown in reverse 1 .. 5 loop
      Ada.Text_IO_Put_Line ("System start in" & Integer'Image(Countdown));
   end loop;
end Measurement_Loop;


-- 1. AUTOMATIC DECLARATION 
--    Unlike in other languages, you don't write 'int i'. 
--    The variable 'Hour' is simply 'invented' by the compiler and deleted again after the loop ends.
--
-- 2. READ-ONLY 
--    In Ada, it is not possible to change the loop variable within the loop 
--    (e.g. Hour := Hour + 1 is not permitted). This prevents endless or faulty loops.
--
-- 3. RANGES 
--    You can use your own types directly! For example, if you have a type 'Day' 
--    defined as (Mon, Tue, Wed), you can write 'for D in Day loop'.
--
-- 4. REVERSE
--    If you want to count backwards, write 'for i in reverse 1..10 loop'. 
--    Note that the range remains 1..10; the word 'reverse' simply reverses the order. 
--    If you wrote 10..1, the loop would not start at all because the lower limit is greater than the upper limit.


-------------------------------------------------------------------------------
-- BARE LOOP (Simple Loop)
-- A loop that runs indefinitely until an 'exit' statement is encountered.
-------------------------------------------------------------------------------

procedure Bare_Loop_Example is
   Sensor_Value : Integer := 0;
begin
   -- The loop starts with the keyword 'loop' on its own
   loop
      -- Imagine reading a sensor here
      Sensor_Value := Sensor_Value + 10;
      
      Ada.Text_IO.Put_Line ("Current Level: " & Integer'Image(Sensor_Value));

      -- EXIT STRATEGY: 
      -- You can use 'exit when' followed by a boolean condition.
      exit when Sensor_Value >= 100;
      
      -- Alternatively, you can use a simple 'if'
      -- if Sensor_Value >= 100 then
      --    exit;
      -- end if;

   end loop; -- Terminated by 'end loop'
   
   Ada.Text_IO.Put_Line ("Target reached. Loop terminated.");
end Bare_Loop_Example;


-- 1. DECLARATION
--    Variables such as Sensor_Value must be declared between 'is' and 'begin'. 
--    In Ada, variables must never be declared in the middle of executable code (after 'begin').
--
-- 2. INFINITE LOOP 
--    Without an 'exit when' statement, this loop would run forever. 
--    This can be useful for systems that need to run permanently in the background, 
--    such as a server loop or an operating system kernel.
--
-- 3. EXIT LOOP
--    Exit when is the preferred 'Ada way' because it is highly readable 
--    (almost like an English sentence: 'exit when sensor value is greater than 100').


-------------------------------------------------------------------------------
-- WHILE LOOP
-- The condition is evaluated BEFORE each iteration.
-- If the condition is false at the start, the loop body is never executed.
-------------------------------------------------------------------------------

procedure Watertank_Drainage is
Water_Level : Integer := 100;

begin
-- the loop continues s long as teh condition remains true
while Water_Level > 0 loop
   Ada.Text_IO_Put_Line ("Draining... Current Level: " & Integer'Image(Water_Level));

   -- Decrease the level
   Water_Level := Water_Level - 20;
end loop;

   Ada.Text_IO_Put_Line("Tank is now empty.");
end Watertank_Drainage;


-- 1. SYNTAX 
--    It begins with the keyword 'while', followed by a Boolean condition 
--    and the word 'loop'. It always ends with 'end loop'.
--
-- 2. HEAD CONTROL
--   Since the check occurs at the beginning, it is safer 
--   than loops that only check at the end because it does not process 
--   impossible' states in the first place.
--
-- 3. ASSIGNMENT vs. COMPARISON: 
--    The strict separation applies here, too: 
--    In the condition (Water_Level > 0), comparison operators are used, 
--    while assignment operators are used in the body (:=).
--
-- 4. NO IMPLICIT DECLARATION: 
--    Unlike with the for loop, you must declare the variable 
--    (here, Water_Level) yourself in advance in the declarative region (between 'is' and 'begin').


-------------------------------------------------------------------------------
-- ADA LOOP STRUCTURES: QUICK REFERENCE
-- Ada provides three distinct loop types, each optimized for specific 
-- safety and logic requirements.
--
-- 1. FOR LOOP (Deterministic)
--    Best Use: When the number of iterations is known in advance.
--    Features: 
--      - Loop parameter is automatically declared and local to the loop.
--      - Loop parameter is READ-ONLY (cannot be modified manually).
--      - Use 'reverse' to count downwards (e.g., 'for I in reverse 1..10').
--
-- 2. WHILE LOOP (Condition-based)
--    Best Use: When a condition must be met BEFORE entering the loop.
--    Features:
--      - Condition is evaluated at the start of each iteration.
--      - Variables must be manually declared and updated within the body.
--      - If the condition is false initially, the body is never executed.
--
-- 3. BARE LOOP (Flexible/Infinite)
--    Best Use: For continuous processes or complex exit logic.
--    Features:
--      - Starts with the keyword 'loop' alone.
--      - Runs indefinitely unless an 'exit' or 'exit when' is triggered.
--      - Provides the most control over where and when to break the loop.

-------------------------------------------------------------------------------
-- ADA LOOP STRUCTURES: QUICK REFERENCE Table
-------------------------------------------------------------------------------
-- Type       | Best Use Case                         | Key Feature
-- -----------|---------------------------------------|------------------------
-- FOR        | Fixed number of iterations.           | Auto-declared & Read-Only.
-- WHILE      | Condition must be met before start.   | Manual var management.
-- LOOP (Bare)| Infinite tasks or complex exits.      | Uses 'exit when' logic.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
