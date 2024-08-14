--    Copyright 2018-2024 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Stories is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Step_Finish_Data is record
      Name: chars_ptr;
      Value: chars_ptr;
   end record;
   type Nim_Finish_Data_Array is array(0 .. 9) of Nim_Step_Finish_Data;
   --## rule on TYPE_INITIAL_VALUES

   function Get_Finished_Story(Index: Positive) return Finished_Story_Data is
      use Interfaces.C;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Steps_Texts is array(0 .. 10) of chars_ptr;
      type Nim_Finished_Story_Data is record
         Index: chars_ptr;
         Steps_Amount: Integer;
         Steps_Text: Nim_Steps_Texts;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Nim_Story: Nim_Finished_Story_Data;
      Story: Finished_Story_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Set_Ada_Finished_Story
        (I: Positive; S: out Nim_Finished_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "setAdaFinishedStory";
   begin
      Set_Ada_Finished_Story(I => Index, S => Nim_Story);
      Story.Index :=
        To_Unbounded_String(Source => Value(Item => Nim_Story.Index));
      Story.Steps_Amount := Nim_Story.Steps_Amount;
      Convert_Text_Loop :
      for Step of Nim_Story.Steps_Text loop
         exit Convert_Text_Loop when Strlen(Item => Step) = 0;
         Story.Steps_Texts.Append
           (New_Item => To_Unbounded_String(Source => Value(Item => Step)));
      end loop Convert_Text_Loop;
      return Story;
   end Get_Finished_Story;

   function Get_Story(Index: Unbounded_String) return Story_Data is
      use Interfaces.C;
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Step_Text_Data is record
         Condition: Integer;
         Text: chars_ptr;
      end record;
      type Nim_Text_Data_Array is array(0 .. 9) of Nim_Step_Text_Data;
      type Nim_Step_Data is record
         Index: chars_ptr;
         Finish_Condition: Integer;
         Finish_Data: Nim_Finish_Data_Array;
         Texts: Nim_Text_Data_Array;
         Fail_Text: chars_ptr;
      end record;
      type Nim_Story_Array is array(0 .. 9) of chars_ptr;
      type Nim_Steps_Array is array(0 .. 9) of Nim_Step_Data;
      type Nim_Story_Data is record
         Start_Condition: Integer;
         Start_Data: Nim_Story_Array;
         Min_Steps: Integer;
         Max_Steps: Integer;
         Starting_Step: Nim_Step_Data;
         Steps: Nim_Steps_Array;
         Final_Step: Nim_Step_Data;
         End_Text: chars_ptr;
         Name: chars_ptr;
         Forbidden_Factions: Nim_Story_Array;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Story_Data;
      Nim_Story: Nim_Story_Data;
      --## rule on IMPROPER_INITIALIZATION
      procedure Get_Ada_Story(I: chars_ptr; Story: out Nim_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaStory";
      function Convert_Step(Nim_Step: Nim_Step_Data) return Step_Data is
         Step: Step_Data; --## rule line off IMPROPER_INITIALIZATION
      begin
         Step.Index :=
           To_Unbounded_String
             (Source => Interfaces.C.Strings.Value(Item => Nim_Step.Index));
         Step.Finish_Condition :=
           Step_Condition_Type'Val(Nim_Step.Finish_Condition);
         Convert_Step_Data_Loop :
         for Data of Nim_Step.Finish_Data loop
            exit Convert_Step_Data_Loop when Strlen(Item => Data.Name) = 0;
            Step.Finish_Data.Append
              (New_Item =>
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value(Item => Data.Name)),
                  Value =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value(Item => Data.Value))));
         end loop Convert_Step_Data_Loop;
         Convert_Step_Texts_Loop :
         for Text of Nim_Step.Texts loop
            exit Convert_Step_Texts_Loop when Text.Condition = -1;
            Step.Texts.Append
              (New_Item =>
                 (Condition => Step_Condition_Type'Val(Text.Condition),
                  Text =>
                    To_Unbounded_String
                      (Source =>
                         Interfaces.C.Strings.Value(Item => Text.Text))));
         end loop Convert_Step_Texts_Loop;
         Step.Fail_Text :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value(Item => Nim_Step.Fail_Text));
         return Step;
      end Convert_Step;
   begin
      Get_Ada_Story
        (I => New_String(Str => To_String(Source => Index)),
         Story => Nim_Story);
      --## rule off IMPROPER_INITIALIZATION
      Temp_Record.Steps.Clear;
      Temp_Record.Start_Data.Clear;
      Temp_Record.Forbidden_Factions.Clear;
      --## rule on IMPROPER_INITIALIZATION
      if Nim_Story.Start_Condition = -1 then
         return Temp_Record;
      end if;
      Temp_Record.Start_Condition :=
        Start_Condition_Type'Val(Nim_Story.Start_Condition);
      Convert_Start_Data_Loop :
      for Data of Nim_Story.Start_Data loop
         exit Convert_Start_Data_Loop when Strlen(Item => Data) = 0;
         Temp_Record.Start_Data.Append
           (New_Item =>
              To_Unbounded_String
                (Source => Interfaces.C.Strings.Value(Item => Data)));
      end loop Convert_Start_Data_Loop;
      Temp_Record.Min_Steps := Nim_Story.Min_Steps;
      Temp_Record.Max_Steps := Nim_Story.Max_Steps;
      Temp_Record.Starting_Step :=
        Convert_Step(Nim_Step => Nim_Story.Starting_Step);
      Convert_Steps_Loop :
      for Step of Nim_Story.Steps loop
         exit Convert_Steps_Loop when Strlen(Item => Step.Index) = 0;
         Temp_Record.Steps.Append(New_Item => Convert_Step(Nim_Step => Step));
      end loop Convert_Steps_Loop;
      Temp_Record.Final_Step := Convert_Step(Nim_Step => Nim_Story.Final_Step);
      Temp_Record.End_Text :=
        To_Unbounded_String
          (Source => Interfaces.C.Strings.Value(Item => Nim_Story.End_Text));
      Temp_Record.Name :=
        To_Unbounded_String
          (Source => Interfaces.C.Strings.Value(Item => Nim_Story.Name));
      Convert_Factions_Loop :
      for Faction of Nim_Story.Forbidden_Factions loop
         exit Convert_Factions_Loop when Strlen(Item => Faction) = 0;
         Temp_Record.Forbidden_Factions.Append
           (New_Item =>
              To_Unbounded_String
                (Source => Interfaces.C.Strings.Value(Item => Faction)));
      end loop Convert_Factions_Loop;
      return Temp_Record;
   end Get_Story;

end Stories;
