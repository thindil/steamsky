--    Copyright 2018-2023 Bartek thindil Jasicki
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
with GNAT.String_Split;
with Bases; use Bases;
with Events;
with Maps;
with Ships; use Ships;

package body Stories is

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Step_Finish_Data is record
      Name: chars_ptr;
      Value: chars_ptr;
   end record;
   type Nim_Finish_Data_Array is array(0 .. 9) of Nim_Step_Finish_Data;
   --## rule on TYPE_INITIAL_VALUES

   procedure Load_Stories is
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
      procedure Get_Ada_Story(Index: chars_ptr; Story: out Nim_Story_Data) with
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
      Clear_Current_Story;
      Convert_Stories_Loop :
      for I in 1 .. 10 loop
         Get_Ada_Story
           (Index => New_String(Str => Positive'Image(I)), Story => Nim_Story);
         exit Convert_Stories_Loop when Nim_Story.Start_Condition = -1;
         Temp_Record.Steps.Clear;
         Temp_Record.Start_Data.Clear;
         Temp_Record.Forbidden_Factions.Clear;
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
            Temp_Record.Steps.Append
              (New_Item => Convert_Step(Nim_Step => Step));
         end loop Convert_Steps_Loop;
         Temp_Record.Final_Step :=
           Convert_Step(Nim_Step => Nim_Story.Final_Step);
         Temp_Record.End_Text :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value(Item => Nim_Story.End_Text));
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
      end loop Convert_Stories_Loop;
   end Load_Stories;

   --## rule off TYPE_INITIAL_VALUES
   type Nim_Current_Story_Data is record
      Index: chars_ptr;
      Step: Positive;
      Current_Step: Integer;
      Max_Steps: Positive;
      Show_Text: Integer;
      Data: chars_ptr;
      Finished_Step: Natural;
   end record;
   --## rule on TYPE_INITIAL_VALUES

   -- ****if* Stories/Set_Current_Story
   -- FUNCTION
   -- Set the current story from Nim
   -- SOURCE
   procedure Set_Current_Story is
      -- ****
      Nim_Current_Story: Nim_Current_Story_Data;
      procedure Set_Ada_Current_Story(Story: out Nim_Current_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "setAdaCurrentStory";
   begin
      Set_Ada_Current_Story(Story => Nim_Current_Story);
      Current_Story :=
        (Index =>
           To_Unbounded_String
             (Source => Value(Item => Nim_Current_Story.Index)),
         Step => Nim_Current_Story.Step,
         Current_Step => Nim_Current_Story.Current_Step,
         Max_Steps => Nim_Current_Story.Max_Steps,
         Show_Text =>
           (if Nim_Current_Story.Show_Text = 1 then True else False),
         Data =>
           To_Unbounded_String
             (Source => Value(Item => Nim_Current_Story.Data)),
         Finished_Step =>
           Step_Condition_Type'Val(Nim_Current_Story.Finished_Step));
   end Set_Current_Story;

   procedure Start_Story
     (Faction_Name: Tiny_String.Bounded_String;
      Condition: Start_Condition_Type) is
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Texts: UnboundedString_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Start_Ada_Story(F_Name: chars_ptr; Con: Integer) with
         Import => True,
         Convention => C,
         External_Name => "startAdaStory";
   begin
      if Current_Story.Index /= Null_Unbounded_String then
         return;
      end if;
      Get_Current_Story;
      Start_Ada_Story
        (F_Name => New_String(Str => To_String(Source => Faction_Name)),
         Con => Start_Condition_Type'Pos(Condition));
      Set_Current_Story;
      if Current_Story.Index /= Null_Unbounded_String then
         Finished_Stories.Append
           (New_Item =>
              (Index => Current_Story.Index,
               Steps_Amount => Current_Story.Max_Steps,
               Steps_Texts => Temp_Texts));
      end if;
   end Start_Story;

   procedure Clear_Current_Story is
   begin
      Current_Story :=
        (Index => Null_Unbounded_String, Step => 1, Current_Step => -3,
         Max_Steps => 1, Show_Text => False, Data => Null_Unbounded_String,
         Finished_Step => ANY);
   end Clear_Current_Story;

   function Progress_Story(Next_Step: Boolean := False) return Boolean is
      use Events;
      use Maps;

      Result: Boolean;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Progress_Ada_Story(N_Step: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "progressAdaStory";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Set_Current_Story;
      Result := Progress_Ada_Story(N_Step => (if Next_Step then 1 else 0)) = 1;
      Get_Current_Story;
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Events_In_Ada_Loop :
      for I in 1 .. Get_Events_Amount loop
         Set_Event(Index => I);
      end loop Set_Events_In_Ada_Loop;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      return Result;
   end Progress_Story;

   function Get_Current_Story_Text return Unbounded_String is
      function Get_Ada_Current_Story_Text return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCurrentStoryText";
   begin
      return
        To_Unbounded_String
          (Source => Value(Item => Get_Ada_Current_Story_Text));
   end Get_Current_Story_Text;

   function Get_Step_Data
     (Finish_Data: StepData_Container.Vector; Name: String)
      return Unbounded_String is
      Nim_Data: Nim_Finish_Data_Array := (others => <>);
      I: Natural := 0;
      function Get_Ada_Step_Data
        (F_Data: Nim_Finish_Data_Array; N: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaStepData";
   begin
      Get_Step_Data_Loop :
      for Data of Finish_Data loop
         Nim_Data(I) :=
           (Name => New_String(Str => To_String(Source => Data.Name)),
            Value => New_String(Str => To_String(Source => Data.Value)));
         I := I + 1;
      end loop Get_Step_Data_Loop;
      Fill_Array_Loop :
      for J in I .. 9 loop
         Nim_Data(I) :=
           (Name => New_String(Str => ""), Value => New_String(Str => ""));
      end loop Fill_Array_Loop;
      return
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Get_Ada_Step_Data
                    (F_Data => Nim_Data, N => New_String(Str => Name))));
   end Get_Step_Data;

   procedure Get_Story_Location
     (Story_X: out Map_X_Range; Story_Y: out Map_Y_Range) is
      use GNAT.String_Split;

      --## rule off IMPROPER_INITIALIZATION
      Tokens: Slice_Set;
      --## rule on IMPROPER_INITIALIZATION
   begin
      Story_X := 1;
      Story_Y := 1;
      if Current_Story.Data = Null_Unbounded_String then
         Story_X := Player_Ship.Sky_X;
         Story_Y := Player_Ship.Sky_Y;
      else
         Create
           (S => Tokens, From => To_String(Source => Current_Story.Data),
            Separators => ";");
         if Slice_Count(S => Tokens) < 3 then
            Get_Story_Location_Loop :
            for Sky_Base of Sky_Bases loop
               if Tiny_String.To_String(Source => Sky_Base.Name) =
                 To_String(Source => Current_Story.Data) then
                  Story_X := Sky_Base.Sky_X;
                  Story_Y := Sky_Base.Sky_Y;
                  exit Get_Story_Location_Loop;
               end if;
            end loop Get_Story_Location_Loop;
         else
            Story_X := Integer'Value(Slice(S => Tokens, Index => 1));
            Story_Y := Integer'Value(Slice(S => Tokens, Index => 2));
         end if;
      end if;
   end Get_Story_Location;

   procedure Get_Current_Story is
      procedure Get_Ada_Current_Story(Story: Nim_Current_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCurrentStory";
   begin
      Get_Ada_Current_Story
        (Story =>
           (Index =>
              New_String(Str => To_String(Source => Current_Story.Index)),
            Step => Current_Story.Step,
            Current_Step => Current_Story.Current_Step,
            Max_Steps => Current_Story.Max_Steps,
            Show_Text => (if Current_Story.Show_Text then 1 else 0),
            Data => New_String(Str => To_String(Source => Current_Story.Data)),
            Finished_Step =>
              Step_Condition_Type'Pos(Current_Story.Finished_Step)));
   end Get_Current_Story;

   procedure Get_Finished_Story(Index: Positive) is
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
      --## rule on IMPROPER_INITIALIZATION
      Step_Index: Natural := 0;
      procedure Get_Ada_Finished_Story
        (I: Positive; Story: Nim_Finished_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaFinishedStory";
   begin
      if Index <= Finished_Stories.Last_Index then
         Nim_Story.Index :=
           New_String
             (Str => To_String(Source => Finished_Stories(Index).Index));
         Nim_Story.Steps_Amount := Finished_Stories(Index).Steps_Amount;
         Convert_Steps_Texts_Loop :
         for Text of Finished_Stories(Index).Steps_Texts loop
            Nim_Story.Steps_Text(Step_Index) :=
              New_String(Str => To_String(Source => Text));
            Step_Index := Step_Index + 1;
         end loop Convert_Steps_Texts_Loop;
      end if;
      Get_Ada_Finished_Story(I => Index, Story => Nim_Story);
   end Get_Finished_Story;

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
        (I: Positive; Story: out Nim_Finished_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "setAdaFinishedStory";
   begin
      Set_Ada_Finished_Story(I => Index, Story => Nim_Story);
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

end Stories;
