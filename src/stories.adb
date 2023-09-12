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
with Crew;
with Events;
with Maps;
with Ships; use Ships;
with Ships.Crew;
with Utils;

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

   procedure Start_Story
     (Faction_Name: Tiny_String.Bounded_String;
      Condition: Start_Condition_Type) is
      use Tiny_String;

      Nim_Current_Story: Nim_Current_Story_Data;
      --## rule off IMPROPER_INITIALIZATION
      Temp_Texts: UnboundedString_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      procedure Start_Ada_Story(F_Name: chars_ptr; Con: Integer) with
         Import => True,
         Convention => C,
         External_Name => "startAdaStory";
      procedure Set_Ada_Current_Story(Story: out Nim_Current_Story_Data) with
         Import => True,
         Convention => C,
         External_Name => "setAdaCurrentStory";
   begin
      if Current_Story.Index /= Null_Unbounded_String then
         return;
      end if;
      Get_Current_Story;
      Start_Ada_Story
        (F_Name => New_String(Str => To_String(Source => Faction_Name)),
         Con => Start_Condition_Type'Pos(Condition));
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
      use Crew;
      use Events;
      use Ships.Crew;
      use Utils;

      Step: Step_Data :=
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
         else Stories_List(Current_Story.Index).Final_Step);
      Max_Random: constant Positive :=
        (if Step.Finish_Condition = DESTROYSHIP and Next_Step then 1
         else Positive'Value
             (To_String
                (Source =>
                   Get_Step_Data
                     (Finish_Data => Step.Finish_Data, Name => "chance"))));
      Finish_Condition: Unbounded_String;
      Chance: Natural;

      function Select_Location
        (S: StepData_Container.Vector) return Unbounded_String is
         use Maps;

         Location_Data, Value: Unbounded_String;
         Location_X: Positive;
         Location_Y: Positive := 1;
      begin
         Value := Get_Step_Data(Finish_Data => S, Name => "x");
         if Value = To_Unbounded_String(Source => "random") then
            Location_X :=
              Get_Random(Min => Sky_Map'First(1), Max => Sky_Map'Last(1));
            Location_Data :=
              To_Unbounded_String(Source => Integer'Image(Location_X));
            Append(Source => Location_Data, New_Item => ";");
         else
            Location_X := Integer'Value(To_String(Source => Value));
            Location_Data := Value;
            Append(Source => Location_Data, New_Item => ";");
         end if;
         Player_Ship.Destination_X := Location_X;
         Value := Get_Step_Data(Finish_Data => S, Name => "y");
         if Value = To_Unbounded_String(Source => "random") then
            Random_Location_Loop :
            loop
               Location_Y :=
                 Get_Random(Min => Sky_Map'First(2), Max => Sky_Map'Last(2));
               exit Random_Location_Loop when Sky_Map(Location_X, Location_Y)
                   .Base_Index =
                 0 and
                 Location_Y /= Player_Ship.Sky_Y;
            end loop Random_Location_Loop;
            Append
              (Source => Location_Data, New_Item => Integer'Image(Location_Y));
            Append(Source => Location_Data, New_Item => ";");
         else
            Location_Y := Integer'Value(To_String(Source => Value));
            Append(Source => Location_Data, New_Item => Value);
            Append(Source => Location_Data, New_Item => ";");
         end if;
         Player_Ship.Destination_Y := Location_Y;
         return Location_Data;
      end Select_Location;

      function Select_Base(Value: String) return Unbounded_String is
         Base_Index: Bases_Range := 1;
      begin
         if Value = "any" then
            return Null_Unbounded_String;
         end if;
         Select_Base_Loop :
         loop
            Base_Index :=
              Get_Random(Min => Sky_Bases'First, Max => Sky_Bases'Last);
            if Sky_Bases(Base_Index).Known and
              Sky_Bases(Base_Index).Reputation.Level > -25 then
               Player_Ship.Destination_X := Sky_Bases(Base_Index).Sky_X;
               Player_Ship.Destination_Y := Sky_Bases(Base_Index).Sky_Y;
               return
                 To_Unbounded_String
                   (Source =>
                      Tiny_String.To_String
                        (Source => Sky_Bases(Base_Index).Name));
            end if;
         end loop Select_Base_Loop;
      end Select_Base;

      function Select_Enemy
        (S: StepData_Container.Vector) return Unbounded_String is
      --## rule off IMPROPER_INITIALIZATION
         Enemies: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
         Enemy_Data, Value: Unbounded_String;
      begin
         Enemy_Data := Select_Location(S => S);
         Value := Get_Step_Data(Finish_Data => S, Name => "ship");
         if Value /= To_Unbounded_String(Source => "random") then
            return Enemy_Data & Value;
         end if;
         Value := Get_Step_Data(Finish_Data => S, Name => "faction");
      --## rule off IMPROPER_INITIALIZATION
         Generate_Enemies
           (Enemies => Enemies,
            Owner =>
              Tiny_String.To_Bounded_String
                (Source => To_String(Source => Value)));
         return
           Enemy_Data &
           Positive'Image
             (Enemies
                (Get_Random
                   (Min => Enemies.First_Index, Max => Enemies.Last_Index)));
      --## rule on IMPROPER_INITIALIZATION
      end Select_Enemy;

      function Select_Loot
        (S: StepData_Container.Vector) return Unbounded_String is
      --## rule off IMPROPER_INITIALIZATION
         Enemies: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
         Loot_Data, Value: Unbounded_String;
      begin
         Loot_Data := Get_Step_Data(Finish_Data => S, Name => "item");
         Append(Source => Loot_Data, New_Item => ";");
         Value := Get_Step_Data(Finish_Data => S, Name => "ship");
         if Value /= To_Unbounded_String(Source => "random") then
            return Loot_Data & Value;
         end if;
         Value := Get_Step_Data(Finish_Data => S, Name => "faction");
      --## rule off IMPROPER_INITIALIZATION
         Generate_Enemies
           (Enemies => Enemies,
            Owner =>
              Tiny_String.To_Bounded_String
                (Source => To_String(Source => Value)));
         return
           Loot_Data &
           Positive'Image
             (Enemies
                (Get_Random
                   (Min => Enemies.First_Index, Max => Enemies.Last_Index)));
      --## rule on IMPROPER_INITIALIZATION
      end Select_Loot;

   begin
      Finish_Condition :=
        Get_Step_Data(Finish_Data => Step.Finish_Data, Name => "condition");
      if Finish_Condition = To_Unbounded_String(Source => "random")
        and then Get_Random(Min => 1, Max => Max_Random) > 1 then
         Update_Game(Minutes => 10);
         return False;
      end if;
      Chance := 0;
      case Step.Finish_Condition is
         when ASKINBASE =>
            Count_Ask_Chance_Block :
            declare
               Trader_Index: constant Natural := Find_Member(Order => TALK);
            begin
               if Trader_Index > 0 then
                  Chance :=
                    Get_Skill_Level
                      (Member => Player_Ship.Crew(Trader_Index),
                       Skill_Index =>
                         Find_Skill_Index
                           (Skill_Name =>
                              To_String(Source => Finish_Condition)));
               end if;
            end Count_Ask_Chance_Block;
         when DESTROYSHIP | EXPLORE =>
            Count_Explore_Chance_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Order in PILOT | GUNNER then
                  Chance :=
                    Chance +
                    Get_Skill_Level
                      (Member => Member,
                       Skill_Index =>
                         Find_Skill_Index
                           (Skill_Name =>
                              To_String(Source => Finish_Condition)));
               end if;
            end loop Count_Explore_Chance_Loop;
         when LOOT =>
            Count_Loot_Chance_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Order = BOARDING then
                  Chance :=
                    Chance +
                    Get_Skill_Level
                      (Member => Member,
                       Skill_Index =>
                         Find_Skill_Index
                           (Skill_Name =>
                              To_String(Source => Finish_Condition)));
               end if;
            end loop Count_Loot_Chance_Loop;
         when ANY =>
            null;
      end case;
      Chance := Chance + Get_Random(Min => 1, Max => 100);
      if Chance < Max_Random then
         Update_Game(Minutes => 10);
         return False;
      end if;
      if Step.Finish_Condition = DESTROYSHIP and not Next_Step then
         return True;
      end if;
      if Finish_Condition /= To_Unbounded_String(Source => "random") then
         case Step.Finish_Condition is
            when ASKINBASE =>
               Ask_Gain_Experience_Block :
               declare
                  Trader_Index: constant Natural := Find_Member(Order => TALK);
               begin
                  if Trader_Index > 0 then
                     Gain_Exp
                       (Amount => 10,
                        Skill_Number =>
                          Find_Skill_Index
                            (Skill_Name =>
                               To_String(Source => Finish_Condition)),
                        Crew_Index => Trader_Index);
                  end if;
               end Ask_Gain_Experience_Block;
            when DESTROYSHIP | EXPLORE =>
               Count_Explore_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = PILOT or
                    Player_Ship.Crew(I).Order = GUNNER then
                     Gain_Exp
                       (Amount => 10,
                        Skill_Number =>
                          Find_Skill_Index
                            (Skill_Name =>
                               To_String(Source => Finish_Condition)),
                        Crew_Index => Crew_Container.To_Index(Position => I));
                  end if;
               end loop Count_Explore_Experience_Loop;
            when LOOT =>
               Count_Loot_Experience_Loop :
               for I in Player_Ship.Crew.Iterate loop
                  if Player_Ship.Crew(I).Order = BOARDING then
                     Gain_Exp
                       (Amount => 10,
                        Skill_Number =>
                          Find_Skill_Index
                            (Skill_Name =>
                               To_String(Source => Finish_Condition)),
                        Crew_Index => Crew_Container.To_Index(Position => I));
                  end if;
               end loop Count_Loot_Experience_Loop;
            when ANY =>
               null;
         end case;
      end if;
      Update_Game(Minutes => 30);
      Update_Finished_Stories_Loop :
      for FinishedStory of Finished_Stories loop
         if FinishedStory.Index = Current_Story.Index then
            FinishedStory.Steps_Texts.Append
              (New_Item => Get_Current_Story_Text);
            exit Update_Finished_Stories_Loop;
         end if;
      end loop Update_Finished_Stories_Loop;
      Current_Story.Step := Current_Story.Step + 1;
      Current_Story.Finished_Step := Step.Finish_Condition;
      Current_Story.Show_Text := True;
      if Current_Story.Step < Current_Story.Max_Steps then
         Current_Story.Current_Step :=
           Get_Random
             (Min => Stories_List(Current_Story.Index).Steps.First_Index,
              Max => Stories_List(Current_Story.Index).Steps.Last_Index);
         Step :=
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step);
      elsif Current_Story.Step = Current_Story.Max_Steps then
         Current_Story.Current_Step := -1;
         Step := Stories_List(Current_Story.Index).Final_Step;
      else
         Current_Story.Current_Step := -2;
      end if;
      if Current_Story.Current_Step /= -2 then
         case Step.Finish_Condition is
            when ASKINBASE =>
               Current_Story.Data :=
                 Select_Base
                   (Value =>
                      To_String
                        (Source =>
                           Get_Step_Data
                             (Finish_Data => Step.Finish_Data,
                              Name => "base")));
            when DESTROYSHIP =>
               Current_Story.Data := Select_Enemy(S => Step.Finish_Data);
            when EXPLORE =>
               Current_Story.Data := Select_Location(S => Step.Finish_Data);
            when LOOT =>
               Current_Story.Data := Select_Loot(S => Step.Finish_Data);
            when ANY =>
               null;
         end case;
      end if;
      return True;
   end Progress_Story;

   function Get_Current_Story_Text return Unbounded_String is
      Step_Texts: constant StepTexts_Container.Vector :=
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step.Texts
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
             .Texts
         else Stories_List(Current_Story.Index).Final_Step.Texts);
   begin
      Current_Story_Text_Loop :
      for Text of Step_Texts loop
         if Text.Condition = Current_Story.Finished_Step then
            return Text.Text;
         end if;
      end loop Current_Story_Text_Loop;
      return Null_Unbounded_String;
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

end Stories;
