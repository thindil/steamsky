--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Bases; use Bases;
with Bases.Cargo;
with BasesTypes;
with Careers;
with Config;
with Crew;
with Events;
with Factions;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with Items;
with Log;
with Maps; use Maps;
with Messages;
with Missions;
with Mobs;
with ShipModules;
with Ships; use Ships;
with Statistics; use Statistics;
with Stories; use Stories;
with Utils;

package body Game is

   procedure New_Game is
      use Bases.Cargo;
      use Config;
      use Factions;
      use Messages;
      use Missions;
      use Tiny_String;
      use Utils;

      Random_Base: Positive := Positive'First;
      Player_Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
   begin
      -- Save game configuration
      Save_Config;
      -- Set game statistics
      Clear_Game_Stats;
      Set_Faction_Career_Block :
      declare
         Roll, Index: Positive range Positive'First .. Get_Factions_Amount :=
           Positive'First;
      begin
         -- Set player faction if random option was selected
         if New_Game_Settings.Player_Faction =
           To_Bounded_String(Source => "random") then
            New_Game_Settings.Player_Career :=
              To_Unbounded_String(Source => "random");
            Roll := Get_Random(Min => 1, Max => Get_Factions_Amount);
            Index := 1;
            New_Game_Settings.Player_Faction :=
              Get_Faction_Index(Number => Roll);
         end if;
         Player_Faction :=
           Get_Faction(Index => New_Game_Settings.Player_Faction);
         -- Set player career if random option was selected
         if New_Game_Settings.Player_Career =
           To_Unbounded_String(Source => "random") then
            Roll :=
              Get_Random
                (Min => 1, Max => Positive(Player_Faction.Careers.Length));
            Index := 1;
            Get_Player_Career_Loop :
            for I in Player_Faction.Careers.Iterate loop
               if Index = Roll then
                  New_Game_Settings.Player_Career :=
                    Factions.Careers_Container.Key(Position => I);
                  exit Get_Player_Career_Loop;
               end if;
               Index := Index + 1;
            end loop Get_Player_Career_Loop;
         end if;
      end Set_Faction_Career_Block;
      -- Set Game time
      Game_Date := Start_Date;
      -- Generate world
      Sky_Map :=
        (others =>
           (others =>
              (Base_Index => 0, Visited => False, Event_Index => 0,
               Mission_Index => 0)));
      Generate_Bases_Block :
      declare
         Max_Spawn_Roll, Max_Base_Spawn_Roll: Natural := 0;
         Faction_Roll: Positive := 1;
         Valid_Location: Boolean := False;
         Temp_X, Temp_Y, Base_Reputation, Pos_X, Pos_Y: Integer := 0;
         Tmp_Recruits: constant Recruit_Container.Vector :=
           Recruit_Container.Empty_Vector;
         Tmp_Missions: constant Mission_Container.Vector :=
           Mission_Container.Empty_Vector;
         Base_Population, Base_Type_Roll: Natural := 0;
         Base_Size: Bases_Size := SMALL;
         Base_Owner: Tiny_String.Bounded_String := Null_Bounded_String;
         Base_Type: Bounded_String := Null_Bounded_String;
         Base_Faction: Faction_Record; --## rule line off IMPROPER_INITIALIZATION
         package Bases_Container is new Hashed_Maps
           (Key_Type => Bounded_String,
            Element_Type => Positive_Container.Vector,
            Hash => Tiny_String_Hash, Equivalent_Keys => Tiny_String."=",
            "=" => Positive_Container."=");
         Bases_Array: Bases_Container.Map := Bases_Container.Empty_Map;
         Attempts: Positive range 1 .. 251 := 1;
      begin
         Count_Spawn_Chance_Loop :
         for I in 1 .. Get_Factions_Amount loop
            Base_Faction := Get_Faction(Number => I);
            Max_Spawn_Roll := Max_Spawn_Roll + Base_Faction.Spawn_Chance;
            Bases_Array.Include
              (Key => Get_Faction_Index(Number => I),
               New_Item => Positive_Container.Empty_Vector);
         end loop Count_Spawn_Chance_Loop;
         Set_Bases_Loop :
         for I in Sky_Bases'Range loop
            Faction_Roll := Get_Random(Min => 1, Max => Max_Spawn_Roll);
            Set_Base_Faction_Loop :
            for J in 1 .. Get_Factions_Amount loop
               Base_Faction := Get_Faction(Number => J);
               if Faction_Roll <= Base_Faction.Spawn_Chance then
                  Base_Owner := Get_Faction_Index(Number => J);
                  Base_Population :=
                    (if Base_Faction.Population(2) = 0 then
                       Base_Faction.Population(1)
                     else Get_Random
                         (Min => Base_Faction.Population(1),
                          Max => Base_Faction.Population(2)));
                  Base_Reputation :=
                    Get_Reputation
                      (Source_Faction => New_Game_Settings.Player_Faction,
                       Target_Faction => Get_Faction_Index(Number => J));
                  Max_Base_Spawn_Roll := 0;
                  Count_Max_Spawn_Chance_Loop :
                  for SpawnChance of Base_Faction.Bases_Types loop
                     Max_Base_Spawn_Roll := Max_Base_Spawn_Roll + SpawnChance;
                  end loop Count_Max_Spawn_Chance_Loop;
                  Base_Type_Roll :=
                    Get_Random(Min => 1, Max => Max_Base_Spawn_Roll);
                  Get_Base_Type_Loop :
                  for K in Base_Faction.Bases_Types.Iterate loop
                     if Base_Type_Roll <= Base_Faction.Bases_Types(K) then
                        Base_Type := BaseType_Container.Key(Position => K);
                        exit Get_Base_Type_Loop;
                     end if;
                     Base_Type_Roll :=
                       Base_Type_Roll - Base_Faction.Bases_Types(K);
                  end loop Get_Base_Type_Loop;
                  exit Set_Base_Faction_Loop;
               end if;
               Faction_Roll := Faction_Roll - Base_Faction.Spawn_Chance;
            end loop Set_Base_Faction_Loop;
            Base_Size :=
              (if Base_Population = 0 then
                 Bases_Size'Val(Get_Random(Min => 0, Max => 2))
               elsif Base_Population < 150 then SMALL
               elsif Base_Population < 300 then MEDIUM else BIG);
            Sky_Bases(I).Name :=
              Generate_Base_Name(Faction_Index => Base_Owner);
            Sky_Bases(I).Visited := (others => 0);
            Sky_Bases(I).Sky_X := 1;
            Sky_Bases(I).Sky_Y := 1;
            Sky_Bases(I).Base_Type := Base_Type;
            Sky_Bases(I).Population := Base_Population;
            Sky_Bases(I).Recruit_Date := (others => 0);
            Sky_Bases(I).Known := False;
            Sky_Bases(I).Asked_For_Bases := False;
            Sky_Bases(I).Asked_For_Events := (others => 0);
            Sky_Bases(I).Reputation :=
              (Level => Base_Reputation, Experience => 0);
            Sky_Bases(I).Missions_Date := (others => 0);
            Sky_Bases(I).Missions := Tmp_Missions;
            Sky_Bases(I).Owner := Base_Owner;
            Sky_Bases(I).Size := Base_Size;
            Recruit_Container.Assign
              (Target => Sky_Bases(I).Recruits, Source => Tmp_Recruits);
            Base_Faction := Get_Faction(Index => Base_Owner);
            if Base_Faction.Flags.Contains
                (Item => To_Unbounded_String(Source => "loner")) then
               Faction_Roll := Get_Random(Min => 1, Max => Max_Spawn_Roll);
               Get_Faction_Loop :
               for J in 1 .. Get_Factions_Amount loop
                  Base_Faction := Get_Faction(Number => J);
                  if Faction_Roll > Base_Faction.Spawn_Chance then
                     Faction_Roll := Faction_Roll - Base_Faction.Spawn_Chance;
                  else
                     Base_Owner := Get_Faction_Index(Number => J);
                  end if;
               end loop Get_Faction_Loop;
            end if;
            Bases_Array(Base_Owner).Append(New_Item => I);
         end loop Set_Bases_Loop;
         Place_Bases_Loop :
         for FactionBases of Bases_Array loop
            Place_Faction_Bases_Loop :
            for I in FactionBases.Iterate loop
               Attempts := 1;
               Count_Base_Position_Loop :
               loop
                  Valid_Location := True;
                  if Positive_Container.To_Index(Position => I) =
                    FactionBases.First_Index or
                    (Get_Faction
                       (Index =>
                          Sky_Bases(FactionBases(FactionBases.First_Index))
                            .Owner)
                       .Flags
                       .Contains
                       (Item => To_Unbounded_String(Source => "loner")) and
                     Get_Faction(Index => Sky_Bases(FactionBases(I)).Owner)
                       .Flags
                       .Contains
                       (Item => To_Unbounded_String(Source => "loner"))) then
                     Pos_X :=
                       Get_Random
                         (Min => Bases_Range'First + 5,
                          Max => Bases_Range'Last - 5);
                     Pos_Y :=
                       Get_Random
                         (Min => Bases_Range'First + 5,
                          Max => Bases_Range'Last - 5);
                  else
                     Pos_X :=
                       Get_Random
                         (Min =>
                            Sky_Bases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .Sky_X -
                            20,
                          Max =>
                            Sky_Bases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .Sky_X +
                            20);
                     Normalize_Coord(Coord => Pos_X);
                     Pos_Y :=
                       Get_Random
                         (Min =>
                            Sky_Bases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .Sky_Y -
                            20,
                          Max =>
                            Sky_Bases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .Sky_Y +
                            20);
                     Normalize_Coord(Coord => Pos_Y, Is_X_Axis => False);
                     Attempts := Attempts + 1;
                     if Attempts = 251 then
                        Pos_X :=
                          Get_Random
                            (Min => Bases_Range'First + 10,
                             Max => Bases_Range'Last - 10);
                        Pos_Y :=
                          Get_Random
                            (Min => Bases_Range'First + 10,
                             Max => Bases_Range'Last - 10);
                        Attempts := 1;
                     end if;
                  end if;
                  Check_X_Coordinate_Loop :
                  for J in -5 .. 5 loop
                     Temp_X := Pos_X + J;
                     Normalize_Coord(Coord => Temp_X);
                     Check_Y_Coordinate_Loop :
                     for K in -5 .. 5 loop
                        Temp_Y := Pos_Y + K;
                        Normalize_Coord(Coord => Temp_Y, Is_X_Axis => False);
                        if Sky_Map(Temp_X, Temp_Y).Base_Index > 0 then
                           Valid_Location := False;
                           exit Check_Y_Coordinate_Loop;
                        end if;
                     end loop Check_Y_Coordinate_Loop;
                     exit Check_X_Coordinate_Loop when not Valid_Location;
                  end loop Check_X_Coordinate_Loop;
                  if Sky_Map(Pos_X, Pos_Y).Base_Index > 0 then
                     Valid_Location := False;
                  end if;
                  exit Count_Base_Position_Loop when Valid_Location;
               end loop Count_Base_Position_Loop;
               Sky_Map(Pos_X, Pos_Y) :=
                 (Base_Index => FactionBases(I), Visited => False,
                  Event_Index => 0, Mission_Index => 0);
               Sky_Bases(FactionBases(I)).Sky_X := Pos_X;
               Sky_Bases(FactionBases(I)).Sky_Y := Pos_Y;
               Get_Ada_Map_Cell
                 (X => Pos_X, Y => Pos_Y, Base_Index => FactionBases(I),
                  Visited => 0, Event_Index => 0, Mission_Index => 0);
               Get_Base_Owner(Base_Index => FactionBases(I));
               Get_Ada_Base_Population
                 (Base_Index => FactionBases(I),
                  Population => Sky_Bases(FactionBases(I)).Population);
               Get_Ada_Base_Location
                 (Base_Index => FactionBases(I), X => Pos_X, Y => Pos_Y);
               Get_Base_Type
                 (Base_Index => FactionBases(I),
                  Base_Type => Sky_Bases(FactionBases(I)).Base_Type);
            end loop Place_Faction_Bases_Loop;
         end loop Place_Bases_Loop;
      end Generate_Bases_Block;
      -- Place player ship in random large base
      Place_Player_Loop :
      loop
         Random_Base := Get_Random(Min => 1, Max => 1_024);
         if New_Game_Settings.Starting_Base =
           To_Bounded_String(Source => "Any") then
            exit Place_Player_Loop when Sky_Bases(Random_Base).Population >
              299 and
              Sky_Bases(Random_Base).Owner = New_Game_Settings.Player_Faction;
         else
            exit Place_Player_Loop when Sky_Bases(Random_Base).Population >
              299 and
              Sky_Bases(Random_Base).Owner =
                New_Game_Settings.Player_Faction and
              Sky_Bases(Random_Base).Base_Type =
                New_Game_Settings.Starting_Base;
         end if;
      end loop Place_Player_Loop;
      -- Create player ship
      Player_Ship :=
        Create_Ship
          (Proto_Index =>
             Player_Faction.Careers(New_Game_Settings.Player_Career)
               .Ship_Index,
           Name =>
             To_Bounded_String
               (Source => To_String(Source => New_Game_Settings.Ship_Name)),
           X => Sky_Bases(Random_Base).Sky_X,
           Y => Sky_Bases(Random_Base).Sky_Y, Speed => DOCKED,
           Random_Upgrades => False);
      -- Add player to ship
      Add_Player_Block :
      declare
         use Crew;
         use Items;
         use Mobs;

         Player_Index_2: constant Positive :=
           Positive'Value
             (To_String
                (Source =>
                   Player_Faction.Careers(New_Game_Settings.Player_Career)
                     .Player_Index));
         Amount: Positive := 1;
         Proto_Player: constant Proto_Mob_Record :=
           Get_Proto_Mob(Index => Player_Index_2);
         --## rule off IMPROPER_INITIALIZATION
         Tmp_Inventory: Inventory_Container.Vector (Capacity => 32);
         --## rule on IMPROPER_INITIALIZATION
         Player_Morale: constant Positive :=
           (if
              Player_Faction.Flags.Contains
                (Item => To_Unbounded_String(Source => "nomorale"))
            then 50
            else 100);
      begin
         Player_Inventory_Loop :
         for I in
           MobInventory_Container.First_Index
             (Container => Proto_Player.Inventory) ..
             MobInventory_Container.Last_Index
               (Container => Proto_Player.Inventory) loop
            Add_Inventory_Block :
            declare
               Proto_Inventory: constant Mob_Inventory_Record :=
                 MobInventory_Container.Element
                   (Container => Proto_Player.Inventory, Index => I);
            begin
               Amount :=
                 (if Proto_Inventory.Max_Amount > 0 then
                    Get_Random
                      (Min => Proto_Inventory.Min_Amount,
                       Max => Proto_Inventory.Max_Amount)
                  else Proto_Inventory.Min_Amount);
               Inventory_Container.Append
                 (Container => Tmp_Inventory,
                  New_Item =>
                    (Proto_Index => Proto_Inventory.Proto_Index,
                     Amount => Amount, Name => Null_Bounded_String,
                     Durability => 100, Price => 0));
            end Add_Inventory_Block;
         end loop Player_Inventory_Loop;
         Player_Ship.Crew.Prepend
           (New_Item =>
              (Amount_Of_Attributes => Attributes_Amount,
               Amount_Of_Skills => Skills_Amount,
               Name =>
                 To_Bounded_String
                   (Source =>
                      To_String(Source => New_Game_Settings.Player_Name)),
               Gender => New_Game_Settings.Player_Gender, Health => 100,
               Tired => 0, Skills => Proto_Player.Skills, Hunger => 0,
               Thirst => 0, Order => Proto_Player.Order,
               Previous_Order => REST, Order_Time => 15,
               Orders => Proto_Player.Priorities,
               Attributes => Proto_Player.Attributes,
               Inventory => Tmp_Inventory, Equipment => Proto_Player.Equipment,
               Payment => (others => 0), Contract_Length => -1,
               Morale => (1 => Player_Morale, 2 => 0), Loyalty => 100,
               Home_Base => Random_Base,
               Faction => New_Game_Settings.Player_Faction));
      end Add_Player_Block;
      Assign_Cabin_Block :
      declare
         use ShipModules;

         Cabin_Assigned: Boolean := False;
      begin
         Player_Ship_Modules_Loop :
         for Module of Player_Ship.Modules loop
            Module_Owner_Loop :
            for Owner of Module.Owner loop
               if Owner > 0 then
                  Owner := Owner + 1;
               end if;
            end loop Module_Owner_Loop;
            if Get_Module(Index => Module.Proto_Index).M_Type = CABIN and
              not Cabin_Assigned then
               Assign_Cabin_Loop :
               for I in Module.Owner.Iterate loop
                  if Module.Owner(I) = 0 then
                     Module.Owner(I) := 1;
                     if Natural_Container.To_Index(Position => I) = 1 then
                        Module.Name :=
                          To_String(Source => New_Game_Settings.Player_Name) &
                          To_Bounded_String(Source => "'s Cabin");
                     end if;
                     Cabin_Assigned := True;
                     exit Assign_Cabin_Loop;
                  end if;
               end loop Assign_Cabin_Loop;
            end if;
         end loop Player_Ship_Modules_Loop;
      end Assign_Cabin_Block;
      -- Set current map field/sky base info
      Sky_Bases(Random_Base).Visited := Game_Date;
      Sky_Bases(Random_Base).Known := True;
      Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited := True;
      Generate_Recruits;
      Generate_Missions;
      Generate_Cargo;
      -- Set player goal if not set yet
      if Current_Goal.G_Type = RANDOM then
         Current_Goal :=
           Goals_List
             (Get_Random
                (Min => Goals_List.First_Index, Max => Goals_List.Last_Index));
      end if;
      -- Set name of savegame
      Generate_Save_Name;
      -- Set player career
      Player_Career := New_Game_Settings.Player_Career;
      Get_Ada_Game_String
        (Name => New_String(Str => "playerCareer"),
         Value => New_String(Str => To_String(Source => Player_Career)));
      Get_New_Game_Settings;
      -- Add welcoming message
      Add_Message
        (Message =>
           "Welcome to Steam Sky. If it is your first game, please consider read help (entry 'Help' in Menu), especially topic 'First Steps'.",
         M_Type => OTHERMESSAGE);
   end New_Game;

   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) is
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Update_Ada_Game(M, In_C: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaGame";
   begin
      Get_Game_Date;
      Set_Ship_In_Nim;
      if Base_Index > 0 then
         Set_Base_In_Nim(Base_Index => Base_Index);
      end if;
      Update_Ada_Game(M => Minutes, In_C => (if In_Combat then 1 else 0));
      if Base_Index > 0 then
         Get_Base_From_Nim(Base_Index => Base_Index);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Game_Date;
      Set_Map_Cell(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
   end Update_Game;

   procedure End_Game(Save: Boolean) is
      use Events;

      procedure End_Ada_Game(S: Integer) with
         Import => True,
         Convention => C,
         External_Name => "endAdaGame";
   begin
      Get_Ada_Save_Name
        (Name => New_String(Str => To_String(Source => Save_Name)));
      if Save then
         Set_Ship_In_Nim;
         Get_Bases_Loop :
         for I in Bases_Range loop
            Set_Base_In_Nim(Base_Index => I);
         end loop Get_Bases_Loop;
         Get_Map_Y_Loop :
         for Y in Map_Y_Range loop
            Get_Map_X_Loop :
            for X in Map_X_Range loop
               Get_Ada_Map_Cell
                 (X => X, Y => Y, Base_Index => Sky_Map(X, Y).Base_Index,
                  Visited => (if Sky_Map(X, Y).Visited then 1 else 0),
                  Event_Index => Sky_Map(X, Y).Event_Index,
                  Mission_Index => Sky_Map(X, Y).Mission_Index);
            end loop Get_Map_X_Loop;
         end loop Get_Map_Y_Loop;
         Get_Current_Goal;
         Get_Current_Story;
         Get_Finished_Stories_Loop :
         for I in Finished_Stories.First_Index .. Finished_Stories.Last_Index loop
            Get_Finished_Story(Index => I);
         end loop Get_Finished_Stories_Loop;
         Set_Nim_Events;
         Get_Ada_Game_String
           (Name => New_String(Str => "playerCareer"),
            Value => New_String(Str => To_String(Source => Player_Career)));
      end if;
      End_Ada_Game(S => (if Save then 1 else 0));
      Events_List.Clear;
      Clear_Game_Stats;
      Clear_Current_Goal;
   end End_Game;

   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index is
      function Find_Ada_Skill_Index
        (S_Name: chars_ptr) return SkillsData_Container.Extended_Index with
         Import => True,
         Convention => C,
         External_Name => "findAdaSkillIndex";
   begin
      return Find_Ada_Skill_Index(S_Name => New_String(Str => Skill_Name));
   end Find_Skill_Index;

   function Load_Game_Data return String is
      use Ada.Exceptions;
      use BasesTypes;
      use Careers;
      use Log;

      Result: chars_ptr;
      function Load_Ada_Game_Data return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaGameData";
      procedure Load_Data is
         use Interfaces.C;
         use Tiny_String;

         Item_Index: Natural;
         --## rule off TYPE_INITIAL_VALUES
         type Nim_Strings_Array is array(0 .. 12) of chars_ptr;
         type Nim_Integers_Array is array(0 .. 10) of Integer;
         --## rule on TYPE_INITIAL_VALUES
         Nim_Strings: Nim_Strings_Array;
         Nim_Integers: Nim_Integers_Array := (others => 0);
         procedure Get_Ada_Game_Strings(Values: out Nim_Strings_Array) with
            Import => True,
            Convention => C,
            External_Name => "getAdaGameStrings";
         procedure Get_Ada_Game_Integers(Values: out Nim_Integers_Array) with
            Import => True,
            Convention => C,
            External_Name => "getAdaGameIntegers";
      begin
         Item_Index := 0;
         Fill_Attributes_Block :
         declare
            --## rule off TYPE_INITIAL_VALUES
            type Attribute_Nim_Array is array(0 .. 1) of chars_ptr;
            --## rule on TYPE_INITIAL_VALUES
            Attribute_Array: Attribute_Nim_Array;
            procedure Get_Ada_Attribute
              (I_Index: Natural; Attribute: out Attribute_Nim_Array) with
               Import => True,
               Convention => C,
               External_Name => "getAdaAttribute";
         begin
            Fill_Attributes_Loop :
            loop
               Get_Ada_Attribute
                 (I_Index => Attributes_Amount, Attribute => Attribute_Array);
               exit Fill_Attributes_Loop when Strlen
                   (Item => Attribute_Array(0)) =
                 0;
               AttributesData_Container.Append
                 (Container => Attributes_List,
                  New_Item =>
                    (Name =>
                       Tiny_String.To_Bounded_String
                         (Source => Value(Item => Attribute_Array(0))),
                     Description =>
                       Short_String.To_Bounded_String
                         (Source => Value(Item => Attribute_Array(1)))));
               Attributes_Amount := Attributes_Amount + 1;
            end loop Fill_Attributes_Loop;
         end Fill_Attributes_Block;
         Fill_Skills_Block :
         declare
            --## rule off TYPE_INITIAL_VALUES
            type Nim_Skill_Record is record
               Name: chars_ptr;
               Attribute: Integer;
               Description: chars_ptr;
               Tool: chars_ptr;
            end record;
            --## rule on TYPE_INITIAL_VALUES
            --## rule off IMPROPER_INITIALIZATION
            Skill: Nim_Skill_Record;
            --## rule on IMPROPER_INITIALIZATION
            procedure Get_Ada_Skill
              (S_Index: Natural; Nim_Skill: out Nim_Skill_Record) with
               Import => True,
               Convention => C,
               External_Name => "getAdaSkill";
         begin
            Item_Index := 1;
            Fill_Skills_Loop :
            loop
               Get_Ada_Skill(S_Index => Item_Index, Nim_Skill => Skill);
               exit Fill_Skills_Loop when Strlen(Item => Skill.Name) = 0;
               Load_Skill_Block :
               declare
                  --## rule off TYPE_INITIAL_VALUES
                  type Nim_Tools_Array is array(0 .. 15, 0 .. 1) of Integer;
                  --## rule on TYPE_INITIAL_VALUES
                  function Get_Ada_Skill_Tools_Amount
                    (S_Index: Natural) return Integer with
                     Import => True,
                     Convention => C,
                     External_Name => "getAdaSkillToolsAmount";
                  procedure Get_Ada_Skill_Tools
                    (S_Index: Natural; Nim_Tools: out Nim_Tools_Array) with
                     Import => True,
                     Convention => C,
                     External_Name => "getAdaSkillTools";
                  Tools: Nim_Tools_Array;
                  Tools_Quality: Tool_Quality_Array
                    (1 ..
                         (if
                            Get_Ada_Skill_Tools_Amount(S_Index => Item_Index) >
                            0
                          then
                            Get_Ada_Skill_Tools_Amount(S_Index => Item_Index)
                          else 1)) :=
                    (others => <>);
                  Tmp_Skill: Skill_Record
                    (Quality_Amount => Tools_Quality'Length) :=
                    (Quality_Amount => Tools_Quality'Length, others => <>);
               begin
                  Get_Ada_Skill_Tools
                    (S_Index => Item_Index, Nim_Tools => Tools);
                  Load_Skills_Loop :
                  for J in Tools_Quality'Range loop
                     Tools_Quality(J) :=
                       (Level => Tools(J - 1, 0), Quality => Tools(J - 1, 1));
                  end loop Load_Skills_Loop;
                  if Get_Ada_Skill_Tools_Amount(S_Index => Item_Index) = 0 then
                     Tools_Quality := Empty_Tool_Quality_Array;
                  end if;
                  Tmp_Skill :=
                    (Quality_Amount => Tools_Quality'Length,
                     Name =>
                       To_Bounded_String(Source => Value(Item => Skill.Name)),
                     Attribute => Skill.Attribute,
                     Description =>
                       Short_String.To_Bounded_String
                         (Source => Value(Item => Skill.Description)),
                     Tool =>
                       Tiny_String.To_Bounded_String
                         (Source => Value(Item => Skill.Tool)),
                     Tools_Quality => Tools_Quality);
                  SkillsData_Container.Append
                    (Container => Skills_List, New_Item => Tmp_Skill);
                  Skills_Amount := Skills_Amount + 1;
               end Load_Skill_Block;
               Item_Index := Item_Index + 1;
            end loop Fill_Skills_Loop;
         end Fill_Skills_Block;
         Get_Ada_Game_Strings(Values => Nim_Strings);
         Repair_Tools :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(0)));
         Cleaning_Tools :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(1)));
         Alchemy_Tools :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(2)));
         Mission_Items_Type :=
           To_Unbounded_String(Source => Value(Item => Nim_Strings(3)));
         Fuel_Type :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(4)));
         Traders_Name :=
           To_Unbounded_String(Source => Value(Item => Nim_Strings(5)));
         Head_Armor :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(6)));
         Chest_Armor :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(7)));
         Arms_Armor :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(8)));
         Legs_Armor :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(9)));
         Shield_Type :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(10)));
         Weapon_Type :=
           To_Bounded_String(Source => Value(Item => Nim_Strings(11)));
         Money_Name :=
           To_Unbounded_String(Source => Value(Item => Nim_Strings(12)));
         Get_Ada_Game_Integers(Values => Nim_Integers);
         Corpse_Index := Nim_Integers(0);
         Money_Index := Nim_Integers(1);
         Condition_Index := Nim_Integers(2);
         Strength_Index := Nim_Integers(3);
         Piloting_Skill := Count_Type(Nim_Integers(4));
         Engineering_Skill := Count_Type(Nim_Integers(5));
         Gunnery_Skill := Count_Type(Nim_Integers(6));
         Talking_Skill := Count_Type(Nim_Integers(7));
         Perception_Skill := Count_Type(Nim_Integers(8));
         Dodge_Skill := Count_Type(Nim_Integers(9));
         Unarmed_Skill := Count_Type(Nim_Integers(10));
      end Load_Data;
   begin
      if Get_Proto_Ship(Proto_Index => 1) /= Empty_Proto_Ship then
         return "";
      end if;
      Result := Load_Ada_Game_Data;
      Load_Data;
      Load_Careers;
      Load_Goals;
      Load_Bases_Types;
      Load_Stories;
      return Value(Item => Result);
   exception
      when An_Exception : others =>
         Log_Message
           (Message => Exception_Message(X => An_Exception),
            Message_Type => EVERYTHING);
         return Exception_Message(X => An_Exception);
   end Load_Game_Data;

   procedure Get_Game_Date is
      procedure Get_Ada_Game_Date
        (Year, Month, Day, Hour, Minutes: Integer) with
         Import => True,
         Convention => C,
         External_Name => "getAdaGameDate";
   begin
      Get_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour,
         Minutes => Game_Date.Minutes);
   end Get_Game_Date;

   procedure Set_Game_Date is
      procedure Set_Ada_Game_Date(Year, Month, Day, Hour, M: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGameDate";
   begin
      Set_Ada_Game_Date
        (Year => Game_Date.Year, Month => Game_Date.Month,
         Day => Game_Date.Day, Hour => Game_Date.Hour, M => Game_Date.Minutes);
   end Set_Game_Date;

end Game;
