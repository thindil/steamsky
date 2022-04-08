--    Copyright 2016-2022 Bartek thindil Jasicki
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

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with DOM.Core;
with DOM.Core.Elements;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.File;
with Bases; use Bases;
with Bases.Ship;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes;
with Careers;
with Config; use Config;
with Crafts; use Crafts;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with Help;
with Items; use Items;
with Log;
with Maps; use Maps;
with Messages; use Messages;
with Missions; use Missions;
with Mobs; use Mobs;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Crew;
with Ships.Repairs;
with Ships.Upgrade;
with Statistics; use Statistics;
with Stories;
with Utils;

package body Game is

   procedure New_Game is
      use Tiny_String;
      use Utils;

      Random_Base: Positive := Positive'First;
   begin
      -- Save game configuration
      Save_Config;
      -- Set game statistics
      Clear_Game_Stats;
      Set_Faction_Career_Block :
      declare
         Roll,
         Index: Positive range Positive'First ..
             Positive(Factions_List.Length) :=
           Positive'First;
      begin
         -- Set player faction if random option was selected
         if New_Game_Settings.Player_Faction =
           To_Bounded_String(Source => "random") then
            New_Game_Settings.Player_Career :=
              To_Unbounded_String(Source => "random");
            Roll :=
              Get_Random(Min => 1, Max => Positive(Factions_List.Length));
            Index := 1;
            Get_Player_Faction_Loop :
            for I in Factions_List.Iterate loop
               if Index = Roll then
                  New_Game_Settings.Player_Faction :=
                    Factions_Container.Key(Position => I);
                  exit Get_Player_Faction_Loop;
               end if;
               Index := Index + 1;
            end loop Get_Player_Faction_Loop;
         end if;
         -- Set player career if random option was selected
         if New_Game_Settings.Player_Career =
           To_Unbounded_String(Source => "random") then
            Roll :=
              Get_Random
                (Min => 1,
                 Max =>
                   Positive
                     (Factions_List(New_Game_Settings.Player_Faction).Careers
                        .Length));
            Index := 1;
            Get_Player_Career_Loop :
            for I in Factions_List(New_Game_Settings.Player_Faction).Careers
              .Iterate loop
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
         Base_Owner: Tiny_String.Bounded_String;
         Base_Type: Unbounded_String := Null_Unbounded_String;
         package Bases_Container is new Hashed_Maps
           (Key_Type => Bounded_String,
            Element_Type => Positive_Container.Vector,
            Hash => Tiny_String_Hash, Equivalent_Keys => Tiny_String."=",
            "=" => Positive_Container."=");
         Bases_Array: Bases_Container.Map := Bases_Container.Empty_Map;
         Attempts: Positive range 1 .. 251 := 1;
      begin
         Count_Spawn_Chance_Loop :
         for I in Factions_List.Iterate loop
            Max_Spawn_Roll := Max_Spawn_Roll + Factions_List(I).Spawn_Chance;
            Bases_Array.Include
              (Key => Factions_Container.Key(Position => I),
               New_Item => Positive_Container.Empty_Vector);
         end loop Count_Spawn_Chance_Loop;
         Set_Bases_Loop :
         for I in Sky_Bases'Range loop
            Faction_Roll := Get_Random(Min => 1, Max => Max_Spawn_Roll);
            Set_Base_Faction_Loop :
            for J in Factions_List.Iterate loop
               if Faction_Roll <= Factions_List(J).Spawn_Chance then
                  Base_Owner := Factions_Container.Key(Position => J);
                  Base_Population :=
                    (if Factions_List(J).Population(2) = 0 then
                       Factions_List(J).Population(1)
                     else Get_Random
                         (Min => Factions_List(J).Population(1),
                          Max => Factions_List(J).Population(2)));
                  Base_Reputation :=
                    Get_Reputation
                      (Source_Faction => New_Game_Settings.Player_Faction,
                       Target_Faction =>
                         Factions_Container.Key(Position => J));
                  Max_Base_Spawn_Roll := 0;
                  Count_Max_Spawn_Chance_Loop :
                  for SpawnChance of Factions_List(J).Bases_Types loop
                     Max_Base_Spawn_Roll := Max_Base_Spawn_Roll + SpawnChance;
                  end loop Count_Max_Spawn_Chance_Loop;
                  Base_Type_Roll :=
                    Get_Random(Min => 1, Max => Max_Base_Spawn_Roll);
                  Get_Base_Type_Loop :
                  for K in Factions_List(J).Bases_Types.Iterate loop
                     if Base_Type_Roll <= Factions_List(J).Bases_Types(K) then
                        Base_Type := BaseType_Container.Key(Position => K);
                        exit Get_Base_Type_Loop;
                     end if;
                     Base_Type_Roll :=
                       Base_Type_Roll - Factions_List(J).Bases_Types(K);
                  end loop Get_Base_Type_Loop;
                  exit Set_Base_Faction_Loop;
               end if;
               Faction_Roll := Faction_Roll - Factions_List(J).Spawn_Chance;
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
            if Factions_List(Base_Owner).Flags.Contains
                (Item => To_Unbounded_String(Source => "loner")) then
               Faction_Roll := Get_Random(Min => 1, Max => Max_Spawn_Roll);
               Get_Faction_Loop :
               for J in Factions_List.Iterate loop
                  if Faction_Roll > Factions_List(J).Spawn_Chance then
                     Faction_Roll :=
                       Faction_Roll - Factions_List(J).Spawn_Chance;
                  else
                     Base_Owner := Factions_Container.Key(Position => J);
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
                    (Factions_List
                       (Sky_Bases(FactionBases(FactionBases.First_Index))
                          .Owner)
                       .Flags
                       .Contains
                       (Item => To_Unbounded_String(Source => "loner")) and
                     Factions_List(Sky_Bases(FactionBases(I)).Owner).Flags
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
            end loop Place_Faction_Bases_Loop;
         end loop Place_Bases_Loop;
      end Generate_Bases_Block;
      -- Place player ship in random large base
      Place_Player_Loop :
      loop
         Random_Base := Get_Random(Min => 1, Max => 1_024);
         if New_Game_Settings.Starting_Base =
           To_Unbounded_String(Source => "Any") then
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
             Factions_List(New_Game_Settings.Player_Faction).Careers
               (New_Game_Settings.Player_Career)
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
         Player_Index_2: constant Positive :=
           Positive'Value
             (To_String
                (Source =>
                   Factions_List(New_Game_Settings.Player_Faction).Careers
                     (New_Game_Settings.Player_Career)
                     .Player_Index));
         Amount: Positive := 1;
         Tmp_Inventory: Inventory_Container.Vector (Capacity => 32);
         Player_Morale: constant Positive :=
           (if
              Factions_List(New_Game_Settings.Player_Faction).Flags.Contains
                (Item => To_Unbounded_String(Source => "nomorale"))
            then 50
            else 100);
      begin
         Player_Inventory_Loop :
         for I in
           MobInventory_Container.First_Index
             (Container =>
                ProtoMobs_Container.Element
                  (Container => Proto_Mobs_List, Index => Player_Index_2)
                  .Inventory) ..
             MobInventory_Container.Last_Index
               (Container =>
                  ProtoMobs_Container.Element
                    (Container => Proto_Mobs_List, Index => Player_Index_2)
                    .Inventory) loop
            Add_Inventory_Block :
            declare
               Proto_Inventory: constant Mob_Inventory_Record :=
                 MobInventory_Container.Element
                   (Container =>
                      ProtoMobs_Container.Element
                        (Container => Proto_Mobs_List, Index => Player_Index_2)
                        .Inventory,
                    Index => I);
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
               Tired => 0,
               Skills =>
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Player_Index_2)
                   .Skills,
               Hunger => 0, Thirst => 0,
               Order =>
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Player_Index_2)
                   .Order,
               Previous_Order => REST, Order_Time => 15,
               Orders =>
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Player_Index_2)
                   .Priorities,
               Attributes =>
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Player_Index_2)
                   .Attributes,
               Inventory => Tmp_Inventory,
               Equipment =>
                 ProtoMobs_Container.Element
                   (Container => Proto_Mobs_List, Index => Player_Index_2)
                   .Equipment,
               Payment => (others => 0), Contract_Length => -1,
               Morale => (1 => Player_Morale, 2 => 0), Loyalty => 100,
               Home_Base => Random_Base,
               Faction => New_Game_Settings.Player_Faction));
      end Add_Player_Block;
      Assign_Cabin_Block :
      declare
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
            if Modules_List(Module.Proto_Index).M_Type = CABIN and
              not Cabin_Assigned then
               Assign_Cabin_Loop :
               for I in Module.Owner.Iterate loop
                  if Module.Owner(I) = 0 then
                     Module.Owner(I) := 1;
                     if Natural_Container.To_Index(Position => I) = 1 then
                        Module.Name :=
                          New_Game_Settings.Player_Name &
                          To_Unbounded_String(Source => "'s Cabin");
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
      -- Add welcoming message
      Add_Message
        (Message =>
           "Welcome to Steam Sky. If it is your first game, please consider read help (entry 'Help' in Menu), especially topic 'First Steps'.",
         M_Type => OTHERMESSAGE);
   end New_Game;

   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) is
      use Bases.Ship;
      use Ships.Crew;
      use Ships.Upgrade;
      use Tiny_String;

      Added_Hours, Added_Minutes: Natural := 0;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Tired_Points: Natural := 0;
      Need_Cleaning, Need_Save_Game: Boolean := False;
      procedure Update_Day is
      begin
         Game_Date.Day := Game_Date.Day + 1;
         Get_Dirty_Loop :
         for Module of Player_Ship.Modules loop
            if Module.M_Type = CABIN and then Module.Cleanliness > 0 then
               Module.Cleanliness := Module.Cleanliness - 1;
               Need_Cleaning := True;
            end if;
         end loop Get_Dirty_Loop;
         if Need_Cleaning then
            Update_Orders(Ship => Player_Ship);
         end if;
         if Player_Ship.Speed = DOCKED then
            Pay_For_Dock;
         end if;
         Daily_Payment;
         if Game_Settings.Auto_Save = DAILY then
            Need_Save_Game := True;
         end if;
      end Update_Day;
   begin
      Tired_Points_Loop :
      for I in 1 .. Minutes loop
         if (Game_Date.Minutes + I) rem 15 = 0 then
            Tired_Points := Tired_Points + 1;
         end if;
      end loop Tired_Points_Loop;
      -- Update game time
      Added_Minutes := Minutes rem 60;
      Added_Hours := Minutes / 60;
      Game_Date.Minutes := Game_Date.Minutes + Added_Minutes;
      if Game_Date.Minutes > 59 then
         Game_Date.Minutes := Game_Date.Minutes - 60;
         Game_Date.Hour := Game_Date.Hour + 1;
      end if;
      while Added_Hours > 23 loop
         Added_Hours := Added_Hours - 24;
         Update_Day;
      end loop;
      Game_Date.Hour := Game_Date.Hour + Added_Hours;
      while Game_Date.Hour > 23 loop
         Game_Date.Hour := Game_Date.Hour - 24;
         Update_Day;
      end loop;
      if Need_Save_Game then
         Save_Game;
      end if;
      if Game_Date.Day > 30 then
         Game_Date.Day := 1;
         Game_Date.Month := Game_Date.Month + 1;
         if Game_Settings.Auto_Save = MONTHLY then
            Save_Game;
         end if;
      end if;
      if Game_Date.Month > 12 then
         Game_Date.Month := 1;
         Game_Date.Year := Game_Date.Year + 1;
         if Game_Settings.Auto_Save = YEARLY then
            Save_Game;
         end if;
      end if;
      -- Update crew
      Update_Crew
        (Minutes => Minutes, Tired_Points => Tired_Points,
         In_Combat => In_Combat);
      -- Repair ship (if needed)
      Ships.Repairs.Repair_Ship(Minutes => Minutes);
      -- Craft items
      Manufacturing(Minutes => Minutes);
      -- Upgrade ship module
      Upgrade_Ship(Minutes => Minutes);
      -- Update base
      if Base_Index > 0 then
         if Sky_Bases(Base_Index).Visited.Year = 0 then
            Game_Stats.Bases_Visited := Game_Stats.Bases_Visited + 1;
            Game_Stats.Points := Game_Stats.Points + 1;
            Update_Goal
              (G_Type => VISIT,
               Target_Index =>
                 To_Unbounded_String
                   (Source =>
                      To_String(Source => Sky_Bases(Base_Index).Owner)));
         end if;
         Sky_Bases(Base_Index).Visited := Game_Date;
         if not Sky_Bases(Base_Index).Known then
            Sky_Bases(Base_Index).Known := True;
            Add_Message
              (Message =>
                 "You discovered base " &
                 To_String(Source => Sky_Bases(Base_Index).Name) & ".",
               M_Type => OTHERMESSAGE);
         end if;
         Update_Population;
         Generate_Recruits;
         Generate_Missions;
         Generate_Cargo;
         Update_Prices;
         Update_Orders(Ship => Player_Ship);
      end if;
      -- Update map cell
      if not Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited then
         Game_Stats.Map_Visited := Game_Stats.Map_Visited + 1;
         Game_Stats.Points := Game_Stats.Points + 1;
         Update_Goal
           (G_Type => DISCOVER, Target_Index => Null_Unbounded_String);
         Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited := True;
      end if;
      -- Update events
      Update_Events(Minutes => Minutes);
      -- Update accepted missions
      Update_Missions(Minutes => Minutes);
   end Update_Game;

   procedure End_Game(Save: Boolean) is
   begin
      if Save then
         Save_Game;
      else
         Delete_Save_Block :
         begin
            Delete_File(Name => To_String(Source => Save_Name));
         exception
            when Name_Error =>
               null;
         end Delete_Save_Block;
      end if;
      Clear_Messages;
      Events_List.Clear;
      Clear_Game_Stats;
      Known_Recipes.Clear;
      Clear_Current_Goal;
      Accepted_Missions.Clear;
      Save_Config;
   end End_Game;

   function Find_Skill_Index
     (Skill_Name: String) return SkillsData_Container.Extended_Index is
      use Tiny_String;
   begin
      Find_Skill_Loop :
      for I in 1 .. Skills_Amount loop
         if To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List, Index => I)
                  .Name) =
           Skill_Name then
            return I;
         end if;
      end loop Find_Skill_Loop;
      return 0;
   end Find_Skill_Index;

   function Load_Game_Data return String is
      use Ada.Exceptions;
      use Log;

      --## rule off TYPE_INITIAL_VALUES
      type Data_Type_Record is record
         Name: Unbounded_String;
         File_Name: Unbounded_String;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Data_Types: constant array(1 .. 12) of Data_Type_Record :=
        (1 =>
           (Name => To_Unbounded_String(Source => "data"),
            File_Name => To_Unbounded_String(Source => "game.dat")),
         2 =>
           (Name => To_Unbounded_String(Source => "items"),
            File_Name => To_Unbounded_String(Source => "items.dat")),
         3 =>
           (Name => To_Unbounded_String(Source => "help"),
            File_Name => To_Unbounded_String(Source => "help.dat")),
         4 =>
           (Name => To_Unbounded_String(Source => "modules"),
            File_Name => To_Unbounded_String(Source => "shipmodules.dat")),
         5 =>
           (Name => To_Unbounded_String(Source => "recipes"),
            File_Name => To_Unbounded_String(Source => "recipes.dat")),
         6 =>
           (Name => To_Unbounded_String(Source => "bases"),
            File_Name => To_Unbounded_String(Source => "bases.dat")),
         7 =>
           (Name => To_Unbounded_String(Source => "mobiles"),
            File_Name => To_Unbounded_String(Source => "mobs.dat")),
         8 =>
           (Name => To_Unbounded_String(Source => "careers"),
            File_Name => To_Unbounded_String(Source => "careers.dat")),
         9 =>
           (Name => To_Unbounded_String(Source => "factions"),
            File_Name => To_Unbounded_String(Source => "factions.dat")),
         10 =>
           (Name => To_Unbounded_String(Source => "ships"),
            File_Name => To_Unbounded_String(Source => "ships.dat")),
         11 =>
           (Name => To_Unbounded_String(Source => "goals"),
            File_Name => To_Unbounded_String(Source => "goals.dat")),
         12 =>
           (Name => To_Unbounded_String(Source => "stories"),
            File_Name => To_Unbounded_String(Source => "stories.dat")));
      Mods_Directories: Search_Type;
      Found_Directory: Directory_Entry_Type;
      procedure Load_Selected_Data(Data_Name, File_Name: String) is
         use Input_Sources.File;

         Files: Search_Type;
         Found_File: Directory_Entry_Type;
         Data_File: File_Input;
         Local_File_Name: Unbounded_String := Null_Unbounded_String;
         procedure Load_Data_File(Local_Data_Name: String) is
            use DOM.Core.Documents;
            use DOM.Core.Nodes;
            use DOM.Readers;
            use BasesTypes;
            use Careers;
            use Help;
            use Stories;

            Data_Type: Unbounded_String;
            Reader: Tree_Reader; --## rule line off IMPROPER_INITIALIZATION
            procedure Load_Data(Current_Reader: Tree_Reader) is
               use DOM.Core;
               use DOM.Core.Elements;
               use Short_String;
               use Tiny_String;

               Game_Data: Document;
               Nodes_List, Child_Nodes: Node_List;
               Delete_Index: Natural := 0;
               Node_Name: Unbounded_String := Null_Unbounded_String;
               Data_Node: Node;
               function Find_Attribute_Index
                 (Attribute_Name: Tiny_String.Bounded_String) return Natural is
               begin
                  Find_Attribute_Loop :
                  for J in
                    AttributesData_Container.First_Index
                      (Container => Attributes_List) ..
                      AttributesData_Container.Last_Index
                        (Container => Attributes_List) loop
                     if AttributesData_Container.Element
                         (Container => Attributes_List, Index => J)
                         .Name =
                       Attribute_Name then
                        return Natural(J);
                     end if;
                  end loop Find_Attribute_Loop;
                  return 0;
               end Find_Attribute_Index;
            begin
               Game_Data := Get_Tree(Read => Current_Reader);
               Nodes_List :=
                 DOM.Core.Nodes.Child_Nodes(N => First_Child(N => Game_Data));
               Child_Nodes := Nodes_List;
               Load_Game_Data_Loop :
               for I in 0 .. Length(List => Nodes_List) - 1 loop
                  Data_Node := Item(List => Nodes_List, Index => I);
                  Node_Name :=
                    To_Unbounded_String
                      (Source => DOM.Core.Nodes.Node_Name(N => Data_Node));
                  if To_String(Source => Node_Name) = "basessyllablepre" then
                     Base_Syllables_Pre.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllablestart" then
                     Base_Syllables_Start.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllableend" then
                     Base_Syllables_End.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllablepost" then
                     Base_Syllables_Post.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllablestart" then
                     SyllableString_Container.Append
                       (Container => Male_Syllables_Start,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllablemiddle" then
                     SyllableString_Container.Append
                       (Container => Male_Syllables_Middle,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllableend" then
                     SyllableString_Container.Append
                       (Container => Male_Syllables_End,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "malesvocal" then
                     SyllableString_Container.Append
                       (Container => Male_Vocals,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "malesconsonant" then
                     SyllableString_Container.Append
                       (Container => Male_Consonants,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllablestart" then
                     SyllableString_Container.Append
                       (Container => Female_Syllables_Start,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllablemiddle" then
                     SyllableString_Container.Append
                       (Container => Female_Syllables_Middle,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllableend" then
                     Female_Syllables_End.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "femalesvocal" then
                     Female_Vocals.Append
                       (New_Item =>
                          To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllablestart" then
                     SyllableString_Container.Append
                       (Container => Ship_Syllables_Start,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllablemiddle" then
                     SyllableString_Container.Append
                       (Container => Ship_Syllables_Middle,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllableend" then
                     SyllableString_Container.Append
                       (Container => Ship_Syllables_End,
                        New_Item =>
                          Syllable_String.To_Bounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "itemtype" then
                     Items_Types.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "repairtools" then
                     Repair_Tools :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "cleaningtools" then
                     Cleaning_Tools :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "alchemytools" then
                     Alchemy_Tools :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "corpseindex" then
                     Corpse_Index :=
                       To_Bounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) =
                    "missionitemstype" then
                     Mission_Items_Type :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "fueltype" then
                     Fuel_Type :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "moneyindex" then
                     Money_Index :=
                       To_Bounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "tradersname" then
                     Traders_Name :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "attribute" then
                     AttributesData_Container.Append
                       (Container => Attributes_List,
                        New_Item =>
                          (Name =>
                             To_Bounded_String
                               (Source =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "name")),
                           Description =>
                             To_Bounded_String
                               (Source =>
                                  Node_Value
                                    (N => First_Child(N => Data_Node)))));
                     Attributes_Amount := Attributes_Amount + 1;
                  elsif To_String(Source => Node_Name) = "skill" then
                     Child_Nodes :=
                       DOM.Core.Elements.Get_Elements_By_Tag_Name
                         (Elem => Data_Node, Name => "toolquality");
                     Load_Skill_Block :
                     declare
                        Tools_Quality: Tool_Quality_Array
                          (1 ..
                               (if Length(List => Child_Nodes) > 0 then
                                  Length(List => Child_Nodes)
                                else 1)) :=
                          (others => <>);
                        Tmp_Skill: Skill_Record
                          (Quality_Amount => Tools_Quality'Length) :=
                          (Quality_Amount => Tools_Quality'Length,
                           others => <>);
                     begin
                        Load_Skills_Loop :
                        for J in 0 .. Length(List => Child_Nodes) - 1 loop
                           Tools_Quality(J + 1) :=
                             (Level =>
                                Skill_Range'Value
                                  (Get_Attribute
                                     (Elem =>
                                        Item(List => Child_Nodes, Index => J),
                                      Name => "level")),
                              Quality =>
                                Skill_Range'Value
                                  (Get_Attribute
                                     (Elem =>
                                        Item(List => Child_Nodes, Index => J),
                                      Name => "quality")));
                        end loop Load_Skills_Loop;
                        if Length(List => Child_Nodes) = 0 then
                           Tools_Quality := Empty_Tool_Quality_Array;
                        end if;
                        Tmp_Skill :=
                          (Quality_Amount => Tools_Quality'Length,
                           Name =>
                             To_Bounded_String
                               (Source =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "name")),
                           Attribute =>
                             Find_Attribute_Index
                               (Attribute_Name =>
                                  To_Bounded_String
                                    (Source =>
                                       Get_Attribute
                                         (Elem => Data_Node,
                                          Name => "attribute"))),
                           Description => Short_String.Null_Bounded_String,
                           Tool => Tiny_String.Null_Bounded_String,
                           Tools_Quality => Tools_Quality);
                        Child_Nodes :=
                          DOM.Core.Elements.Get_Elements_By_Tag_Name
                            (Elem => Data_Node, Name => "description");
                        if Length(List => Child_Nodes) > 0 then
                           Tmp_Skill.Description :=
                             To_Bounded_String
                               (Source =>
                                  Node_Value
                                    (N =>
                                       First_Child
                                         (N =>
                                            Item
                                              (List => Child_Nodes,
                                               Index => 0))));
                        end if;
                        if Get_Attribute(Elem => Data_Node, Name => "tool") /=
                          "" then
                           Tmp_Skill.Tool :=
                             To_Bounded_String
                               (Source =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "tool"));
                        end if;
                        SkillsData_Container.Append
                          (Container => Skills_List, New_Item => Tmp_Skill);
                        Skills_Amount := Skills_Amount + 1;
                     end Load_Skill_Block;
                  elsif To_String(Source => Node_Name) = "conditionname" then
                     Condition_Index :=
                       Find_Attribute_Index
                         (Attribute_Name =>
                            To_Bounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "strengthname" then
                     Strength_Index :=
                       Find_Attribute_Index
                         (Attribute_Name =>
                            To_Bounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "pilotingskill" then
                     Piloting_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) =
                    "engineeringskill" then
                     Engineering_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "gunneryskill" then
                     Gunnery_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "talkingskill" then
                     Talking_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "perceptionskill" then
                     Perception_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "headarmor" then
                     Head_Armor :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "chestarmor" then
                     Chest_Armor :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "armsarmor" then
                     Arms_Armor :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "legsarmor" then
                     Legs_Armor :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "shieldtype" then
                     Shield_Type :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "weapontype" then
                     Weapon_Type :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "dodgeskill" then
                     Dodge_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "unarmedskill" then
                     Unarmed_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "remove" then
                     if Get_Attribute(Elem => Data_Node, Name => "name") =
                       "skill" then
                        Delete_Skill_Block :
                        declare
                           Delete_Index: constant SkillsData_Container
                             .Extended_Index :=
                             Find_Skill_Index
                               (Skill_Name =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "value"));
                        begin
                           if Delete_Index > 0 then
                              SkillsData_Container.Delete
                                (Container => Skills_List,
                                 Index => Delete_Index);
                           end if;
                        end Delete_Skill_Block;
                     elsif Get_Attribute(Elem => Data_Node, Name => "name") =
                       "attribute" then
                        Delete_Index :=
                          Find_Attribute_Index
                            (Attribute_Name =>
                               To_Bounded_String
                                 (Source =>
                                    Get_Attribute
                                      (Elem => Data_Node, Name => "value")));
                        if Delete_Index > 0 then
                           AttributesData_Container.Delete
                             (Container => Attributes_List,
                              Index => Delete_Index);
                        end if;
                     elsif Get_Attribute(Elem => Data_Node, Name => "name") =
                       "itemtype" then
                        Delete_Index := 0;
                        Load_Item_Types_Loop :
                        for J in
                          Items_Types.First_Index ..
                            Items_Types.Last_Index loop
                           if Items_Types(J) =
                             To_Unbounded_String
                               (Source =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "value")) then
                              Delete_Index := J;
                              exit Load_Item_Types_Loop;
                           end if;
                        end loop Load_Item_Types_Loop;
                        if Delete_Index > 0 then
                           Items_Types.Delete(Index => Delete_Index);
                        end if;
                     end if;
                  end if;
               end loop Load_Game_Data_Loop;
            end Load_Data;
         begin
            --## rule off IMPROPER_INITIALIZATION
            Parse(Parser => Reader, Input => Data_File);
            Data_Type :=
              To_Unbounded_String
                (Source =>
                   Node_Name
                     (N => Get_Element(Doc => Get_Tree(Read => Reader))));
            --## rule on IMPROPER_INITIALIZATION
            if Data_Type = To_Unbounded_String(Source => Local_Data_Name) or
              Local_Data_Name = "" then
               Log_Message
                 (Message =>
                    "Loading " & To_String(Source => Data_Type) & " file: " &
                    To_String(Source => Local_File_Name),
                  Message_Type => EVERYTHING);
               if To_String(Source => Data_Type) = "factions" then
                  Load_Factions(Reader => Reader);
               elsif To_String(Source => Data_Type) = "goals" then
                  Load_Goals(Reader => Reader);
               elsif To_String(Source => Data_Type) = "help" then
                  Load_Help(Reader => Reader);
               elsif To_String(Source => Data_Type) = "items" then
                  Load_Items(Reader => Reader);
               elsif To_String(Source => Data_Type) = "mobiles" then
                  Load_Mobs(Reader => Reader);
               elsif To_String(Source => Data_Type) = "recipes" then
                  Load_Recipes(Reader => Reader);
               elsif To_String(Source => Data_Type) = "bases" then
                  Load_Bases_Types(Reader => Reader);
               elsif To_String(Source => Data_Type) = "modules" then
                  Load_Ship_Modules(Reader => Reader);
               elsif To_String(Source => Data_Type) = "ships" then
                  Load_Ships(Reader => Reader);
               elsif To_String(Source => Data_Type) = "stories" then
                  Load_Stories(Reader => Reader);
               elsif To_String(Source => Data_Type) = "data" then
                  Load_Data(Current_Reader => Reader);
               elsif To_String(Source => Data_Type) = "careers" then
                  Load_Careers(Reader => Reader);
               end if;
            end if;
            Free(Read => Reader); --## rule line off IMPROPER_INITIALIZATION
         end Load_Data_File;
      begin
         if File_Name = "" then
            Start_Search
              (Search => Files, Directory => Data_Name, Pattern => "*.dat");
            Load_Data_Files_Loop :
            while More_Entries(Search => Files) loop
               Get_Next_Entry(Search => Files, Directory_Entry => Found_File);
               Open
                 (Filename => Full_Name(Directory_Entry => Found_File),
                  Input => Data_File);
               Local_File_Name :=
                 To_Unbounded_String
                   (Source => Full_Name(Directory_Entry => Found_File));
               Load_Data_File(Local_Data_Name => "");
               Close(Input => Data_File);
            end loop Load_Data_Files_Loop;
            End_Search(Search => Files);
         else
            Open
              (Filename => To_String(Source => Data_Directory) & File_Name,
               Input => Data_File);
            Local_File_Name := To_Unbounded_String(Source => File_Name);
            Load_Data_File(Local_Data_Name => Data_Name);
            Close(Input => Data_File);
         end if;
      end Load_Selected_Data;
   begin
      if Factions_List.Length > 0 then
         return "";
      end if;
      -- Load standard game data
      Load_Standard_Data_Loop :
      for Data_Type of Data_Types loop
         Load_Selected_Data
           (Data_Name => To_String(Source => Data_Type.Name),
            File_Name => To_String(Source => Data_Type.File_Name));
      end loop Load_Standard_Data_Loop;
      -- Load modifications
      Start_Search
        (Search => Mods_Directories,
         Directory => To_String(Source => Mods_Directory), Pattern => "",
         Filter => (Directory => True, others => False));
      Load_Modifications_Loop :
      while More_Entries(Search => Mods_Directories) loop
         Get_Next_Entry
           (Search => Mods_Directories, Directory_Entry => Found_Directory);
         if Simple_Name(Directory_Entry => Found_Directory) not in "." |
               ".." then
            Load_Selected_Data
              (Data_Name => Full_Name(Directory_Entry => Found_Directory),
               File_Name => "");
         end if;
      end loop Load_Modifications_Loop;
      End_Search(Search => Mods_Directories);
      Set_Tools_List;
      return "";
   exception
      when An_Exception : others =>
         Log_Message
           (Message => Exception_Message(X => An_Exception),
            Message_Type => EVERYTHING);
         return Exception_Message(X => An_Exception);
   end Load_Game_Data;

end Game;
