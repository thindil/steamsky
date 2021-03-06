--    Copyright 2016-2021 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded.Hash;
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
      use Utils;

      Random_Base: Positive := Positive'First;
   begin
      -- Save game configuration
      Save_Config;
      -- Set game statistics
      ClearGameStats;
      Set_Faction_Career_Block :
      declare
         Roll,
         Index: Positive range Positive'First ..
             Positive(Factions_List.Length) :=
           Positive'First;
      begin
         -- Set player faction if random option was selected
         if New_Game_Settings.Player_Faction =
           To_Unbounded_String(Source => "random") then
            New_Game_Settings.Player_Career :=
              To_Unbounded_String(Source => "random");
            Roll := GetRandom(Min => 1, Max => Positive(Factions_List.Length));
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
              GetRandom
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
      SkyMap :=
        (others =>
           (others =>
              (BaseIndex => 0, Visited => False, EventIndex => 0,
               MissionIndex => 0)));
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
         Tmp_Cargo: constant BaseCargo_Container.Vector :=
           BaseCargo_Container.Empty_Vector;
         Base_Size: Bases_Size := Small;
         Base_Owner, Base_Type: Unbounded_String := Null_Unbounded_String;
         package Bases_Container is new Hashed_Maps
           (Key_Type => Unbounded_String,
            Element_Type => Positive_Container.Vector,
            Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => "=",
            "=" => Positive_Container."=");
         Bases_Array: Bases_Container.Map := Bases_Container.Empty_Map;
         Attempts: Positive range 1 .. 251 := 1;
      begin
         Count_Spawn_Chance_Loop :
         for I in Factions_List.Iterate loop
            Max_Spawn_Roll := Max_Spawn_Roll + Factions_List(I).SpawnChance;
            Bases_Array.Include
              (Key => Factions_Container.Key(Position => I),
               New_Item => Positive_Container.Empty_Vector);
         end loop Count_Spawn_Chance_Loop;
         Set_Bases_Loop :
         for I in SkyBases'Range loop
            Faction_Roll := GetRandom(Min => 1, Max => Max_Spawn_Roll);
            Set_Base_Faction_Loop :
            for J in Factions_List.Iterate loop
               if Faction_Roll <= Factions_List(J).SpawnChance then
                  Base_Owner := Factions_Container.Key(Position => J);
                  Base_Population :=
                    (if Factions_List(J).Population(2) = 0 then
                       Factions_List(J).Population(1)
                     else GetRandom
                         (Min => Factions_List(J).Population(1),
                          Max => Factions_List(J).Population(2)));
                  Base_Reputation :=
                    GetReputation
                      (SourceFaction => New_Game_Settings.Player_Faction,
                       TargetFaction => Factions_Container.Key(Position => J));
                  Max_Base_Spawn_Roll := 0;
                  Count_Max_Spawn_Chance_Loop :
                  for SpawnChance of Factions_List(J).BasesTypes loop
                     Max_Base_Spawn_Roll := Max_Base_Spawn_Roll + SpawnChance;
                  end loop Count_Max_Spawn_Chance_Loop;
                  Base_Type_Roll :=
                    GetRandom(Min => 1, Max => Max_Base_Spawn_Roll);
                  Get_Base_Type_Loop :
                  for K in Factions_List(J).BasesTypes.Iterate loop
                     if Base_Type_Roll <= Factions_List(J).BasesTypes(K) then
                        Base_Type := BaseType_Container.Key(Position => K);
                        exit Get_Base_Type_Loop;
                     end if;
                     Base_Type_Roll :=
                       Base_Type_Roll - Factions_List(J).BasesTypes(K);
                  end loop Get_Base_Type_Loop;
                  exit Set_Base_Faction_Loop;
               end if;
               Faction_Roll := Faction_Roll - Factions_List(J).SpawnChance;
            end loop Set_Base_Faction_Loop;
            Base_Size :=
              (if Base_Population = 0 then
                 Bases_Size'Val(GetRandom(Min => 0, Max => 2))
               elsif Base_Population < 150 then Small
               elsif Base_Population < 300 then Medium else Big);
            SkyBases(I) :=
              (Name => GenerateBaseName(FactionIndex => Base_Owner),
               Visited => (others => 0), SkyX => 1, SkyY => 1,
               BaseType => Base_Type, Population => Base_Population,
               RecruitDate => (others => 0), Recruits => Tmp_Recruits,
               Known => False, AskedForBases => False,
               AskedForEvents => (others => 0),
               Reputation => (1 => Base_Reputation, 2 => 0),
               MissionsDate => (others => 0), Missions => Tmp_Missions,
               Owner => Base_Owner, Cargo => Tmp_Cargo, Size => Base_Size);
            if Factions_List(Base_Owner).Flags.Contains
                (Item => To_Unbounded_String(Source => "loner")) then
               Faction_Roll := GetRandom(Min => 1, Max => Max_Spawn_Roll);
               Get_Faction_Loop :
               for J in Factions_List.Iterate loop
                  if Faction_Roll > Factions_List(J).SpawnChance then
                     Faction_Roll :=
                       Faction_Roll - Factions_List(J).SpawnChance;
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
                       (SkyBases(FactionBases(FactionBases.First_Index)).Owner)
                       .Flags
                       .Contains
                       (Item => To_Unbounded_String(Source => "loner")) and
                     Factions_List(SkyBases(FactionBases(I)).Owner).Flags
                       .Contains
                       (Item => To_Unbounded_String(Source => "loner"))) then
                     Pos_X :=
                       GetRandom
                         (Min => Bases_Range'First + 5,
                          Max => Bases_Range'Last - 5);
                     Pos_Y :=
                       GetRandom
                         (Min => Bases_Range'First + 5,
                          Max => Bases_Range'Last - 5);
                  else
                     Pos_X :=
                       GetRandom
                         (Min =>
                            SkyBases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .SkyX -
                            20,
                          Max =>
                            SkyBases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .SkyX +
                            20);
                     NormalizeCoord(Coord => Pos_X);
                     Pos_Y :=
                       GetRandom
                         (Min =>
                            SkyBases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .SkyY -
                            20,
                          Max =>
                            SkyBases
                              (FactionBases
                                 (Positive_Container.To_Index(Position => I) -
                                  1))
                              .SkyY +
                            20);
                     NormalizeCoord(Coord => Pos_Y, IsXAxis => False);
                     Attempts := Attempts + 1;
                     if Attempts = 251 then
                        Pos_X :=
                          GetRandom
                            (Min => Bases_Range'First + 10,
                             Max => Bases_Range'Last - 10);
                        Pos_Y :=
                          GetRandom
                            (Min => Bases_Range'First + 10,
                             Max => Bases_Range'Last - 10);
                        Attempts := 1;
                     end if;
                  end if;
                  Check_X_Coordinate_Loop :
                  for J in -5 .. 5 loop
                     Temp_X := Pos_X + J;
                     NormalizeCoord(Coord => Temp_X);
                     Check_Y_Coordinate_Loop :
                     for K in -5 .. 5 loop
                        Temp_Y := Pos_Y + K;
                        NormalizeCoord(Coord => Temp_Y, IsXAxis => False);
                        if SkyMap(Temp_X, Temp_Y).BaseIndex > 0 then
                           Valid_Location := False;
                           exit Check_Y_Coordinate_Loop;
                        end if;
                     end loop Check_Y_Coordinate_Loop;
                     exit Check_X_Coordinate_Loop when not Valid_Location;
                  end loop Check_X_Coordinate_Loop;
                  if SkyMap(Pos_X, Pos_Y).BaseIndex > 0 then
                     Valid_Location := False;
                  end if;
                  exit Count_Base_Position_Loop when Valid_Location;
               end loop Count_Base_Position_Loop;
               SkyMap(Pos_X, Pos_Y) :=
                 (BaseIndex => FactionBases(I), Visited => False,
                  EventIndex => 0, MissionIndex => 0);
               SkyBases(FactionBases(I)).SkyX := Pos_X;
               SkyBases(FactionBases(I)).SkyY := Pos_Y;
            end loop Place_Faction_Bases_Loop;
         end loop Place_Bases_Loop;
      end Generate_Bases_Block;
      -- Place player ship in random large base
      Place_Player_Loop :
      loop
         Random_Base := GetRandom(Min => 1, Max => 1_024);
         if New_Game_Settings.Starting_Base =
           To_Unbounded_String(Source => "Any") then
            exit Place_Player_Loop when SkyBases(Random_Base).Population >
              299 and
              SkyBases(Random_Base).Owner = New_Game_Settings.Player_Faction;
         else
            exit Place_Player_Loop when SkyBases(Random_Base).Population >
              299 and
              SkyBases(Random_Base).Owner =
                New_Game_Settings.Player_Faction and
              SkyBases(Random_Base).BaseType = New_Game_Settings.Starting_Base;
         end if;
      end loop Place_Player_Loop;
      -- Create player ship
      Player_Ship :=
        Create_Ship
          (Proto_Index =>
             Factions_List(New_Game_Settings.Player_Faction).Careers
               (New_Game_Settings.Player_Career)
               .ShipIndex,
           Name => New_Game_Settings.Ship_Name,
           X => SkyBases(Random_Base).SkyX, Y => SkyBases(Random_Base).SkyY,
           Speed => DOCKED, Random_Upgrades => False);
      -- Add player to ship
      Add_Player_Block :
      declare
         Player_Index_2: constant Unbounded_String :=
           Factions_List(New_Game_Settings.Player_Faction).Careers
             (New_Game_Settings.Player_Career)
             .PlayerIndex;
         Amount: Positive := 1;
         Tmp_Inventory: Inventory_Container.Vector :=
           Inventory_Container.Empty_Vector;
         Player_Morale: constant Positive :=
           (if
              Factions_List(New_Game_Settings.Player_Faction).Flags.Contains
                (Item => To_Unbounded_String(Source => "nomorale"))
            then 50
            else 100);
      begin
         Player_Inventory_Loop :
         for I in ProtoMobs_List(Player_Index_2).Inventory.Iterate loop
            Amount :=
              (if ProtoMobs_List(Player_Index_2).Inventory(I).MaxAmount > 0
               then
                 GetRandom
                   (Min =>
                      ProtoMobs_List(Player_Index_2).Inventory(I).MinAmount,
                    Max =>
                      ProtoMobs_List(Player_Index_2).Inventory(I).MaxAmount)
               else ProtoMobs_List(Player_Index_2).Inventory(I).MinAmount);
            Tmp_Inventory.Append
              (New_Item =>
                 (ProtoIndex =>
                    ProtoMobs_List(Player_Index_2).Inventory(I).ProtoIndex,
                  Amount => Amount, Name => Null_Unbounded_String,
                  Durability => 100, Price => 0));
         end loop Player_Inventory_Loop;
         Player_Ship.Crew.Prepend
           (New_Item =>
              (Name => New_Game_Settings.Player_Name,
               Gender => New_Game_Settings.Player_Gender, Health => 100,
               Tired => 0, Skills => ProtoMobs_List(Player_Index_2).Skills,
               Hunger => 0, Thirst => 0,
               Order => ProtoMobs_List(Player_Index_2).Order,
               PreviousOrder => Rest, OrderTime => 15,
               Orders => ProtoMobs_List(Player_Index_2).Priorities,
               Attributes => ProtoMobs_List(Player_Index_2).Attributes,
               Inventory => Tmp_Inventory,
               Equipment => ProtoMobs_List(Player_Index_2).Equipment,
               Payment => (others => 0), ContractLength => -1,
               Morale => (1 => Player_Morale, 2 => 0), Loyalty => 100,
               HomeBase => Random_Base,
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
            if Modules_List(Module.Proto_Index).MType = CABIN and
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
      SkyBases(Random_Base).Visited := Game_Date;
      SkyBases(Random_Base).Known := True;
      SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited := True;
      GenerateRecruits;
      GenerateMissions;
      GenerateCargo;
      -- Set player goal if not set yet
      if CurrentGoal.GType = RANDOM then
         CurrentGoal :=
           Goals_List
             (GetRandom
                (Min => Goals_List.First_Index, Max => Goals_List.Last_Index));
      end if;
      -- Set name of savegame
      Generate_Save_Name;
      -- Set player career
      Player_Career := New_Game_Settings.Player_Career;
      -- Add welcoming message
      AddMessage
        (Message =>
           "Welcome to Steam Sky. If it is your first game, please consider read help (entry 'Help' in Menu), especially topic 'First Steps'.",
         MType => OtherMessage);
   end New_Game;

   procedure Update_Game(Minutes: Positive; In_Combat: Boolean := False) is
      use Bases.Ship;
      use Ships.Crew;
      use Ships.Upgrade;

      Added_Hours, Added_Minutes: Natural := 0;
      Base_Index: constant Extended_Base_Range :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Tired_Points: Natural := 0;
      Need_Cleaning: Boolean := False;
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
      Game_Date.Hour := Game_Date.Hour + Added_Hours;
      if Game_Date.Hour > 23 then
         Game_Date.Hour := Game_Date.Hour - 24;
         Game_Date.Day := Game_Date.Day + 1;
         Get_Dirty_Loop :
         for Module of Player_Ship.Modules loop
            if Module.M_Type = CABIN and then Module.Cleanliness > 0 then
               Module.Cleanliness := Module.Cleanliness - 1;
               Need_Cleaning := True;
            end if;
         end loop Get_Dirty_Loop;
         if Need_Cleaning then
            UpdateOrders(Ship => Player_Ship);
         end if;
         if Player_Ship.Speed = DOCKED then
            PayForDock;
         end if;
         DailyPayment;
         if Game_Settings.Auto_Save = DAILY then
            Save_Game;
         end if;
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
      UpdateCrew
        (Minutes => Minutes, TiredPoints => Tired_Points,
         InCombat => In_Combat);
      -- Repair ship (if needed)
      Ships.Repairs.RepairShip(Minutes => Minutes);
      -- Craft items
      Manufacturing(Minutes => Minutes);
      -- Upgrade ship module
      UpgradeShip(Minutes => Minutes);
      -- Update base
      if Base_Index > 0 then
         if SkyBases(Base_Index).Visited.Year = 0 then
            GameStats.BasesVisited := GameStats.BasesVisited + 1;
            GameStats.Points := GameStats.Points + 1;
            UpdateGoal
              (GType => VISIT, TargetIndex => SkyBases(Base_Index).Owner);
         end if;
         SkyBases(Base_Index).Visited := Game_Date;
         if not SkyBases(Base_Index).Known then
            SkyBases(Base_Index).Known := True;
            AddMessage
              (Message =>
                 "You discovered base " &
                 To_String(Source => SkyBases(Base_Index).Name) & ".",
               MType => OtherMessage);
         end if;
         UpdatePopulation;
         GenerateRecruits;
         GenerateMissions;
         GenerateCargo;
         UpdatePrices;
         UpdateOrders(Ship => Player_Ship);
      end if;
      -- Update map cell
      if not SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited then
         GameStats.MapVisited := GameStats.MapVisited + 1;
         GameStats.Points := GameStats.Points + 1;
         UpdateGoal(GType => DISCOVER, TargetIndex => Null_Unbounded_String);
         SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).Visited := True;
      end if;
      -- Update events
      UpdateEvents(Minutes => Minutes);
      -- Update accepted missions
      UpdateMissions(Minutes => Minutes);
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
      ClearMessages;
      Events_List.Clear;
      ClearGameStats;
      Known_Recipes.Clear;
      ClearCurrentGoal;
      AcceptedMissions.Clear;
      Save_Config;
   end End_Game;

   function Find_Skill_Index(Skill_Name: Unbounded_String) return Natural is
   begin
      Find_Skill_Loop :
      for I in Skills_List.Iterate loop
         if Skills_List(I).Name = Skill_Name then
            return SkillsData_Container.To_Index(Position => I);
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

               Game_Data: Document;
               Nodes_List, Child_Nodes: Node_List;
               Delete_Index: Natural := 0;
               Tmp_Skill: Skill_Record := Empty_Skill;
               Node_Name: Unbounded_String := Null_Unbounded_String;
               Data_Node: Node;
               Tool_Quality: constant Attributes_Container.Vector :=
                 Attributes_Container.Empty_Vector;
               function Find_Attribute_Index
                 (Attribute_Name: Unbounded_String) return Natural is
               begin
                  Find_Attribute_Loop :
                  for J in
                    Attributes_List.First_Index ..
                      Attributes_List.Last_Index loop
                     if Attributes_List(J).Name = Attribute_Name then
                        return J;
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
                     BaseSyllablesPre.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllablestart" then
                     BaseSyllablesStart.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllableend" then
                     BaseSyllablesEnd.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "basessyllablepost" then
                     BaseSyllablesPost.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllablestart" then
                     MaleSyllablesStart.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllablemiddle" then
                     MaleSyllablesMiddle.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "malessyllableend" then
                     MaleSyllablesEnd.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "malesvocal" then
                     MaleVocals.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "malesconsonant" then
                     MaleConsonants.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllablestart" then
                     FemaleSyllablesStart.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllablemiddle" then
                     FemaleSyllablesMiddle.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "femalessyllableend" then
                     FemaleSyllablesEnd.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "femalesvocal" then
                     FemaleVocals.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllablestart" then
                     Ship_Syllables_Start.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllablemiddle" then
                     Ship_Syllables_Middle.Append
                       (New_Item =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "shipssyllableend" then
                     Ship_Syllables_End.Append
                       (New_Item =>
                          To_Unbounded_String
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
                       To_Unbounded_String
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
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "tradersname" then
                     Traders_Name :=
                       To_Unbounded_String
                         (Source =>
                            Get_Attribute(Elem => Data_Node, Name => "value"));
                  elsif To_String(Source => Node_Name) = "attribute" then
                     Attributes_List.Append
                       (New_Item =>
                          (Name =>
                             To_Unbounded_String
                               (Source =>
                                  Get_Attribute
                                    (Elem => Data_Node, Name => "name")),
                           Description =>
                             To_Unbounded_String
                               (Source =>
                                  Node_Value
                                    (N => First_Child(N => Data_Node)))));
                  elsif To_String(Source => Node_Name) = "skill" then
                     Tmp_Skill :=
                       (Name =>
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "name")),
                        Attribute =>
                          Find_Attribute_Index
                            (Attribute_Name =>
                               To_Unbounded_String
                                 (Source =>
                                    Get_Attribute
                                      (Elem => Data_Node,
                                       Name => "attribute"))),
                        Description => Null_Unbounded_String,
                        Tool => Null_Unbounded_String,
                        Tools_Quality => Tool_Quality);
                     if Get_Attribute(Elem => Data_Node, Name => "tool") /=
                       "" then
                        Tmp_Skill.Tool :=
                          To_Unbounded_String
                            (Source =>
                               Get_Attribute
                                 (Elem => Data_Node, Name => "tool"));
                     end if;
                     Child_Nodes :=
                       DOM.Core.Elements.Get_Elements_By_Tag_Name
                         (Elem => Data_Node, Name => "toolquality");
                     if Length(List => Child_Nodes) > 0 then
                        Tmp_Skill.Tools_Quality.Clear;
                     end if;
                     Load_Skills_Loop :
                     for J in 0 .. Length(List => Child_Nodes) - 1 loop
                        Tmp_Skill.Tools_Quality.Append
                          (New_Item =>
                             (1 =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem =>
                                        Item(List => Child_Nodes, Index => J),
                                      Name => "level")),
                              2 =>
                                Integer'Value
                                  (Get_Attribute
                                     (Elem =>
                                        Item(List => Child_Nodes, Index => J),
                                      Name => "quality"))));
                     end loop Load_Skills_Loop;
                     if Tmp_Skill.Tools_Quality.Length = 0 then
                        Tmp_Skill.Tools_Quality.Append
                          (New_Item => (1 => 100, 2 => 100));
                     end if;
                     Child_Nodes :=
                       DOM.Core.Elements.Get_Elements_By_Tag_Name
                         (Elem => Data_Node, Name => "description");
                     if Length(List => Child_Nodes) > 0 then
                        Tmp_Skill.Description :=
                          To_Unbounded_String
                            (Source =>
                               Node_Value
                                 (N =>
                                    First_Child
                                      (N =>
                                         Item
                                           (List => Child_Nodes,
                                            Index => 0))));
                     end if;
                     Skills_List.Append(New_Item => Tmp_Skill);
                  elsif To_String(Source => Node_Name) = "conditionname" then
                     Condition_Index :=
                       Find_Attribute_Index
                         (Attribute_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "strengthname" then
                     Strength_Index :=
                       Find_Attribute_Index
                         (Attribute_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "pilotingskill" then
                     Piloting_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) =
                    "engineeringskill" then
                     Engineering_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "gunneryskill" then
                     Gunnery_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "talkingskill" then
                     Talking_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "perceptionskill" then
                     Perception_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
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
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "unarmedskill" then
                     Unarmed_Skill :=
                       Find_Skill_Index
                         (Skill_Name =>
                            To_Unbounded_String
                              (Source =>
                                 Get_Attribute
                                   (Elem => Data_Node, Name => "value")));
                  elsif To_String(Source => Node_Name) = "remove" then
                     if Get_Attribute(Elem => Data_Node, Name => "name") =
                       "skill" then
                        Delete_Index :=
                          Find_Skill_Index
                            (Skill_Name =>
                               To_Unbounded_String
                                 (Source =>
                                    Get_Attribute
                                      (Elem => Data_Node, Name => "value")));
                        if Delete_Index > 0 then
                           Skills_List.Delete(Index => Delete_Index);
                        end if;
                     elsif Get_Attribute(Elem => Data_Node, Name => "name") =
                       "attribute" then
                        Delete_Index :=
                          Find_Attribute_Index
                            (Attribute_Name =>
                               To_Unbounded_String
                                 (Source =>
                                    Get_Attribute
                                      (Elem => Data_Node, Name => "value")));
                        if Delete_Index > 0 then
                           Attributes_List.Delete(Index => Delete_Index);
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
                  LoadFactions(Reader => Reader);
               elsif To_String(Source => Data_Type) = "goals" then
                  LoadGoals(Reader => Reader);
               elsif To_String(Source => Data_Type) = "help" then
                  LoadHelp(Reader => Reader);
               elsif To_String(Source => Data_Type) = "items" then
                  LoadItems(Reader => Reader);
               elsif To_String(Source => Data_Type) = "mobiles" then
                  LoadMobs(Reader => Reader);
               elsif To_String(Source => Data_Type) = "recipes" then
                  LoadRecipes(Reader => Reader);
               elsif To_String(Source => Data_Type) = "bases" then
                  LoadBasesTypes(Reader => Reader);
               elsif To_String(Source => Data_Type) = "modules" then
                  LoadShipModules(Reader => Reader);
               elsif To_String(Source => Data_Type) = "ships" then
                  Load_Ships(Reader => Reader);
               elsif To_String(Source => Data_Type) = "stories" then
                  LoadStories(Reader => Reader);
               elsif To_String(Source => Data_Type) = "data" then
                  Load_Data(Current_Reader => Reader);
               elsif To_String(Source => Data_Type) = "careers" then
                  LoadCareers(Reader => Reader);
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
      SetToolsList;
      return "";
   exception
      when An_Exception : others =>
         Log_Message
           (Message => Exception_Message(X => An_Exception),
            Message_Type => EVERYTHING);
         return Exception_Message(X => An_Exception);
   end Load_Game_Data;

end Game;
