--    Copyright 2016-2018 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package Game is

   type GameStates is
     (Quit,
      Main_Menu,
      Sky_Map_View,
      Control_Speed,
      Ship_Info,
      Crew_Info,
      Giving_Orders,
      Messages_View,
      Trade_View,
      Help_View,
      Quit_Confirm,
      New_Game,
      Combat_State,
      Combat_Orders,
      Craft_View,
      License_Info,
      License_Full,
      Wait_Order,
      News_View,
      Cargo_Info,
      Help_Topic,
      Repairs_View,
      Clear_Confirm,
      Module_Options,
      Shipyard_View,
      Recruits_View,
      Dismiss_Confirm,
      Rename_Module,
      Drop_Cargo,
      Trade_Form,
      Rename_Ship,
      Assign_Owner,
      Recipe_Setting,
      Move_Map,
      Bases_List,
      Events_View,
      Assign_Ammo,
      ShipyardTypesMenu,
      GameMenu,
      GameStats_View,
      Death_Confirm,
      TradeRecipes_View,
      BaseMissions_View,
      Missions_View,
      Orders_For_All,
      Enemy_Info,
      Orders_Priorities,
      BasesList_Types,
      BasesList_Statuses,
      BasesList_Owners,
      WaitX_Order,
      PilotRest_Confirm,
      EngineerRest_Confirm,
      GameOptions_View,
      Heal_View,
      GoalsList_View,
      GoalsTypes_View,
      Resign_Confirm,
      DetailedStats_View,
      Hall_Of_Fame,
      Loot_View,
      Loot_Form,
      Small_Terminal,
      ChangeHome_Confirm,
      Inventory_View,
      MoveItem_Form,
      Cargo_Menu,
      CargoMove_Form,
      School_View,
      SchoolSkills_Menu,
      Inventory_Menu,
      Boarding_Menu); -- Game states
   type Date_Record is -- Data for game date/time
   record
      Year: Natural;
      Month: Natural;
      Day: Natural;
      Hour: Natural;
      Minutes: Natural;
   end record;
   GameDate: Date_Record;
   GameVersion: constant String := "Version: 2.0.10";
   package UnboundedString_Container is new Vectors
     (Positive,
      Unbounded_String);
   package Positive_Container is new Vectors(Positive, Positive);
   package Natural_Container is new Vectors(Positive, Natural);
   type Skill_Record is -- Data for skills
   record
      Name: Unbounded_String; -- Name of skill
      Attribute: Positive; -- Attribute used with that skill
   end record;
   package SkillsData_Container is new Vectors(Positive, Skill_Record);
   Skills_List: SkillsData_Container.Vector; -- Contains data for all skills
   RepairTools: Unbounded_String; -- Name of item type used as tool in repairing/upgrading ship
   CleaningTools: Unbounded_String; -- Name of item type used as tool in cleaning ship
   HealingTools: Unbounded_String; -- Name of item type used as tool in healing crew members
   PlayerShipIndex: Unbounded_String; -- Index of ship prototype used as player ship
   AlchemyTools: Unbounded_String; -- Name of item type used as alchemy tools (mainly in deconstructing orders)
   DrinksType: Unbounded_String; -- Name of item type used as drinks
   CorpseIndex: Unbounded_String; -- Index of item used to create mobs corpses
   MissionItemsType: Unbounded_String; -- Name of item type used for delivery missions
   FoodTypes: UnboundedString_Container.Vector; -- Contains food types names
   FuelType: Unbounded_String; -- Name of item type used as fuel for ships
   MoneyIndex: Unbounded_String; -- Index of item used as money
   MoneyName: Unbounded_String; -- Name of money (taken from MoneyIndex)
   SaveDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" &
        Dir_Separator); -- Path to directory where are savegame and logs
   DataDirectory: Unbounded_String :=
     To_Unbounded_String
       ("data" & Dir_Separator); -- Path to directory where are game data files
   TradersName: Unbounded_String; -- Word used in ships names for traders ships (for events)
   Attributes_Names: UnboundedString_Container
     .Vector; -- Names of characters attributes
   ConditionIndex: Positive; -- Index of attribute used as bonus to character condition
   StrengthIndex: Positive; -- Index of attribute used to count max character encumbrance
   HealingSkill: Positive; -- Index of skill used to heal wounded crew members
   PilotingSkill: Positive; -- Index of skill used to piloting ship
   EngineeringSkill: Positive; -- Index of skill used by engineer on ship
   GunnerySkill: Positive; -- Index of skill used by gunners
   TalkingSkill: Positive; -- Index of skill used for talk in bases or with other ships
   PerceptionSkill: Positive; -- Index of skill used for spoting
   DodgeSkill: Positive; -- Index of skill used for dodge in character's combat
   UnarmedSkill: Positive; -- Index of skill used for unarmed attacks in character's combat
   HeadArmor: Unbounded_String; -- Name of item type used as characters head armor
   ChestArmor: Unbounded_String; -- Name of item type used as characters torso armor
   ArmsArmor: Unbounded_String; -- Name of item type used as characters arms armor
   LegsArmor: Unbounded_String; -- Name of item type used as characters legs armor
   ShieldType: Unbounded_String; -- Name of item type used as characters shield
   WeaponType: Unbounded_String; -- Name of item type used as characters weapon

   procedure NewGame
     (CharName, ShipName: Unbounded_String;
      Gender: Character); -- Start new game: create map, place ship, crew, etc
   procedure UpdateGame
     (Minutes: Positive); -- Game ticks (update time, crew, ship, etc)
   function LoadData
     return Boolean; -- Load game data from file, return false if file not found
   function DaysDifference
     (DateToCompare: Date_Record)
     return Natural; -- Return days difference between selected date and current game date
   procedure EndGame
     (Save: Boolean); -- Save (or not) game and clear all temporary data
   function FindSkillIndex
     (SkillName: Unbounded_String)
     return Positive; -- Return vector index of selected skill

end Game;
