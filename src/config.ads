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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ships; use Ships;

-- ****h* Config/Config
-- FUNCTION
-- Provide code for load and save the game configuration
-- SOURCE
package Config is
-- ****

   -- ****t* Config/Config.Bonus_Type
   -- FUNCTION
   -- Used to store the game difficulty settings
   -- SOURCE
   subtype Bonus_Type is Float range 0.0 .. 5.0;
   -- ****

   -- ****t* Config/Config.Difficulty_Type
   -- FUNCTION
   -- Used to set the game difficulty level
   -- SOURCE
   type Difficulty_Type is (VERY_EASY, EASY, NORMAL, HARD, VERY_HARD, CUSTOM);
   -- ****

   -- ****s* Config/Config.New_Game_Record
   -- FUNCTION
   -- Data for new game settings
   -- PARAMETERS
   -- Player_Name               - Default player name
   -- Player_Gender             - Default player gender
   -- Ship_Name                 - Default ship name
   -- Player_Faction            - Default player faction index
   -- Player_Career             - Default player career index
   -- Starting_Base             - Default starting base type
   -- Enemy_Damage_Bonus        - Default bonus for enemy ship to damage
   -- Player_Damage_Bonus       - Default bonus for player ship to damage
   -- Enemy_Melee_Damage_Bonus  - Default bonus for enemy to damage in melee
   --                             combat
   -- Player_Melee_Damage_Bonus - Default bonus for player and player's ship
   --                             crew to damage in melee combat
   -- Experience_Bonus          - Default bonus to gained experience
   -- Reputation_Bonus          - Default bonus to gained or lost reputation in
   --                             bases
   -- Upgrade_Cost_Bonus        - Default bonus to amount of materials needed for
   --                             player's ship upgrades.
   -- Prices_Bonus              - Default bonus to prices for services in bases
   -- Difficulty_Level          - Default the game difficulty level
   -- SOURCE
   type New_Game_Record is record
      Player_Name: Unbounded_String;
      Player_Gender: Character;
      Ship_Name: Unbounded_String;
      Player_Faction: Unbounded_String;
      Player_Career: Unbounded_String;
      Starting_Base: Unbounded_String;
      Enemy_Damage_Bonus: Bonus_Type;
      Player_Damage_Bonus: Bonus_Type;
      Enemy_Melee_Damage_Bonus: Bonus_Type;
      Player_Melee_Damage_Bonus: Bonus_Type;
      Experience_Bonus: Bonus_Type;
      Reputation_Bonus: Bonus_Type;
      Upgrade_Cost_Bonus: Bonus_Type;
      Prices_Bonus: Bonus_Type;
      Difficulty_Level: Difficulty_Type;
   end record;
   -- ****

   -- ****t* Config/Config.AutoMoveBreak
   -- FUNCTION
   -- Options when stop auto move of player ship
   -- SOURCE
   type AutoMoveBreak is (NEVER, ANY, FRIENDLY, ENEMY);
   -- ****

   -- ****t* Config/Config.MessagesOrderType
   -- FUNCTION
   -- Options to set showing messages order
   -- SOURCE
   type MessagesOrderType is (OLDER_FIRST, NEWER_FIRST);
   -- ****

   -- ****t* Config/Config.AutoSaveType
   -- FUNCTION
   -- Type used to set how often autosave is done
   -- SOURCE
   type AutoSaveType is (NONE, DOCK, UNDOCK, DAILY, MONTHLY, YEARLY);
   -- ****

   -- ****s* Config/Config.GameSettingsRecord
   -- FUNCTION
   -- Data for game settings
   -- PARAMETERS
   -- AutoRest              - If true, rest when pilot/engineer need rest
   -- UndockSpeed           - Default player ship speed after undock
   -- AutoCenter            - If true, back to ship after sets destination for
   --                         it
   -- AutoReturn            - If true, set base as destination for ship after
   --                         finished mission
   -- AutoFinish            - If true, complete mission if ship is near
   --                         corresponding base
   -- LowFuel               - Amount of fuel below which warning about low
   --                         level is show
   -- LowDrinks             - Amount of drinkis below which warning about low
   --                         level is show
   -- LowFood               - Amount of food below which warning about low
   --                         level is show
   -- AutoMoveStop          - When stop automoving of player ship
   -- WindowWidth           - Game window default width
   -- WindowHeight          - Game window default height
   -- MessagesLimit         - Max amount of messages showed in game
   -- SavedMessages         - Max amount fo messages saved to file
   -- HelpFontSize          - Size of font used in help
   -- MapFontSize           - Size of font used in map
   -- InterfaceFontSize     - Size of font used in interface
   -- InterfaceTheme        - Name of current user interface theme
   -- MessagesOrder         - Order of showing messages
   -- AutoAskForBases       - If true, auto ask for new bases when ship docked
   -- AutoAskForEvents      - If true, auto ask for new events in bases when
   --                         ship docked
   -- ShowTooltips          - If true, show tooltips to player
   -- ShowLastMessages      - If true, show last messages window everywhere
   -- MessagesPosition      - Height of last messages window in pixels from
   --                         bottom of the game window
   -- FullScreen            - If true, set the game window in full screen mode
   -- AutoCloseMessagesTime - Amount of seconds after which message box is
   --                         auto closed
   -- AutoSave              - How often game is autosaved
   -- TopicPosition         - Position of help topics window in pixels from
   --                         top of the help window
   -- ShowNumbers           - If true, show numbers values instead of text for
   --                         various things (like weapon strength, etc)
   -- SOURCE
   type GameSettingsRecord is record
      AutoRest: Boolean;
      UndockSpeed: ShipSpeed;
      AutoCenter: Boolean;
      AutoReturn: Boolean;
      AutoFinish: Boolean;
      LowFuel: Positive range 1 .. 10_000;
      LowDrinks: Positive range 1 .. 10_000;
      LowFood: Positive range 1 .. 10_000;
      AutoMoveStop: AutoMoveBreak;
      WindowWidth: Positive;
      WindowHeight: Positive;
      MessagesLimit: Positive range 10 .. 5_000;
      SavedMessages: Positive range 5 .. 200;
      HelpFontSize: Positive range 2 .. 51;
      MapFontSize: Positive range 2 .. 51;
      InterfaceFontSize: Positive range 2 .. 51;
      InterfaceTheme: Unbounded_String;
      MessagesOrder: MessagesOrderType;
      AutoAskForBases: Boolean;
      AutoAskForEvents: Boolean;
      ShowTooltips: Boolean;
      ShowLastMessages: Boolean;
      MessagesPosition: Natural;
      FullScreen: Boolean;
      AutoCloseMessagesTime: Positive range 1 .. 60;
      AutoSave: AutoSaveType;
      TopicsPosition: Natural;
      ShowNumbers: Boolean;
   end record;
   -- ****

   -- ****v* Config/Config.NewGameSettings
   -- FUNCTION
   -- Settings for the new game
   -- SOURCE
   NewGameSettings: New_Game_Record;
   -- ****

   -- ****v* Config/Config.GameSettings
   -- FUNCTION
   -- General settings for the game
   -- SOURCE
   GameSettings: GameSettingsRecord;
   -- ****

   -- ****f* Config/Config.LoadConfig
   -- FUNCTION
   -- Load game configuration from file
   -- SOURCE
   procedure LoadConfig;
   -- ****

   -- ****f* Config/Config.SaveConfig
   -- FUNCTION
   -- Save game configuration to file
   -- SOURCE
   procedure SaveConfig;
   -- ****

end Config;
