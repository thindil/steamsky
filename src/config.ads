--    Copyright 2016-2020 Bartek thindil Jasicki
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

   -- ****s* Config/Config.NewGameRecord
   -- FUNCTION
   -- Data for new game settings
   -- PARAMETERS
   -- PlayerName             - Default player name
   -- PlayerGender           - Default player gender
   -- ShipName               - Default ship name
   -- PlayerFaction          - Default player faction index
   -- PlayerCareer           - Default player career index
   -- StartingBase           - Default starting base type
   -- EnemyDamageBonus       - Default bonus for enemy ship to damage
   -- PlayerDamageBonus      - Default bonus for player ship to damage
   -- EnemyMeleeDamageBonus  - Default bonus for enemy to damage in melee
   --                          combat
   -- PlayerMeleeDamageBonus - Default bonus for player and player's ship crew
   --                          to damage in melee combat
   -- ExperienceBonus        - Default bonus to gained experience
   -- ReputationBonus        - Default bonus to gained or lost reputation in
   --                          bases
   -- UpgradeCostBonus       - Default bonus to amount of materials needed for
   --                          player's ship upgrades.
   -- PricesBonus            - Default bonus to prices for services in bases
   -- DifficultyLevel        - Default the game difficulty level
   -- SOURCE
   type NewGameRecord is record
      PlayerName: Unbounded_String;
      PlayerGender: Character;
      ShipName: Unbounded_String;
      PlayerFaction: Unbounded_String;
      PlayerCareer: Unbounded_String;
      StartingBase: Unbounded_String;
      EnemyDamageBonus: Bonus_Type;
      PlayerDamageBonus: Bonus_Type;
      EnemyMeleeDamageBonus: Bonus_Type;
      PlayerMeleeDamageBonus: Bonus_Type;
      ExperienceBonus: Bonus_Type;
      ReputationBonus: Bonus_Type;
      UpgradeCostBonus: Bonus_Type;
      PricesBonus: Bonus_Type;
      DifficultyLevel: Difficulty_Type;
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
   NewGameSettings: NewGameRecord;
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
