--    Copyright 2016-2019 Bartek thindil Jasicki
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

-- ****h* Steamsky/Config
-- FUNCTION
-- Provide code for load and save the game configuration
-- SOURCE
package Config is
-- ****

   -- ****t* Config/NewGameRecord
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
   -- SOURCE
   type NewGameRecord is record
      PlayerName: Unbounded_String;
      PlayerGender: Character;
      ShipName: Unbounded_String;
      PlayerFaction: Unbounded_String;
      PlayerCareer: Unbounded_String;
      StartingBase: Unbounded_String;
      EnemyDamageBonus: Float;
      PlayerDamageBonus: Float;
      EnemyMeleeDamageBonus: Float;
      PlayerMeleeDamageBonus: Float;
      ExperienceBonus: Float;
      ReputationBonus: Float;
      UpgradeCostBonus: Float;
      PricesBonus: Float;
   end record;
   -- ****

   -- ****t* Config/AutoMoveBreak
   -- FUNCTION
   -- Options when stop auto move of player ship
   -- SOURCE
   type AutoMoveBreak is (NEVER, ANY, FRIENDLY, ENEMY);
   -- ****

   -- ****t* Config/MessagesOrderType
   -- FUNCTION
   -- Options to set showing messages order
   -- SOURCE
   type MessagesOrderType is (OLDER_FIRST, NEWER_FIRST);
   -- ****

   -- ****t* Config/AutoSaveType
   -- FUNCTION
   -- Type used to set how often autosave is done
   -- SOURCE
   type AutoSaveType is (NONE, DOCK, UNDOCK, DAILY, MONTHLY, YEARLY);
   -- ****

   -- ****t* Config/GameSettingsRecord
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
   -- AnimationsEnabled     - If 1 then UI animations are enabled, disabled
   --                         when 0
   -- AnimationType         - Type of animation used to move between views
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
   -- MessagesPosition      - Position of last messages window in pixels from
   --                         top of the game window
   -- FullScreen            - If true, set the game window in full screen mode
   -- AutoCloseMessagesTime - Amount of seconds after which message box is
   --                         auto closed
   -- AutoSave              - How often game is autosaved
   -- TopicPosition         - Position of help topics window in pixels from
   --                         top of the help window
   -- ShowBaseInfo          - If true, show detailed information about
   --                         selected base in bases list
   -- ShowCargoInfo         - If true, show detailed information about
   --                         selected item in player ship cargo
   -- ShowInventoryInfo     - If true, show detailed information about
   --                         selected item in crew member inventory
   -- ShowNumbers           - If true, show numbers values instead of text for
   --                         various things (like weapon strength, etc)
   -- SOURCE
   type GameSettingsRecord is record
      AutoRest: Boolean;
      UndockSpeed: ShipSpeed;
      AutoCenter: Boolean;
      AutoReturn: Boolean;
      AutoFinish: Boolean;
      LowFuel: Positive;
      LowDrinks: Positive;
      LowFood: Positive;
      AutoMoveStop: AutoMoveBreak;
      WindowWidth: Positive;
      WindowHeight: Positive;
      AnimationsEnabled: Natural;
      AnimationType: Positive;
      MessagesLimit: Positive;
      SavedMessages: Positive;
      HelpFontSize: Positive;
      MapFontSize: Positive;
      InterfaceFontSize: Positive;
      InterfaceTheme: Unbounded_String;
      MessagesOrder: MessagesOrderType;
      AutoAskForBases: Boolean;
      AutoAskForEvents: Boolean;
      ShowTooltips: Boolean;
      ShowLastMessages: Boolean;
      MessagesPosition: Natural;
      FullScreen: Boolean;
      AutoCloseMessagesTime: Positive;
      AutoSave: AutoSaveType;
      TopicsPosition: Natural;
      ShowBaseInfo: Boolean;
      ShowCargoInfo: Boolean;
      ShowInventoryInfo: Boolean;
      ShowNumbers: Boolean;
   end record;
   -- ****

   -- ****v* Config/NewGameSettings
   -- FUNCTION
   -- Settings for the new game
   -- SOURCE
   NewGameSettings: NewGameRecord;
   -- ****

   -- ****v* Config/GameSettings
   -- FUNCTION
   -- General settings for the game
   -- SOURCE
   GameSettings: GameSettingsRecord;
   -- ****

   -- ****f* Config/LoadConfig
   -- FUNCTION
   -- Load game configuration from file
   -- SOURCE
   procedure LoadConfig;
   -- ****

   -- ****f* Config/SaveConfig
   -- FUNCTION
   -- Save game configuration to file
   -- SOURCE
   procedure SaveConfig;
   -- ****

end Config;
