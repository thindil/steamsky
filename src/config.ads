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

   -- ****d* Config/Config.Default_Difficulty_Type
   -- FUNCTION
   -- Default difficulty level for the game
   -- SOURCE
   Default_Difficulty_Type: constant Difficulty_Type := NORMAL;
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

   -- ****d* Config/Config.Default_New_Game_Settings
   -- FUNCTION
   -- Default settings for the new game
   -- SOURCE
   Default_New_Game_Settings: constant New_Game_Record :=
     (Player_Name => To_Unbounded_String(Source => "Laeran"), Player_Gender => 'M',
      Ship_Name => To_Unbounded_String(Source => "Anaria"),
      Player_Faction => To_Unbounded_String(Source => "POLEIS"),
      Player_Career => To_Unbounded_String(Source => "general"),
      Starting_Base => To_Unbounded_String(Source => "Any"), Enemy_Damage_Bonus => 1.0,
      Player_Damage_Bonus => 1.0, Enemy_Melee_Damage_Bonus => 1.0,
      Player_Melee_Damage_Bonus => 1.0, Experience_Bonus => 1.0,
      Reputation_Bonus => 1.0, Upgrade_Cost_Bonus => 1.0, Prices_Bonus => 1.0,
      Difficulty_Level => Default_Difficulty_Type);
   -- ****

   -- ****t* Config/Config.Auto_Move_Break
   -- FUNCTION
   -- Options when stop auto move of player ship
   -- SOURCE
   type Auto_Move_Break is (NEVER, ANY, FRIENDLY, ENEMY);
   -- ****

   -- ****d* Config/Config.Default_Auto_Move_Stop
   -- FUNCTION
   -- Default setting for stop automovement of the player ship
   -- SOURCE
   Default_Auto_Move_Stop: constant Auto_Move_Break := NEVER;
   -- ****

   -- ****t* Config/Config.Messages_Order_Type
   -- FUNCTION
   -- Options to set showing messages order
   -- SOURCE
   type Messages_Order_Type is (OLDER_FIRST, NEWER_FIRST);
   -- ****

   -- ****d* Config/Config.Default_Messages_Order
   -- FUNCTION
   -- Default order of show the last messages
   -- SOURCE
   Default_Messages_Order: constant Messages_Order_Type := OLDER_FIRST;
   -- ****

   -- ****t* Config/Config.Auto_Save_Type
   -- FUNCTION
   -- Type used to set how often autosave is done
   -- SOURCE
   type Auto_Save_Type is (NONE, DOCK, UNDOCK, DAILY, MONTHLY, YEARLY);
   -- ****

   -- ****d* Config/Config.Default_Auto_Save_Time
   -- FUNCTION
   -- Default time when to auto save the game
   -- SOURCE
   Default_Auto_Save_Time: constant Auto_Save_Type := NONE;
   -- ****

   -- ****s* Config/Config.Game_Settings_Record
   -- FUNCTION
   -- Data for game settings
   -- PARAMETERS
   -- Auto_Rest                - If true, rest when pilot/engineer need rest
   -- Undock_Speed             - Default player ship speed after undock
   -- Auto_Center              - If true, back to ship after sets destination for
   --                            it
   -- Auto_Return              - If true, set base as destination for ship after
   --                            finished mission
   -- Auto_Finish              - If true, complete mission if ship is near
   --                            corresponding base
   -- Low_Fuel                 - Amount of fuel below which warning about low
   --                            level is show
   -- Low_Drinks               - Amount of drinkis below which warning about low
   --                            level is show
   -- Low_Food                 - Amount of food below which warning about low
   --                            level is show
   -- Auto_Move_Stop           - When stop automoving of player ship
   -- Window_Width             - Game window default width
   -- Window_Height            - Game window default height
   -- Messages_Limit           - Max amount of messages showed in game
   -- Saved_Messages           - Max amount fo messages saved to file
   -- Help_Font_Size           - Size of font used in help
   -- Map_Font_Size            - Size of font used in map
   -- Interface_Font_Size      - Size of font used in interface
   -- Interface_Theme          - Name of current user interface theme
   -- Messages_Order           - Order of showing messages
   -- Auto_Ask_For_Bases       - If true, auto ask for new bases when ship docked
   -- Auto_Ask_For_Events      - If true, auto ask for new events in bases when
   --                            ship docked
   -- Show_Tooltips            - If true, show tooltips to player
   -- Show_Last_Messages       - If true, show last messages window everywhere
   -- Messages_Position        - Height of last messages window in pixels from
   --                            bottom of the game window
   -- Full_Screen              - If true, set the game window in full screen mode
   -- Auto_Close_Messages_Time - Amount of seconds after which message box is
   --                            auto closed
   -- Auto_Save                - How often game is autosaved
   -- Topic_Position           - Position of help topics window in pixels from
   --                            top of the help window
   -- Show_Numbers             - If true, show numbers values instead of text for
   --                            various things (like weapon strength, etc)
   -- SOURCE
   type Game_Settings_Record is record
      Auto_Rest: Boolean;
      Undock_Speed: ShipSpeed;
      Auto_Center: Boolean;
      Auto_Return: Boolean;
      Auto_Finish: Boolean;
      Low_Fuel: Positive range 1 .. 10_000;
      Low_Drinks: Positive range 1 .. 10_000;
      Low_Food: Positive range 1 .. 10_000;
      Auto_Move_Stop: Auto_Move_Break;
      Window_Width: Positive;
      Window_Height: Positive;
      Messages_Limit: Positive range 10 .. 5_000;
      Saved_Messages: Positive range 5 .. 200;
      Help_Font_Size: Positive range 2 .. 51;
      Map_Font_Size: Positive range 2 .. 51;
      Interface_Font_Size: Positive range 2 .. 51;
      Interface_Theme: Unbounded_String;
      Messages_Order: Messages_Order_Type;
      Auto_Ask_For_Bases: Boolean;
      Auto_Ask_For_Events: Boolean;
      Show_Tooltips: Boolean;
      Show_Last_Messages: Boolean;
      Messages_Position: Natural;
      Full_Screen: Boolean;
      Auto_Close_Messages_Time: Positive range 1 .. 60;
      Auto_Save: Auto_Save_Type;
      Topics_Position: Natural;
      Show_Numbers: Boolean;
   end record;
   -- ****

   -- ****d* Config/Config.Default_Game_Settings
   -- FUNCTION
   -- Default setting for the game
   -- SOURCE
   Default_Game_Settings: constant Game_Settings_Record :=
     (Auto_Rest => True, Undock_Speed => FULL_SPEED, Auto_Center => True,
      Auto_Return => True, Auto_Finish => True, Low_Fuel => 100,
      Low_Drinks => 50, Low_Food => 25,
      Auto_Move_Stop => Default_Auto_Move_Stop, Window_Width => 800,
      Window_Height => 600, Messages_Limit => 500, Saved_Messages => 10,
      Help_Font_Size => 14, Map_Font_Size => 16, Interface_Font_Size => 14,
      Interface_Theme => To_Unbounded_String(Source => "steamsky"),
      Messages_Order => Default_Messages_Order, Auto_Ask_For_Bases => False,
      Auto_Ask_For_Events => False, Show_Tooltips => True,
      Show_Last_Messages => True, Messages_Position => 213,
      Full_Screen => False, Auto_Close_Messages_Time => 6,
      Auto_Save => Default_Auto_Save_Time, Topics_Position => 200,
      Show_Numbers => False);
   -- ****

   -- ****v* Config/Config.New_Game_Settings
   -- FUNCTION
   -- Settings for the new game
   -- SOURCE
   New_Game_Settings: New_Game_Record;
   -- ****

   -- ****v* Config/Config.Game_Settings
   -- FUNCTION
   -- General settings for the game
   -- SOURCE
   Game_Settings: Game_Settings_Record;
   -- ****

   -- ****f* Config/Config.Load_Config
   -- FUNCTION
   -- Load game configuration from file
   -- SOURCE
   procedure Load_Config;
   -- ****

   -- ****f* Config/Config.SaveConfig
   -- FUNCTION
   -- Save game configuration to file
   -- SOURCE
   procedure Save_Config;
   -- ****

end Config;
