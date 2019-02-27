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

package Config is

   type NewGameRecord is -- Data for new game settings
   record
      PlayerName: Unbounded_String; -- Default player name
      PlayerGender: Character; -- Default player gender
      ShipName: Unbounded_String; -- Default ship name
      PlayerFaction: Unbounded_String; -- Default player faction index
      PlayerCareer: Unbounded_String; -- Default player career index
      StartingBase: Unbounded_String; -- Default starting base type
   end record;
   type AutoMoveBreak is
     (NEVER, ANY, FRIENDLY,
      ENEMY); -- Options when stop auto move of player ship
   type MessagesOrderType is
     (OLDER_FIRST, NEWER_FIRST); -- Options to set showing messages order
   type GameSettingsRecord is -- Data for game settings
   record
      AutoRest: Boolean; -- If true, rest when pilot/engineer need rest
      UndockSpeed: ShipSpeed; -- Default player ship speed after undock
      AutoCenter: Boolean; -- If true, back to ship after sets destination for it
      AutoReturn: Boolean; -- If true, set base as destination for ship after finished mission
      AutoFinish: Boolean; -- If true, complete mission if ship is near corresponding base
      LowFuel: Positive; -- Amount of fuel below which warning about low level is show
      LowDrinks: Positive; -- Amount of drinkis below which warning about low level is show
      LowFood: Positive; -- Amount of food below which warning about low level is show
      AutoMoveStop: AutoMoveBreak; -- When stop automoving of player ship
      WindowWidth: Positive; -- Game window default width
      WindowHeight: Positive; -- Game window default height
      AnimationsEnabled: Natural; -- If 1 then UI animations are enabled, disabled when 0
      AnimationType: Positive; -- Type of animation used to move between views
      MessagesLimit: Positive; -- Max amount of messages showed in game
      SavedMessages: Positive; -- Max amount fo messages saved to file
      HelpFontSize: Positive; -- Size of font used in help
      MapFontSize: Positive; -- Size of font used in map
      InterfaceFontSize: Positive; -- Size of font used in interface
      InterfaceTheme: Unbounded_String; -- Name of current user interface theme
      MessagesOrder: MessagesOrderType; -- Order of showing messages
      AutoAskForBases: Boolean; -- If true, auto ask for new bases when ship docked
      AutoAskForEvents: Boolean; -- If true, auto ask for new events in bases when ship docked
      ShowTooltips: Boolean; -- If true, show tooltips to player
   end record;
   NewGameSettings: NewGameRecord;
   GameSettings: GameSettingsRecord;

   procedure LoadConfig; -- Load game configuration from file
   procedure SaveConfig; -- Save game configuration to file

end Config;
