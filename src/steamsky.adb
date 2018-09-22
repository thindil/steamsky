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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses_Constants;
use Terminal_Interface.Curses_Constants;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with UserInterface.Keys; use UserInterface.Keys;
with Maps.UI; use Maps.UI;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Messages.UI; use Messages.UI;
with Crew; use Crew;
with Crew.UI.Keys; use Crew.UI.Keys;
with Ships; use Ships;
with Ships.UI; use Ships.UI;
with Ships.UI.Cargo; use Ships.UI.Cargo;
with Ships.UI.Ship.Keys; use Ships.UI.Ship.Keys;
with Bases.UI.Repair; use Bases.UI.Repair;
with Bases.UI.Shipyard; use Bases.UI.Shipyard;
with Bases.UI.Recruits; use Bases.UI.Recruits;
with Bases.UI.Recipes; use Bases.UI.Recipes;
with Bases.UI.Missions; use Bases.UI.Missions;
with Bases.UI.Heal; use Bases.UI.Heal;
with Bases.UI.Loot; use Bases.UI.Loot;
with Bases.UI.School; use Bases.UI.School;
with Events; use Events;
with Events.UI; use Events.UI;
with Combat.UI; use Combat.UI;
with Crafts.UI; use Crafts.UI;
with Help.UI; use Help.UI;
with MainMenu; use MainMenu;
with BasesList; use BasesList;
with Config; use Config;
with Statistics.UI; use Statistics.UI;
with Missions; use Missions;
with Missions.UI; use Missions.UI;
with Log; use Log;
with GameOptions; use GameOptions;
with Goals.UI; use Goals.UI;
with HallOfFame; use HallOfFame;
with Trades.UI.Keys; use Trades.UI.Keys;
with Utils.UI; use Utils.UI;

procedure SteamSky is
   GameState, OldState: GameStates := Main_Menu;
   Key: Key_Code;
   Result: Integer;
   ErrorFile: File_Type;
   Visibility: Cursor_Visibility := Invisible;
   Message: Unbounded_String;
   procedure ErrorInfo(Message: String) is
   begin
      Move_Cursor(Line => (Lines / 2), Column => 2);
      Add(Str => Message);
      Key := Get_Keystroke;
      End_Windows;
      EndLogging;
   end ErrorInfo;
begin
   Set_Directory(Dir_Name(Command_Name));
   Setenv("ESCDELAY", "10");
   Setenv("TERMINFO", "terminfo");
   -- Initiate ncurses
   Init_Screen;
   Start_Color;
   Set_Timeout_Mode(Standard_Window, Blocking, 0);
   Init_Pair(1, COLOR_YELLOW, COLOR_BLACK);
   Init_Pair(2, COLOR_GREEN, COLOR_BLACK);
   Init_Pair(3, COLOR_RED, COLOR_BLACK);
   Init_Pair(4, COLOR_BLUE, COLOR_BLACK);
   Init_Pair(5, COLOR_CYAN, COLOR_BLACK);
   begin
      Init_Color(8, 80, 80, 80);
      Init_Pair(6, 8, 8);
      Init_Pair(7, COLOR_WHITE, 8);
   exception
      when CURSES_EXCEPTION =>
         Init_Pair(6, COLOR_BLACK, COLOR_BLACK);
         Init_Pair(7, COLOR_WHITE, COLOR_BLACK);
   end;
   Init_Pair(8, COLOR_WHITE, COLOR_RED);
   Init_Pair(9, COLOR_WHITE, COLOR_YELLOW);
   Init_Pair(10, COLOR_BLACK, COLOR_WHITE);
   Init_Pair(11, COLOR_YELLOW, COLOR_BLUE);
   Set_KeyPad_Mode(SwitchOn => True);

   if Columns < 60 or Lines < 24 then
      ErrorInfo
        ("Your terminal size is too small for game. Minimal size is 60x24. Press any key, to exit from game.");
      return;
   end if;

   -- Set default attributes for menus
   Set_Foreground(Null_Menu, BoldCharacters, 11);
   Set_Mark(Null_Menu, "");

   -- Command line arguments
   for I in 1 .. Argument_Count loop
      if Argument(I)'Length > 8 then
         if Argument(I)(1 .. 8) = "--debug=" then
            for J in Debug_Types loop
               if To_Upper(Argument(I)(9 .. (Argument(I)'Last))) =
                 Debug_Types'Image(J) then
                  DebugMode := J;
                  exit;
               end if;
            end loop;
            StartLogging;
         elsif Argument(I)(1 .. 8) = "--datadi" then
            DataDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if Element(DataDirectory, Length(DataDirectory)) /=
              Dir_Separator then
               Append(DataDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Data directory sets to: " & To_String(DataDirectory),
               Everything);
            if not Exists(To_String(DataDirectory)) then
               ErrorInfo
                 ("Directory " &
                  To_String(DataDirectory) &
                  " not exists. You must use existing directory as data directory.");
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--savedi" then
            SaveDirectory :=
              To_Unbounded_String(Argument(I)(11 .. (Argument(I)'Last)));
            if Element(SaveDirectory, Length(SaveDirectory)) /=
              Dir_Separator then
               Append(SaveDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Save directory sets to: " & To_String(SaveDirectory),
               Everything);
            if not Exists(To_String(SaveDirectory)) then
               ErrorInfo
                 ("Directory " &
                  To_String(SaveDirectory) &
                  " not exists. You must use existing directory as save directory.");
               return;
            end if;
         elsif Argument(I)(1 .. 8) = "--docdir" then
            DocDirectory :=
              To_Unbounded_String(Argument(I)(10 .. (Argument(I)'Last)));
            if Element(DocDirectory, Length(DocDirectory)) /=
              Dir_Separator then
               Append(DocDirectory, Dir_Separator);
            end if;
            LogMessage
              ("Documentation directory sets to: " & To_String(DocDirectory),
               Everything);
            if not Exists(To_String(DocDirectory)) then
               ErrorInfo
                 ("Directory " &
                  To_String(DocDirectory) &
                  " not exists. You must use existing directory as documentation directory.");
               return;
            end if;
         end if;
      end if;
   end loop;

   if not LoadData then
      ErrorInfo
        ("Can't load game data. Probably missing file " &
         To_String(DataDirectory) &
         "game.dat");
      return;
   end if;

   LoadConfig;
   LoadHallOfFame;
   ShowMainMenu;

   while GameState /= Quit loop
      Key := Get_Keystroke;
      while Key = Terminal_Interface.Curses.Key_Resize loop
         Erase;
         if Columns < 60 or Lines < 24 then
            Move_Cursor(Line => (Lines / 2), Column => 2);
            Add
              (Str =>
                 "Your terminal size is too small for game. Minimal size is 60x24. Please resize it again.");
            Refresh;
            if GameState /= Small_Terminal then
               OldState := GameState;
               GameState := Small_Terminal;
            end if;
         else
            if GameState = Small_Terminal then
               GameState := OldState;
            end if;
            Visibility := Invisible;
            case GameState is
               when Main_Menu =>
                  ShowMainMenu;
               when New_Game |
                 License_Info |
                 License_Full |
                 News_View |
                 Hall_Of_Fame =>
                  RedrawMainMenu(GameState);
               when Trade_Form =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Trade_View;
                  DrawGame(Trade_View);
               when BasesList_Types | BasesList_Statuses | BasesList_Owners =>
                  GameState := Bases_List;
                  DrawGame(Bases_List);
               when ShipyardTypesMenu =>
                  GameState := Shipyard_View;
                  DrawGame(Shipyard_View);
               when Combat_Orders | Enemy_Info | Boarding_Menu =>
                  GameState := Combat_State;
                  DrawGame(Combat_State);
               when Recipe_Setting =>
                  GameState := Craft_View;
                  DrawGame(Craft_View);
               when Drop_Cargo | Cargo_Menu =>
                  GameState := Cargo_Info;
                  DrawGame(Cargo_Info);
               when Rename_Module |
                 Assign_Ammo |
                 Module_Options |
                 Assign_Owner |
                 Rename_Ship =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Ship_Info;
                  DrawGame(Ship_Info);
               when GoalsList_View | GoalsTypes_View =>
                  GameState := OldState;
                  if GameState = GameStats_View then
                     DrawGame(GameState);
                  else
                     RedrawMainMenu(GameState);
                  end if;
               when Giving_Orders | Orders_For_All | Orders_Priorities =>
                  GameState := Crew_Info;
                  DrawGame(Crew_Info);
               when MoveItem_Form =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Inventory_View;
                  DrawGame(GameState);
               when CargoMove_Form =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Cargo_Info;
                  DrawGame(GameState);
               when Move_Map =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Sky_Map_View;
                  DrawGame(GameState);
               when Loot_Form =>
                  Set_Cursor_Visibility(Visibility);
                  GameState := Loot_View;
                  DrawGame(GameState);
               when SchoolSkills_Menu =>
                  GameState := School_View;
                  DrawGame(GameState);
               when Inventory_Menu =>
                  GameState := Inventory_View;
                  DrawGame(GameState);
               when GameMenu | Control_Speed | Wait_Order =>
                  GameState := Sky_Map_View;
                  DrawGame(GameState);
               when Messages_View =>
                  DeleteMessagesPad;
                  DrawGame(GameState);
               when others =>
                  DrawGame(GameState);
            end case;
         end if;
         Key := Get_Keystroke;
      end loop;
      if GameState /= Main_Menu and
        GameState /= New_Game and
        GameState /= License_Info and
        GameState /= License_Full and
        GameState /= News_View and
        GameState /= GameStats_View and
        GameState /= GoalsList_View and
        GameState /= GoalsTypes_View and
        GameState /= Hall_Of_Fame and
        GameState /= Small_Terminal then
         if PlayerShip.Crew.Element(1).Health = 0 then -- Player is dead
            GameState := Death_Confirm;
            DrawGame(Death_Confirm);
         end if;
      end if;
      if HideDialog then
         Key := Get_Keystroke;
      end if;
      case GameState is
         when Main_Menu =>
            GameState := MainMenuKeys(Key);
         when Sky_Map_View =>
            Result := SkyMapKeys(Key);
            OldState := GameState;
            case Result is
               when 0 =>
                  GameState := GameMenuKeys(GameState, Key);
               when 1 =>
                  GameState := CheckForEvent(GameState);
                  if GameState = Sky_Map_View and GameSettings.AutoFinish then
                     Message := To_Unbounded_String(AutoFinishMissions);
                     if Length(Message) > 0 then
                        ShowDialog(To_String(Message));
                     end if;
                  end if;
                  DrawGame(GameState);
               when 5 =>
                  GameState := Combat_State;
                  DrawGame(GameState);
               when 6 =>
                  GameState := PilotRest_Confirm;
                  DrawGame(GameState);
               when 7 =>
                  GameState := EngineerRest_Confirm;
                  DrawGame(GameState);
               when 8 =>
                  GameState := CheckForEvent(GameState);
                  if GameState = Sky_Map_View then
                     WaitForRest;
                     GameState := CheckForEvent(GameState);
                  end if;
                  if GameState = Sky_Map_View and GameSettings.AutoFinish then
                     Message := To_Unbounded_String(AutoFinishMissions);
                     if Length(Message) > 0 then
                        ShowDialog(To_String(Message));
                     end if;
                  end if;
                  DrawGame(GameState);
               when others =>
                  DrawGame(GameState);
            end case;
         when Control_Speed =>
            GameState := OrdersMenuKeys(OldState, Key);
         when Ship_Info =>
            GameState := ShipInfoKeys(Key, OldState);
         when Crew_Info =>
            GameState := CrewInfoKeys(Key, OldState);
         when Giving_Orders =>
            GameState := CrewOrdersKeys(Key);
         when Messages_View =>
            GameState := MessagesKeys(Key, OldState);
         when Trade_View =>
            GameState := TradeKeys(Key);
         when Help_View =>
            GameState := HelpMenuKeys(Key);
         when Quit_Confirm |
           Clear_Confirm |
           Dismiss_Confirm |
           Death_Confirm |
           PilotRest_Confirm |
           EngineerRest_Confirm |
           Resign_Confirm |
           ChangeHome_Confirm =>
            GameState := ConfirmKeys(GameState, Key);
         when New_Game =>
            OldState := GameState;
            GameState := NewGameKeys(Key);
         when Combat_State =>
            OldState := GameState;
            GameState := CombatKeys(Key);
         when Combat_Orders =>
            GameState := CombatOrdersKeys(Key);
         when Craft_View =>
            GameState := CraftKeys(Key);
         when License_Info =>
            GameState := LicenseKeys(Key);
         when License_Full =>
            GameState := FullLicenseKeys(Key);
         when Wait_Order =>
            GameState := WaitMenuKeys(OldState, Key);
         when News_View =>
            GameState := NewsKeys(Key);
         when Cargo_Info =>
            GameState := CargoInfoKeys(Key, OldState);
         when Help_Topic =>
            GameState := HelpKeys(Key);
         when Repairs_View =>
            GameState := RepairKeys(Key);
         when Module_Options =>
            GameState := ModuleOptionsKeys(Key);
         when Shipyard_View =>
            GameState := ShipyardKeys(Key);
         when Recruits_View =>
            GameState := RecruitKeys(Key);
         when Rename_Module | Drop_Cargo | Rename_Ship =>
            GameState := ShipFormKeys(Key, GameState);
         when Trade_Form =>
            GameState := TradeFormKeys(Key);
         when Assign_Owner =>
            GameState := AssignOwnerKeys(Key);
         when Move_Map =>
            GameState := MoveFormKeys(Key);
         when Bases_List =>
            GameState := BasesListKeys(Key);
         when Events_View =>
            GameState := ShowEventsKeys(Key);
         when Assign_Ammo =>
            GameState := AssignAmmoKeys(Key);
         when ShipyardTypesMenu =>
            GameState := ShipyardTypesKeys(Key);
         when GameMenu =>
            GameState := GameMenuKeys(GameState, Key);
         when GameStats_View =>
            OldState := GameState;
            GameState := ShowGameStatsKeys(Key);
         when TradeRecipes_View =>
            GameState := TradeRecipesKeys(Key);
         when BaseMissions_View =>
            GameState := BaseMissionsKeys(Key);
         when Missions_View =>
            GameState := ShowMissionsKeys(Key);
         when Orders_For_All =>
            GameState := CrewOrdersAllKeys(Key);
         when Enemy_Info =>
            GameState := EnemyInfoKeys(Key);
         when Orders_Priorities =>
            GameState := OrdersPrioritiesKeys(Key);
         when BasesList_Types | BasesList_Statuses | BasesList_Owners =>
            GameState := BasesOptionsKeys(Key, GameState);
         when WaitX_Order =>
            GameState := WaitFormKeys(Key);
         when GameOptions_View =>
            GameState := GameOptionsKeys(Key);
         when Heal_View =>
            GameState := HealKeys(Key);
         when GoalsList_View | GoalsTypes_View =>
            GameState := GoalsMenuKeys(Key, GameState, OldState);
         when DetailedStats_View =>
            GameState := DetailedStatsKeys(Key);
         when Hall_Of_Fame =>
            GameState := HallOfFameKeys(Key);
         when Loot_View =>
            GameState := LootKeys(Key);
         when Loot_Form =>
            GameState := LootFormKeys(Key);
         when Recipe_Setting =>
            GameState := RecipeFormKeys(Key);
         when Small_Terminal =>
            Erase;
            Move_Cursor(Line => (Lines / 2), Column => 2);
            Add
              (Str =>
                 "Your terminal size is too small for game. Minimal size is 60x24. Please resize it again.");
            Refresh;
         when Inventory_View =>
            GameState := InventoryKeys(Key);
         when MoveItem_Form =>
            GameState := MoveItemFormKeys(Key);
         when Cargo_Menu =>
            GameState := CargoMenuKeys(Key);
         when CargoMove_Form =>
            GameState := CargoMoveFormKeys(Key);
         when School_View =>
            GameState := SchoolKeys(Key);
         when SchoolSkills_Menu =>
            GameState := SchoolSkillsMenuKeys(Key);
         when Inventory_Menu =>
            GameState := InventoryMenuKeys(Key);
         when Boarding_Menu =>
            GameState := BoardingMenuKeys(Key);
         when others =>
            GameState := GameMenuKeys(GameState, Key);
      end case;
   end loop;

   End_Windows;
   EndLogging;
exception
   when An_Exception : others =>
      if GameState /= Main_Menu and
        GameState /= New_Game and
        GameState /= License_Info and
        GameState /= License_Full and
        GameState /= News_View and
        GameState /= Hall_Of_Fame and
        GameState /= Small_Terminal then
         SaveGame;
      end if;
      if Exists(To_String(SaveDirectory) & "error.log") then
         Open(ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
      else
         Create
           (ErrorFile,
            Append_File,
            To_String(SaveDirectory) & "error.log");
      end if;
      Put_Line(ErrorFile, Ada.Calendar.Formatting.Image(Clock));
      Put_Line(ErrorFile, GameVersion);
      Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
      Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
      Put_Line(ErrorFile, "-------------------------------------------------");
      Put_Line(ErrorFile, Symbolic_Traceback(An_Exception));
      Put_Line(ErrorFile, "-------------------------------------------------");
      Close(ErrorFile);
      Erase;
      Refresh;
      Move_Cursor(Line => (Lines / 2), Column => 2);
      Add
        (Str =>
           "Oops, something bad happens and game crashed. Game should save your progress, but better check it. Also, please, remember what you done before crash and report this problem at https://github.com/thindil/steamsky/issues (or if you prefer, on mail thindil@laeran.pl) and attach (if possible) file 'error.log' from '" &
           To_String(SaveDirectory) &
           "' directory. Hit any key to quit game.");
      Key := Get_Keystroke;
      End_Windows;
      EndLogging;
end SteamSky;
