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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Help; use Help;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Ships; use Ships;
with Config; use Config;
with Crew; use Crew;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Log; use Log;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with Goals.UI; use Goals.UI;
with HallOfFame; use HallOfFame;
with Utils.UI; use Utils.UI;
with Mobs; use Mobs;

package body MainMenu is

   StartIndex: Integer := 0;
   EndIndex: Integer;
   LicensePad, NewsPad, HallOfFamePad: Window := Null_Window;
   NewGameForm: Forms.Form;
   FormWindow: Window;
   GameMenu: Menu;
   MenuWindow: Window;

   procedure ShowMainMenu is
      Visibility: Cursor_Visibility := Invisible;
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 7);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 2;
   begin
      Set_Echo_Mode(False);
      Set_Cursor_Visibility(Visibility);
      -- Game logo
      Move_Cursor(Line => 3, Column => (Columns / 2) - 25);
      Add(Str => "  ______                            ______ _");
      Move_Cursor(Line => 4, Column => (Columns / 2) - 25);
      Add(Str => " / _____) _                        / _____) |");
      Move_Cursor(Line => 5, Column => (Columns / 2) - 25);
      Add(Str => "( (____ _| |_ _____ _____ ____    ( (____ | |  _ _   _");
      Move_Cursor(Line => 6, Column => (Columns / 2) - 25);
      Add(Str => " \____ (_   _) ___ (____ |    \    \____ \| |_/ ) | | |");
      Move_Cursor(Line => 7, Column => (Columns / 2) - 25);
      Add(Str => " _____) )| |_| ____/ ___ | | | |   _____) )  _ (| |_| |");
      Move_Cursor(Line => 8, Column => (Columns / 2) - 25);
      Add(Str => "(______/  \__)_____)_____|_|_|_|  (______/|_| \_)\__  |");
      Move_Cursor(Line => 9, Column => (Columns / 2) - 25);
      Add(Str => "                                                (____/ ");
      -- Game version
      Move_Cursor(Line => 10, Column => (Columns - 17) / 2);
      Add(Str => GameVersion);
      -- Game menu
      Menu_Items.all(1) := New_Item("New game");
      if Exists(To_String(SaveDirectory) & "savegame.dat") then
         Menu_Items.all(2) := New_Item("Load game");
         MenuIndex := 3;
      end if;
      if HallOfFame_Array(1).Name /= Null_Unbounded_String then
         Menu_Items.all(MenuIndex) := New_Item("Hall of Fame");
         MenuIndex := MenuIndex + 1;
      end if;
      Menu_Items.all(MenuIndex) := New_Item("News");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := New_Item("License");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := New_Item("Quit game");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Menu_Items'Last loop
         Menu_Items.all(I) := Null_Item;
      end loop;
      GameMenu := New_Menu(Menu_Items);
      Set_Format(GameMenu, Lines - 5, 1);
      Scale(GameMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 14, (Columns - 17) / 2);
      Set_Window(GameMenu, MenuWindow);
      Set_Sub_Window
        (GameMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(GameMenu);
      -- Copyright
      Move_Cursor(Line => Lines - 1, Column => (Columns / 2) - 18);
      Add(Str => "(c) 2016-2018 Bartek thindil Jasicki");
      Refresh;
      Refresh(MenuWindow);
   end ShowMainMenu;

   procedure ShowNewGameForm(CurrentField: Positive := 2) is
      NewGame_Fields: constant Field_Array_Access := new Field_Array(1 .. 12);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      Genders: constant Enumeration_Info :=
        (C => 2,
         Names => (new String'("Male ->"), new String'("Female ->")),
         Case_Sensitive => False,
         Match_Must_Be_Unique => False);
      procedure CreateLabel(Line: Line_Position; Text: String) is
         FormIndex: constant Positive := Natural(Line * 2) + 1;
      begin
         NewGame_Fields.all(FormIndex) := New_Field(1, 18, Line, 0, 0, 0);
         Set_Buffer(NewGame_Fields.all(FormIndex), 0, Text);
         FieldOptions := Get_Options(NewGame_Fields.all(FormIndex));
         FieldOptions.Active := False;
         Set_Options(NewGame_Fields.all(FormIndex), FieldOptions);
      end CreateLabel;
   begin
      if CurrentField = 2 then
         Set_Cursor_Visibility(Visibility);
      end if;
      CreateLabel(0, "Character Name: ");
      NewGame_Fields.all(2) := New_Field(1, 12, 0, 18, 0, 0);
      Set_Buffer
        (NewGame_Fields.all(2),
         0,
         To_String(NewGameSettings.PlayerName));
      FieldOptions := Get_Options(NewGame_Fields.all(2));
      FieldOptions.Auto_Skip := False;
      Set_Options(NewGame_Fields.all(2), FieldOptions);
      CreateLabel(1, "Character Gender: ");
      NewGame_Fields.all(4) := New_Field(1, 12, 1, 18, 0, 0);
      Set_Field_Type(NewGame_Fields.all(4), Create(Genders, True));
      if NewGameSettings.PlayerGender = 'M' then
         Set_Buffer(NewGame_Fields.all(4), 0, "Male ->");
      else
         Set_Buffer(NewGame_Fields.all(4), 0, "Female ->");
      end if;
      FieldOptions := Get_Options(NewGame_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(NewGame_Fields.all(4), FieldOptions);
      CreateLabel(2, "Ship Name: ");
      NewGame_Fields.all(6) := New_Field(1, 12, 2, 18, 0, 0);
      Set_Buffer
        (NewGame_Fields.all(6),
         0,
         To_String(NewGameSettings.ShipName));
      FieldOptions := Get_Options(NewGame_Fields.all(6));
      FieldOptions.Auto_Skip := False;
      Set_Options(NewGame_Fields.all(6), FieldOptions);
      CreateLabel(3, "Character Goal: ");
      NewGame_Fields.all(8) := New_Field(1, 32, 3, 18, 0, 0);
      if CurrentGoal.GType = RANDOM then
         Set_Buffer(NewGame_Fields.all(8), 0, "Random");
      else
         Set_Buffer(NewGame_Fields.all(8), 0, GoalText(0));
      end if;
      FieldOptions := Get_Options(NewGame_Fields.all(8));
      FieldOptions.Edit := False;
      Set_Options(NewGame_Fields.all(8), FieldOptions);
      NewGame_Fields.all(9) := New_Field(2, 50, 5, 0, 0, 0);
      FieldOptions := Get_Options(NewGame_Fields.all(9));
      FieldOptions.Active := False;
      Set_Options(NewGame_Fields.all(9), FieldOptions);
      NewGame_Fields.all(10) := New_Field(1, 6, 8, 15, 0, 0);
      Set_Buffer(NewGame_Fields.all(10), 0, "[Quit]");
      FieldOptions := Get_Options(NewGame_Fields.all(10));
      FieldOptions.Edit := False;
      Set_Options(NewGame_Fields.all(10), FieldOptions);
      NewGame_Fields.all(11) := New_Field(1, 7, 8, 23, 0, 0);
      FieldOptions := Get_Options(NewGame_Fields.all(11));
      FieldOptions.Edit := False;
      Set_Options(NewGame_Fields.all(11), FieldOptions);
      Set_Buffer(NewGame_Fields.all(11), 0, "[Start]");
      NewGame_Fields.all(12) := Null_Field;
      if CurrentField = 2 then
         Set_Foreground(NewGame_Fields.all(2), BoldCharacters, 11);
         Set_Background(NewGame_Fields.all(2), BoldCharacters, 11);
         Set_Buffer(NewGame_Fields.all(9), 0, "Press Enter for random name.");
      else
         Set_Foreground(NewGame_Fields.all(8), BoldCharacters, 11);
         Set_Background(NewGame_Fields.all(8), BoldCharacters, 11);
         Set_Buffer
           (NewGame_Fields.all(9),
            0,
            "Press Enter to start selecting character goal.");
      end if;
      NewGameForm := New_Form(NewGame_Fields);
      Set_Current(NewGameForm, NewGame_Fields(CurrentField));
      Set_Options(NewGameForm, (others => False));
      Scale(NewGameForm, FormHeight, FormLength);
      FormWindow :=
        Create
          (FormHeight + 2,
           FormLength + 2,
           ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      WindowFrame(FormWindow, 5, "New game settings");
      Set_Window(NewGameForm, FormWindow);
      Set_Sub_Window
        (NewGameForm,
         Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(NewGameForm);
      Refresh;
      Refresh(FormWindow);
   end ShowNewGameForm;

   procedure ShowLicenseInfo is
      InfoWindow: Window;
      CurrentLine: Line_Position;
      CurrentColumn: Column_Position;
   begin
      InfoWindow := Create(15, (Columns / 2), (Lines / 3), (Columns / 4));
      Add
        (Win => InfoWindow,
         Str =>
           "Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.");
      Get_Cursor_Position(InfoWindow, CurrentLine, CurrentColumn);
      Move_Cursor(Win => InfoWindow, Line => CurrentLine + 2, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
      Refresh(InfoWindow);
      Move_Cursor(Line => (Lines - 2), Column => (Columns / 4));
      Add(Str => "F for full license text, other keys back to main menu.");
      Change_Attributes
        (Line => (Lines - 2),
         Column => (Columns / 4),
         Count => 1,
         Color => 1);
   end ShowLicenseInfo;

   procedure ShowFullLicense is
      LicenseFile: File_Type;
      LinesAmount, TmpLinesAmount: Line_Position;
      LicenseText: Unbounded_String := Null_Unbounded_String;
   begin
      if LicensePad = Null_Window then
         LinesAmount := 0;
         if not Exists(To_String(DocDirectory) & "COPYING") then
            LicenseText :=
              To_Unbounded_String
                ("Can't find license file. Did 'COPYING' file is in '" &
                 To_String(DocDirectory) &
                 "' directory?");
         else
            Open(LicenseFile, In_File, To_String(DocDirectory) & "COPYING");
            while not End_Of_File(LicenseFile) loop
               Append(LicenseText, Get_Line(LicenseFile));
               Append(LicenseText, ASCII.LF);
               LinesAmount := LinesAmount + 1;
            end loop;
            Close(LicenseFile);
         end if;
         TmpLinesAmount :=
           Line_Position(Length(LicenseText)) / Line_Position(Columns - 2);
         if TmpLinesAmount < 1 then
            TmpLinesAmount := 1;
         end if;
         if TmpLinesAmount > LinesAmount then
            LinesAmount := TmpLinesAmount;
         end if;
         LicensePad := New_Pad(LinesAmount + 1, Columns);
         Add(Win => LicensePad, Str => To_String(LicenseText));
         EndIndex := Integer(LinesAmount - (Lines - 3));
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
      end if;
      Refresh
        (LicensePad,
         Line_Position(StartIndex),
         0,
         3,
         0,
         (Lines - 1),
         Columns);
   end ShowFullLicense;

   procedure ShowNews(NewestOnly: Boolean := True) is
      ChangesFile: File_Type;
      LinesAmount, TmpLinesAmount: Line_Position;
      NewsText: Unbounded_String := Null_Unbounded_String;
      FileText: Unbounded_String;
   begin
      if NewsPad = Null_Window then
         LinesAmount := 1;
         if not Exists(To_String(DocDirectory) & "CHANGELOG.md") then
            NewsText :=
              To_Unbounded_String
                ("Can't find changelog file. Did 'CHANGELOG.md' file is in '" &
                 To_String(DocDirectory) &
                 "' directory?");
         else
            Open
              (ChangesFile,
               In_File,
               To_String(DocDirectory) & "CHANGELOG.md");
            Set_Line(ChangesFile, 6);
            while not End_Of_File(ChangesFile) loop
               FileText := To_Unbounded_String(Get_Line(ChangesFile));
               if Length(FileText) > 1 and NewestOnly then
                  exit when Slice(FileText, 1, 3) = "## ";
               end if;
               Append(NewsText, FileText);
               Append(NewsText, ASCII.LF);
               LinesAmount := LinesAmount + 1;
            end loop;
            Close(ChangesFile);
         end if;
         TmpLinesAmount :=
           Line_Position(Length(NewsText)) / Line_Position(Columns - 2);
         if TmpLinesAmount < 1 then
            TmpLinesAmount := 1;
         end if;
         if TmpLinesAmount > LinesAmount then
            LinesAmount := TmpLinesAmount;
         end if;
         NewsPad := New_Pad(LinesAmount + 1, Columns);
         Add(Win => NewsPad, Str => To_String(NewsText));
         EndIndex := Integer(LinesAmount - (Lines - 3));
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
      end if;
      Refresh
        (NewsPad,
         Line_Position(StartIndex),
         0,
         3,
         0,
         (Lines - 1),
         Columns);
   end ShowNews;

   procedure ShowHallOfFame is
      CurrentLine, LinesAmount: Line_Position := 0;
   begin
      if HallOfFamePad = Null_Window then
         for I in HallOfFame_Array'Range loop
            exit when HallOfFame_Array(I).Name = Null_Unbounded_String;
            LinesAmount := LinesAmount + 2;
            if I < 10 then
               LinesAmount := LinesAmount + 2;
            end if;
         end loop;
         HallOfFamePad := New_Pad(LinesAmount, Columns);
         for I in HallOfFame_Array'Range loop
            exit when HallOfFame_Array(I).Name = Null_Unbounded_String;
            Move_Cursor
              (Win => HallOfFamePad,
               Line => CurrentLine,
               Column => 4);
            Add
              (Win => HallOfFamePad,
               Str =>
                 Positive'Image(I) &
                 ". Name: " &
                 To_String(HallOfFame_Array(I).Name));
            Move_Cursor
              (Win => HallOfFamePad,
               Line => CurrentLine,
               Column => (Columns / 2));
            Add
              (Win => HallOfFamePad,
               Str => "Points:" & Natural'Image(HallOfFame_Array(I).Points));
            Move_Cursor
              (Win => HallOfFamePad,
               Line => CurrentLine + 1,
               Column => 5);
            Add
              (Win => HallOfFamePad,
               Str =>
                 "Died from " & To_String(HallOfFame_Array(I).DeathReason));
            CurrentLine := CurrentLine + 3;
         end loop;
         EndIndex := Integer(LinesAmount - (Lines - 3));
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
      end if;
      Refresh
        (HallOfFamePad,
         Line_Position(StartIndex),
         0,
         3,
         0,
         (Lines - 1),
         Columns);
   end ShowHallOfFame;

   procedure RedrawMainMenu(GameState: GameStates) is
   begin
      case GameState is
         when New_Game =>
            ShowMainMenu;
            ShowNewGameForm;
         when License_Info =>
            Refresh_Without_Update;
            ShowLicenseInfo;
            Update_Screen;
         when License_Full =>
            Delete(LicensePad);
            StartIndex := 0;
            Add
              (Str =>
                 "Up/down arrows to scroll on line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, any other key - back to main menu.");
            Refresh_Without_Update;
            ShowFullLicense;
            Update_Screen;
         when News_View =>
            Delete(NewsPad);
            Add
              (Str =>
                 "Up/down arrows to scroll one line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, F to show all changes (not only since last version), any other key - back to main menu.");
            Refresh_Without_Update;
            ShowNews;
            Update_Screen;
         when Hall_Of_Fame =>
            Delete(HallOfFamePad);
            Add
              (Str =>
                 "Up/down arrows to scroll one line, any other key - back to main menu.");
            Refresh_Without_Update;
            ShowHallOfFame;
            Update_Screen;
         when others =>
            null;
      end case;
   end RedrawMainMenu;

   function MainMenuKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      Option: constant String := Name(Current(GameMenu));
      function LoadGameData(NewGame: Boolean := True) return Boolean is
         procedure ShowErrorInfo(Message: String) is
         begin
            if NewGame then
               Erase;
               ShowMainMenu;
               Refresh_Without_Update;
            end if;
            ShowDialog(Message);
            Update_Panels;
            Update_Screen;
         end ShowErrorInfo;
      begin
         LoadHelp;
         LoadItems;
         LoadShipModules;
         LoadRecipes;
         LoadMobs;
         LoadShips;
         LoadGoals;
         SetToolsList;
         if not NewGame then
            LoadGame;
         end if;
         return True;
      exception
         when Help_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load help data. Directory with help files not found.");
            return False;
         when Help_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load help data. Files with help data not found.");
            return False;
         when Items_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load items data. Directory with items data files not found.");
            return False;
         when Items_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load items data. Files with items data not found.");
            return False;
         when Modules_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load ship modules data. Directory with modules data files not found.");
            return False;
         when Modules_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load ship modules data. Files with modules data not found.");
            return False;
         when Recipes_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load recipes data. Directory with recipes data files not found.");
            return False;
         when Recipes_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load recipes data. Files with recipes data not found.");
            return False;
         when An_Exception : Recipes_Invalid_Data =>
            LogMessage(Exception_Message(An_Exception), Everything);
            ShowErrorInfo
              ("Can't load recipes data. Invalid value in file. Run game in debug mode to get more info.");
            return False;
         when Mobs_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load mobs data. Directory with mobs data files not found.");
            return False;
         when Mobs_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load mobs data. Files with mobs data not found.");
            return False;
         when An_Exception : Mobs_Invalid_Data =>
            LogMessage(Exception_Message(An_Exception), Everything);
            ShowErrorInfo
              ("Can't load mobs data. Invalid value in file. Run game in debug mode to get more info.");
            return False;
         when Ships_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load ships data. Directory with ships data files not found.");
            return False;
         when Ships_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load ships data. Files with ships data not found.");
            return False;
         when An_Exception : Ships_Invalid_Data =>
            LogMessage(Exception_Message(An_Exception), Everything);
            ShowErrorInfo
              ("Can't load ships data. Invalid value in file. Run game in debug mode to get more info.");
            return False;
         when SaveGame_Invalid_Version =>
            ShowErrorInfo
              ("This saved game is incompatible with this version of game and can't be loaded.");
            return False;
         when An_Exception : SaveGame_Invalid_Data =>
            LogMessage
              ("Invalid data in savegame: " & Exception_Message(An_Exception),
               Everything);
            ShowErrorInfo
              ("Can't load savegame file. Invalid data. Run game in debug mode to get more info.");
            return False;
         when Goals_Directory_Not_Found =>
            ShowErrorInfo
              ("Can't load goals data. Directory with goals files not found.");
            return False;
         when Goals_Files_Not_Found =>
            ShowErrorInfo
              ("Can't load goals data. Files with goals data not found.");
            return False;
      end LoadGameData;
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous option
            Result := Driver(GameMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(GameMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next option
            Result := Driver(GameMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(GameMenu, M_First_Item);
            end if;
         when 10 => -- Select option
            if Option = "New game" then
               if not LoadGameData then
                  return Main_Menu;
               end if;
               ShowNewGameForm;
               return New_Game;
            elsif Option = "Load game" then
               if not LoadGameData(False) then
                  return Main_Menu;
               end if;
               CenterMap;
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            elsif Option = "News" then
               StartIndex := 0;
               Erase;
               Add
                 (Str =>
                    "Up/down arrows to scroll one line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, F to show all changes (not only since last version), any other key - back to main menu.");
               Refresh_Without_Update;
               ShowNews;
               Update_Screen;
               return News_View;
            elsif Option = "License" then
               Erase;
               Refresh_Without_Update;
               ShowLicenseInfo;
               Update_Screen;
               return License_Info;
            elsif Option = "Hall of Fame" then
               Erase;
               Add
                 (Str =>
                    "Up/down arrows to scroll one line, any other key - back to main menu.");
               Refresh_Without_Update;
               ShowHallOfFame;
               Update_Screen;
               return Hall_Of_Fame;
            else
               return Quit;
            end if;
         when 27 => -- Escape select quit option, second time exit from game
            if Option /= "Quit game" then
               Result := Driver(GameMenu, M_Last_Item);
            else
               return Quit;
            end if;
         when others =>
            Result := Driver(GameMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(GameMenu, M_Clear_Pattern);
               Result := Driver(GameMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow);
      end if;
      return Main_Menu;
   end MainMenuKeys;

   function NewGameKeys(Key: Key_Code) return GameStates is
      NewCharName, NewShipName: Unbounded_String;
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(NewGameForm));
      Visibility: Cursor_Visibility := Invisible;
      CharGender: Character;
      procedure RemoveSemicolons(Name: in out Unbounded_String) is
         SemicolonIndex: Natural;
      begin
         SemicolonIndex := Index(Name, ";");
         while SemicolonIndex > 0 loop
            Delete(Name, SemicolonIndex, SemicolonIndex);
            Name := Name & " ";
            SemicolonIndex := Index(Name, ";");
         end loop;
      end RemoveSemicolons;
      procedure CloseForm is
      begin
         Set_Cursor_Visibility(Visibility);
         Post(NewGameForm, False);
         Delete(NewGameForm);
         Erase;
         Refresh;
         ShowMainMenu;
      end CloseForm;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(NewGameForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(NewGameForm));
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_End_Line);
            end if;
         when KEY_DOWN => -- Select next field
            Result := Driver(NewGameForm, F_Next_Field);
            FieldIndex := Get_Index(Current(NewGameForm));
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_End_Line);
            end if;
         when 10 => -- quit/start game, change gender or show goals types list, depends on form field
            CharGender := Get_Buffer(Fields(NewGameForm, 4))(1);
            if FieldIndex = 2 then
               NewCharName := GenerateMemberName(CharGender);
               Set_Buffer(Fields(NewGameForm, 2), 0, To_String(NewCharName));
               Result := Driver(NewGameForm, F_End_Line);
               Refresh(FormWindow);
            end if;
            if FieldIndex = 4 then
               Result := Driver(NewGameForm, F_Next_Choice);
               Refresh(FormWindow);
            end if;
            if FieldIndex = 6 then
               NewShipName := GenerateShipName;
               Set_Buffer(Fields(NewGameForm, 6), 0, To_String(NewShipName));
               Result := Driver(NewGameForm, F_End_Line);
               Refresh(FormWindow);
            end if;
            NewCharName :=
              To_Unbounded_String(Get_Buffer(Fields(NewGameForm, 2)));
            NewShipName :=
              To_Unbounded_String(Get_Buffer(Fields(NewGameForm, 6)));
            RemoveSemicolons(NewCharName);
            RemoveSemicolons(NewShipName);
            Trim(NewCharName, Ada.Strings.Both);
            Trim(NewShipName, Ada.Strings.Both);
            if Length(NewCharName) = 0 then
               NewCharName := NewGameSettings.PlayerName;
            end if;
            if Length(NewShipName) = 0 then
               NewShipName := NewGameSettings.ShipName;
            end if;
            if FieldIndex = 8 then
               NewGameSettings :=
                 (PlayerName => NewCharName,
                  PlayerGender => CharGender,
                  ShipName => NewShipName);
               Set_Cursor_Visibility(Visibility);
               ShowGoalsTypes;
               return GoalsTypes_View;
            end if;
            if FieldIndex < 10 then
               return New_Game;
            end if;
            if FieldIndex = 10 then
               CloseForm;
               return Main_Menu;
            end if;
            Set_Cursor_Visibility(Visibility);
            Post(NewGameForm, False);
            Delete(NewGameForm);
            NewGame(NewCharName, NewShipName, CharGender);
            CenterMap;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when Key_Backspace | 127 => -- delete last character
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_Delete_Previous);
            end if;
         when KEY_DC => -- delete character at cursor
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_Delete_Char);
            end if;
         when KEY_RIGHT => -- Move cursor right
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_Right_Char);
            elsif FieldIndex = 4 then
               Result := Driver(NewGameForm, F_Next_Choice);
            end if;
         when Character'Pos('m') | Character'Pos('M') => -- Select male gender
            if FieldIndex = 4 then
               Set_Buffer(Current(NewGameForm), 0, "Male ->");
            else
               Result := Driver(NewGameForm, Key);
            end if;
         when Character'Pos('f') |
           Character'Pos('F') => -- Select female gender
            if FieldIndex = 4 then
               Set_Buffer(Current(NewGameForm), 0, "Female ->");
            else
               Result := Driver(NewGameForm, Key);
            end if;
         when KEY_LEFT => -- Move cursor left
            if FieldIndex = 2 or FieldIndex = 6 then
               Result := Driver(NewGameForm, F_Left_Char);
            elsif FieldIndex = 4 then
               Result := Driver(NewGameForm, F_Previous_Choice);
            end if;
         when 27 => -- Escape select quit button, second time closes form
            if FieldIndex /= 10 then
               FieldIndex := 10;
               Set_Current(NewGameForm, Fields(NewGameForm, 10));
               Result := Form_Ok;
            else
               CloseForm;
               return Main_Menu;
            end if;
         when others =>
            Result := Driver(NewGameForm, Key);
      end case;
      if Result = Form_Ok then
         Set_Foreground(Current(NewGameForm), BoldCharacters, 11);
         Set_Background(Current(NewGameForm), BoldCharacters, 11);
         if FieldIndex = 2 or FieldIndex = 6 then
            Set_Buffer
              (Fields(NewGameForm, 9),
               0,
               "Press Enter for random name.");
            Visibility := Normal;
         else
            if FieldIndex = 4 then
               Set_Buffer
                 (Fields(NewGameForm, 9),
                  0,
                  "Left or Right arrow to change gender.");
            elsif FieldIndex = 8 then
               Set_Buffer
                 (Fields(NewGameForm, 9),
                  0,
                  "Press Enter to start selecting character goal.");
            else
               Set_Buffer(Fields(NewGameForm, 9), 0, "");
            end if;
         end if;
         Set_Cursor_Visibility(Visibility);
         for I in 2 .. 11 loop
            if FieldIndex /= I then
               Set_Foreground(Fields(NewGameForm, I));
               Set_Background(Fields(NewGameForm, I));
            end if;
         end loop;
         Refresh(FormWindow);
      end if;
      return New_Game;
   end NewGameKeys;

   function LicenseKeys(Key: Key_Code) return GameStates is
   begin
      Erase;
      Refresh;
      case Key is
         when Character'Pos('f') | Character'Pos('F') => -- Show full license
            StartIndex := 0;
            Add
              (Str =>
                 "Up/down arrows to scroll on line, PgUp/PgDown to scroll one screen, Home/End to go begining or end, any other key - back to main menu.");
            ShowFullLicense;
            return License_Full;
         when others => -- Back to main menu
            ShowMainMenu;
            return Main_Menu;
      end case;
   end LicenseKeys;

   function FullLicenseKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when 56 | KEY_UP => -- Scroll license up one line
            StartIndex := StartIndex - 1;
         when 50 | KEY_DOWN => -- Scroll license down one line
            StartIndex := StartIndex + 1;
         when 51 | KEY_NPAGE => -- Scroll license down one page
            StartIndex := StartIndex + Integer(Lines - 3);
         when 57 | KEY_PPAGE => -- Scroll license up one page
            StartIndex := StartIndex - Integer(Lines - 3);
         when 55 | Key_Home => -- Scroll license to start
            StartIndex := 0;
         when 49 | Key_End => -- Scroll license to end
            StartIndex := EndIndex;
         when others => -- Back to main menu
            StartIndex := 0;
            Delete(LicensePad);
            Erase;
            Refresh;
            ShowMainMenu;
            return Main_Menu;
      end case;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      ShowFullLicense;
      return License_Full;
   end FullLicenseKeys;

   function NewsKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when 56 | KEY_UP => -- Scroll news up one line
            StartIndex := StartIndex - 1;
         when 50 | KEY_DOWN => -- Scroll news down one line
            StartIndex := StartIndex + 1;
         when 51 | KEY_NPAGE => -- Scroll news down one page
            StartIndex := StartIndex + Integer(Lines - 3);
         when 57 | KEY_PPAGE => -- Scroll news up one page
            StartIndex := StartIndex - Integer(Lines - 3);
         when 55 | Key_Home => -- Scroll news to start
            StartIndex := 0;
         when 49 | Key_End => -- Scroll news to end
            StartIndex := EndIndex;
         when Character'Pos('f') | Character'Pos('F') => -- Show full changelog
            StartIndex := 0;
            Delete(NewsPad);
            ShowNews(False);
            return News_View;
         when others => -- Back to main menu
            StartIndex := 0;
            Delete(NewsPad);
            Erase;
            Refresh;
            ShowMainMenu;
            return Main_Menu;
      end case;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      ShowNews;
      return News_View;
   end NewsKeys;

   function HallOfFameKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when 56 | KEY_UP => -- Scroll hall of fame up one line
            StartIndex := StartIndex - 1;
         when 50 | KEY_DOWN => -- Scroll hall of fame down one line
            StartIndex := StartIndex + 1;
         when others => -- Back to main menu
            StartIndex := 0;
            Delete(HallOfFamePad);
            Erase;
            Refresh;
            ShowMainMenu;
            return Main_Menu;
      end case;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      ShowHallOfFame;
      return Hall_Of_Fame;
   end HallOfFameKeys;

end MainMenu;
