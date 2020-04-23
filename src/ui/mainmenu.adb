--    Copyright 2018-2020 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Overlay; use Gtk.Overlay;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Stack_Switcher; use Gtk.Stack_Switcher;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Config; use Config;
with DebugUI; use DebugUI;
with ErrorDialog; use ErrorDialog;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with Goals.UI; use Goals.UI;
with HallOfFame; use HallOfFame;
with Help; use Help;
with Help.UI; use Help.UI;
with Log; use Log;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Themes; use Themes;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body MainMenu is

   -- ****iv* MainMenu/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   -- ****iv* MainMenu/AllNews
   -- FUNCTION
   -- If true, show all news, not only from last version. Default is false
   -- SOURCE
   AllNews: Boolean := False;
   -- ****

   -- ****iv* MainMenu/Setting
   -- FUNCTION
   -- If true, UI is in setting state. Default is false
   -- SOURCE
   Setting: Boolean := False;
   -- ****

   -- ****iv* MainMenu/DataError
   -- FUNCTION
   -- Used to store errors related to loading the game data
   -- SOURCE
   DataError: Unbounded_String;
   -- ****

   -- ****iv* MainMenu/AdjNames
   -- FUNCTION
   -- Array of Gtk_Adjustments names for the game difficulty
   -- SOURCE
   AdjNames: constant array(Positive range <>) of Unbounded_String :=
     (To_Unbounded_String("adjenemydamage"),
      To_Unbounded_String("adjplayerdamage"),
      To_Unbounded_String("adjenemymelee"),
      To_Unbounded_String("adjplayermelee"),
      To_Unbounded_String("adjexperience"),
      To_Unbounded_String("adjreputation"), To_Unbounded_String("adjupdate"),
      To_Unbounded_String("adjprices"));
   -- ****

   -- ****if* MainMenu/BaseTypeName
   -- FUNCTION
   -- Type of base for new game
   -- SOURCE
   BaseTypeName: Unbounded_String;
   -- ****

   -- ****iv* MainMenu/MainMenuWindow
   -- FUNCTION
   -- Gtk window for main menu
   -- SOURCE
   MainMenuWindow: Gtk_Window;
   -- ****

   -- ****iv* MainMenu/InfoLabel
   -- FUNCTION
   -- Label with detailed information
   -- SOURCE
   InfoLabel: Gtk_Label;
   -- ****

   -- ****iv* MainMenu/DifficultyCombo
   -- FUNCTION
   -- Combo box with difficulty level set
   -- SOURCE
   DifficultyCombo: Gtk_Combo_Box_Text;
   -- ****

   -- ****iv* MainMenu/NewGameStack
   -- FUNCTION
   -- Gtk Stack with new game settings
   -- SOURCE
   NewGameStack: Gtk_Stack;
   -- ****

   -- ****iv* MainMenu/MainMenuStack
   -- FUNCTION
   -- Gtk Stack with whole main menu UI
   -- SOURCE
   MainMenuStack: Gtk_Stack;
   -- ****

   -- ****iv* MainMenu/MenuOverlay
   -- FUNCTION
   -- Gtk Overlay with whole main menu UI
   -- SOURCE
   MenuOverlay: Gtk_Overlay;
   -- ****

   -- ****iv* MainMenu/NewsView
   -- FUNCTION
   -- Gtk_Text_View with the last game changes
   -- SOURCE
   NewsView: Gtk_Text_View;
   -- ****

   -- ****iv* MainMenu/LicenseView
   -- FUNCTION
   -- Gtk_Text_View with the various files content
   -- SOURCE
   FileView: Gtk_Text_View;
   -- ****

   -- ****iv* MainMenu/LoadView
   -- FUNCTION
   -- Gtk_Tree_View with the saved games list
   -- SOURCE
   LoadView: Gtk_Tree_View;
   -- ****

   -- ****if* MainMenu/QuitGame
   -- FUNCTION
   -- Quit from the game
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure QuitGame(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Gtk.Main.Main_Quit;
   end QuitGame;

   -- ****if* MainMenu/RefreshSavesList
   -- FUNCTION
   -- Refresh list of available saved games
   -- SOURCE
   procedure RefreshSavesList is
      -- ****
      SavesList: constant Gtk_List_Store := -(Get_Model(LoadView));
      Iter: Gtk_Tree_Iter;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      Tokens: Slice_Set;
   begin
      Clear(SavesList);
      Start_Search(Files, To_String(SaveDirectory), "*.sav");
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         Create(Tokens, Simple_Name(FoundFile), "_");
         Append(SavesList, Iter);
         Set(SavesList, Iter, 0, Slice(Tokens, 1));
         Set(SavesList, Iter, 1, Slice(Tokens, 2));
         Set
           (SavesList, Iter, 2,
            Ada.Calendar.Formatting.Image
              (Modification_Time(FoundFile), False, UTC_Time_Offset));
         Set(SavesList, Iter, 3, Full_Name(FoundFile));
      end loop;
      End_Search(Files);
   end RefreshSavesList;

   -- ****if* MainMenu/LoadFile
   -- FUNCTION
   -- Load license, readme, contributing, etc files to UI
   -- PARAMETERS
   -- FileName - Name of file to load
   -- SOURCE
   procedure LoadFile(FileName: String) is
      -- ****
      LicenseFile: File_Type;
      LicenseText: Unbounded_String := Null_Unbounded_String;
   begin
      if not Exists(To_String(DocDirectory) & FileName) then
         LicenseText :=
           To_Unbounded_String
             ("Can't find file to load. Did '" & FileName & "' file is in '" &
              To_String(DocDirectory) & "' directory?");
      else
         Open(LicenseFile, In_File, To_String(DocDirectory) & FileName);
         while not End_Of_File(LicenseFile) loop
            Append(LicenseText, Get_Line(LicenseFile));
            Append(LicenseText, LF);
         end loop;
         Close(LicenseFile);
      end if;
      Set_Text(Get_Buffer(FileView), To_String(LicenseText));
   end LoadFile;

   -- ****if* MainMenu/SetBaseType
   -- FUNCTION
   -- Set base type for new game based on the game settings
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with bases types
   -- Path  - Gtk_Tree_Path to currently selected base type. Unused
   -- Iter  - Gtk_Tree_Iter to currently selected base type
   -- RESULT
   -- Return true if current base type was set as default, otherwise false
   -- SOURCE
   function SetBaseType
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
      PlayerGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0"));
   begin
      if Get_String(Model, Iter, 0) = To_String(BaseTypeName) then
         Set_Active_Iter(Gtk_Combo_Box(Get_Child_At(PlayerGrid, 1, 6)), Iter);
         return True;
      end if;
      return False;
   end SetBaseType;

   -- ****if* MainMenu/ShowNewGame
   -- FUNCTION
   -- Show the new game setting page
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowNewGame(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      PlayerGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0"));
   begin
      if Get_Text(Gtk_GEntry(Get_Child_At(PlayerGrid, 1, 0))) = "" then
         Set_Text
           (Gtk_Entry(Get_Child_At(PlayerGrid, 1, 0)),
            To_String(NewGameSettings.PlayerName));
      end if;
      if Get_Text(Gtk_GEntry(Get_Child_At(PlayerGrid, 1, 2))) = "" then
         Set_Text
           (Gtk_Entry(Get_Child_At(PlayerGrid, 1, 2)),
            To_String(NewGameSettings.ShipName));
      end if;
      if NewGameSettings.PlayerGender = 'M' then
         Set_Active(Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 1)), 0);
      else
         Set_Active(Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 1)), 1);
      end if;
      if not Set_Active_Id
          (Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 4)),
           To_String(NewGameSettings.PlayerFaction)) then
         return;
      end if;
      if Factions_Container.Contains
          (Factions_List, NewGameSettings.PlayerFaction) then
         for I in Factions_List(NewGameSettings.PlayerFaction).Careers
           .Iterate loop
            if Careers_Container.Key(I) = NewGameSettings.PlayerCareer then
               if not Set_Active_Id
                   (Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 5)),
                    To_String(Careers_Container.Key(I))) then
                  return;
               end if;
               exit;
            end if;
         end loop;
      end if;
      if NewGameSettings.StartingBase /= "Any" then
         for I in BasesTypes_List.Iterate loop
            if BasesTypes_Container.Key(I) = NewGameSettings.StartingBase then
               BaseTypeName := BasesTypes_List(I).Name;
               exit;
            end if;
         end loop;
         Foreach
           (-(Get_Model(Gtk_Combo_Box(Get_Child_At(PlayerGrid, 1, 6)))),
            SetBaseType'Access);
      else
         Set_Active(Gtk_Combo_Box(Get_Child_At(PlayerGrid, 1, 6)), 0);
      end if;
      CreateGoalsMenu;
      Set_Visible_Child_Name(MainMenuStack, "page1");
      Grab_Focus(Get_Child_At(PlayerGrid, 1, 0));
   end ShowNewGame;

   -- ****if* MainMenu/UpdateNews
   -- FUNCTION
   -- Refresh list of the game news
   -- SOURCE
   procedure UpdateNews is
      -- ****
      ChangesFile: File_Type;
      NewsText: Unbounded_String := Null_Unbounded_String;
      FileText: Unbounded_String;
   begin
      if NewsView = null then
         return;
      end if;
      if not Exists(To_String(DocDirectory) & "CHANGELOG.md") then
         NewsText :=
           To_Unbounded_String
             ("Can't find changelog file. Did 'CHANGELOG.md' file is in '" &
              To_String(DocDirectory) & "' directory?");
      else
         Open(ChangesFile, In_File, To_String(DocDirectory) & "CHANGELOG.md");
         Set_Line(ChangesFile, 6);
         while not End_Of_File(ChangesFile) loop
            FileText := To_Unbounded_String(Get_Line(ChangesFile));
            if Length(FileText) > 1 and not AllNews then
               exit when Slice(FileText, 1, 3) = "## ";
            end if;
            Append(NewsText, FileText);
            Append(NewsText, LF);
         end loop;
         Close(ChangesFile);
      end if;
      Set_Text(Get_Buffer(NewsView), To_String(NewsText));
   end UpdateNews;

   -- ****if* MainMenu/ShowAllNews
   -- Show all the game news
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowAllNews(Self: access Gtk_Button_Record'Class) is
   -- ****
   begin
      AllNews := not AllNews;
      UpdateNews;
      if AllNews then
         Set_Label(Self, "Show only newest changes");
      else
         Set_Label(Self, "Show all changes");
      end if;
   end ShowAllNews;

   -- ****if* MainMenu/RandomPlayerName
   -- FUNCTION
   -- Generate random player name, baesd on selected faction
   -- PARAMETERS
   -- Self - Gtk_GEntry which was activated
   -- SOURCE
   procedure RandomPlayerName(Self: access Gtk_Entry_Record'Class) is
      -- ****
      PlayerGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0"));
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 4))));
   begin
      if Get_Active(Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 1))) =
        0 then
         Set_Text(Self, To_String(GenerateMemberName('M', FactionIndex)));
      else
         Set_Text(Self, To_String(GenerateMemberName('F', FactionIndex)));
      end if;
   end RandomPlayerName;

   -- ****if* MainMenu/ShowGoals
   -- Show goal selection UI
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure ShowGoals(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowGoalsMenu;
   end ShowGoals;

   -- ****if* MainMenu/StartGame
   -- FUNCTION
   -- Initialize the game data and UI
   -- SOURCE
   procedure StartGame is
   -- ****
   begin
      GenerateTraders;
      Hide(MainMenuWindow);
      CreateHelpUI;
      CreateGoalsMenu;
      if DebugMode = Menu or DebugMode = Everything then
         CreateDebugUI;
      end if;
      CreateSkyMap;
   end StartGame;

   -- ****if* MainMenu/LoadGame
   -- FUNCTION
   -- Load selected file with the game data
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure LoadGame(Self: access Gtk_Button_Record'Class) is
      -- ****
      SavesIter: Gtk_Tree_Iter;
      SavesModel: Gtk_Tree_Model;
   begin
      Get_Selected
        (Get_Selection
           (Gtk_Tree_View
              (Get_Child
                 (Gtk_Scrolled_Window
                    (Get_Child(Gtk_Box(Get_Parent(Get_Parent(Self))), 0))))),
         SavesModel, SavesIter);
      if SavesIter = Null_Iter then
         return;
      end if;
      SaveName := To_Unbounded_String(Get_String(SavesModel, SavesIter, 3));
      LoadGame;
      StartGame;
   exception
      when An_Exception : SaveGame_Invalid_Data =>
         ShowDialog
           ("Can't load this game. Reason: " &
            Exception_Message(An_Exception));
   end LoadGame;

   -- ****if* MainMenu/RandomDifficulty
   -- FUNCTION
   -- Set random difficulty levels for the game
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure RandomDifficulty(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Setting := True;
      for Name of AdjNames loop
         Set_Value
           (Gtk_Adjustment(Get_Object(Builder, To_String(Name))),
            Gdouble(GetRandom(1, 500)));
      end loop;
      Set_Active(DifficultyCombo, 5);
      Setting := False;
   end RandomDifficulty;

   -- ****if* MainMenu/NewGame
   -- FUNCTION
   -- Start a new game
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure NewGame(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      Gender: Character;
      PlayerGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0"));
   begin
      if Get_Active(Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 1))) =
        0 then
         Gender := 'M';
      else
         Gender := 'F';
      end if;
      if Get_Active
          (Gtk_Toggle_Button
             (Get_Child
                (Gtk_Box
                   (Get_Child
                      (Gtk_Viewport
                         (Get_Child
                            (Gtk_Scrolled_Window
                               (Get_Child_By_Name(NewGameStack, "page1")))))),
                 3))) then
         RandomDifficulty(null);
      end if;
      NewGameSettings :=
        (PlayerName =>
           To_Unbounded_String
             (Get_Text(Gtk_Entry(Get_Child_At(PlayerGrid, 1, 0)))),
         PlayerGender => Gender,
         ShipName =>
           To_Unbounded_String
             (Get_Text(Gtk_Entry(Get_Child_At(PlayerGrid, 1, 2)))),
         PlayerFaction =>
           To_Unbounded_String
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 4)))),
         PlayerCareer =>
           To_Unbounded_String
             (Get_Active_Id
                (Gtk_Combo_Box_Text(Get_Child_At(PlayerGrid, 1, 5)))),
         StartingBase =>
           To_Unbounded_String
             (Gtk.List_Store.Get_String
                (-(Get_Model(Gtk_Combo_Box(Get_Child_At(PlayerGrid, 1, 6)))),
                 Get_Active_Iter
                   (Gtk_Combo_Box(Get_Child_At(PlayerGrid, 1, 6))),
                 0)),
         EnemyDamageBonus =>
           Float
             (Get_Value
                (Gtk_Adjustment(Get_Object(Builder, "adjenemydamage"))) /
              100.0),
         PlayerDamageBonus =>
           Float
             (Get_Value
                (Gtk_Adjustment(Get_Object(Builder, "adjplayerdamage"))) /
              100.0),
         EnemyMeleeDamageBonus =>
           Float
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjenemymelee"))) /
              100.0),
         PlayerMeleeDamageBonus =>
           Float
             (Get_Value
                (Gtk_Adjustment(Get_Object(Builder, "adjplayermelee"))) /
              100.0),
         ExperienceBonus =>
           Float
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjexperience"))) /
              100.0),
         ReputationBonus =>
           Float
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjreputation"))) /
              100.0),
         UpgradeCostBonus =>
           Float
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjupdate"))) /
              100.0),
         PricesBonus =>
           Float
             (Get_Value(Gtk_Adjustment(Get_Object(Builder, "adjprices"))) /
              100.0),
         DifficultyLevel => Natural(Get_Active(DifficultyCombo)));
      for I in BasesTypes_List.Iterate loop
         if BasesTypes_List(I).Name = NewGameSettings.StartingBase then
            NewGameSettings.StartingBase := BasesTypes_Container.Key(I);
            exit;
         end if;
      end loop;
      NewGame;
      StartGame;
   end NewGame;

   -- ****if* MainMenu/DeleteGame
   -- FUNCTION
   -- Delete selected file with game data
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure DeleteGame(Self: access Gtk_Button_Record'Class) is
      -- ****
      SavesIter: Gtk_Tree_Iter;
      SavesModel: Gtk_Tree_Model;
      TreeSaves: constant Gtk_Tree_View :=
        Gtk_Tree_View
          (Get_Child
             (Gtk_Scrolled_Window
                (Get_Child(Gtk_Box(Get_Parent(Get_Parent(Self))), 0))));
   begin
      if not ShowConfirmDialog
          ("Are you sure you want delete this savegame?", MainMenuWindow) then
         return;
      end if;
      Get_Selected(Get_Selection(TreeSaves), SavesModel, SavesIter);
      if SavesIter = Null_Iter then
         return;
      end if;
      SaveName := To_Unbounded_String(Get_String(SavesModel, SavesIter, 3));
      Delete_File(To_String(SaveName));
      RefreshSavesList;
      if N_Children(SavesModel) = 0 then
         ShowMainMenu;
      else
         Set_Cursor
           (TreeSaves, Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end DeleteGame;

   -- ****if* MainMenu/ShowFactionDescription
   -- FUNCTION
   -- Updated faction description when player select new faction
   -- PARAMETERS
   -- Self - Gtk_Combo_Box which value was changed
   -- SOURCE
   procedure ShowFactionDescription(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String(Get_Active_Id(Self));
      CareerComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 5));
      GenderComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 1));
   begin
      if FactionIndex = Null_Unbounded_String or Setting then
         return;
      end if;
      if FactionIndex = To_Unbounded_String("random") then
         Hide(CareerComboBox);
         Hide(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 5));
         Set_Label
           (InfoLabel,
            Get_Tooltip_Text(Self) & LF & LF &
            "Faction and career will be randomly selected for you during creating new game. Not recommended for new player.");
         return;
      end if;
      Remove_All(CareerComboBox);
      for I in Factions_List(FactionIndex).Careers.Iterate loop
         Append
           (CareerComboBox, To_String(Careers_Container.Key(I)),
            To_String(Factions_List(FactionIndex).Careers(I).Name));
      end loop;
      Append(CareerComboBox, "random", "Random");
      Setting := True;
      Set_Active(Gtk_Combo_Box(CareerComboBox), 0);
      Setting := False;
      Show_All(CareerComboBox);
      Show_All(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 5));
      if InfoLabel /= null then
         Set_Label
           (InfoLabel,
            Get_Tooltip_Text(Self) & LF & LF &
            To_String(Factions_List(FactionIndex).Description));
      end if;
      if Factions_List(FactionIndex).Flags.Contains
          (To_Unbounded_String("nogender")) then
         Set_Active(GenderComboBox, 0);
         Hide(GenderComboBox);
         Hide(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 1));
      else
         Show_All(GenderComboBox);
         Show_All(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 0, 1));
      end if;
      declare
         BaseComboBox: constant Gtk_Combo_Box :=
           Gtk_Combo_Box(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 6));
         BasesList: constant Gtk_List_Store := -(Get_Model(BaseComboBox));
         BaseIter: Gtk_Tree_Iter;
      begin
         BasesList.Clear;
         Append(BasesList, BaseIter);
         Set(BasesList, BaseIter, 0, "Any");
         Set
           (BasesList, BaseIter, 1,
            "Start game in randomly selected base type.");
         for I in Factions_List(FactionIndex).BasesTypes.Iterate loop
            Append(BasesList, BaseIter);
            Set
              (BasesList, BaseIter, 0,
               To_String(BasesTypes_List(BaseType_Container.Key(I)).Name));
            Set
              (BasesList, BaseIter, 1,
               To_String
                 (BasesTypes_List(BaseType_Container.Key(I)).Description));
         end loop;
         Setting := True;
         Set_Active
           (Gtk_Combo_Box(Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 6)), 0);
         Setting := False;
      end;
   end ShowFactionDescription;

   -- ****if* MainMenu/ShowCareerDescription
   -- FUNCTION
   -- Show selected career description
   -- PARAMETERS
   -- Self - Gtk_Combo_Box which value was changed
   -- SOURCE
   procedure ShowCareerDescription(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id
             (Gtk_Combo_Box_Text
                (Get_Child_At(Gtk_Grid(Get_Parent(Self)), 1, 4))));
      CareerIndex: constant Unbounded_String :=
        To_Unbounded_String(Get_Active_Id(Self));
   begin
      if FactionIndex = Null_Unbounded_String or
        CareerIndex = Null_Unbounded_String or Setting then
         return;
      end if;
      if CareerIndex /= To_Unbounded_String("random") then
         Set_Label
           (InfoLabel,
            Get_Tooltip_Text(Self) & LF & LF &
            To_String
              (Factions_List(FactionIndex).Careers(CareerIndex).Description));
      else
         Set_Label
           (InfoLabel,
            Get_Tooltip_Text(Self) & LF & LF &
            "Career will be randomly selected for you during creating new game. Not recommended for new player.");
      end if;
   end ShowCareerDescription;

   -- ****if* MainMenu/ShowBaseDescription
   -- FUNCTION
   -- Show selected base type description, when player select new type
   -- PARAMETERS
   -- Self - Gtk_Combo_Box which value was changed
   -- SOURCE
   procedure ShowBaseDescription(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      BaseTypeIndex: constant Integer := Integer(Get_Active(Self));
      BasesTypesList: constant Gtk_List_Store := -(Get_Model(Self));
   begin
      if BaseTypeIndex = -1 or Setting then
         return;
      end if;
      Set_Label
        (InfoLabel,
         Get_Tooltip_Text(Self) & LF & LF &
         Get_String
           (BasesTypesList,
            Get_Iter_From_String(BasesTypesList, Integer'Image(BaseTypeIndex)),
            1));
   end ShowBaseDescription;

   -- ****if* MainMenu/NewGameKeyPressed
   -- FUNCTION
   -- Use Page Up and Page Down buttons to scroll new game UI
   -- PARAMETERS
   -- Self  - Gtk_Widget on which key was pressed (unused)
   -- Event - Details about key pressed event
   -- RESULT
   -- This function always return false
   -- SOURCE
   function NewGameKeyPressed
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean is
      -- ****
      ScrollBar: constant Gtk_Adjustment :=
        Get_Vadjustment
          (Gtk_Scrolled_Window
             (Get_Child(Gtk_Box(Get_Child(Gtk_Box(Self), 1)), 1)));
   begin
      if Event.Keyval = GDK_Page_Up then
         Set_Value
           (ScrollBar, Get_Value(ScrollBar) - Get_Page_Increment(ScrollBar));
      elsif Event.Keyval = GDK_Page_Down then
         Set_Value
           (ScrollBar, Get_Value(ScrollBar) + Get_Page_Increment(ScrollBar));
      end if;
      return False;
   end NewGameKeyPressed;

   -- ****if* MainMenu/UpdateSummary
   -- FUNCTION
   -- Update amount of bonus or malus to the game point from difficulty
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure UpdateSummary(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      MalusNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("adjplayerdamage"),
         To_Unbounded_String("adjplayermelee"),
         To_Unbounded_String("adjexperience"),
         To_Unbounded_String("adjreputation"));
      Bonus, Value: Integer := 0;
   begin
      for Name of AdjNames loop
         Value :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Object, To_String(Name)))));
         for I in MalusNames'Range loop
            if Name = MalusNames(I) then
               if Value < 100 then
                  Value := 100 + ((100 - Value) * 4);
               elsif Value > 100 then
                  Value := 100 - Value;
               end if;
               exit;
            end if;
         end loop;
         Bonus := Bonus + Value;
      end loop;
      Bonus := Bonus / AdjNames'Length;
      if Bonus < 1 then
         Bonus := 1;
      end if;
      if Get_Child_By_Name(NewGameStack, "page1") /= null then
         Set_Text
           (Gtk_Label
              (Get_Child
                 (Gtk_Box
                    (Get_Child
                       (Gtk_Viewport
                          (Get_Child
                             (Gtk_Scrolled_Window
                                (Get_Child_By_Name(NewGameStack, "page1")))))),
                  4)),
            "Total gained points:" & Integer'Image(Bonus) & "%");
      end if;
      if not Setting then
         Set_Active(DifficultyCombo, 5);
      end if;
   end UpdateSummary;

   -- ****if* MainMenu/RandomDifficultyToggled
   -- FUNCTION
   -- Show or hide info about bonus to the game points on toggle random
   -- difficulty
   -- PARAMETERS
   -- Self - Gtk_Check_Button which was clicked
   -- SOURCE
   procedure RandomDifficultyToggled
     (Self: access Gtk_Toggle_Button_Record'Class) is
   -- ****
   begin
      Set_Label(InfoLabel, Get_Tooltip_Text(Self));
      if Get_Active(Self) then
         Set_Text
           (Gtk_Label(Get_Child(Gtk_Box(Get_Parent(Self)), 4)),
            "Total gained points: unknown");
      else
         UpdateSummary(Builder);
      end if;
   end RandomDifficultyToggled;

   -- ****if* MainMenu/SetDifficulty
   -- FUNCTION
   -- Set presetted level of the game difficulty
   -- PARAMETERS
   -- Self - Gtk_Combo_Box which value were changed
   -- SOURCE
   procedure SetDifficulty(Self: access Gtk_Combo_Box_Record'Class) is
      -- ****
      type DifficultyArray is array(1 .. 8) of Gdouble;
      CurrentLevel: constant Gint := Get_Active(Self);
      procedure UpdateDifficulty(Values: DifficultyArray) is
      begin
         for I in AdjNames'Range loop
            Set_Value
              (Gtk_Adjustment(Get_Object(Builder, To_String(AdjNames(I)))),
               Values(I));
         end loop;
      end UpdateDifficulty;
   begin
      Setting := True;
      case CurrentLevel is
         when 0 =>
            UpdateDifficulty
              ((10.0, 450.0, 10.0, 450.0, 450.0, 450.0, 10.0, 10.0));
         when 1 =>
            UpdateDifficulty
              ((50.0, 250.0, 50.0, 250.0, 250.0, 250.0, 50.0, 50.0));
         when 2 =>
            UpdateDifficulty
              ((100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0));
         when 3 =>
            UpdateDifficulty
              ((250.0, 50.0, 250.0, 50.0, 50.0, 50.0, 250.0, 250.0));
         when 4 =>
            UpdateDifficulty
              ((450.0, 10.0, 450.0, 10.0, 10.0, 10.0, 450.0, 450.0));
         when others =>
            null;
      end case;
      Set_Active(Self, CurrentLevel);
      Setting := False;
   end SetDifficulty;

   -- ****if* MainMenu/BackToMenu
   -- FUNCTION
   -- Back to main menu after clicking the button
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure BackToMenu(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowMainMenu;
   end BackToMenu;

   -- ****if* MainMenu/LoadGameView
   -- FUNCTION
   -- Load selected save game file
   -- PARAMETERS
   -- Self   - Gtk_Tree_View with save game files list
   -- Path   - Path to the selected save game file. Unused
   -- Column - Column which was clicked. Unused
   -- SOURCE
   procedure LoadGameView
     (Self: access Gtk_Tree_View_Record'Class;
      Path: Gtk.Tree_Model.Gtk_Tree_Path;
      Column: not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'
        Class) is
      pragma Unreferenced(Path, Column);
      -- ****
      SavesIter: Gtk_Tree_Iter;
      SavesModel: Gtk_Tree_Model;
   begin
      Get_Selected(Get_Selection(Self), SavesModel, SavesIter);
      if SavesIter = Null_Iter then
         return;
      end if;
      SaveName := To_Unbounded_String(Get_String(SavesModel, SavesIter, 3));
      LoadGame;
      StartGame;
   end LoadGameView;

   -- ****if* MainMenu/ShowLicense
   -- FUNCTION
   -- Show license text after clicking the button
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowLicense(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      LoadFile("COPYING");
      Set_Visible_Child_Name(MainMenuStack, "page5");
      Grab_Focus(Get_Child(Gtk_Box(Get_Visible_Child(MainMenuStack)), 1));
   end ShowLicense;

   -- ****if* MainMenu/ShowContributing
   -- FUNCTION
   -- Show contributing guide text after clicking the button
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowContributing(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      LoadFile("CONTRIBUTING.md");
      Set_Visible_Child_Name(MainMenuStack, "page5");
      Grab_Focus(Get_Child(Gtk_Box(Get_Visible_Child(MainMenuStack)), 1));
   end ShowContributing;

   -- ****if* MainMenu/ShowModding
   -- FUNCTION
   -- Show modding guide text after clicking the button
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowModding(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      LoadFile("MODDING.md");
      Set_Visible_Child_Name(MainMenuStack, "page5");
      Grab_Focus(Get_Child(Gtk_Box(Get_Visible_Child(MainMenuStack)), 1));
   end ShowModding;

   -- ****if* MainMenu/ShowReadme
   -- FUNCTION
   -- Show README.md text after clicking the button
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowReadme(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      LoadFile("README.md");
      Set_Visible_Child_Name(MainMenuStack, "page5");
      Grab_Focus(Get_Child(Gtk_Box(Get_Visible_Child(MainMenuStack)), 1));
   end ShowReadme;

   -- ****if* MainMenu/UpdateInfoLabel
   -- FUNCTION
   -- Update text of InfoLabel with tooltip of the selected widget
   -- PARAMETERS
   -- Self  - Gtk_Widget from which tooltip will be taken
   -- Event - Info about Gdk event which triggered this function. Unused.
   -- RESULT
   -- This function always return False
   -- SOURCE
   function UpdateInfoLabel
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Focus)
      return Boolean is
      pragma Unreferenced(Event);
      -- ****
   begin
      Set_Label(InfoLabel, Get_Tooltip_Text(Self));
      return False;
   end UpdateInfoLabel;

   -- ****if* MainMenu/UpdateInfoLabelMap
   -- FUNCTION
   -- Update text of InfoLabel with tooltip of the selected widget
   -- PARAMETERS
   -- Self  - Gtk_Widget from which tooltip will be taken
   -- SOURCE
   procedure UpdateInfoLabelMap(Self: access Gtk_Widget_Record'Class) is
   -- ****
   begin
      Set_Label(InfoLabel, Get_Tooltip_Text(Self));
   end UpdateInfoLabelMap;

   -- ****if* MainMenu/GenerateShipName
   -- FUNCTION
   -- Generate random ship name, based on selected faction
   -- PARAMETERS
   -- Self - Gtk_GEntry which was activated
   -- SOURCE
   procedure GenerateShipName(Self: access Gtk_Entry_Record'Class) is
      -- ****
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id
             (Gtk_Combo_Box_Text
                (Get_Child_At
                   (Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0")), 1,
                    4))));
   begin
      Set_Text(Self, To_String(GenerateShipName(FactionIndex)));
   end GenerateShipName;

   -- ****if* MainMenu/ShowAbout
   -- FUNCTION
   -- Show info about the game
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowAbout(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Set_Visible_Child_Name(MainMenuStack, "page4");
      Grab_Focus
        (Get_Child
           (Gtk_Box
              (Get_Child
                 (Gtk_Box(Get_Child_By_Name(MainMenuStack, "page4")), 5)),
            1));
   end ShowAbout;

   -- ****if* MainMenu/ShowNews
   -- FUNCTION
   -- Show the game news
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowNews(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      Set_Visible_Child_Name(MainMenuStack, "page3");
      Grab_Focus
        (Get_Child
           (Gtk_Box
              (Get_Child
                 (Gtk_Box(Get_Child_By_Name(MainMenuStack, "page3")), 1)),
            1));
   end ShowNews;

   -- ****if* MainMenu/ShowHallOfFame
   -- FUNCTION
   -- Show the game hall of fame
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowHallOfFame(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      declare
         HofList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "hoflist"));
         Iter: Gtk_Tree_Iter;
      begin
         Clear(HofList);
         for I in HallOfFame_Array'Range loop
            exit when HallOfFame_Array(I).Name = Null_Unbounded_String;
            Append(HofList, Iter);
            Set(HofList, Iter, 0, Gint(I));
            Set(HofList, Iter, 1, To_String(HallOfFame_Array(I).Name));
            Set(HofList, Iter, 2, Gint(HallOfFame_Array(I).Points));
            Set(HofList, Iter, 3, To_String(HallOfFame_Array(I).DeathReason));
         end loop;
      end;
      Set_Visible_Child_Name(MainMenuStack, "page2");
      Grab_Focus
        (Get_Child(Gtk_Box(Get_Child_By_Name(MainMenuStack, "page2")), 1));
   end ShowHallOfFame;

   -- ****if* MainMenu/ShowLoadGame
   -- FUNCTION
   -- Show list of saved games
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked.
   -- SOURCE
   procedure ShowLoadGame(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      RefreshSavesList;
      Set_Visible_Child_Name(MainMenuStack, "page6");
      Grab_Focus
        (Get_Child
           (Gtk_Box
              (Get_Child
                 (Gtk_Box(Get_Child_By_Name(MainMenuStack, "page6")), 1)),
            1));
      Set_Cursor
        (Gtk_Tree_View
           (Get_Child
              (Gtk_Scrolled_Window
                 (Get_Child
                    (Gtk_Box(Get_Child_By_Name(MainMenuStack, "page6")), 0)))),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end ShowLoadGame;

   procedure CreateMainMenu is
      Error: aliased GError;
      AdjValues: constant array(Positive range <>) of Gdouble :=
        (Gdouble(NewGameSettings.EnemyDamageBonus),
         Gdouble(NewGameSettings.PlayerDamageBonus),
         Gdouble(NewGameSettings.EnemyMeleeDamageBonus),
         Gdouble(NewGameSettings.PlayerMeleeDamageBonus),
         Gdouble(NewGameSettings.ExperienceBonus),
         Gdouble(NewGameSettings.ReputationBonus),
         Gdouble(NewGameSettings.UpgradeCostBonus),
         Gdouble(NewGameSettings.PricesBonus));
      Accelerators: constant Gtk_Accel_Group := Gtk_Accel_Group_New;
      FactionComboBox: constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
      LoadButton: constant Gtk_Button :=
        Gtk_Button_New_With_Mnemonic("_Load game");
      NewGameButton: constant Gtk_Button :=
        Gtk_Button_New_With_Mnemonic("_New game");
   begin
      LoadThemes;
      SetFontSize(ALLFONTS);
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "mainmenu.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      MainMenuWindow := Gtk_Window_New;
      CreateErrorUI(MainMenuWindow);
      Register_Handler(Builder, "Update_Summary", UpdateSummary'Access);
      Do_Connect(Builder);
      SetUtilsBuilder(Builder);
      DataError := To_Unbounded_String(LoadGameData);
      Setting := True;
      MainMenuStack := Gtk_Stack_New;
      Set_Margin_Bottom(MainMenuStack, 5);
      Set_Margin_Top(MainMenuStack, 5);
      Set_Margin_Left(MainMenuStack, 5);
      Set_Margin_Right(MainMenuStack, 5);
      MenuOverlay := Gtk_Overlay_New;
      Add_Overlay(MenuOverlay, MainMenuStack);
      Add(MainMenuWindow, MenuOverlay);
      for I in AdjNames'Range loop
         Set_Value
           (Gtk_Adjustment(Get_Object(Builder, To_String(AdjNames(I)))),
            (AdjValues(I) * 100.0));
      end loop;
      Setting := False;
      declare
         MainMenuBox: constant Gtk_Vbox := Gtk_Vbox_New;
         MainMenuButtons: constant Gtk_Button_Box :=
           Gtk_Button_Box_New(Orientation_Vertical);
         Button: Gtk_Button;
         Label: Gtk_Label;
      begin
         Label := Gtk_Label_New;
         Set_Markup(Label, "<span font_desc=""Rye 70"">Steam Sky</span>");
         Pack_Start(MainMenuBox, Label, False);
         Label := Gtk_Label_New(GameVersion & " (development)");
         Pack_Start(MainMenuBox, Label, False);
         On_Clicked(NewGameButton, ShowNewGame'Access);
         Pack_Start(MainMenuButtons, NewGameButton);
         On_Clicked(LoadButton, ShowLoadGame'Access);
         Pack_Start(MainMenuButtons, LoadButton);
         Button := Gtk_Button_New_With_Mnemonic("_Hall of Fame");
         On_Clicked(Button, ShowHallOfFame'Access);
         Pack_Start(MainMenuButtons, Button);
         if HallOfFame_Array(1).Name = Null_Unbounded_String then
            Hide(Button);
         end if;
         Button := Gtk_Button_New_With_Mnemonic("N_ews");
         On_Clicked(Button, ShowNews'Access);
         Pack_Start(MainMenuButtons, Button);
         Button := Gtk_Button_New_With_Mnemonic("_About");
         On_Clicked(Button, ShowAbout'Access);
         Pack_Start(MainMenuButtons, Button);
         Button := Gtk_Button_New_With_Mnemonic("_Quit game");
         On_Clicked(Button, QuitGame'Access);
         Pack_Start(MainMenuButtons, Button);
         Pack_Start(MainMenuBox, MainMenuButtons, False);
         Add_Named(MainMenuStack, MainMenuBox, "page0");
      end;
      declare
         NewGameBox: constant Gtk_Vbox := Gtk_Vbox_New;
         DifficultyBox: constant Gtk_Vbox := Gtk_Vbox_New;
         HBox: Gtk_Hbox := Gtk_Hbox_New;
         Button: Gtk_Button;
         NewGameAlign: constant Gtk_Alignment :=
           Gtk_Alignment_New(0.5, 0.5, 1.0, 1.0);
         NewGameFrame: constant Gtk_Frame := Gtk_Frame_New("Info");
         InfoScrollBar: constant Gtk_Scrolled_Window :=
           Gtk_Scrolled_Window_New;
         NewGameBox2: constant Gtk_Hbox := Gtk_Hbox_New;
         Label: Gtk_Label;
         RandomDifficultyButton: constant Gtk_Check_Button :=
           Gtk_Check_Button_New_With_Label
             ("Randomize difficulty on game start");
         DifficultyGrid: constant Gtk_Grid := Gtk_Grid_New;
         SpinButton: Gtk_Spin_Button;
         type SpinButton_Data is record
            Adjustment: Unbounded_String;
            Tooltip: Unbounded_String;
         end record;
         SpinButtonsArray: constant array(0 .. 7) of SpinButton_Data :=
           ((To_Unbounded_String("adjenemydamage"),
             To_Unbounded_String
               ("Percentage of damage done by enemy ships in combat. Lowering it makes the  game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjplayerdamage"),
             To_Unbounded_String
               ("Percentage of damage done by the player's ship in combat. Raising it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjenemymelee"),
             To_Unbounded_String
               ("Percentage of damage done by enemies in melee combat. Lowering it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjplayermelee"),
             To_Unbounded_String
               ("Percentage of damage done by player's crew (and player character) in melee combat. Raising it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjexperience"),
             To_Unbounded_String
               ("Percentage of experience gained by player and their crew from actions. Raising it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjreputation"),
             To_Unbounded_String
               ("Percentage of reputation in bases gained or lost by player in sky bases due to player actions. Raising it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjupdate"),
             To_Unbounded_String
               ("Percentage of the standard material cost and time needed for upgrading ship modules. Lowering it makes the game easier but lowers the amount of score gained as well.")),
            (To_Unbounded_String("adjprices"),
             To_Unbounded_String
               ("Percentage of the standard prices for services in bases (docking, repairing ship, recruiting new crew members, etc). Lowering it makes the game easier but lowers the amount of score gained as well.")));
         LabelsArray: constant array(0 .. 7) of Unbounded_String :=
           (To_Unbounded_String("Enemy ship damage:"),
            To_Unbounded_String("Player ship damage:"),
            To_Unbounded_String("Enemy damage in melee combat:"),
            To_Unbounded_String("Player crew damage in melee combat:"),
            To_Unbounded_String("Experience gained:"),
            To_Unbounded_String("Reputation gained:"),
            To_Unbounded_String("Upgrade cost:"),
            To_Unbounded_String("Prices in bases:"));
         DifficultyScroll: constant Gtk_Scrolled_Window :=
           Gtk_Scrolled_Window_New;
         BasesList: constant Gtk_List_Store :=
           Gtk_List_Store_Newv((GType_String, GType_String));
         BaseIter: Gtk_Tree_Iter;
         ComboBoxBasesTypes: constant Gtk_Combo_Box :=
           Gtk_Combo_Box_New_With_Model(+(BasesList));
         Renderer: constant Gtk_Cell_Renderer_Text :=
           Gtk_Cell_Renderer_Text_New;
         ComboBox: Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
         PlayerGrid: constant Gtk_Grid := Gtk_Grid_New;
         TextEntry: Gtk_GEntry := Gtk_Entry_New;
         Labels2Array: constant array(0 .. 6) of Unbounded_String :=
           (To_Unbounded_String("Character name:"),
            To_Unbounded_String("Character gender:"),
            To_Unbounded_String("Ship name:"),
            To_Unbounded_String("Character's goal:"),
            To_Unbounded_String("Character faction:"),
            To_Unbounded_String("Character career:"),
            To_Unbounded_String("Starting base type:"));
         NewGameSwitch: constant Gtk_Stack_Switcher := Gtk_Stack_Switcher_New;
      begin
         NewGameStack := Gtk_Stack_New;
         Set_Stack(NewGameSwitch, NewGameStack);
         Set_Halign(NewGameSwitch, Align_Center);
         Pack_Start(NewGameBox, NewGameSwitch, False);
         Pack_Start(NewGameBox, NewGameBox2);
         Pack_Start(NewGameBox2, NewGameStack, False);
         for I in Labels2Array'Range loop
            Label := Gtk_Label_New(To_String(Labels2Array(I)));
            Attach(PlayerGrid, Label, 0, Gint(I));
         end loop;
         On_Map(PlayerGrid, UpdateInfoLabelMap'Access);
         Set_Text(TextEntry, To_String(NewGameSettings.PlayerName));
         Set_Tooltip_Text
           (TextEntry,
            "Enter character name or press Enter key for random name.");
         On_Activate(TextEntry, RandomPlayerName'Access);
         On_Focus_In_Event(TextEntry, UpdateInfoLabel'Access);
         Attach(PlayerGrid, TextEntry, 1, 0);
         Remove_All(ComboBox);
         Append_Text(ComboBox, "Male");
         Append_Text(ComboBox, "Female");
         Set_Active(ComboBox, 0);
         Set_Tooltip_Text(ComboBox, "Select the gender of your character. ");
         Attach(PlayerGrid, ComboBox, 1, 1);
         TextEntry := Gtk_Entry_New;
         Set_Text(TextEntry, To_String(NewGameSettings.ShipName));
         Set_Tooltip_Text
           (TextEntry, "Enter ship name or press Enter for random ship name.");
         On_Activate(TextEntry, GenerateShipName'Access);
         On_Focus_In_Event(TextEntry, UpdateInfoLabel'Access);
         Attach(PlayerGrid, TextEntry, 1, 2);
         Button := Gtk_Button_New_With_Label("Random");
         Set_Tooltip_Text
           (Button,
            "Select starting goal for your character. You can change it later in game.");
         On_Clicked(Button, ShowGoals'Access);
         On_Focus_In_Event(Button, UpdateInfoLabel'Access);
         Attach(PlayerGrid, Button, 1, 3);
         Remove_All(FactionComboBox);
         for I in Factions_List.Iterate loop
            if Factions_List(I).Careers.Length > 0 then
               Append
                 (FactionComboBox, To_String(Factions_Container.Key(I)),
                  To_String(Factions_List(I).Name));
            end if;
         end loop;
         Append(FactionComboBox, "random", "Random");
         Set_Active(FactionComboBox, 0);
         On_Changed(FactionComboBox, ShowFactionDescription'Access);
         Set_Tooltip_Text
           (FactionComboBox,
            "Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later.");
         Attach(PlayerGrid, FactionComboBox, 1, 4);
         ComboBox := Gtk_Combo_Box_Text_New;
         On_Changed(ComboBox, ShowCareerDescription'Access);
         Set_Tooltip_Text
           (ComboBox,
            "Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later.");
         Attach(PlayerGrid, ComboBox, 1, 5);
         for BaseType of BasesTypes_List loop
            Append(BasesList, BaseIter);
            Set(BasesList, BaseIter, 0, To_String(BaseType.Name));
            Set(BasesList, BaseIter, 1, To_String(BaseType.Description));
         end loop;
         Pack_Start(ComboBoxBasesTypes, Renderer, True);
         Add_Attribute(ComboBoxBasesTypes, Renderer, "text", 0);
         Set_Id_Column(ComboBoxBasesTypes, 1);
         On_Changed(ComboBoxBasesTypes, ShowBaseDescription'Access);
         Set_Tooltip_Text
           (ComboBoxBasesTypes,
            "Select type of base in which you will start the ame. This may have some impact on game difficulty.");
         Attach(PlayerGrid, ComboBoxBasesTypes, 1, 6);
         Set_Tooltip_Text
           (PlayerGrid,
            "General player character settings. Select field which you want to set to see more information about.");
         On_Map(PlayerGrid, UpdateInfoLabelMap'Access);
         Add_Titled(NewGameStack, PlayerGrid, "page0", "Player");
         Label := Gtk_Label_New("Difficulty Level:");
         Set_Line_Wrap(Label, True);
         Pack_Start(HBox, Label, False);
         DifficultyCombo := Gtk_Combo_Box_Text_New;
         Remove_All(DifficultyCombo);
         Append_Text(DifficultyCombo, "Very Easy");
         Append_Text(DifficultyCombo, "Easy");
         Append_Text(DifficultyCombo, "Normal");
         Append_Text(DifficultyCombo, "Hard");
         Append_Text(DifficultyCombo, "Very hard");
         Append_Text(DifficultyCombo, "Custom");
         On_Changed(DifficultyCombo, SetDifficulty'Access);
         Set_Tooltip_Text
           (DifficultyCombo, "Select game difficulty preset level.");
         Set_Active(DifficultyCombo, Gint(NewGameSettings.DifficultyLevel));
         Pack_Start(HBox, DifficultyCombo, False);
         Pack_Start(DifficultyBox, HBox, False);
         Setting := True;
         for I in 0 .. 7 loop
            Label := Gtk_Label_New(To_String(LabelsArray(I)));
            Attach(DifficultyGrid, Label, 0, Gint(I));
            Set_Line_Wrap(Label, True);
            SpinButton :=
              Gtk_Spin_Button_New
                (Gtk_Adjustment
                   (Get_Object
                      (Builder, To_String(SpinButtonsArray(I).Adjustment))),
                 0.0);
            Set_Tooltip_Text
              (SpinButton, To_String(SpinButtonsArray(I).Tooltip));
            On_Focus_In_Event(SpinButton, UpdateInfoLabel'Access);
            Attach(DifficultyGrid, SpinButton, 1, Gint(I));
            Label := Gtk_Label_New("%");
            Attach(DifficultyGrid, Label, 2, Gint(I));
         end loop;
         Setting := False;
         Set_Tooltip_Text
           (DifficultyGrid,
            "Set difficulty of new game. Each value can be between 1 and 500. Each change has an impact not only on the game's difficulty but also on amount of points gained in the game. Select a field to get more information about it.");
         On_Map(DifficultyGrid, UpdateInfoLabelMap'Access);
         Pack_Start(DifficultyBox, DifficultyGrid, False);
         Button := Gtk_Button_New_With_Mnemonic("_Random");
         Set_Tooltip_Text(Button, "Select random values for all settings.");
         On_Clicked(Button, RandomDifficulty'Access);
         Pack_Start(DifficultyBox, Button, False);
         Set_Tooltip_Text
           (RandomDifficultyButton,
            "If you select this option, all difficulty settings will be randomized during start new game. Not recommended for new players.");
         On_Toggled
           (Gtk_Toggle_Button(RandomDifficultyButton),
            RandomDifficultyToggled'Access);
         Set_Line_Wrap(Gtk_Label(Get_Child(RandomDifficultyButton)), True);
         Pack_Start(DifficultyBox, RandomDifficultyButton, False);
         Label := Gtk_Label_New("Total gained points: 100%");
         Set_Line_Wrap(Label, True);
         Pack_Start(DifficultyBox, Label, False);
         Add(DifficultyScroll, DifficultyBox);
         Add_Titled(NewGameStack, DifficultyScroll, "page1", "Difficulty");
         InfoLabel := Gtk_Label_New;
         Set_Line_Wrap(InfoLabel, True);
         Set_Alignment(InfoLabel, 0.0, 0.0);
         Add(NewGameAlign, InfoLabel);
         Add(NewGameFrame, NewGameAlign);
         Add(InfoScrollBar, NewGameFrame);
         Pack_Start(NewGameBox2, InfoScrollBar);
         HBox := Gtk_Hbox_New;
         Button := Gtk_Button_New_With_Mnemonic("_Start game");
         On_Clicked(Button, NewGame'Access);
         Pack_Start(HBox, Button, False);
         Button := Gtk_Button_New_With_Mnemonic("_Back to menu");
         On_Clicked(Button, BackToMenu'Access);
         Pack_Start(HBox, Button, False);
         Set_Halign(HBox, Align_Center);
         Pack_Start(NewGameBox, HBox, False);
         On_Key_Press_Event(NewGameBox, NewGameKeyPressed'Access);
         Add_Named(MainMenuStack, NewGameBox, "page1");
      end;
      declare
         HallOfFameBox: constant Gtk_Vbox := Gtk_Vbox_New;
         BackButton: constant Gtk_Button :=
           Gtk_Button_New_With_Mnemonic("_Back to menu");
         HoFView: constant Gtk_Tree_View :=
           Gtk_Tree_View_New_With_Model
             (+(Gtk_List_Store(Get_Object(Builder, "hoflist"))));
         Column: Gtk_Tree_View_Column;
         Area: Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
         Renderer: Gtk_Cell_Renderer_Text;
      begin
         Set_Enable_Search(HoFView, False);
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Position");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 0);
         if Append_Column(HoFView, Column) /= 1 then
            raise Program_Error
              with "Can't add column position to hall of fame list.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 1);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Name");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 1);
         if Append_Column(HoFView, Column) /= 2 then
            raise Program_Error
              with "Can't add column name to hall of fame list.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 2);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Points");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 2);
         if Append_Column(HoFView, Column) /= 3 then
            raise Program_Error
              with "Can't add column points to hall of fame list.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 3);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Died from");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 3);
         if Append_Column(HoFView, Column) /= 4 then
            raise Program_Error
              with "Can't add column died from to hall of fame list.";
         end if;
         Pack_Start(HallOfFameBox, HoFView);
         On_Clicked(BackButton, BackToMenu'Access);
         Add_Accelerator
           (BackButton, "clicked", Accelerators, GDK_Escape, 0, Accel_Visible);
         Set_Halign(BackButton, Align_End);
         Pack_Start(HallOfFameBox, BackButton, False);
         Add_Named(MainMenuStack, HallOfFameBox, "page2");
      end;
      declare
         ChangelogBox: constant Gtk_Vbox := Gtk_Vbox_New;
         ButtonBox: constant Gtk_Button_Box :=
           Gtk_Button_Box_New(Orientation_Horizontal);
         Button: Gtk_Button;
         NewsScroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      begin
         NewsView := Gtk_Text_View_New_With_Buffer(Gtk_Text_Buffer_New);
         Set_Editable(NewsView, False);
         Set_Cursor_Visible(NewsView, False);
         Set_Wrap_Mode(NewsView, Wrap_Word);
         Set_Property(NewsView, Gtk.Widget.Name_Property, "normalfont");
         Add(NewsScroll, NewsView);
         Set_Policy(NewsScroll, Policy_Never, Policy_Automatic);
         Pack_Start(ChangelogBox, NewsScroll);
         UpdateNews;
         Button := Gtk_Button_New_With_Mnemonic("_Show all changes");
         On_Clicked(Button, ShowAllNews'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Mnemonic("_Back to menu");
         On_Clicked(Button, BackToMenu'Access);
         Add_Accelerator
           (Button, "clicked", Accelerators, GDK_Escape, 0, Accel_Visible);
         Pack_Start(ButtonBox, Button);
         Set_Halign(ButtonBox, Align_End);
         Pack_Start(ChangelogBox, ButtonBox, False);
         Add_Named(MainMenuStack, ChangelogBox, "page3");
      end;
      declare
         AboutBox: constant Gtk_Vbox := Gtk_Vbox_New;
         ButtonBox: Gtk_Button_Box :=
           Gtk_Button_Box_New(Orientation_Horizontal);
         Button: Gtk_Button;
         Label: Gtk_Label;
         LinkButton: Gtk_Link_Button;
      begin
         Label := Gtk_Label_New("Roguelike in the sky with a steampunk theme");
         Set_Line_Wrap(Label, True);
         Pack_Start(AboutBox, Label);
         LinkButton :=
           Gtk_Link_Button_New_With_Label
             ("https://thindil.itch.io/steam-sky", "Website");
         Set_Property(LinkButton, Gtk.Widget.Name_Property, "flatbutton");
         Set_Halign(LinkButton, Align_Center);
         Pack_Start(AboutBox, LinkButton, False);
         LinkButton :=
           Gtk_Link_Button_New_With_Label
             ("mailto:thindil@laeran.pl",
              "(c)2016-2020 Bartek thindil Jasicki");
         Set_Property(LinkButton, Gtk.Widget.Name_Property, "flatbutton");
         Set_Halign(LinkButton, Align_Center);
         Pack_Start(AboutBox, LinkButton, False);
         Button := Gtk_Button_New_With_Label("Get Involved");
         On_Clicked(Button, ShowContributing'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Label("Modify game");
         On_Clicked(Button, ShowModding'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Label("README");
         On_Clicked(Button, ShowReadme'Access);
         Pack_Start(ButtonBox, Button);
         Set_Halign(ButtonBox, Align_Center);
         Pack_Start(AboutBox, ButtonBox, False);
         Label :=
           Gtk_Label_New
             ("Steam Sky is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version." &
              LF &
              "Steam Sky is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
         Set_Line_Wrap(Label, True);
         Pack_Start(AboutBox, Label);
         ButtonBox := Gtk_Button_Box_New(Orientation_Horizontal);
         Button := Gtk_Button_New_With_Mnemonic("_Show full license");
         On_Clicked(Button, ShowLicense'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Mnemonic("_Back to menu");
         On_Clicked(Button, BackToMenu'Access);
         Add_Accelerator
           (Button, "clicked", Accelerators, GDK_Escape, 0, Accel_Visible);
         Pack_Start(ButtonBox, Button);
         Set_Halign(ButtonBox, Align_End);
         Pack_Start(AboutBox, ButtonBox, False);
         Add_Named(MainMenuStack, AboutBox, "page4");
      end;
      declare
         ShowFileBox: constant Gtk_Vbox := Gtk_Vbox_New;
         BackButton: constant Gtk_Button :=
           Gtk_Button_New_With_Mnemonic("_Back");
         FileScroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      begin
         FileView := Gtk_Text_View_New_With_Buffer(Gtk_Text_Buffer_New);
         Set_Editable(FileView, False);
         Set_Cursor_Visible(FileView, False);
         Set_Wrap_Mode(FileView, Wrap_Word);
         Set_Property(FileView, Gtk.Widget.Name_Property, "normalfont");
         Add(FileScroll, FileView);
         Set_Policy(FileScroll, Policy_Never, Policy_Automatic);
         Pack_Start(ShowFileBox, FileScroll);
         On_Clicked(BackButton, BackToMenu'Access);
         Add_Accelerator
           (BackButton, "clicked", Accelerators, GDK_Escape, 0, Accel_Visible);
         Set_Halign(BackButton, Align_End);
         Set_Valign(BackButton, Align_End);
         Pack_Start(ShowFileBox, BackButton, False);
         Add_Named(MainMenuStack, ShowFileBox, "page5");
      end;
      declare
         LoadBox: constant Gtk_Vbox := Gtk_Vbox_New;
         ButtonBox: constant Gtk_Button_Box :=
           Gtk_Button_Box_New(Orientation_Horizontal);
         Button: Gtk_Button;
         LoadScroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
         Column: Gtk_Tree_View_Column;
         Area: Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
         Renderer: Gtk_Cell_Renderer_Text;
      begin
         LoadView :=
           Gtk_Tree_View_New_With_Model
             (+(Gtk_List_Store_Newv
                 ((GType_String, GType_String, GType_String, GType_String))));
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Player name");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 0);
         if Append_Column(LoadView, Column) /= 1 then
            raise Program_Error
              with "Can't add column player name to saved games list.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 1);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Ship name");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 1);
         if Append_Column(LoadView, Column) /= 2 then
            raise Program_Error
              with "Can't add column ship name to saved games list.";
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 2);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Title(Column, "Last saved");
         Set_Clickable(Column, True);
         Set_Sort_Indicator(Column, True);
         Set_Sort_Column_Id(Column, 2);
         if Append_Column(LoadView, Column) /= 3 then
            raise Program_Error
              with "Can't add column last saved to saved games list.";
         end if;
         Set_Enable_Search(LoadView, False);
         On_Row_Activated(LoadView, LoadGameView'Access);
         Add(LoadScroll, LoadView);
         Pack_Start(LoadBox, LoadScroll);
         Button := Gtk_Button_New_With_Mnemonic("_Delete game");
         On_Clicked(Button, DeleteGame'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Mnemonic("_Load game");
         On_Clicked(Button, LoadGame'Access);
         Pack_Start(ButtonBox, Button);
         Button := Gtk_Button_New_With_Mnemonic("_Back to main menu");
         On_Clicked(Button, BackToMenu'Access);
         Add_Accelerator
           (Button, "clicked", Accelerators, GDK_Escape, 0, Accel_Visible);
         Pack_Start(ButtonBox, Button);
         Pack_Start(LoadBox, ButtonBox, False);
         Add_Named(MainMenuStack, LoadBox, "page6");
      end;
      declare
         Label: constant Gtk_Label :=
           Gtk_Label(Get_Child(Get_Content_Area(ErrorDialog.ErrorDialog), 4));
         ErrorFileDirectory: Unbounded_String :=
           To_Unbounded_String(Current_Directory);
         NewDataDirectory: Unbounded_String := DataDirectory;
         DotIndex: Natural := 1;
         LastDirSeparator: Natural :=
           Index(ErrorFileDirectory, "" & Dir_Separator, Backward);
      begin
         loop
            DotIndex := Index(NewDataDirectory, ".." & Dir_Separator, 1);
            exit when DotIndex = 0;
            Delete
              (ErrorFileDirectory, LastDirSeparator,
               Length(ErrorFileDirectory));
            Delete(NewDataDirectory, DotIndex, 3);
            LastDirSeparator :=
              Index(ErrorFileDirectory, "" & Dir_Separator, Backward);
         end loop;
         Append
           (ErrorFileDirectory, Dir_Separator & To_String(NewDataDirectory));
         Set_Label
           (Label,
            Get_Label(Label) & " from '" & To_String(ErrorFileDirectory) &
            "' directory.");
      end;
      Add_Accel_Group(MainMenuWindow, Accelerators);
      Set_Title(MainMenuWindow, "Steam Sky - main menu");
      Set_Default_Size(MainMenuWindow, 600, 400);
      Set_Position(MainMenuWindow, Win_Pos_Center);
      if not Set_Icon_From_File
          (MainMenuWindow,
           To_String(DataDirectory) & Dir_Separator & "ui" & Dir_Separator &
           "images" & Dir_Separator & "icon.png") then
         raise Program_Error with "Can't set icon for the main menu window";
      end if;
      ShowMainMenu;
      if DataError /= Null_Unbounded_String then
         Hide(LoadButton);
         Hide(NewGameButton);
         ShowDialog
           ("Can't load game data files. Error: " & To_String(DataError));
         return;
      end if;
      if not Is_Write_Accessible_File(To_String(SaveDirectory)) then
         Hide(LoadButton);
         Hide(NewGameButton);
         ShowDialog
           ("Directory " & To_String(SaveDirectory) &
            " is not write accessible, thus save games cannot be saved.");
      end if;
      ShowFactionDescription(FactionComboBox);
   end CreateMainMenu;

   procedure UpdateGoalButton(Message: String) is
   begin
      Set_Label
        (Gtk_Button
           (Get_Child_At
              (Gtk_Grid(Get_Child_By_Name(NewGameStack, "page0")), 1, 3)),
         Message);
   end UpdateGoalButton;

   procedure ShowMainMenu is
      Files: Search_Type;
      ButtonsBox: constant Gtk_Button_Box :=
        Gtk_Button_Box
          (Get_Child(Gtk_Box(Get_Child_By_Name(MainMenuStack, "page0")), 2));
   begin
      Show_All(MainMenuWindow);
      Set_Visible_Child_Name(MainMenuStack, "page0");
      Start_Search(Files, To_String(SaveDirectory), "*.sav");
      if not More_Entries(Files) then
         Hide(Get_Child(ButtonsBox, 1));
         Grab_Focus(Get_Child(ButtonsBox, 0));
      else
         Grab_Focus(Get_Child(ButtonsBox, 1));
      end if;
      End_Search(Files);
      if MessageBox = null then
         declare
            MessageLabel: constant Gtk_Label := Gtk_Label_New;
         begin
            Set_Line_Wrap(MessageLabel, True);
            Set_Width_Chars(MessageLabel, 40);
            Set_Max_Width_Chars(MessageLabel, 40);
            MessageBox := Gtk_Info_Bar_New;
            Set_Show_Close_Button(MessageBox, True);
            Set_Message_Type(MessageBox, Message_Info);
            Pack_Start
              (Gtk_Box(Get_Content_Area(MessageBox)), MessageLabel, False);
            On_Response(MessageBox, HideDialog'Access);
            Set_Halign(MessageBox, Align_Center);
            Set_Valign(MessageBox, Align_Center);
            Set_No_Show_All(MessageBox, True);
         end;
         Add_Overlay(Gtk_Overlay(Get_Parent(MainMenuStack)), MessageBox);
      end if;
      if DataError /= Null_Unbounded_String then
         Hide(Get_Child(ButtonsBox, 1));
         Hide(Get_Child(ButtonsBox, 0));
      else
         Hide(MessageBox);
      end if;
      if not Exists(To_String(SaveDirectory) & "halloffame.dat") then
         Hide(Get_Child(ButtonsBox, 2));
      end if;
   end ShowMainMenu;

end MainMenu;
