--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with GNAT.String_Split; use GNAT.String_Split;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Button; use Gtk.Button;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Overlay; use Gtk.Overlay;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Game; use Game;
with HallOfFame; use HallOfFame;
with Ships; use Ships;
with Crew; use Crew;
with Config; use Config;
with Goals.UI; use Goals.UI;
with Maps.UI; use Maps.UI;
with Help; use Help;
with Goals; use Goals;
with Game.SaveLoad; use Game.SaveLoad;
with Utils.UI; use Utils.UI;
with Log; use Log;
with Help.UI; use Help.UI;
with Factions; use Factions;
with Events; use Events;
with Themes; use Themes;
with Bases; use Bases;

package body MainMenu is

   Builder: Gtkada_Builder;
   AllNews, Setting: Boolean := False;
   DataError: Unbounded_String;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Gtk.Main.Main_Quit;
   end Quit;

   procedure RefreshSavesList is
      SavesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "saveslist"));
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

   procedure LoadFile(FileName: String) is
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
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "licensebuffer")),
         To_String(LicenseText));
   end LoadFile;

   procedure ShowPage(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btnnewgame") then
         if Get_Text(Gtk_GEntry(Get_Object(Builder, "entrycharactername"))) =
           "" then
            Set_Text
              (Gtk_Entry(Get_Object(Builder, "entrycharactername")),
               To_String(NewGameSettings.PlayerName));
         end if;
         if Get_Text(Gtk_GEntry(Get_Object(Builder, "entryshipname"))) =
           "" then
            Set_Text
              (Gtk_Entry(Get_Object(Builder, "entryshipname")),
               To_String(NewGameSettings.ShipName));
         end if;
         if NewGameSettings.PlayerGender = 'M' then
            Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbgender")), 0);
         else
            Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbgender")), 1);
         end if;
         if not Set_Active_Id
             (Gtk_Combo_Box(Get_Object(Builder, "cmbfaction")),
              To_String(NewGameSettings.PlayerFaction)) then
            return;
         end if;
         if Factions_Container.Contains
             (Factions_List, NewGameSettings.PlayerFaction) then
            for I in Factions_List(NewGameSettings.PlayerFaction).Careers
              .Iterate loop
               if Careers_Container.Key(I) = NewGameSettings.PlayerCareer then
                  if not Set_Active_Id
                      (Gtk_Combo_Box_Text(Get_Object(Builder, "cmbcareer")),
                       To_String(Careers_Container.Key(I))) then
                     return;
                  end if;
                  exit;
               end if;
            end loop;
         end if;
         Set_Active
           (Gtk_Combo_Box(Get_Object(Builder, "cmbbasetype")),
            Bases_Types'Pos
              (Bases_Types'Value(To_String(NewGameSettings.StartingBase))));
         CreateGoalsMenu;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page1");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "entrycharactername")));
      elsif User_Data = Get_Object(Builder, "btnback") then
         ShowMainMenu;
      elsif User_Data = Get_Object(Builder, "btnhalloffame") then
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
               Set
                 (HofList, Iter, 3,
                  To_String(HallOfFame_Array(I).DeathReason));
            end loop;
         end;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page2");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback2")));
      elsif User_Data = Get_Object(Builder, "btnnews") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page3");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnfull")));
      elsif User_Data = Get_Object(Builder, "btnabout") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page4");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback4")));
      elsif User_Data = Get_Object(Builder, "btnlicense") then
         LoadFile("COPYING");
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page5");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback5")));
      elsif User_Data = Get_Object(Builder, "btnloadgame") then
         RefreshSavesList;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page6");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnload")));
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treesaves")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columnplayername")),
            False);
      elsif User_Data = Get_Object(Builder, "btncontribute") then
         LoadFile("CONTRIBUTING.md");
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page5");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback5")));
      elsif User_Data = Get_Object(Builder, "btnmodding") then
         LoadFile("MODDING.md");
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page5");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback5")));
      elsif User_Data = Get_Object(Builder, "btnreadme") then
         LoadFile("README.md");
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page5");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback5")));
      end if;
   end ShowPage;

   procedure UpdateNews is
      ChangesFile: File_Type;
      NewsText: Unbounded_String := Null_Unbounded_String;
      FileText: Unbounded_String;
   begin
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
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "newsbuffer")),
         To_String(NewsText));
   end UpdateNews;

   procedure ShowAllNews(Object: access Gtkada_Builder_Record'Class) is
   begin
      AllNews := not AllNews;
      UpdateNews;
      if AllNews then
         Set_Label
           (Gtk_Button(Get_Object(Object, "btnfull")),
            "Show only newest changes");
      else
         Set_Label
           (Gtk_Button(Get_Object(Object, "btnfull")), "Show all changes");
      end if;
   end ShowAllNews;

   procedure RandomName(User_Data: access GObject_Record'Class) is
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Builder, "cmbfaction"))));
   begin
      if User_Data = Get_Object(Builder, "entryshipname") then
         Set_Text
           (Gtk_Entry(User_Data), To_String(GenerateShipName(FactionIndex)));
      else
         if Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbgender"))) =
           0 then
            Set_Text
              (Gtk_Entry(User_Data),
               To_String(GenerateMemberName('M', FactionIndex)));
         else
            Set_Text
              (Gtk_Entry(User_Data),
               To_String(GenerateMemberName('F', FactionIndex)));
         end if;
      end if;
   end RandomName;

   procedure ShowGoals(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowGoalsMenu;
   end ShowGoals;

   procedure StartGame is
   begin
      GenerateTraders;
      Hide(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
      CreateHelpUI;
      CreateGoalsMenu;
      CreateSkyMap;
   end StartGame;

   procedure LoadGame(Object: access Gtkada_Builder_Record'Class) is
      SavesIter: Gtk_Tree_Iter;
      SavesModel: Gtk_Tree_Model;
   begin
      Get_Selected
        (Get_Selection(Gtk_Tree_View(Get_Object(Object, "treesaves"))),
         SavesModel, SavesIter);
      if SavesIter = Null_Iter then
         return;
      end if;
      SaveName := To_Unbounded_String(Get_String(SavesModel, SavesIter, 3));
      LoadGame;
      StartGame;
   end LoadGame;

   procedure NewGame(Object: access Gtkada_Builder_Record'Class) is
      CharacterName: constant String :=
        Get_Text(Gtk_Entry(Get_Object(Object, "entrycharactername")));
      ShipName: constant String :=
        Get_Text(Gtk_Entry(Get_Object(Object, "entryshipname")));
      Gender: Character;
   begin
      if Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbgender"))) = 0 then
         Gender := 'M';
      else
         Gender := 'F';
      end if;
      NewGame
        (To_Unbounded_String(CharacterName), To_Unbounded_String(ShipName),
         To_Unbounded_String
           (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbcareer")))),
         To_Unbounded_String
           (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbfaction")))),
         Gender,
         Natural
           (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbbasetype")))));
      StartGame;
   end NewGame;

   procedure DeleteGame(Object: access Gtkada_Builder_Record'Class) is
      SavesIter: Gtk_Tree_Iter;
      SavesModel: Gtk_Tree_Model;
   begin
      if not ShowConfirmDialog
          ("Are you sure you want delete this savegame?",
           Gtk_Window(Get_Object(Object, "mainmenuwindow"))) then
         return;
      end if;
      Get_Selected
        (Get_Selection(Gtk_Tree_View(Get_Object(Object, "treesaves"))),
         SavesModel, SavesIter);
      if SavesIter = Null_Iter then
         return;
      end if;
      SaveName := To_Unbounded_String(Get_String(SavesModel, SavesIter, 3));
      Delete_File(To_String(SaveName));
      RefreshSavesList;
      if N_Children(SavesModel) = 0 then
         ShowPage(Get_Object(Builder, "btnback"));
      else
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treesaves")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columnplayername")),
            False);
      end if;
   end DeleteGame;

   procedure ShowFactionDescription
     (Object: access Gtkada_Builder_Record'Class) is
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbfaction"))));
      CareerComboBox: constant Gtk_Combo_Box_Text :=
        Gtk_Combo_Box_Text(Get_Object(Object, "cmbcareer"));
   begin
      if FactionIndex = Null_Unbounded_String then
         return;
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblnewgameinfo")),
         Get_Tooltip_Text(Gtk_Widget(Get_Object(Object, "cmbfaction"))) & LF &
         LF & To_String(Factions_List(FactionIndex).Description));
      if Factions_List(FactionIndex).Flags.Contains
          (To_Unbounded_String("nogender")) then
         Set_Active(Gtk_Combo_Box(Get_Object(Object, "cmbgender")), 0);
         Hide(Gtk_Widget(Get_Object(Object, "cmbgender")));
         Hide(Gtk_Widget(Get_Object(Object, "lblgender")));
      else
         Show_All(Gtk_Widget(Get_Object(Object, "cmbgender")));
         Show_All(Gtk_Widget(Get_Object(Object, "lblgender")));
      end if;
      Remove_All(CareerComboBox);
      for I in Factions_List(FactionIndex).Careers.Iterate loop
         Append
           (CareerComboBox, To_String(Careers_Container.Key(I)),
            To_String(Factions_List(FactionIndex).Careers(I).Name));
      end loop;
      Setting := True;
      Set_Active(Gtk_Combo_Box(CareerComboBox), 0);
      Setting := False;
   end ShowFactionDescription;

   procedure ShowCareerDescription
     (Object: access Gtkada_Builder_Record'Class) is
      FactionIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbfaction"))));
      CareerIndex: constant Unbounded_String :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Object, "cmbcareer"))));
   begin
      if FactionIndex = Null_Unbounded_String or
        CareerIndex = Null_Unbounded_String or Setting then
         return;
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblnewgameinfo")),
         Get_Tooltip_Text(Gtk_Widget(Get_Object(Object, "cmbcareer"))) & LF &
         LF &
         To_String
           (Factions_List(FactionIndex).Careers(CareerIndex).Description));
   end ShowCareerDescription;

   procedure ShowBaseDescription(Object: access Gtkada_Builder_Record'Class) is
      BaseTypeIndex: constant Integer :=
        Integer(Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbbasetype"))));
      BasesTypesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Object, "basesstore"));
   begin
      if BaseTypeIndex = -1 then
         return;
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblnewgameinfo")),
         Get_Tooltip_Text(Gtk_Widget(Get_Object(Object, "cmbbasetype"))) & LF &
         LF &
         Get_String
           (BasesTypesList,
            Get_Iter_From_String(BasesTypesList, Integer'Image(BaseTypeIndex)),
            1));
   end ShowBaseDescription;

   function NewGameKeyPressed(Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced(Self);
      ScrollBar: constant Gtk_Adjustment :=
        Get_Vadjustment
          (Gtk_Scrolled_Window(Get_Object(Builder, "scrollinfo")));
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

   function UpdateInfo
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblnewgameinfo")),
         Get_Tooltip_Text(Gtk_Widget(User_Data)));
      return False;
   end UpdateInfo;

   procedure UpdateInfoProc(User_Data: access GObject_Record'Class) is
   begin
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblnewgameinfo")),
         Get_Tooltip_Text(Gtk_Widget(User_Data)));
   end UpdateInfoProc;

   procedure HideDialog(User_Data: access GObject_Record'Class) is
   begin
      Hide(Gtk_Widget(User_Data));
   end HideDialog;

   procedure CreateMainMenu is
      Error: aliased GError;
   begin
      LoadThemes;
      LoadTheme;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "mainmenu.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Add_Overlay
        (Gtk_Overlay(Get_Object(Builder, "menuoverlay")),
         Gtk_Widget(Get_Object(Builder, "messagebox")));
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_All_News", ShowAllNews'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Random_Name", RandomName'Access);
      Register_Handler(Builder, "Show_Goals", ShowGoals'Access);
      Register_Handler(Builder, "Load_Game", LoadGame'Access);
      Register_Handler(Builder, "New_Game", NewGame'Access);
      Register_Handler(Builder, "Show_Page", ShowPage'Access);
      Register_Handler(Builder, "Delete_Game", DeleteGame'Access);
      Register_Handler
        (Builder, "Show_Faction_Description", ShowFactionDescription'Access);
      Register_Handler
        (Builder, "Show_Career_Description", ShowCareerDescription'Access);
      Register_Handler
        (Builder, "Show_Base_Description", ShowBaseDescription'Access);
      Register_Handler(Builder, "Update_Info", UpdateInfo'Access);
      Register_Handler(Builder, "Update_Info_Proc", UpdateInfoProc'Access);
      Register_Handler(Builder, "Hide_Dialog", HideDialog'Access);
      Do_Connect(Builder);
      SetUtilsBuilder(Builder);
      Set_Label(Gtk_Label(Get_Object(Builder, "lblversion")), GameVersion);
      if HallOfFame_Array(1).Name = Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "btnhalloffame")));
      end if;
      UpdateNews;
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "entrycharactername")),
         To_String(NewGameSettings.PlayerName));
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "entryshipname")),
         To_String(NewGameSettings.ShipName));
      DataError := To_Unbounded_String(LoadGameData);
      declare
         FactionComboBox: constant Gtk_Combo_Box_Text :=
           Gtk_Combo_Box_Text(Get_Object(Builder, "cmbfaction"));
      begin
         Remove_All(FactionComboBox);
         for I in Factions_List.Iterate loop
            if Factions_List(I).Careers.Length > 0 then
               Append
                 (FactionComboBox, To_String(Factions_Container.Key(I)),
                  To_String(Factions_List(I).Name));
            end if;
         end loop;
         Set_Active(FactionComboBox, 0);
      end;
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "newgamebox")),
         NewGameKeyPressed'Access);
      ShowMainMenu;
      if DataError /= Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
         ShowDialog
           ("Can't load game data files. Error: " & To_String(DataError));
      end if;
   end CreateMainMenu;

   procedure ShowErrorInfo(Message: Unbounded_String) is
      Label: constant Gtk_Label := Gtk_Label(Get_Object(Builder, "lblerror"));
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
           (ErrorFileDirectory, LastDirSeparator, Length(ErrorFileDirectory));
         Delete(NewDataDirectory, DotIndex, 3);
         LastDirSeparator :=
           Index(ErrorFileDirectory, "" & Dir_Separator, Backward);
      end loop;
      Append(ErrorFileDirectory, Dir_Separator & To_String(NewDataDirectory));
      Set_Label
        (Label,
         Get_Label(Label) & " from '" & To_String(ErrorFileDirectory) &
         "' directory.");
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "errorbuffer")),
         To_String(Message));
      Show_All(Gtk_Widget(Get_Object(Builder, "errordialog")));
   end ShowErrorInfo;

   procedure UpdateGoalButton(Message: String) is
   begin
      Set_Label(Gtk_Button(Get_Object(Builder, "btngoal")), Message);
   end UpdateGoalButton;

   procedure ShowMainMenu is
      Files: Search_Type;
   begin
      Show_All(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "mainmenustack")), "page0");
      Start_Search(Files, To_String(SaveDirectory), "*.sav");
      if not More_Entries(Files) then
         Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
      else
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
      end if;
      End_Search(Files);
      if DataError /= Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
      end if;
      if not Exists(To_String(SaveDirectory) & "halloffame.dat") then
         Hide(Gtk_Widget(Get_Object(Builder, "btnhalloffame")));
      end if;
      Hide(Gtk_Widget(Get_Object(Builder, "messagebox")));
   end ShowMainMenu;

   procedure SaveException(An_Exception: Exception_Occurrence;
      PrintToTerminal: Boolean) is
      ErrorFile: File_Type;
      ErrorText: Unbounded_String;
   begin
      if Natural(PlayerShip.Crew.Length) > 0 then
         SaveGame;
      end if;
      if Exists(To_String(SaveDirectory) & "error.log") then
         Open(ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
      else
         Create
           (ErrorFile, Append_File, To_String(SaveDirectory) & "error.log");
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, LF);
      Append(ErrorText, GameVersion);
      Append(ErrorText, LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Append(ErrorText, LF);
      Append(ErrorText, Symbolic_Traceback(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      if PrintToTerminal then
         Put_Line(To_String(ErrorText));
      else
         ShowErrorInfo(ErrorText);
      end if;
      EndLogging;
   end SaveException;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
   begin
      SaveException(An_Exception, False);
   end On_Exception;

end MainMenu;
