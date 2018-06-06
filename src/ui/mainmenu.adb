--    Copyright 2018 Bartek thindil Jasicki
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Main; use Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Button; use Gtk.Button;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Window; use Gtk.Window;
with Gtk.Css_Provider; use Gtk.Css_Provider;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Image; use Gtk.Image;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Gdk.Screen; use Gdk.Screen;
with Gdk.Display; use Gdk.Display;
with Game; use Game;
with HallOfFame; use HallOfFame;
with Ships; use Ships;
with Crew; use Crew;
with Config; use Config;
with Goals.UI; use Goals.UI;
with Maps.UI; use Maps.UI;
with Help; use Help;
with Items; use Items;
with ShipModules; use ShipModules;
with Crafts; use Crafts;
with Mobs; use Mobs;
with Goals; use Goals;
with Game.SaveLoad; use Game.SaveLoad;
with Utils.UI; use Utils.UI;
with Log; use Log;
with Help.UI; use Help.UI;
with Factions; use Factions;

package body MainMenu is

   Builder: Gtkada_Builder;
   AllNews: Boolean := False;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Gtk.Main.Main_Quit;
   end Quit;

   function LoadGameData(NewGame: Boolean := True) return Boolean is
      Parent: constant Gtk_Window :=
        Gtk_Window(Get_Object(Builder, "mainmenuwindow"));
   begin
      LoadHelp;
      LoadItems;
      LoadShipModules;
      LoadRecipes;
      LoadMobs;
      LoadFactions;
      LoadShips;
      LoadGoals;
      SetToolsList;
      if not NewGame then
         LoadGame;
      end if;
      return True;
   exception
      when Help_Directory_Not_Found =>
         ShowDialog
           ("Can't load help data. Directory with help files not found.",
            Parent);
         return False;
      when Help_Files_Not_Found =>
         ShowDialog
           ("Can't load help data. Files with help data not found.",
            Parent);
         return False;
      when Items_Directory_Not_Found =>
         ShowDialog
           ("Can't load items data. Directory with items data files not found.",
            Parent);
         return False;
      when Items_Files_Not_Found =>
         ShowDialog
           ("Can't load items data. Files with items data not found.",
            Parent);
         return False;
      when Modules_Directory_Not_Found =>
         ShowDialog
           ("Can't load ship modules data. Directory with modules data files not found.",
            Parent);
         return False;
      when Modules_Files_Not_Found =>
         ShowDialog
           ("Can't load ship modules data. Files with modules data not found.",
            Parent);
         return False;
      when Recipes_Directory_Not_Found =>
         ShowDialog
           ("Can't load recipes data. Directory with recipes data files not found.",
            Parent);
         return False;
      when Recipes_Files_Not_Found =>
         ShowDialog
           ("Can't load recipes data. Files with recipes data not found.",
            Parent);
         return False;
      when An_Exception : Recipes_Invalid_Data =>
         LogMessage(Exception_Message(An_Exception), Everything);
         ShowDialog
           ("Can't load recipes data. Invalid value in file. Run game in debug mode to get more info.",
            Parent);
         return False;
      when Mobs_Directory_Not_Found =>
         ShowDialog
           ("Can't load mobs data. Directory with mobs data files not found.",
            Parent);
         return False;
      when Mobs_Files_Not_Found =>
         ShowDialog
           ("Can't load mobs data. Files with mobs data not found.",
            Parent);
         return False;
      when An_Exception : Mobs_Invalid_Data =>
         LogMessage(Exception_Message(An_Exception), Everything);
         ShowDialog
           ("Can't load mobs data. Invalid value in file. Run game in debug mode to get more info.",
            Parent);
         return False;
      when Ships_Directory_Not_Found =>
         ShowDialog
           ("Can't load ships data. Directory with ships data files not found.",
            Parent);
         return False;
      when Ships_Files_Not_Found =>
         ShowDialog
           ("Can't load ships data. Files with ships data not found.",
            Parent);
         return False;
      when An_Exception : Ships_Invalid_Data =>
         LogMessage(Exception_Message(An_Exception), Everything);
         ShowDialog
           ("Can't load ships data. Invalid value in file. Run game in debug mode to get more info.",
            Parent);
         return False;
      when An_Exception : SaveGame_Invalid_Data =>
         LogMessage
           ("Invalid data in savegame: " & Exception_Message(An_Exception),
            Everything);
         ShowDialog
           ("Can't load savegame file. Invalid data. Run game in debug mode to get more info.",
            Parent);
         return False;
      when Goals_Directory_Not_Found =>
         ShowDialog
           ("Can't load goals data. Directory with goals files not found.",
            Parent);
         return False;
      when Goals_Files_Not_Found =>
         ShowDialog
           ("Can't load goals data. Files with goals data not found.",
            Parent);
         return False;
      when Factions_Directory_Not_Found =>
         ShowDialog
           ("Can't load NPC factions data. Directory with NPC factions data files not found.",
            Parent);
         return False;
      when Factions_Files_Not_Found =>
         ShowDialog
           ("Can't load NPC factions data. Files with NPC factions data not found.",
            Parent);
         return False;
   end LoadGameData;

   procedure ShowPage(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btnnewgame") then
         if LoadGameData then
            if Get_Text
                (Gtk_GEntry(Get_Object(Builder, "entrycharactername"))) =
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
            CreateGoalsMenu;
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
            Hide(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
         end if;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page1");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "entrycharactername")));
      elsif User_Data = Get_Object(Builder, "btnback") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page0");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
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
                 (HofList,
                  Iter,
                  3,
                  To_String(HallOfFame_Array(I).DeathReason));
            end loop;
         end;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page2");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback2")));
      elsif User_Data = Get_Object(Builder, "btnnews") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page3");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnfull")));
      elsif User_Data = Get_Object(Builder, "btnabout") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page4");
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnback4")));
      elsif User_Data = Get_Object(Builder, "btnlicense") then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
            "page5");
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
              To_String(DocDirectory) &
              "' directory?");
      else
         Open(ChangesFile, In_File, To_String(DocDirectory) & "CHANGELOG.md");
         Set_Line(ChangesFile, 6);
         while not End_Of_File(ChangesFile) loop
            FileText := To_Unbounded_String(Get_Line(ChangesFile));
            if Length(FileText) > 1 and not AllNews then
               exit when Slice(FileText, 1, 3) = "## ";
            end if;
            Append(NewsText, FileText);
            Append(NewsText, ASCII.LF);
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
           (Gtk_Button(Get_Object(Object, "btnfull")),
            "Show all changes");
      end if;
   end ShowAllNews;

   procedure RandomName(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "entryshipname") then
         Set_Text
           (Gtk_Entry(User_Data),
            To_String(GenerateShipName(PlayerFaction)));
      else
         if Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbgender"))) =
           0 then
            Set_Text(Gtk_Entry(User_Data), To_String(GenerateMemberName('M')));
         else
            Set_Text(Gtk_Entry(User_Data), To_String(GenerateMemberName('F')));
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
      Hide(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
      CreateHelpUI;
      CreateGoalsMenu;
      CreateSkyMap;
   end StartGame;

   procedure LoadGame(Object: access Gtkada_Builder_Record'Class) is
   begin
      if LoadGameData(False) then
         StartGame;
      else
         Hide(Gtk_Widget(Get_Object(Object, "btnloadgame")));
      end if;
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
        (To_Unbounded_String(CharacterName),
         To_Unbounded_String(ShipName),
         Gender);
      StartGame;
   end NewGame;

   procedure LoadLicense is
      LicenseFile: File_Type;
      LicenseText: Unbounded_String := Null_Unbounded_String;
   begin
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
         end loop;
         Close(LicenseFile);
      end if;
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "licensebuffer")),
         To_String(LicenseText));
   end LoadLicense;

   procedure CreateMainMenu is
      Error: aliased GError;
      CssProvider: Gtk_Css_Provider;
   begin
      Gtk_New(CssProvider);
      if not Load_From_Path
          (CssProvider,
           To_String(DataDirectory) & "ui" & Dir_Separator & "steamsky.css",
           Error'Access) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Add_Provider_For_Screen
        (Get_Default_Screen(Get_Default),
         +(CssProvider),
         Guint'Last);
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "mainmenu.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_All_News", ShowAllNews'Access);
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Random_Name", RandomName'Access);
      Register_Handler(Builder, "Show_Goals", ShowGoals'Access);
      Register_Handler(Builder, "Load_Game", LoadGame'Access);
      Register_Handler(Builder, "New_Game", NewGame'Access);
      Register_Handler(Builder, "Show_Page", ShowPage'Access);
      Do_Connect(Builder);
      Set_Label(Gtk_Label(Get_Object(Builder, "lblversion")), GameVersion);
      if HallOfFame_Array(1).Name = Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "btnhalloffame")));
      end if;
      UpdateNews;
      LoadLicense;
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "entrycharactername")),
         To_String(NewGameSettings.PlayerName));
      Set_Text
        (Gtk_Entry(Get_Object(Builder, "entryshipname")),
         To_String(NewGameSettings.ShipName));
      Set
        (Gtk_Image(Get_Object(Builder, "imglogo")),
         To_String(DataDirectory) &
         "ui" &
         Dir_Separator &
         "images" &
         Dir_Separator &
         "logo.png");
      ShowMainMenu;
   end CreateMainMenu;

   procedure ShowErrorInfo(Message: Unbounded_String) is
      Label: constant Gtk_Label := Gtk_Label(Get_Object(Builder, "lblerror"));
   begin
      Set_Label
        (Label,
         Get_Label(Label) &
         " from '" &
         To_String(DataDirectory) &
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
   begin
      Show_All(Gtk_Widget(Get_Object(Builder, "mainmenuwindow")));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "mainmenustack")),
         "page0");
      if not Exists(To_String(SaveDirectory) & "savegame.dat") then
         Hide(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnnewgame")));
      else
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "btnloadgame")));
      end if;
      if not Exists(To_String(SaveDirectory) & "halloffame.dat") then
         Hide(Gtk_Widget(Get_Object(Builder, "btnhalloffame")));
      end if;
   end ShowMainMenu;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
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
           (ErrorFile,
            Append_File,
            To_String(SaveDirectory) & "error.log");
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, GameVersion);
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception));
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, "-------------------------------------------------");
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, Symbolic_Traceback(An_Exception));
      Append(ErrorText, ASCII.LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      ShowErrorInfo(ErrorText);
      EndLogging;
   end On_Exception;

end MainMenu;
