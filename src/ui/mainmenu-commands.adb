-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Game.SaveLoad; use Game.SaveLoad;
with Goals; use Goals;
with HallOfFame; use HallOfFame;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body MainMenu.Commands is

   function Open_Link_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      Command: constant String := Locate_Exec_On_Path((if OsName = "Windows"
         then "start" elsif OsName = "Darwin" then "open"
            else "xdg-open")).all;
      ProcessId: Process_Id;
   begin
      ProcessId :=
        Non_Blocking_Spawn
          (Command,
           Argument_String_To_List(CArgv.Arg(Argv, 1)).all);
      if ProcessId = Invalid_Pid then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Open_Link_Command;

   -- ****o* MCommands/Show_File_Command
   -- FUNCTION
   -- Show the selected file content
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowFile filename
   -- Filename is the name of the file in the documentation directory which
   -- will be show
   -- SOURCE
   function Show_File_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TextView: constant Tk_Text := Get_Widget(".showfilemenu.text", Interp);
      ShowFile: File_Type;
      FileName: constant String := CArgv.Arg(Argv, 1);
   begin
      configure(TextView, "-state normal");
      Delete(TextView, "1.0", "end");
      if not Exists(To_String(Doc_Directory) & FileName) then
         Insert
           (TextView, "end",
            "{Can't find file to load. Did '" & FileName & "' file is in '" &
            To_String(Doc_Directory) & "' directory?}");
      else
         Open(ShowFile, In_File, To_String(Doc_Directory) & FileName);
         Load_File_Line_Loop :
         while not End_Of_File(ShowFile) loop
            Insert(TextView, "end", "{" & Get_Line(ShowFile) & LF & "}");
         end loop Load_File_Line_Loop;
         Close(ShowFile);
      end if;
      configure(TextView, "-state disabled");
      Bind_To_Main_Window
        (Interp, "<Alt-b>", "{InvokeButton .showfilemenu.back}");
      Bind_To_Main_Window
        (Interp, "<Escape>", "{InvokeButton .showfilemenu.back}");
      return TCL_OK;
   end Show_File_Command;

   -- ****iv* MCommands/AllNews
   -- FUNCTION
   -- If true, show all news, not only from last version. Default is false
   -- SOURCE
   AllNews: Boolean := False;
   -- ****

   -- ****o* MCommands/Show_News_Command
   -- FUNCTION
   -- Show changes in the game, all or just recent
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowNews boolean
   -- If boolean is true, show all news, otherwise only recent
   -- SOURCE
   function Show_News_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_News_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TextView: constant Tk_Text := Get_Widget(".newsmenu.text", Interp);
      ChangesFile: File_Type;
      FileText: Unbounded_String;
      AllNewsButton: constant Ttk_Button :=
        Get_Widget(".newsmenu.showall", Interp);
   begin
      if CArgv.Arg(Argv, 1) = "false" then
         AllNews := False;
         configure
           (AllNewsButton,
            "-text {Show all changes} -command {ShowNews true}");
         Add
           (AllNewsButton,
            "Show all changes to the game since previous big stable version");
      else
         AllNews := True;
         configure
           (AllNewsButton,
            "-text {Show only newest changes} -command {ShowNews false}");
         Add
           (AllNewsButton,
            "Show only changes to the game since previous relese");
      end if;
      configure(TextView, "-state normal");
      Delete(TextView, "1.0", "end");
      if not Exists(To_String(Doc_Directory) & "CHANGELOG.md") then
         Insert
           (TextView, "end",
            "{Can't find changelog file. Did 'CHANGELOG.md' file is in '" &
            To_String(Doc_Directory) & "' directory?}");
      else
         Open(ChangesFile, In_File, To_String(Doc_Directory) & "CHANGELOG.md");
         Set_Line(ChangesFile, 6);
         Load_Changes_File_Loop :
         while not End_Of_File(ChangesFile) loop
            FileText := To_Unbounded_String(Get_Line(ChangesFile));
            if Length(FileText) > 1 and not AllNews then
               exit Load_Changes_File_Loop when Slice(FileText, 1, 3) = "## ";
            end if;
            Insert(TextView, "end", "{" & To_String(FileText) & LF & "}");
         end loop Load_Changes_File_Loop;
         Close(ChangesFile);
      end if;
      configure(TextView, "-state disabled");
      return TCL_OK;
   end Show_News_Command;

   -- ****o* MCommands/Show_Hall_Of_Fame_Command
   -- FUNCTION
   -- Show the Hall of Fame
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowHallOfFame
   -- SOURCE
   function Show_Hall_Of_Fame_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Hall_Of_Fame_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HofView: constant Ttk_Tree_View := Get_Widget(".hofmenu.view", Interp);
   begin
      Delete(HofView, "[list " & Children(HofView, "{}") & "]");
      Load_Hall_Of_Fame_Loop :
      for I in Hall_Of_Fame_Array'Range loop
         exit Load_Hall_Of_Fame_Loop when Hall_Of_Fame_Array(I).Name =
           Null_Unbounded_String;
         Insert
           (HofView,
            "{} end -values [list " & Positive'Image(I) & " " &
            To_String(Hall_Of_Fame_Array(I).Name) & " " &
            Natural'Image(Hall_Of_Fame_Array(I).Points) & " " &
            To_String(Hall_Of_Fame_Array(I).Death_Reason) & "]");
      end loop Load_Hall_Of_Fame_Loop;
      return TCL_OK;
   end Show_Hall_Of_Fame_Command;

   -- ****o* MCommands/Show_Load_Game_Command
   -- FUNCTION
   -- Show available saved games
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoadGame
   -- SOURCE
   function Show_Load_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Load_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      LoadView: constant Ttk_Tree_View := Get_Widget(".loadmenu.view", Interp);
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      Tokens: Slice_Set;
      Selected: Boolean := False;
   begin
      Delete(LoadView, "[list " & Children(LoadView, "{}") & "]");
      Start_Search(Files, To_String(Save_Directory), "*.sav");
      Load_Saves_List_Loop :
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         Create(Tokens, Simple_Name(FoundFile), "_");
         Insert
           (LoadView,
            "{} end -id {" & Simple_Name(FoundFile) & "} -values [list " &
            Slice(Tokens, 1) & " " & Slice(Tokens, 2) & " {" &
            Ada.Calendar.Formatting.Image
              (Modification_Time(FoundFile), False, UTC_Time_Offset) &
            "}] -tags [list itemrow]");
         if not Selected then
            Selection_Set(LoadView, "{" & Simple_Name(FoundFile) & "}");
            Selected := True;
         end if;
      end loop Load_Saves_List_Loop;
      End_Search(Files);
      return TCL_OK;
   end Show_Load_Game_Command;

   -- ****o* MCommands/Delete_Game_Command
   -- FUNCTION
   -- Delete the selected save file
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeleteGame
   -- SOURCE
   function Delete_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowQuestion
        ("Are you sure you want delete this savegame?", "deletesave", False);
      return TCL_OK;
   end Delete_Game_Command;

   -- ****if* MCommands/StartGame
   -- FUNCTION
   -- Start the game
   -- SOURCE
   procedure StartGame is
      -- ****
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      X, Y: Integer;
   begin
      X :=
        (Positive'Value(Winfo_Get(MainWindow, "vrootwidth")) -
         Game_Settings.Window_Width) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(MainWindow, "vrootheight")) -
         Game_Settings.Window_Height) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (MainWindow, "geometry",
         Trim(Positive'Image(Game_Settings.Window_Width), Left) & "x" &
         Trim(Positive'Image(Game_Settings.Window_Height), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      GenerateTraders;
      CreateGameUI;
   end StartGame;

   -- ****o* MCommands/Load_Game_Command
   -- FUNCTION
   -- Load the selected save file and start the game
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LoadGame
   -- SOURCE
   function Load_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Load_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      LoadView: constant Ttk_Tree_View := Get_Widget(".loadmenu.view", Interp);
   begin
      if Selection(LoadView) = "" then
         return TCL_OK;
      end if;
      SaveName := Save_Directory & Selection(LoadView);
      LoadGame;
      StartGame;
      return TCL_OK;
   exception
      when An_Exception : SaveGame_Invalid_Data =>
         Show_Main_Menu;
         ShowMessage
           ("Can't load this game. Reason: " & Exception_Message(An_Exception),
            ".mainmenu", "Can't load the game");
         return TCL_OK;
   end Load_Game_Command;

   -- ****o* MCommands/Set_Faction_Command
   -- FUNCTION
   -- Set faction destription and available bases and careers
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetFaction
   -- SOURCE
   function Set_Faction_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Faction_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FactionName, Values: Unbounded_String;
      FrameName: constant String := ".newgamemenu.canvas.player";
      ComboBox: Ttk_ComboBox :=
        Get_Widget(FrameName & ".faction", Interp);
      Label: Ttk_Label;
      GenderFrame: constant Ttk_Frame :=
        Get_Widget(FrameName & ".gender", Interp);
      procedure UpdateInfo(NewText: String) is
         InfoText: constant Tk_Text :=
           Get_Widget(".newgamemenu.info.text", Interp);
      begin
         configure(InfoText, "-state normal");
         Delete(InfoText, "1.0", "end");
         Insert
           (InfoText, "end",
            "{Select your faction from a list. Factions have the biggest impact on game. They determine the amount of bases and some playing styles. More information about each faction can be found after selecting it. You can't change this later." &
            LF & LF & "}");
         Insert(InfoText, "end", NewText);
         configure(InfoText, "-state disabled");
      end UpdateInfo;
   begin
      Label.Interp := Interp;
      FactionName := To_Unbounded_String(Get(ComboBox));
      if FactionName = To_Unbounded_String("Random") then
         Label.Name := New_String(FrameName & ".labelcareer");
         Grid_Remove(Label);
         ComboBox.Name := New_String(FrameName & ".career");
         Set(ComboBox, "Random");
         Grid_Remove(ComboBox);
         Label.Name := New_String(FrameName & ".labelbase");
         Grid_Remove(Label);
         ComboBox.Name := New_String(FrameName & ".base");
         Set(ComboBox, "Any");
         Grid_Remove(ComboBox);
         UpdateInfo
           ("{Faction, career and base type will be randomly selected for you during creating new game. Not recommended for new player.}");
         return TCL_OK;
      else
         Label.Name := New_String(FrameName & ".labelcareer");
         Tcl.Tk.Ada.Grid.Grid(Label);
         ComboBox.Name := New_String(FrameName & ".career");
         Tcl.Tk.Ada.Grid.Grid(ComboBox);
         Label.Name := New_String(FrameName & ".labelbase");
         Tcl.Tk.Ada.Grid.Grid(Label);
         ComboBox.Name := New_String(FrameName & ".base");
         Tcl.Tk.Ada.Grid.Grid(ComboBox);
      end if;
      Load_Faction_Based_Info_Loop :
      for Faction of Factions_List loop
         if Faction.Name /= FactionName then
            goto End_Of_Faction_Info_Loop;
         end if;
         if Faction.Flags.Contains(To_Unbounded_String("nogender")) then
            Label.Name := New_String(FrameName & ".labelgender");
            Grid_Remove(Label);
            Grid_Remove(GenderFrame);
            Tcl_SetVar(Interp, "playergender", "M");
         else
            Label.Name := New_String(FrameName & ".labelgender");
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl.Tk.Ada.Grid.Grid(GenderFrame);
         end if;
         Values := Null_Unbounded_String;
         Load_Careers_Loop :
         for I in Faction.Careers.Iterate loop
            Append(Values, " " & Faction.Careers(I).Name);
         end loop Load_Careers_Loop;
         Append(Values, " Random");
         ComboBox.Name := New_String(FrameName & ".career");
         configure(ComboBox, "-values [list " & To_String(Values) & "]");
         Set(ComboBox, "General");
         Values := To_Unbounded_String(" Any");
         Load_Bases_Types_Loop :
         for I in Faction.BasesTypes.Iterate loop
            Append
              (Values,
               " {" & BasesTypes_List(BaseType_Container.Key(I)).Name & "}");
         end loop Load_Bases_Types_Loop;
         ComboBox.Name := New_String(FrameName & ".base");
         configure(ComboBox, "-values [list " & To_String(Values) & "]");
         Set(ComboBox, "Any");
         UpdateInfo("{" & To_String(Faction.Description) & "}");
         exit Load_Faction_Based_Info_Loop;
         <<End_Of_Faction_Info_Loop>>
      end loop Load_Faction_Based_Info_Loop;
      return TCL_OK;
   end Set_Faction_Command;

   -- ****o* MCommands/Set_Career_Command
   -- FUNCTION
   -- Set career description
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCareer
   -- SOURCE
   function Set_Career_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Career_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FactionName, CareerName: Unbounded_String;
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".newgamemenu.canvas.player.faction", Interp);
      InfoText: constant Tk_Text :=
        Get_Widget(".newgamemenu.info.text", Interp);
   begin
      FactionName := To_Unbounded_String(Get(ComboBox));
      ComboBox.Name := New_String(".newgamemenu.canvas.player.career");
      CareerName := To_Unbounded_String(Get(ComboBox));
      configure(InfoText, "-state normal");
      Delete(InfoText, "1.0", "end");
      Insert
        (InfoText, "end",
         "{Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later." &
         LF & LF & "}");
      Set_Faction_Careers_Loop :
      for Faction of Factions_List loop
         if Faction.Name = FactionName then
            Load_Careers_Loop :
            for Career of Faction.Careers loop
               if Career.Name = CareerName then
                  Insert
                    (InfoText, "end",
                     "{" & To_String(Career.Description) & "}");
                  exit Set_Faction_Careers_Loop;
               end if;
            end loop Load_Careers_Loop;
         end if;
      end loop Set_Faction_Careers_Loop;
      if CareerName = "Random" then
         Insert
           (InfoText, "end",
            "{Career will be randomly selected for you during creating new game. Not recommended for new player.}");
      end if;
      configure(InfoText, "-state disabled");
      return TCL_OK;
   end Set_Career_Command;

   -- ****o* MCommands/Set_Base_Command
   -- FUNCTION
   -- Set starting base description
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBase
   -- SOURCE
   function Set_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Base_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      BaseName: Unbounded_String;
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(".newgamemenu.canvas.player.base", Interp);
      InfoText: constant Tk_Text :=
        Get_Widget(".newgamemenu.info.text", Interp);
   begin
      BaseName := To_Unbounded_String(Get(ComboBox));
      configure(InfoText, "-state normal");
      Delete(InfoText, "1.0", "end");
      Insert
        (InfoText, "end",
         "{Select your career from a list. Careers have some impact on gameplay (each have bonuses to gaining experience in some fields plus they determine your starting ship and crew). More info about each career can be found after selecting it. You can't change career later." &
         LF & LF & "}");
      Find_Base_Type_Loop :
      for Base of BasesTypes_List loop
         if Base.Name = BaseName then
            Insert(InfoText, "end", "{" & To_String(Base.Description) & "}");
            exit Find_Base_Type_Loop;
         end if;
      end loop Find_Base_Type_Loop;
      if BaseName = "Any" then
         Insert
           (InfoText, "end",
            "{Start the game in randomly selected base type.}");
      end if;
      configure(InfoText, "-state disabled");
      return TCL_OK;
   end Set_Base_Command;

   -- ****o* MCommands/Random_Name_Command
   -- FUNCTION
   -- Generate random player or ship name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RandomName type
   -- Type is type of name which should be generated. Possible options are
   -- player or ship
   -- SOURCE
   function Random_Name_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Random_Name_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(".newgamemenu.canvas.player.faction", Interp);
      FactionName, FactionIndex: Unbounded_String;
      Gender: Character;
      NameEntry: constant Ttk_Entry :=
        Get_Widget
          (".newgamemenu.canvas.player." & CArgv.Arg(Argv, 1) & "name",
           Interp);
   begin
      FactionName := To_Unbounded_String(Get(ComboBox));
      Find_Faction_Index_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = FactionName then
            FactionIndex := Factions_Container.Key(I);
            exit Find_Faction_Index_Loop;
         end if;
      end loop Find_Faction_Index_Loop;
      if CArgv.Arg(Argv, 1) = "player" then
         Gender := Tcl_GetVar(Interp, "playergender")(1);
         Delete(NameEntry, "0", "end");
         Insert
           (NameEntry, "end",
            To_String(GenerateMemberName(Gender, FactionIndex)));
         return TCL_OK;
      end if;
      Delete(NameEntry, "0", "end");
      Insert(NameEntry, "end", To_String(GenerateShipName(FactionIndex)));
      return TCL_OK;
   end Random_Name_Command;

   -- ****o* MCommands/New_Game_Command
   -- FUNCTION
   -- Set all parameters and start a new game
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NewGame
   -- SOURCE
   function New_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function New_Game_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(".newgamemenu.canvas.player.faction", Interp);
      GoalButton: constant Ttk_Button :=
        Get_Widget(".newgamemenu.canvas.player.goal", Interp);
      TextEntry: Ttk_Entry :=
        Get_Widget(".newgamemenu.canvas.player.playername", Interp);
      SpinBox: Ttk_SpinBox :=
        Get_Widget(".newgamemenu.canvas.difficulty.enemydamage", Interp);
   begin
      New_Game_Settings.Player_Gender := Tcl_GetVar(Interp, "playergender")(1);
      if cget(GoalButton, "-text") = "Random" then
         ClearCurrentGoal;
         CurrentGoal :=
           Goals_List
             (GetRandom(Goals_List.First_Index, Goals_List.Last_Index));
      end if;
      New_Game_Settings.Player_Name := To_Unbounded_String(Get(TextEntry));
      TextEntry.Name := New_String(".newgamemenu.canvas.player.shipname");
      New_Game_Settings.Ship_Name := To_Unbounded_String(Get(TextEntry));
      Find_Faction_Loop :
      for I in Factions_List.Iterate loop
         if Factions_List(I).Name = To_Unbounded_String(Get(ComboBox)) then
            New_Game_Settings.Player_Faction := Factions_Container.Key(I);
            ComboBox.Name := New_String(".newgamemenu.canvas.player.career");
            Find_Career_Loop :
            for J in Factions_List(I).Careers.Iterate loop
               if Factions_List(I).Careers(J).Name =
                 To_Unbounded_String(Get(ComboBox)) then
                  New_Game_Settings.Player_Career := Careers_Container.Key(J);
                  exit Find_Faction_Loop;
               end if;
            end loop Find_Career_Loop;
         end if;
      end loop Find_Faction_Loop;
      ComboBox.Name := New_String(".newgamemenu.canvas.player.base");
      Set_Starting_Base_Loop :
      for I in BasesTypes_List.Iterate loop
         if BasesTypes_List(I).Name = To_Unbounded_String(Get(ComboBox)) then
            New_Game_Settings.Starting_Base := BasesTypes_Container.Key(I);
            exit Set_Starting_Base_Loop;
         end if;
      end loop Set_Starting_Base_Loop;
      ComboBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.difficultylevel");
      New_Game_Settings.Difficulty_Level :=
        Difficulty_Type'Val(Natural'Value(Current(ComboBox)));
      New_Game_Settings.Enemy_Damage_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.playerdamage");
      New_Game_Settings.Player_Damage_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.enemymeleedamage");
      New_Game_Settings.Enemy_Melee_Damage_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name :=
        New_String(".newgamemenu.canvas.difficulty.playermeleedamage");
      New_Game_Settings.Player_Melee_Damage_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.experience");
      New_Game_Settings.Experience_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.reputation");
      New_Game_Settings.Reputation_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.upgrade");
      New_Game_Settings.Upgrade_Cost_Bonus :=
        Bonus_Type'Value(Get(SpinBox)) / 100.0;
      SpinBox.Name := New_String(".newgamemenu.canvas.difficulty.prices");
      New_Game_Settings.Prices_Bonus := Bonus_Type'Value(Get(SpinBox)) / 100.0;
      New_Game;
      StartGame;
      return TCL_OK;
   end New_Game_Command;

   -- ****o* MCommands/Show_Main_Menu_Command
   -- FUNCTION
   -- Clear the main game window and show main menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMainMenu
   -- SOURCE
   function Show_Main_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Main_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      Show_Main_Menu;
      return TCL_OK;
   end Show_Main_Menu_Command;

   procedure AddCommands is
   begin
      AddCommand("OpenLink", Open_Link_Command'Access);
      AddCommand("ShowFile", Show_File_Command'Access);
      AddCommand("ShowNews", Show_News_Command'Access);
      AddCommand("ShowHallOfFame", Show_Hall_Of_Fame_Command'Access);
      AddCommand("ShowLoadGame", Show_Load_Game_Command'Access);
      AddCommand("DeleteGame", Delete_Game_Command'Access);
      AddCommand("LoadGame", Load_Game_Command'Access);
      AddCommand("SetFaction", Set_Faction_Command'Access);
      AddCommand("SetCareer", Set_Career_Command'Access);
      AddCommand("SetBase", Set_Base_Command'Access);
      AddCommand("RandomName", Random_Name_Command'Access);
      AddCommand("NewGame", New_Game_Command'Access);
      AddCommand("ShowMainMenu", Show_Main_Menu_Command'Access);
   end AddCommands;

end MainMenu.Commands;
