-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Utils.UI; use Utils.UI;

package body Help.UI is

   -- ****f* HUI/Show_Topic_Command
   -- FUNCTION
   -- Show the content of the selected topic help
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Topic_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Topic_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      NewText, TagText: Unbounded_String;
      StartIndex, EndIndex, OldIndex, TopicIndex: Natural;
      type Variables_Data is record
         Name: Unbounded_String;
         Value: Unbounded_String;
      end record;
      Variables: constant array(Positive range <>) of Variables_Data :=
        (1 => (Name => To_Unbounded_String("MoneyName"), Value => MoneyName),
         2 =>
           (Name => To_Unbounded_String("FuelName"),
            Value => Items_List(FindProtoItem(ItemType => FuelType)).Name),
         3 =>
           (Name => To_Unbounded_String("StrengthName"),
            Value => Attributes_List(StrengthIndex).Name),
         4 =>
           (Name => To_Unbounded_String("PilotingSkill"),
            Value => Skills_List(PilotingSkill).Name),
         5 =>
           (Name => To_Unbounded_String("EngineeringSkill"),
            Value => Skills_List(EngineeringSkill).Name),
         6 =>
           (Name => To_Unbounded_String("GunnerySkill"),
            Value => Skills_List(GunnerySkill).Name),
         7 =>
           (Name => To_Unbounded_String("TalkingSkill"),
            Value => Skills_List(TalkingSkill).Name),
         8 =>
           (Name => To_Unbounded_String("PerceptionSkill"),
            Value => Skills_List(PerceptionSkill).Name),
         9 =>
           (Name => To_Unbounded_String("ConditionName"),
            Value => Attributes_List(ConditionIndex).Name),
         10 =>
           (Name => To_Unbounded_String("DodgeSkill"),
            Value => Skills_List(DodgeSkill).Name),
         11 =>
           (Name => To_Unbounded_String("UnarmedSkill"),
            Value => Skills_List(UnarmedSkill).Name));
--      AccelNames: constant array(Positive range <>) of Unbounded_String :=
--        (To_Unbounded_String("<skymapwindow>/btnupleft"),
--         To_Unbounded_String("<skymapwindow>/btnup"),
--         To_Unbounded_String("<skymapwindow>/btnupright"),
--         To_Unbounded_String("<skymapwindow>/btnleft"),
--         To_Unbounded_String("<skymapwindow>/btnmovewait"),
--         To_Unbounded_String("<skymapwindow>/btnright"),
--         To_Unbounded_String("<skymapwindow>/btnbottomleft"),
--         To_Unbounded_String("<skymapwindow>/btnbottom"),
--         To_Unbounded_String("<skymapwindow>/btnbottomright"),
--         To_Unbounded_String("<skymapwindow>/btnmoveto"),
--         To_Unbounded_String("<skymapwindow>/Menu/ShipInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/ShipCargoInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/CrewInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/ShipOrders"),
--         To_Unbounded_String("<skymapwindow>/Menu/CraftInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/MessagesInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/BasesInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/EventsInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/MissionsInfo"),
--         To_Unbounded_String("<skymapwindow>/Menu/MoveMap"),
--         To_Unbounded_String("<skymapwindow>/Menu/GameStats"),
--         To_Unbounded_String("<skymapwindow>/Menu/Help"),
--         To_Unbounded_String("<skymapwindow>/Menu/GameOptions"),
--         To_Unbounded_String("<skymapwindow>/Menu/QuitGame"),
--         To_Unbounded_String("<skymapwindow>/Menu/ResignFromGame"),
--         To_Unbounded_String("<skymapwindow>/Menu"),
--         To_Unbounded_String("<skymapwindow>/Menu/WaitOrders"),
--         To_Unbounded_String("<skymapwindow>/zoomin"),
--         To_Unbounded_String("<skymapwindow>/zoomout"));
      type FontTag is record
         Tag: String(1 .. 1);
         TextTag: Unbounded_String;
      end record;
      FontTags: constant array(Positive range <>) of FontTag :=
        (1 => (Tag => "b", TextTag => To_Unbounded_String("bold")),
         2 => (Tag => "u", TextTag => To_Unbounded_String("underline")),
         3 => (Tag => "i", TextTag => To_Unbounded_String("italic")));
      FlagsTags: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("diseaseimmune"),
         To_Unbounded_String("nofatigue"), To_Unbounded_String("nomorale"),
         To_Unbounded_String("naturalarmor"),
         To_Unbounded_String("toxicattack"),
         To_Unbounded_String("sentientships"),
         To_Unbounded_String("fanaticism"), To_Unbounded_String("loner"));
      FactionsWithFlag: Unbounded_String;
      BasesFlags: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("shipyard"), To_Unbounded_String("temple"),
         To_Unbounded_String("blackmarket"), To_Unbounded_String("barracks"));
      BasesWithFlag: Unbounded_String;
      TopicsView: Ttk_Tree_View;
      SelectedIndex: Positive;
      HelpView: Tk_Text;
   begin
      TopicsView.Interp := Interp;
      TopicsView.Name := New_String(".help.paned.topics.view");
      HelpView.Interp := Interp;
      HelpView.Name := New_String(".help.paned.content.view");
      configure(HelpView, "-state normal");
      Delete(HelpView, "1.0", "end");
      SelectedIndex := Positive'Value(Selection(TopicsView));
      TopicIndex := 1;
      for Help of Help_List loop
         if TopicIndex = SelectedIndex then
            NewText := Help.Text;
            exit;
         end if;
         TopicIndex := TopicIndex + 1;
      end loop;
      TopicIndex := 0;
      OldIndex := 1;
      loop
         StartIndex := Index(NewText, "{", OldIndex);
         if StartIndex > 0 then
            Insert
              (HelpView, "end",
               "{" & Slice(NewText, OldIndex, StartIndex - 1) & "}");
         else
            Insert
              (HelpView, "end",
               "{" & Slice(NewText, OldIndex, Length(NewText)) & "}");
            exit;
         end if;
         EndIndex := Index(NewText, "}", StartIndex) - 1;
         TagText := Unbounded_Slice(NewText, StartIndex + 1, EndIndex);
         for I in Variables'Range loop
            if TagText = Variables(I).Name then
               Insert
                 (HelpView, "end",
                  "{" & To_String(Variables(I).Value) & "} [list special]");
               exit;
            end if;
         end loop;
--         -- TODO: Keyboard shortcuts
--         for I in AccelNames'Range loop
--            if TagText =
--              To_Unbounded_String("GameKey") &
--                To_Unbounded_String(Positive'Image(I)) then
--               Lookup_Entry(To_String(AccelNames(I)), Key, Found);
--               Insert_With_Tags
--                 (HelpBuffer, Iter,
--                  "'" & Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods) &
--                  "'",
--                  SpecialText);
--               exit;
--            end if;
--         end loop;
         for I in FontTags'Range loop
            if TagText = To_Unbounded_String(FontTags(I).Tag) then
               StartIndex := Index(NewText, "{", EndIndex) - 1;
               Insert
                 (HelpView, "end",
                  "{" & Slice(NewText, EndIndex + 2, StartIndex) & "} [list " &
                  To_String(FontTags(I).TextTag) & "]");
               EndIndex := Index(NewText, "}", StartIndex) - 1;
               exit;
            end if;
         end loop;
         for I in FlagsTags'Range loop
            if TagText = FlagsTags(I) then
               FactionsWithFlag := Null_Unbounded_String;
               for Faction of Factions_List loop
                  if Faction.Flags.Contains(TagText) then
                     if FactionsWithFlag /= Null_Unbounded_String then
                        Append(FactionsWithFlag, " and ");
                     end if;
                     Append(FactionsWithFlag, Faction.Name);
                  end if;
               end loop;
               while Ada.Strings.Unbounded.Count(FactionsWithFlag, " and ") >
                 1 loop
                  Replace_Slice
                    (FactionsWithFlag, Index(FactionsWithFlag, " and "),
                     Index(FactionsWithFlag, " and ") + 4, ", ");
               end loop;
               Insert
                 (HelpView, "end", "{" & To_String(FactionsWithFlag) & "}");
               exit;
            end if;
         end loop;
         for BaseFlag of BasesFlags loop
            if TagText /= BaseFlag then
               goto Bases_Flags_Loop_End;
            end if;
            BasesWithFlag := Null_Unbounded_String;
            for BaseType of BasesTypes_List loop
               if BaseType.Flags.Contains(TagText) then
                  if BasesWithFlag /= Null_Unbounded_String then
                     Append(BasesWithFlag, " and ");
                  end if;
                  Append(BasesWithFlag, BaseType.Name);
               end if;
            end loop;
            while Ada.Strings.Unbounded.Count(BasesWithFlag, " and ") > 1 loop
               Replace_Slice
                 (BasesWithFlag, Index(BasesWithFlag, " and "),
                  Index(BasesWithFlag, " and ") + 4, ", ");
            end loop;
            Insert(HelpView, "end", "{" & To_String(BasesWithFlag) & "}");
            exit;
            <<Bases_Flags_Loop_End>>
         end loop;
         OldIndex := EndIndex + 2;
      end loop;
      configure(HelpView, "-state disabled");
      return TCL_OK;
   end Show_Topic_Command;

   -- ****f* HUI/Show_Help_Command
   -- FUNCTION
   -- Show help window to the playera
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      HelpWindow: Tk_Toplevel;
      X, Y: Integer;
      Paned: Ttk_PanedWindow;
      TopicsView: Ttk_Tree_View;
      TopicIndex: Positive := 1;
   begin
      Tcl_EvalFile
        (Interp, To_String(DataDirectory) & "ui" & Dir_Separator & "help.tcl");
      HelpWindow.Interp := Interp;
      HelpWindow.Name := New_String(".help");
      X :=
        (Positive'Value(Winfo_Get(HelpWindow, "vrootwidth")) -
         GameSettings.WindowWidth) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(HelpWindow, "vrootheight")) -
         GameSettings.WindowHeight) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (HelpWindow, "geometry",
         Trim(Positive'Image(GameSettings.WindowWidth), Left) & "x" &
         Trim(Positive'Image(GameSettings.WindowHeight), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      Tcl_Eval(Interp, "update");
      Paned.Interp := Interp;
      Paned.Name := New_String(".help.paned");
      SashPos(Paned, "0", Natural'Image(GameSettings.TopicsPosition));
      TopicsView.Interp := Interp;
      TopicsView.Name := New_String(".help.paned.topics.view");
      for I in Help_List.Iterate loop
         Insert
           (TopicsView,
            "{} end -id" & Positive'Image(TopicIndex) & " -text {" &
            To_String(Help_Container.Key(I)) & "}");
         TopicIndex := TopicIndex + 1;
      end loop;
      Bind(TopicsView, "<<TreeviewSelect>>", "ShowTopic");
      Selection_Set(TopicsView, CArgv.Arg(Argv, 1));
      return TCL_OK;
   end Show_Help_Command;

   -- ****f* HUI/Close_Help_Command
   -- FUNCTION
   -- Destroy help window and save sash position to the game configuration
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Close_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Help_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HelpWindow: Tk_Toplevel;
      Paned: Ttk_PanedWindow;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".help.paned");
      GameSettings.TopicsPosition := Natural'Value(SashPos(Paned, "0"));
      HelpWindow.Interp := Interp;
      HelpWindow.Name := New_String(".help");
      Destroy(HelpWindow);
      return TCL_OK;
   end Close_Help_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowTopic", Show_Topic_Command'Access);
      AddCommand("ShowHelp", Show_Help_Command'Access);
      AddCommand("CloseHelp", Close_Help_Command'Access);
   end AddCommands;

end Help.UI;
