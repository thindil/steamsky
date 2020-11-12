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
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Help.UI is

   -- ****o* HUI/Show_Topic_Command
   -- FUNCTION
   -- Show the content of the selected topic help
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTopic
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
      StartIndex, EndIndex, OldIndex: Natural;
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
      AccelNames: constant array(1 .. 25) of Unbounded_String :=
        (MapAccelerators(5), MapAccelerators(6), MapAccelerators(7),
         MapAccelerators(8), MapAccelerators(9), MapAccelerators(10),
         MapAccelerators(11), MapAccelerators(12), MapAccelerators(13),
         MapAccelerators(14), MenuAccelerators(1), MenuAccelerators(2),
         MenuAccelerators(3), MenuAccelerators(4), MenuAccelerators(5),
         MenuAccelerators(6), MapAccelerators(2), MenuAccelerators(7),
         MenuAccelerators(9), MenuAccelerators(10), MenuAccelerators(11),
         MapAccelerators(1), MenuAccelerators(8), MapAccelerators(3),
         MapAccelerators(4));
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
      TopicsView: constant Ttk_Tree_View :=
        Get_Widget(".help.paned.topics.view", Interp);
      HelpView: constant Tk_Text :=
        Get_Widget(".help.paned.content.view", Interp);
   begin
      configure(HelpView, "-state normal");
      Delete(HelpView, "1.0", "end");
      for Help of Help_List loop
         if Help.Index = To_Unbounded_String(Selection(TopicsView)) then
            NewText := Help.Text;
            exit;
         end if;
      end loop;
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
         for I in AccelNames'Range loop
            if TagText =
              To_Unbounded_String("GameKey") &
                To_Unbounded_String(Positive'Image(I)) then
               Insert
                 (HelpView, "end",
                  "{'" & To_String(AccelNames(I)) & "'} [list special]");
               exit;
            end if;
         end loop;
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

   -- ****o* HUI/Show_Help_Command
   -- FUNCTION
   -- Show help window to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowHelp topicindex
   -- Topicindex is the index of the help topic which content will be show
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
      HelpWindow: constant Tk_Toplevel := Get_Widget(".help", Interp);
      X, Y: Integer;
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(HelpWindow & ".gameframe.paned", Interp);
      TopicsView: constant Ttk_Tree_View :=
        Get_Widget(Paned & ".topics.view", Interp);
   begin
      Tcl_EvalFile
        (Interp, To_String(DataDirectory) & "ui" & Dir_Separator & "help.tcl");
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
      SashPos(Paned, "0", Natural'Image(GameSettings.TopicsPosition));
      for I in Help_List.Iterate loop
         Insert
           (TopicsView,
            "{} end -id {" & To_String(Help_List(I).Index) & "} -text {" &
            To_String(Help_Container.Key(I)) & "}");
      end loop;
      Bind(TopicsView, "<<TreeviewSelect>>", "ShowTopic");
      Selection_Set(TopicsView, CArgv.Arg(Argv, 1));
      return TCL_OK;
   end Show_Help_Command;

   -- ****o* HUI/Close_Help_Command
   -- FUNCTION
   -- Destroy help window and save sash position to the game configuration
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseHelp
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
      HelpWindow: Tk_Toplevel := Get_Widget(".help", Interp);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(HelpWindow & ".gameframe.paned", Interp);
   begin
      GameSettings.TopicsPosition := Natural'Value(SashPos(Paned, "0"));
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
