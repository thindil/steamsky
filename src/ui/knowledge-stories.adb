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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv; use CArgv;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Font; use Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with CoreUI; use CoreUI;
with Factions; use Factions;
with Items; use Items;
with Ships; use Ships;
with Stories; use Stories;
with Utils.UI; use Utils.UI;

package body Knowledge.Stories is

   -- ****o* KStories/KStories.Show_Story_Command
   -- FUNCTION
   -- Show the current story information
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStory
   -- SOURCE
   function Show_Story_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FrameName: constant String :=
        Main_Paned & ".knowledgeframe.stories.canvas.frame";
      StoryView: constant Tk_Text := Get_Widget(FrameName & ".view", Interp);
      StoryText: Unbounded_String;
      Tokens: Slice_Set;
      Step: Step_Data;
      StoryIndex: Positive;
      StoriesBox: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".options.titles", Interp);
      Button: Ttk_Button := Get_Widget(FrameName & ".options.show", Interp);
      Rows: Positive := 1;
      LineWidth: constant Positive :=
        (Positive'Value(Winfo_Get(StoriesBox, "reqwidth")) +
         Positive'Value(Winfo_Get(Button, "reqwidth"))) /
        Positive'Value(Measure("InterfaceFont", "{ }"));
   begin
      StoryIndex := Natural'Value(Current(StoriesBox)) + 1;
      configure(StoryView, "-state normal -width" & Positive'Image(LineWidth));
      Delete(StoryView, "1.0", "end");
      Story_Steps_Info_Loop:
      for StepText of FinishedStories(StoryIndex).StepsTexts loop
         Append(StoryText, StepText & LF);
         Rows := Rows + (Length(StepText) / LineWidth) + 1;
      end loop Story_Steps_Info_Loop;
      if Natural(FinishedStories(StoryIndex).StepsTexts.Length) <
        FinishedStories(StoryIndex).StepsAmount then
         Append(StoryText, GetCurrentStoryText & LF);
         Rows := Rows + (Length(GetCurrentStoryText & LF) / LineWidth) + 1;
         if CurrentStory.Data /= Null_Unbounded_String then
            Step :=
              (if
                 CurrentStory.CurrentStep = 0
               then
                 Stories_List(CurrentStory.Index).StartingStep
               elsif
                 CurrentStory.CurrentStep > 0
               then
                 Stories_List(CurrentStory.Index).Steps
                   (CurrentStory.CurrentStep)
               else Stories_List(CurrentStory.Index).FinalStep);
            Create(Tokens, To_String(CurrentStory.Data), ";");
            case Step.FinishCondition is
               when ASKINBASE =>
                  if Slice_Count(Tokens) < 2 then
                     Append
                       (StoryText,
                        "You must travel to base " &
                        CurrentStory.Data &
                        " at X:");
                     Base_Location_Loop:
                     for I in SkyBases'Range loop
                        if SkyBases(I).Name = CurrentStory.Data then
                           Append(StoryText, Positive'Image(SkyBases(I).SkyX));
                           Append(StoryText, " Y:");
                           Append(StoryText, Positive'Image(SkyBases(I).SkyY));
                           exit Base_Location_Loop;
                        end if;
                     end loop Base_Location_Loop;
                  else
                     Append(StoryText, "You can ask in any base. ");
                  end if;
               when DESTROYSHIP =>
                  Append
                    (StoryText,
                     "You must find " &
                     ProtoShips_List(To_Unbounded_String(Slice(Tokens, 3)))
                       .Name &
                     " at X:" &
                     Slice(Tokens, 1) &
                     " Y:" &
                     Slice(Tokens, 2));
               when EXPLORE =>
                  Append
                    (StoryText,
                     "You must travel to X:" &
                     Slice(Tokens, 1) &
                     " Y:" &
                     Slice(Tokens, 2));
               when LOOT =>
                  Append
                    (StoryText,
                     "You must loot: " &
                     Items_List(To_Unbounded_String((Slice(Tokens, 1)))).Name &
                     " from ");
                  if Slice(Tokens, 2) = "any" then
                     Append(StoryText, "any ");
                     if Factions_Container.Contains
                         (Factions_List,
                          GetStepData(Step.FinishData, "faction")) then
                        Append
                          (StoryText,
                           Factions_List
                             (GetStepData(Step.FinishData, "faction"))
                             .Name);
                     end if;
                     Append(StoryText, " ship.");
                  else
                     Find_Proto_Ship_Loop:
                     for I in ProtoShips_List.Iterate loop
                        if ProtoShips_Container.Key(I) =
                          To_Unbounded_String(Slice(Tokens, 2)) then
                           Append(StoryText, ProtoShips_List(I).Name);
                           Append(StoryText, ".");
                           exit Find_Proto_Ship_Loop;
                        end if;
                     end loop Find_Proto_Ship_Loop;
                  end if;
               when ANY =>
                  null;
            end case;
         end if;
         Insert(StoryView, "end", "{" & To_String(StoryText) & "}");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name :=
           New_String
             (".gameframe.paned.knowledgeframe.stories.canvas.frame.options.set");
         Tcl.Tk.Ada.Grid.Grid(Button);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name :=
           New_String
             (".gameframe.paned.knowledgeframe.stories.canvas.frame.options.set");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      end if;
      configure(StoryView, "-state disabled -height" & Positive'Image(Rows));
      return TCL_OK;
   end Show_Story_Command;

   -- ****o* KStories/KStories.Show_Story_Location_Command
   -- FUNCTION
   -- Show the current story event on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStoryLocation
   -- SOURCE
   function Show_Story_Location_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Location_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      NewX, NewY: Positive := 1;
   begin
      GetStoryLocation(NewX, NewY);
      return Show_On_Map_Command
          (ClientData,
           Interp,
           3,
           CArgv.Empty &
           CArgv.Arg(Argv, 0) &
           Positive'Image(NewX) &
           Positive'Image(NewY));
   end Show_Story_Location_Command;

   -- ****o* KStories/KStories.Set_Story_Command
   -- FUNCTION
   -- Set the current story event as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetStory
   -- SOURCE
   function Set_Story_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Story_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      NewX, NewY: Positive := 1;
   begin
      GetStoryLocation(NewX, NewY);
      return Set_Destination_Command
          (ClientData,
           Interp,
           3,
           CArgv.Empty &
           CArgv.Arg(Argv, 0) &
           Positive'Image(NewX) &
           Positive'Image(NewY));
   end Set_Story_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowStory", Show_Story_Command'Access);
      AddCommand("ShowStoryLocation", Show_Story_Location_Command'Access);
      AddCommand("SetStory", Set_Story_Command'Access);
   end AddCommands;

end Knowledge.Stories;
