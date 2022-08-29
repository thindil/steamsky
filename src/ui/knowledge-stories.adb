-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl; use Tcl;
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
with Game; use Game;
with Items; use Items;
with Ships; use Ships;
with Stories; use Stories;
with Utils.UI; use Utils.UI;

package body Knowledge.Stories is

   -- ****o* KStories/KStories.Show_Story_Command
   -- FUNCTION
   -- Show the current story information
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStory
   -- SOURCE
   function Show_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Frame_Name: constant String :=
        Main_Paned & ".knowledgeframe.stories.canvas.frame";
      Story_View: constant Tk_Text :=
        Get_Widget(pathName => Frame_Name & ".view", Interp => Interp);
      Story_Text: Unbounded_String;
      Tokens: Slice_Set;
      Step: Step_Data;
      Story_Index: Positive;
      Stories_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Frame_Name & ".options.titles", Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget(pathName => Frame_Name & ".options.show", Interp => Interp);
      Rows: Positive := 1;
      Line_Width: constant Positive :=
        (Positive'Value(Winfo_Get(Widgt => Stories_Box, Info => "reqwidth")) +
         Positive'Value(Winfo_Get(Widgt => Button, Info => "reqwidth"))) /
        Positive'Value(Measure(Font => "InterfaceFont", Text => "{ }"));
   begin
      Story_Index := Natural'Value(Current(ComboBox => Stories_Box)) + 1;
      configure
        (Widgt => Story_View,
         options => "-state normal -width" & Positive'Image(Line_Width));
      Delete(TextWidget => Story_View, StartIndex => "1.0", Indexes => "end");
      Story_Steps_Info_Loop :
      for StepText of Finished_Stories(Story_Index).Steps_Texts loop
         Append(Source => Story_Text, New_Item => StepText & LF);
         Rows := Rows + (Length(Source => StepText) / Line_Width) + 1;
      end loop Story_Steps_Info_Loop;
      if Natural(Finished_Stories(Story_Index).Steps_Texts.Length) <
        Finished_Stories(Story_Index).Steps_Amount then
         Append(Source => Story_Text, New_Item => Get_Current_Story_Text & LF);
         Rows :=
           Rows +
           (Length(Source => Get_Current_Story_Text & LF) / Line_Width) + 1;
         if Current_Story.Data /= Null_Unbounded_String then
            Step :=
              (if Current_Story.Current_Step = 0 then
                 Stories_List(Current_Story.Index).Starting_Step
               elsif Current_Story.Current_Step > 0 then
                 Stories_List(Current_Story.Index).Steps
                   (Current_Story.Current_Step)
               else Stories_List(Current_Story.Index).Final_Step);
            Create
              (S => Tokens, From => To_String(Source => Current_Story.Data),
               Separators => ";");
            case Step.Finish_Condition is
               when ASKINBASE =>
                  if Slice_Count(S => Tokens) < 2 then
                     Append
                       (Source => Story_Text,
                        New_Item =>
                          "You must travel to base " & Current_Story.Data &
                          " at X:");
                     Base_Location_Loop :
                     for I in Sky_Bases'Range loop
                        if Tiny_String.To_String(Source => Sky_Bases(I).Name) =
                          To_String(Source => Current_Story.Data) then
                           Append
                             (Source => Story_Text,
                              New_Item => Positive'Image(Sky_Bases(I).Sky_X));
                           Append(Source => Story_Text, New_Item => " Y:");
                           Append
                             (Source => Story_Text, New_Item => Positive'Image(Sky_Bases(I).Sky_Y));
                           exit Base_Location_Loop;
                        end if;
                     end loop Base_Location_Loop;
                  else
                     Append(Source => Story_Text, New_Item => "You can ask in any base. ");
                  end if;
               when DESTROYSHIP =>
                  Append
                    (Source => Story_Text,
                     New_Item => "You must find " &
                     To_String
                       (Source =>
                          Proto_Ships_List(Positive'Value(Slice(S => Tokens, Index => 3)))
                            .Name) &
                     " at X:" & Slice(S => Tokens, Index => 1) & " Y:" & Slice(S => Tokens, Index => 2));
               when EXPLORE =>
                  Append
                    (Source => Story_Text,
                     New_Item => "You must travel to X:" & Slice(S => Tokens, Index => 1) & " Y:" &
                     Slice(S => Tokens, Index => 2));
               when LOOT =>
                  Append
                    (Source => Story_Text,
                     New_Item => "You must loot: " &
                     To_String
                       (Source =>
                          Objects_Container.Element
                            (Container => Items_List,
                             Index => Positive'Value((Slice(S => Tokens, Index => 1))))
                            .Name) &
                     " from ");
                  if Slice(S => Tokens, Index => 2) = "any" then
                     Append(Source => Story_Text, New_Item => "any ");
                     if Factions_Container.Contains
                         (Container => Factions_List,
                          Key => To_Bounded_String
                            (Source =>
                               To_String
                                 (Source =>
                                    Get_Step_Data
                                      (Finish_Data => Step.Finish_Data, Name => "faction")))) then
                        Append
                          (Source => Story_Text,
                           New_Item => To_String
                             (Source =>
                                Factions_List
                                  (To_Bounded_String
                                     (Source =>
                                        To_String
                                          (Source =>
                                             Get_Step_Data
                                               (Finish_Data => Step.Finish_Data, Name => "faction"))))
                                  .Name));
                     end if;
                     Append(Source => Story_Text, New_Item => " ship.");
                  else
                     Find_Proto_Ship_Loop :
                     for I in Proto_Ships_List.Iterate loop
                        if Proto_Ships_Container.To_Index(Position => I) =
                          Positive'Value(Slice(Tokens, 2)) then
                           Append
                             (Story_Text,
                              To_String(Source => Proto_Ships_List(I).Name));
                           Append(Story_Text, ".");
                           exit Find_Proto_Ship_Loop;
                        end if;
                     end loop Find_Proto_Ship_Loop;
                  end if;
               when ANY =>
                  null;
            end case;
         end if;
         Insert(Story_View, "end", "{" & To_String(Story_Text) & "}");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(Frame_Name & ".options.set");
         Tcl.Tk.Ada.Grid.Grid(Button);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name := New_String(Frame_Name & ".options.set");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      end if;
      configure(Story_View, "-state disabled -height" & Positive'Image(Rows));
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Location_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      NewX, NewY: Positive := 1;
   begin
      Get_Story_Location(NewX, NewY);
      return
        Show_On_Map_Command
          (ClientData, Interp, 3,
           CArgv.Empty & CArgv.Arg(Argv, 0) & Positive'Image(NewX) &
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Story_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      NewX, NewY: Positive := 1;
   begin
      Get_Story_Location(NewX, NewY);
      return
        Set_Destination_Command
          (ClientData, Interp, 3,
           CArgv.Empty & CArgv.Arg(Argv, 0) & Positive'Image(NewX) &
           Positive'Image(NewY));
   end Set_Story_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowStory", Show_Story_Command'Access);
      Add_Command("ShowStoryLocation", Show_Story_Location_Command'Access);
      Add_Command("SetStory", Set_Story_Command'Access);
   end Add_Commands;

end Knowledge.Stories;
