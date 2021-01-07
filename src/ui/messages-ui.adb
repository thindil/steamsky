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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Config; use Config;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Messages.UI is

   -- ****if* MUI2/ShowMessage
   -- FUNCTION
   -- Show the selected message to a player
   -- PARAMETERS
   -- Message      - The message to show
   -- MessagesView - The treeview in which the message will be shown
   -- MessagesType - The selected type of messages to show
   -- SOURCE
   procedure ShowMessage
     (Message: Message_Data; MessagesView: Tk_Text;
      MessagesType: Message_Type) is
      -- ****
      MessageTag: Unbounded_String;
   begin
      if Message.MType = MessagesType or MessagesType = Default then
         case Message.Color is
            when YELLOW =>
               MessageTag := To_Unbounded_String(" [list yellow]");
            when GREEN =>
               MessageTag := To_Unbounded_String(" [list green]");
            when RED =>
               MessageTag := To_Unbounded_String(" [list red]");
            when BLUE =>
               MessageTag := To_Unbounded_String(" [list blue]");
            when CYAN =>
               MessageTag := To_Unbounded_String(" [list cyan]");
            when others =>
               MessageTag := Null_Unbounded_String;
         end case;
         Insert
           (MessagesView, "end",
            "{" & To_String(Message.Message) & LF & "}" &
            To_String(MessageTag));
      end if;
   end ShowMessage;

   -- ****o* MUI2/Show_Last_Messages_Command
   -- FUNCTION
   -- Show the list of last messages to a player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLastMessages messagestype
   -- MessagesType is the type of messages to show, default all
   -- SOURCE
   function Show_Last_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Last_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      MessagesFrame: Ttk_Frame := Get_Widget(Paned & ".messagesframe", Interp);
      MessagesCanvas: constant Tk_Canvas :=
        Get_Widget(MessagesFrame & ".canvas", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      MessagesType: Message_Type := Default;
      MessagesView: constant Tk_Text :=
        Get_Widget(MessagesCanvas & ".messages.list.view", Interp);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(MessagesCanvas & ".messages.options.types", Interp);
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(MessagesCanvas & ".messages.options.search", Interp);
   begin
      if Winfo_Get(MessagesCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "messages.tcl");
         Bind(MessagesFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(MessagesCanvas, "ismapped") = "1" and Argc = 1 then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         Tcl_Eval(Interp, "InvokeButton " & CloseButton);
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      if Argc > 1 then
         MessagesType := Message_Type'Val(Natural'Value(CArgv.Arg(Argv, 1)));
      else
         Current(TypeBox, "0");
      end if;
      Delete(SearchEntry, "0", "end");
      configure(MessagesView, "-state normal");
      Delete(MessagesView, "1.0", "end");
      if MessagesAmount(MessagesType) = 0 then
         Insert(MessagesView, "end", "{There are no messages of that type.}");
      else
         if GameSettings.MessagesOrder = OLDER_FIRST then
            for Message of Messages_List loop
               ShowMessage(Message, MessagesView, MessagesType);
            end loop;
         else
            for Message of reverse Messages_List loop
               ShowMessage(Message, MessagesView, MessagesType);
            end loop;
         end if;
      end if;
      configure(MessagesView, "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      MessagesFrame.Name :=
        New_String(Widget_Image(MessagesCanvas) & ".messages");
      configure
        (MessagesCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (MessagesCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(MessagesFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (MessagesCanvas,
         "-scrollregion [list " & BBox(MessagesCanvas, "all") & "]");
      ShowScreen("messagesframe");
      return TCL_OK;
   end Show_Last_Messages_Command;

   -- ****o* MUI2/Select_Messages_Command
   -- FUNCTION
   -- Show only messages of the selected type
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SelectMessages
   -- SOURCE
   function Select_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Select_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.messagesframe.canvas.messages.options.types",
           Interp);
   begin
      return Show_Last_Messages_Command
          (ClientData, Interp, 2, Argv & Current(TypeBox));
   end Select_Messages_Command;

   -- ****o* MUI2/Delete_Messages_Command
   -- FUNCTION
   -- Delete all messages
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeleteMessages
   -- SOURCE
   function Delete_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.messagesframe.canvas.messages.options.types",
           Interp);
   begin
      if MessageBox
          ("-message {Are you sure you want to clear all messages?} -icon question -type yesno") /=
        "yes" then
         return TCL_OK;
      end if;
      ClearMessages;
      Current(TypeBox, "0");
      return TCL_OK;
   end Delete_Messages_Command;

   -- ****o* MUI2/Search_Messages_Command
   -- FUNCTION
   -- Show only this messages which contains the selected sequence
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchMessages text
   -- Text is the string to search in the messages
   -- SOURCE
   function Search_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Messages_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.messagesframe.canvas.messages.options.types",
           Interp);
      MessagesType: Message_Type;
      MessagesView: constant Tk_Text :=
        Get_Widget
          (".gameframe.paned.messagesframe.canvas.messages.list.view", Interp);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      MessagesType := Message_Type'Val(Natural'Value(Current(TypeBox)));
      configure(MessagesView, "-state normal");
      Delete(MessagesView, "1.0", "end");
      if SearchText'Length = 0 then
         if GameSettings.MessagesOrder = OLDER_FIRST then
            for Message of Messages_List loop
               ShowMessage(Message, MessagesView, MessagesType);
            end loop;
         else
            for Message of reverse Messages_List loop
               ShowMessage(Message, MessagesView, MessagesType);
            end loop;
         end if;
         Tcl_SetResult(Interp, "1");
         return TCL_OK;
      end if;
      if GameSettings.MessagesOrder = OLDER_FIRST then
         for Message of Messages_List loop
            if Index
                (To_Lower(To_String(Message.Message)), To_Lower(SearchText),
                 1) >
              0 then
               ShowMessage(Message, MessagesView, MessagesType);
            end if;
         end loop;
      else
         for Message of reverse Messages_List loop
            if Index
                (To_Lower(To_String(Message.Message)), To_Lower(SearchText),
                 1) >
              0 then
               ShowMessage(Message, MessagesView, MessagesType);
            end if;
         end loop;
      end if;
      configure(MessagesView, "-state disable");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Search_Messages_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowLastMessages", Show_Last_Messages_Command'Access);
      AddCommand("SelectMessages", Select_Messages_Command'Access);
      AddCommand("DeleteMessages", Delete_Messages_Command'Access);
      AddCommand("SearchMessages", Search_Messages_Command'Access);
   end AddCommands;

end Messages.UI;
