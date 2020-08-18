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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
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
     (Message: Message_Data; MessagesView: Ttk_Tree_View;
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
           (MessagesView,
            "{} end -text {" & To_String(Message.Message) &
            To_String(MessageTag) & "}");
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
      Paned: Ttk_PanedWindow;
      MessagesCanvas: Tk_Canvas;
      MessagesFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      MessagesType: Message_Type := Default;
      MessagesView: Ttk_Tree_View;
      TypeBox: Ttk_ComboBox;
      SearchEntry: Ttk_Entry;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      MessagesFrame.Interp := Interp;
      MessagesFrame.Name := New_String(Widget_Image(Paned) & ".messagesframe");
      MessagesCanvas.Interp := Interp;
      MessagesCanvas.Name :=
        New_String(Widget_Image(MessagesFrame) & ".canvas");
      if Winfo_Get(MessagesCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "messages.tcl");
         Bind(MessagesFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(MessagesCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      if Argc > 1 then
         MessagesType := Message_Type'Val(Natural'Value(CArgv.Arg(Argv, 1)));
      else
         TypeBox.Interp := Interp;
         TypeBox.Name :=
           New_String
             (Widget_Image(MessagesCanvas) & ".messages.options.types");
         Current(TypeBox, "0");
      end if;
      SearchEntry.Interp := Interp;
      SearchEntry.Name :=
        New_String(Widget_Image(MessagesCanvas) & ".messages.options.search");
      Delete(SearchEntry, "0", "end");
      MessagesView.Interp := Interp;
      MessagesView.Name :=
        New_String(Widget_Image(MessagesCanvas) & ".messages.list.view");
      Delete(MessagesView, "[list " & Children(MessagesView, "{}") & "]");
      if MessagesAmount(MessagesType) = 0 then
         Insert
           (MessagesView,
            "{} end -text {There are no messages of that type.}");
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
         "[expr " & Winfo_Get(MessagesFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(MessagesFrame, "reqheight") & " / 2] -window " &
         Widget_Image(MessagesFrame));
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
      TypeBox: Ttk_ComboBox;
   begin
      TypeBox.Interp := Interp;
      TypeBox.Name :=
        New_String(".paned.messagesframe.canvas.messages.options.types");
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
      TypeBox: Ttk_ComboBox;
   begin
      if MessageBox
          ("-message {Are you sure you want to clear all messages?} -icon question -type yesno") /=
        "yes" then
         return TCL_OK;
      end if;
      ClearMessages;
      TypeBox.Interp := Interp;
      TypeBox.Name :=
        New_String(".paned.messagesframe.canvas.messages.options.types");
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
      TypeBox: Ttk_ComboBox;
      MessagesType: Message_Type;
      MessagesView: Ttk_Tree_View;
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      TypeBox.Interp := Interp;
      TypeBox.Name :=
        New_String(".paned.messagesframe.canvas.messages.options.types");
      MessagesType := Message_Type'Val(Natural'Value(Current(TypeBox)));
      MessagesView.Interp := Interp;
      MessagesView.Name :=
        New_String(".paned.messagesframe.canvas.messages.list.view");
      Delete(MessagesView, "[list " & Children(MessagesView, "{}") & "]");
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
            if Index(Message.Message, SearchText, 1) > 0 then
               ShowMessage(Message, MessagesView, MessagesType);
            end if;
         end loop;
      else
         for Message of reverse Messages_List loop
            if Index(Message.Message, SearchText, 1) > 0 then
               ShowMessage(Message, MessagesView, MessagesType);
            end if;
         end loop;
      end if;
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
