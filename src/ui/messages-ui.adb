-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs;
with Utils.UI; use Utils.UI;

package body Messages.UI is

   -- ****if* MUI2/MUI2.Show_Message
   -- FUNCTION
   -- Show the selected message to a player
   -- PARAMETERS
   -- Message       - The message to show
   -- Messages_View - The treeview in which the message will be shown
   -- Messages_Type - The selected type of messages to show
   -- SOURCE
   procedure Show_Message
     (Message: Message_Data; Messages_View: Tk_Text;
      Messages_Type: Message_Type) is
      -- ****
      use Interfaces.C.Strings;

      procedure Show_Ada_Message
        (M, M_View: chars_ptr; Color, M_Type, Me_Type: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaMessageUI";
   begin
      Show_Ada_Message
        (M => New_String(Str => To_String(Source => Message.Message)),
         M_View => New_String(Str => Widget_Image(Win => Messages_View)),
         Color => Message_Color'Pos(Message.Color),
         M_Type => Message_Type'Pos(Message.M_Type),
         Me_Type => Message_Type'Pos(Messages_Type));
   end Show_Message;

   -- ****o* MUI2/MUI2.Show_Last_Messages_Command
   -- FUNCTION
   -- Show the list of last messages to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLastMessages messagestype
   -- MessagesType is the type of messages to show, default all
   -- SOURCE
   function Show_Last_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Last_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Interfaces.C.Strings;
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkEntry;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Winfo;

      Messages_Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".messagesframe", Interp => Interp);
      Messages_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Messages_Frame & ".canvas", Interp => Interp);
      Messages_Type: constant Message_Type :=
        (if Argc = 1 then DEFAULT
         else Message_Type'Val
             (Natural'Value(CArgv.Arg(Argv => Argv, N => 1))));
      Messages_View: constant Tk_Text :=
        Get_Widget
          (pathName => Messages_Canvas & ".messages.list.view",
           Interp => Interp);
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Messages_Canvas & ".messages.options.types",
           Interp => Interp);
      Search_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => Messages_Canvas & ".messages.options.search",
           Interp => Interp);
   begin
      if Winfo_Get(Widgt => Messages_Canvas, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "messages.tcl");
         Bind
           (Widgt => Messages_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Widgt => Messages_Canvas, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         return TCL_OK;
      end if;
      if Argc = 1 then
         Current(ComboBox => Type_Box, NewIndex => "0");
      end if;
      Delete(TextEntry => Search_Entry, FirstIndex => "0", LastIndex => "end");
      configure(Widgt => Messages_View, options => "-state normal");
      Delete
        (TextWidget => Messages_View, StartIndex => "1.0", Indexes => "end");
      if Messages_Amount(M_Type => Messages_Type) = 0 then
         Insert
           (TextWidget => Messages_View, Index => "end",
            Text => "{There are no messages of that type.}");
      else
         if Get_Messages_Order = OLDER_FIRST then
            Show_Older_First_Loop :
            for I in 1 .. Messages_Amount loop
               Show_Message
                 (Message => Get_Message(Message_Index => I),
                  Messages_View => Messages_View,
                  Messages_Type => Messages_Type);
            end loop Show_Older_First_Loop;
         else
            Show_Newer_First_Loop :
            for I in reverse 1 .. Messages_Amount loop
               Show_Message
                 (Message => Get_Message(Message_Index => I),
                  Messages_View => Messages_View,
                  Messages_Type => Messages_Type);
            end loop Show_Newer_First_Loop;
         end if;
      end if;
      configure(Widgt => Messages_View, options => "-state disabled");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      Messages_Frame.Name :=
        New_String(Str => Widget_Image(Win => Messages_Canvas) & ".messages");
      configure
        (Widgt => Messages_Canvas,
         options =>
           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => Messages_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Messages_Frame));
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Messages_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Messages_Canvas, TagOrId => "all") & "]");
      Show_Screen(New_Screen_Name => "messagesframe");
      return TCL_OK;
   end Show_Last_Messages_Command;

   -- ****o* MUI2/MUI2.Select_Messages_Command
   -- FUNCTION
   -- Show only messages of the selected type
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SelectMessages
   -- SOURCE
   function Select_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Select_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".messagesframe.canvas.messages.options.types",
           Interp => Interp);
   begin
      return
        Show_Last_Messages_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => Argv & Current(ComboBox => Type_Box));
   end Select_Messages_Command;

   -- ****o* MUI2/MUI2.Delete_Messages_Command
   -- FUNCTION
   -- Delete all messages
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeleteMessages
   -- SOURCE
   function Delete_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      use Dialogs;

   begin
      Show_Question
        (Question => "Are you sure you want to clear all messages?",
         Result => "messages");
      return TCL_OK;
   end Delete_Messages_Command;

   -- ****o* MUI2/MUI2.Search_Messages_Command
   -- FUNCTION
   -- Show only this messages which contains the selected sequence
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchMessages text
   -- Text is the string to search in the messages
   -- SOURCE
   function Search_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;

      Frame_Name: constant String :=
        Main_Paned & ".messagesframe.canvas.messages";
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Frame_Name & ".options.types", Interp => Interp);
      Messages_Type: Message_Type;
      Messages_View: constant Tk_Text :=
        Get_Widget(pathName => Frame_Name & ".list.view", Interp => Interp);
      Search_Text: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      Messages_Type :=
        Message_Type'Val(Natural'Value(Current(ComboBox => Type_Box)));
      configure(Widgt => Messages_View, options => "-state normal");
      Delete
        (TextWidget => Messages_View, StartIndex => "1.0", Indexes => "end");
      if Search_Text'Length = 0 then
         if Get_Messages_Order = OLDER_FIRST then
            Show_Older_First_Loop :
            for I in 1 .. Messages_Amount loop
               Show_Message
                 (Message => Get_Message(Message_Index => I),
                  Messages_View => Messages_View,
                  Messages_Type => Messages_Type);
            end loop Show_Older_First_Loop;
         else
            Show_Newer_First_Loop :
            for I in reverse 1 .. Messages_Amount loop
               Show_Message
                 (Message => Get_Message(Message_Index => I),
                  Messages_View => Messages_View,
                  Messages_Type => Messages_Type);
            end loop Show_Newer_First_Loop;
         end if;
         Tcl_SetResult(interp => Interp, str => "1");
         return TCL_OK;
      end if;
      if Get_Messages_Order = OLDER_FIRST then
         Search_Older_First_Loop :
         for I in 1 .. Messages_Amount loop
            Show_Selected_Messages_Block :
            declare
               Message: constant Message_Data :=
                 Get_Message(Message_Index => I);
            begin
               if Index
                   (Source =>
                      To_Lower(Item => To_String(Source => Message.Message)),
                    Pattern => To_Lower(Item => Search_Text), From => 1) >
                 0 then
                  Show_Message
                    (Message => Message, Messages_View => Messages_View,
                     Messages_Type => Messages_Type);
               end if;
            end Show_Selected_Messages_Block;
         end loop Search_Older_First_Loop;
      else
         Search_Newer_First_Loop :
         for I in reverse 1 .. Messages_Amount loop
            Show_Selected_Reverse_Messages_Block :
            declare
               Message: constant Message_Data :=
                 Get_Message(Message_Index => I);
            begin
               if Index
                   (Source =>
                      To_Lower(Item => To_String(Source => Message.Message)),
                    Pattern => To_Lower(Item => Search_Text), From => 1) >
                 0 then
                  Show_Message
                    (Message => Message, Messages_View => Messages_View,
                     Messages_Type => Messages_Type);
               end if;
            end Show_Selected_Reverse_Messages_Block;
         end loop Search_Newer_First_Loop;
      end if;
      configure(Widgt => Messages_View, options => "-state disable");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Search_Messages_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowLastMessages",
         Ada_Command => Show_Last_Messages_Command'Access);
      Add_Command
        (Name => "SelectMessages",
         Ada_Command => Select_Messages_Command'Access);
      Add_Command
        (Name => "DeleteMessages",
         Ada_Command => Delete_Messages_Command'Access);
      Add_Command
        (Name => "SearchMessages",
         Ada_Command => Search_Messages_Command'Access);
   end Add_Commands;

end Messages.UI;
