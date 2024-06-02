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

-- with Ada.Characters.Handling;
-- with Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada;
-- with Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
-- with Tcl.Tk.Ada.Widgets.TtkEntry;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
-- with Config;
-- with CoreUI;
with Utils.UI;

package body Messages.UI is

   --## rule off REDUCEABLE_SCOPE
   -- ****if* MUI2/MUI2.Show_Message
   -- FUNCTION
   -- Show the selected message to a player
   -- PARAMETERS
   -- Message       - The message to show
   -- Messages_View - The treeview in which the message will be shown
   -- Messages_Type - The selected type of messages to show
   -- SOURCE
--   procedure Show_Message
--     (Message: Message_Data; Messages_View: Tk_Text;
--      Messages_Type: Message_Type) is
--      -- ****
--      use Interfaces.C.Strings;
--
--      procedure Show_Ada_Message
--        (M, M_View: chars_ptr; Color, M_Type, Me_Type: Integer) with
--         Import => True,
--         Convention => C,
--         External_Name => "showAdaMessageUI";
--   begin
--      Show_Ada_Message
--        (M => New_String(Str => To_String(Source => Message.Message)),
--         M_View => New_String(Str => Widget_Image(Win => Messages_View)),
--         Color => Message_Color'Pos(Message.Color),
--         M_Type => Message_Type'Pos(Message.M_Type),
--         Me_Type => Message_Type'Pos(Messages_Type));
--   end Show_Message;
   --## rule on REDUCEABLE_SCOPE

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
      Import => True,
      Convention => C,
      External_Name => "showLastMessagesCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "selectMessagesCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "deleteMessagesCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "deleteMessagesCommand";
      -- ****

--   function Search_Messages_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc);
--      use Ada.Characters.Handling;
--      use Ada.Strings.Fixed;
--      use Tcl.Ada;
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
--      use Config;
--      use CoreUI;
--
--      Frame_Name: constant String :=
--        Main_Paned & ".messagesframe.canvas.messages";
--      Type_Box: constant Ttk_ComboBox :=
--        Get_Widget
--          (pathName => Frame_Name & ".options.types", Interp => Interp);
--      Messages_Type: Message_Type;
--      Messages_View: constant Tk_Text :=
--        Get_Widget(pathName => Frame_Name & ".list.view", Interp => Interp);
--      Search_Text: constant String := CArgv.Arg(Argv => Argv, N => 1);
--   begin
--      Messages_Type :=
--        Message_Type'Val(Natural'Value(Current(ComboBox => Type_Box)));
--      configure(Widgt => Messages_View, options => "-state normal");
--      Delete
--        (TextWidget => Messages_View, StartIndex => "1.0", Indexes => "end");
--      if Search_Text'Length = 0 then
--         if Get_Messages_Order = OLDER_FIRST then
--            Show_Older_First_Loop :
--            for I in 1 .. Messages_Amount loop
--               Show_Message
--                 (Message => Get_Message(Message_Index => I),
--                  Messages_View => Messages_View,
--                  Messages_Type => Messages_Type);
--            end loop Show_Older_First_Loop;
--         else
--            Show_Newer_First_Loop :
--            for I in reverse 1 .. Messages_Amount loop
--               Show_Message
--                 (Message => Get_Message(Message_Index => I),
--                  Messages_View => Messages_View,
--                  Messages_Type => Messages_Type);
--            end loop Show_Newer_First_Loop;
--         end if;
--         Tcl_SetResult(interp => Interp, str => "1");
--         return TCL_OK;
--      end if;
--      if Get_Messages_Order = OLDER_FIRST then
--         Search_Older_First_Loop :
--         for I in 1 .. Messages_Amount loop
--            Show_Selected_Messages_Block :
--            declare
--               Message: constant Message_Data :=
--                 Get_Message(Message_Index => I);
--            begin
--               if Index
--                   (Source =>
--                      To_Lower(Item => To_String(Source => Message.Message)),
--                    Pattern => To_Lower(Item => Search_Text), From => 1) >
--                 0 then
--                  Show_Message
--                    (Message => Message, Messages_View => Messages_View,
--                     Messages_Type => Messages_Type);
--               end if;
--            end Show_Selected_Messages_Block;
--         end loop Search_Older_First_Loop;
--      else
--         Search_Newer_First_Loop :
--         for I in reverse 1 .. Messages_Amount loop
--            Show_Selected_Reverse_Messages_Block :
--            declare
--               Message: constant Message_Data :=
--                 Get_Message(Message_Index => I);
--            begin
--               if Index
--                   (Source =>
--                      To_Lower(Item => To_String(Source => Message.Message)),
--                    Pattern => To_Lower(Item => Search_Text), From => 1) >
--                 0 then
--                  Show_Message
--                    (Message => Message, Messages_View => Messages_View,
--                     Messages_Type => Messages_Type);
--               end if;
--            end Show_Selected_Reverse_Messages_Block;
--         end loop Search_Newer_First_Loop;
--      end if;
--      configure(Widgt => Messages_View, options => "-state disable");
--      Tcl_SetResult(interp => Interp, str => "1");
--      return TCL_OK;
--   end Search_Messages_Command;

   procedure Add_Commands is
      use Utils.UI;
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
