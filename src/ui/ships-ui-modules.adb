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

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
-- with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkWidget;
-- with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
-- with Tcl.Tklib.Ada.Autoscroll;
with Config;
-- with Crafts;
with Dialogs; use Dialogs;
with Maps;
with Maps.UI;
with Messages;
with Ships.Cargo;
with Ships.Crew;
with Ships.UI.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;
with ShipModules;

package body Ships.UI.Modules is

   -- ****iv* SUModules/SUModules.Modules_Table
   -- FUNCTION
   -- Table with info about the installed modules on the player ship
   -- SOURCE
   Modules_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****iv* SUModules/SUModules.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Modules_Indexes: Positive_Container.Vector;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- FUNCTION
   -- Assign member, ammo or skill to module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AssignModule assigntype moduleindex assignindex
   -- assigntype is type of item to assing to module: crew, ammo, skills.
   -- moduleindex is the index of the Player_Ship module to which item will be
   -- assigned. assignindex is the index of the item which will be assigned
   -- to the module
   -- SOURCE
--   function Assign_Module_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Assign_Module_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Ada.Exceptions;
--      use Tiny_String;
--
--      Module_Index: constant Positive :=
--        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
--      Assign_Index: constant Positive :=
--        Positive'Value(CArgv.Arg(Argv => Argv, N => 3));
--      Assigned: Boolean := False;
--      procedure Update_Order(Order: Crew_Orders) is
--      begin
--         Give_Orders
--           (Ship => Player_Ship, Member_Index => Assign_Index,
--            Given_Order => Order, Module_Index => Module_Index);
--         if Player_Ship.Crew(Assign_Index).Order /= Order then
--            Tcl_SetVar
--              (interp => Interp,
--               varName =>
--                 ".moduledialog.canvas.frame.crewbutton" &
--                 CArgv.Arg(Argv => Argv, N => 3),
--               newValue => "0");
--         end if;
--      end Update_Order;
--   begin
--      if CArgv.Arg(Argv => Argv, N => 1) = "crew" then
--         case Get_Module
--           (Index => Player_Ship.Modules(Module_Index).Proto_Index)
--           .M_Type is
--            when CABIN =>
--               Modules_Loop :
--               for Module of Player_Ship.Modules loop
--                  if Module.M_Type = CABIN then
--                     Find_Owner_Loop :
--                     for Owner of Module.Owner loop
--                        if Owner = Assign_Index then
--                           Owner := 0;
--                           exit Modules_Loop;
--                        end if;
--                     end loop Find_Owner_Loop;
--                  end if;
--               end loop Modules_Loop;
--               Assigned := False;
--               Check_Assigned_Loop :
--               for Owner of Player_Ship.Modules(Module_Index).Owner loop
--                  if Owner = 0 then
--                     Owner := Assign_Index;
--                     Assigned := True;
--                     exit Check_Assigned_Loop;
--                  end if;
--               end loop Check_Assigned_Loop;
--               if not Assigned then
--                  Player_Ship.Modules(Module_Index).Owner(1) := Assign_Index;
--               end if;
--               Add_Message
--                 (Message =>
--                    "You assigned " &
--                    To_String
--                      (Source => Player_Ship.Modules(Module_Index).Name) &
--                    " to " &
--                    To_String(Source => Player_Ship.Crew(Assign_Index).Name) &
--                    ".",
--                  M_Type => ORDERMESSAGE);
--            when GUN | HARPOON_GUN =>
--               Update_Order(Order => GUNNER);
--            when ALCHEMY_LAB .. GREENHOUSE =>
--               Update_Order(Order => CRAFT);
--            when MEDICAL_ROOM =>
--               Update_Order(Order => HEAL);
--            when TRAINING_ROOM =>
--               Update_Order(Order => TRAIN);
--            when others =>
--               null;
--         end case;
--         Update_Header;
--      elsif CArgv.Arg(Argv => Argv, N => 1) = "ammo" then
--         if Player_Ship.Modules(Module_Index).M_Type = GUN then
--            Player_Ship.Modules(Module_Index).Ammo_Index := Assign_Index;
--         else
--            Player_Ship.Modules(Module_Index).Harpoon_Index := Assign_Index;
--         end if;
--         Add_Message
--           (Message =>
--              "You assigned " &
--              To_String
--                (Source =>
--                   Get_Proto_Item
--                     (Index =>
--                        Inventory_Container.Element
--                          (Container => Player_Ship.Cargo,
--                           Index => Assign_Index)
--                          .Proto_Index)
--                     .Name) &
--              " to " &
--              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
--              ".",
--            M_Type => ORDERMESSAGE);
--      elsif CArgv.Arg(Argv => Argv, N => 1) = "skill" then
--         if Player_Ship.Modules(Module_Index).Trained_Skill =
--           Skills_Amount_Range(Assign_Index) then
--            return TCL_OK;
--         end if;
--         Player_Ship.Modules(Module_Index).Trained_Skill :=
--           Skills_Amount_Range(Assign_Index);
--         Add_Message
--           (Message =>
--              "You prepared " &
--              To_String(Source => Player_Ship.Modules(Module_Index).Name) &
--              " for training " &
--              To_String
--                (Source =>
--                   SkillsData_Container.Element
--                     (Container => Skills_List,
--                      Index => Skills_Amount_Range(Assign_Index))
--                     .Name) &
--              ".",
--            M_Type => ORDERMESSAGE);
--         Update_Messages;
--         return TCL_OK;
--      end if;
--      Update_Messages;
--      return
--        Show_Ship_Info_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
--           Argv => Argv);
--   exception
--      when An_Exception : Crew_Order_Error =>
--         Show_Message
--           (Text => Exception_Message(X => An_Exception),
--            Title => "Can't assign crew");
--         return TCL_OK;
--   end Assign_Module_Command;

   -- FUNCTION
   -- Update assign the crew member UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateAssignCrew moduleindex ?crewindex?
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned. Crewindex is the index of the crew member which will be
   -- assigned or removed
   -- SOURCE
--   function Update_Assign_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Update_Assign_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Interfaces.C.Strings;
--
--      Module_Index: constant Positive :=
--        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
--      Assigned: Natural := 0;
--      Frame_Name: constant String := ".moduledialog.canvas.frame";
--      --## rule off IMPROPER_INITIALIZATION
--      Crew_Button: Ttk_CheckButton;
--      Button_Name: Unbounded_String;
--      --## rule on IMPROPER_INITIALIZATION
--      Crew_Index: constant Natural :=
--        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv => Argv, N => 2))
--         else 0);
--      Info_Label: constant Ttk_Label :=
--        Get_Widget(pathName => Frame_Name & ".infolabel", Interp => Interp);
--   begin
--      if Argc = 3 then
--         if Tcl_GetVar
--             (interp => Interp,
--              varName =>
--                Frame_Name & ".crewbutton" & CArgv.Arg(Argv => Argv, N => 2)) =
--           "0" then
--            Remove_Owner_Loop :
--            for Owner of Player_Ship.Modules(Module_Index).Owner loop
--               if Owner = Crew_Index then
--                  Owner := 0;
--                  exit Remove_Owner_Loop;
--               end if;
--            end loop Remove_Owner_Loop;
--            if Get_Module
--                (Index => Player_Ship.Modules(Module_Index).Proto_Index)
--                .M_Type /=
--              CABIN then
--               Give_Orders
--                 (Ship => Player_Ship, Member_Index => Crew_Index,
--                  Given_Order => REST, Module_Index => 0,
--                  Check_Priorities => False);
--            end if;
--         elsif Assign_Module_Command
--             (Client_Data => Client_Data, Interp => Interp, Argc => 4,
--              Argv =>
--                CArgv.Empty & "AssignModule" & "crew" &
--                CArgv.Arg(Argv => Argv, N => 1) &
--                CArgv.Arg(Argv => Argv, N => 2)) /=
--           TCL_OK then
--            return TCL_ERROR;
--         end if;
--      end if;
--      Crew_Button.Interp := Interp;
--      Enable_Buttons_Loop :
--      for I in Player_Ship.Crew.Iterate loop
--         Crew_Button.Name :=
--           New_String
--             (Str =>
--                Frame_Name & ".crewbutton" &
--                Trim
--                  (Source =>
--                     Positive'Image(Crew_Container.To_Index(Position => I)),
--                   Side => Left));
--         State(Widget => Crew_Button, StateSpec => "!disabled");
--         configure(Widgt => Crew_Button, options => "-takefocus 1");
--      end loop Enable_Buttons_Loop;
--      Count_Owners_Loop :
--      for Owner of Player_Ship.Modules(Module_Index).Owner loop
--         if Owner /= 0 then
--            Assigned := Assigned + 1;
--         end if;
--      end loop Count_Owners_Loop;
--      if Assigned =
--        Positive(Player_Ship.Modules(Module_Index).Owner.Length) then
--         Disable_Buttons_Loop :
--         for I in Player_Ship.Crew.Iterate loop
--            Button_Name :=
--              To_Unbounded_String
--                (Source =>
--                   Frame_Name & ".crewbutton" &
--                   Trim
--                     (Source =>
--                        Positive'Image(Crew_Container.To_Index(Position => I)),
--                      Side => Left));
--            if Tcl_GetVar
--                (interp => Interp,
--                 varName => To_String(Source => Button_Name)) =
--              "0" then
--               Crew_Button.Name :=
--                 New_String(Str => To_String(Source => Button_Name));
--               State(Widget => Crew_Button, StateSpec => "disabled");
--               configure(Widgt => Crew_Button, options => "-takefocus 0");
--            end if;
--         end loop Disable_Buttons_Loop;
--      end if;
--      if Winfo_Get(Widgt => Info_Label, Info => "exists") = "1" then
--         configure
--           (Widgt => Info_Label,
--            options =>
--              "-text {Available:" &
--              Natural'Image
--                (Positive(Player_Ship.Modules(Module_Index).Owner.Length) -
--                 Assigned) &
--              "}");
--         Update_Header;
--         Update_Crew_Info;
--      end if;
--      return TCL_OK;
--   end Update_Assign_Crew_Command;
   --## rule on REDUCEABLE_SCOPE

   -- FUNCTION
   -- Show assign the crew member UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignCrew moduleindex
   -- Moduleindex is the index of the module to which a new crew members will
   -- be assigned.
   -- SOURCE
--   function Show_Assign_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Show_Assign_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
--      use Tcl.Tklib.Ada.Autoscroll;
--      use Crafts;
--      use Tiny_String;
--
--      Module_Index: constant Positive :=
--        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
--      Module: constant Module_Data := Player_Ship.Modules(Module_Index);
--      Module_Dialog: constant Ttk_Frame :=
--        Create_Dialog
--          (Name => ".moduledialog",
--           Title =>
--             "Assign a crew member to " & To_String(Source => Module.Name),
--           Title_Width => 250);
--      Y_Scroll: constant Ttk_Scrollbar :=
--        Create
--          (pathName => Module_Dialog & ".yscroll",
--           options =>
--             "-orient vertical -command [list .moduledialog.canvas yview]");
--      Crew_Canvas: constant Tk_Canvas :=
--        Create
--          (pathName => Module_Dialog & ".canvas",
--           options => "-yscrollcommand [list " & Y_Scroll & " set]");
--      Crew_Frame: constant Ttk_Frame :=
--        Create(pathName => Crew_Canvas & ".frame");
--      Close_Button: constant Ttk_Button :=
--        Create
--          (pathName => Module_Dialog & ".button",
--           options =>
--             "-text Close -command {CloseDialog " &
--             Widget_Image(Win => Module_Dialog) & "}");
--      Height: Positive := 10;
--      Width: Positive := 250;
--      --## rule off IMPROPER_INITIALIZATION
--      Crew_Button: Ttk_CheckButton;
--      Info_Label: Ttk_Label;
--      --## rule on IMPROPER_INITIALIZATION
--      Assigned: Natural := 0;
--      Recipe: constant Craft_Data :=
--        (if Module.M_Type = WORKSHOP then
--           Set_Recipe_Data
--             (Recipe_Index =>
--                To_Bounded_String
--                  (Source => To_String(Source => Module.Crafting_Index)))
--         else Craft_Data'(others => <>));
--   begin
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Crew_Canvas, Options => "-sticky nwes -padx 5 -pady 5");
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Y_Scroll,
--         Options => "-sticky ns -padx {0 5} -pady {5 0} -row 0 -column 1");
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Close_Button, Options => "-pady {0 5} -columnspan 2");
--      Focus(Widgt => Close_Button);
--      Autoscroll(Scroll => Y_Scroll);
--      Load_Crew_List_Loop :
--      for I in Player_Ship.Crew.Iterate loop
--         Crew_Button :=
--           Create
--             (pathName =>
--                Crew_Frame & ".crewbutton" &
--                Trim
--                  (Source =>
--                     Positive'Image(Crew_Container.To_Index(Position => I)),
--                   Side => Left),
--              options =>
--                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
--                (if Module.M_Type = WORKSHOP then
--                   Get_Skill_Marks
--                     (Skill_Index => Recipe.Skill,
--                      Member_Index => Crew_Container.To_Index(Position => I))
--                 else "") &
--                "} -command {UpdateAssignCrew" & Positive'Image(Module_Index) &
--                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
--         Tcl_SetVar
--           (interp => Interp, varName => Widget_Image(Win => Crew_Button),
--            newValue => "0");
--         Count_Assigned_Loop :
--         for Owner of Module.Owner loop
--            if Owner = Crew_Container.To_Index(Position => I) then
--               Tcl_SetVar
--                 (interp => Interp,
--                  varName => Widget_Image(Win => Crew_Button),
--                  newValue => "1");
--               Assigned := Assigned + 1;
--               exit Count_Assigned_Loop;
--            end if;
--         end loop Count_Assigned_Loop;
--         Tcl.Tk.Ada.Pack.Pack(Slave => Crew_Button, Options => "-anchor w");
--         Height :=
--           Height +
--           Positive'Value
--             (Winfo_Get(Widgt => Crew_Button, Info => "reqheight"));
--         if Positive'Value
--             (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
--           10 >
--           Width then
--            Width :=
--              Positive'Value
--                (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
--              10;
--         end if;
--         Bind
--           (Widgt => Crew_Button, Sequence => "<Escape>",
--            Script => "{" & Close_Button & " invoke;break}");
--         Bind
--           (Widgt => Crew_Button, Sequence => "<Tab>",
--            Script =>
--              "{focus [GetActiveButton" &
--              Positive'Image(Crew_Container.To_Index(Position => I)) &
--              "];break}");
--      end loop Load_Crew_List_Loop;
--      if Update_Assign_Crew_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
--           Argv => Argv) /=
--        TCL_OK then
--         return TCL_ERROR;
--      end if;
--      Info_Label :=
--        Create
--          (pathName => Crew_Frame & ".infolabel",
--           options =>
--             "-text {Available:" &
--             Natural'Image(Positive(Module.Owner.Length) - Assigned) & "}");
--      Tcl.Tk.Ada.Pack.Pack(Slave => Info_Label);
--      Height :=
--        Height +
--        Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqheight"));
--      if Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqwidth")) >
--        Width then
--         Width :=
--           Positive'Value(Winfo_Get(Widgt => Info_Label, Info => "reqwidth"));
--      end if;
--      if Height > 500 then
--         Height := 500;
--      end if;
--      Canvas_Create
--        (Parent => Crew_Canvas, Child_Type => "window",
--         Options =>
--           "0 0 -anchor nw -window " & Widget_Image(Win => Crew_Frame));
--      Tcl_Eval(interp => Interp, strng => "update");
--      configure
--        (Widgt => Crew_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Crew_Canvas, TagOrId => "all") & "] -height" &
--           Positive'Image(Height) & " -width" & Positive'Image(Width));
--      Bind
--        (Widgt => Close_Button, Sequence => "<Escape>",
--         Script => "{" & Close_Button & " invoke;break}");
--      Bind
--        (Widgt => Close_Button, Sequence => "<Tab>",
--         Script => "{focus [GetActiveButton 0];break}");
--      Show_Dialog(Dialog => Module_Dialog, Relative_Y => 0.2);
--      return TCL_OK;
--   end Show_Assign_Crew_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Skill_Command
   -- FUNCTION
   -- Show assign the skill UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssignSkill moduleindex
   -- Moduleindex is the index of the module to which a new skill will
   -- be assigned.
   -- SOURCE
   function Show_Assign_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Skill_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tcl.Ada;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Config;
      use Ships.Cargo;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Module_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".moduledialog",
           Title =>
             "Assign skill to " &
             To_String(Source => Player_Ship.Modules(Module_Index).Name),
           Title_Width => 400);
      Skills_Frame: constant Ttk_Frame :=
        Create(pathName => Module_Dialog & ".frame");
      Skill_Name, Tool_Color: Unbounded_String := Null_Unbounded_String;
      Proto_Index: Natural := 0;
      Tool_Name: Bounded_String := Null_Bounded_String;
      Skills_Table: Table_Widget (Amount => 2) :=
        Create_Table
          (Parent => Widget_Image(Win => Skills_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Skill"),
              2 => To_Unbounded_String(Source => "Training tool")));
      Dialog_Close_Button: constant Ttk_Button :=
        Get_Widget(pathName => Module_Dialog & ".button");
   begin
      Load_Skills_List_Loop :
      for I in 1 .. Skills_Amount loop
         if SkillsData_Container.Element(Container => Skills_List, Index => I)
             .Tool /=
           Null_Bounded_String then
            Proto_Index :=
              Find_Proto_Item
                (Item_Type =>
                   SkillsData_Container.Element
                     (Container => Skills_List, Index => I)
                     .Tool);
            Tool_Name :=
              (if
                 Get_Proto_Item(Index => Proto_Index).Show_Type /=
                 Null_Bounded_String
               then Get_Proto_Item(Index => Proto_Index).Show_Type
               else Get_Proto_Item(Index => Proto_Index).I_Type);
         end if;
         Skill_Name :=
           To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => I)
                       .Name));
         Tool_Color := To_Unbounded_String(Source => "green");
         if Get_Item_Amount
             (Item_Type => Get_Proto_Item(Index => Proto_Index).I_Type) =
           0 then
            Append(Source => Skill_Name, New_Item => " (no tool)");
            Tool_Color := To_Unbounded_String(Source => "red");
         end if;
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Skill_Name),
            Tooltip =>
              "Press mouse " &
              (if Get_Boolean_Setting(Name => "rightButton") then "right"
               else "left") &
              " button to set as trained skill",
            Command =>
              "AssignModule skill" & Positive'Image(Module_Index) &
              Skills_Amount_Range'Image(I),
            Column => 1);
         Add_Button
           (Table => Skills_Table, Text => To_String(Source => Tool_Name),
            Tooltip =>
              "Press mouse " &
              (if Get_Boolean_Setting(Name => "rightButton") then "right"
               else "left") &
              " button to set as trained skill",
            Command =>
              "AssignModule skill" & Positive'Image(Module_Index) &
              Skills_Amount_Range'Image(I),
            Column => 2, New_Row => True,
            Color => To_String(Source => Tool_Color));
      end loop Load_Skills_List_Loop;
      Update_Table(Table => Skills_Table);
      Tcl.Tk.Ada.Grid.Grid(Slave => Skills_Frame, Options => "-padx 2");
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Skills_Table.Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Skills_Table.Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Skills_Table.Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Skills_Table.Canvas, Fraction => "0.0");
      Add_Close_Button
        (Name => Module_Dialog & ".button", Text => "Close",
         Command => "CloseDialog " & Module_Dialog, Row => 2);
      Bind
        (Widgt => Dialog_Close_Button, Sequence => "<Tab>",
         Script => "{focus " & Skills_Table.Canvas & ";break}");
      Bind
        (Widgt => Skills_Table.Canvas, Sequence => "<Escape>",
         Script => "{" & Dialog_Close_Button & " invoke;break}");
      Show_Dialog(Dialog => Module_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Assign_Skill_Command;

   -- ****o* SUModules/SUModules.Cancel_Order_Command
   -- FUNCTION
   -- Cancel the current crafting order
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Cancel moduleindex
   -- Moduleindex is the index of the module which the crafting order will
   -- be canceled
   -- SOURCE
   function Cancel_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Maps.UI;
      use Messages;
      use Ships.Crew;
      use Ships.UI.Crew;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Player_Ship.Modules(Module_Index).Crafting_Index := Null_Bounded_String;
      Player_Ship.Modules(Module_Index).Crafting_Amount := 0;
      Player_Ship.Modules(Module_Index).Crafting_Time := 0;
      Give_Orders_Loop :
      for Owner of Player_Ship.Modules(Module_Index).Owner loop
         if Owner > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Owner,
               Given_Order => REST);
         end if;
      end loop Give_Orders_Loop;
      Add_Message
        (Message =>
           "You cancelled crafting order in " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) & ".",
         M_Type => CRAFTMESSAGE, Color => RED);
      Update_Messages;
      Update_Header;
      Update_Crew_Info;
      return TCL_OK;
   end Cancel_Order_Command;

   -- ****o* SUModules/SUModules.Get_Active_Button_Command
   -- FUNCTION
   -- Get the next active button in assing crew dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetActiveButton crewindex
   -- Crewindex is the index of the crew member which is currently selected
   -- or 0 for close button
   -- SOURCE
   function Get_Active_Button_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_Active_Button_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
      use Tcl.Tk.Ada.Widgets.TtkWidget;

      Crew_Index: constant Natural :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Button_Name: Unbounded_String := Null_Unbounded_String;
      Button: Ttk_CheckButton; --## rule line off IMPROPER_INITIALIZATION
   begin
      Find_Active_Button_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Button_Name :=
           To_Unbounded_String
             (Source =>
                ".moduledialog.canvas.frame.crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left));
         Button :=
           Get_Widget
             (pathName => To_String(Source => Button_Name), Interp => Interp);
         exit Find_Active_Button_Loop when InState
             (Widget => Button, StateSpec => "disabled") =
           "0" and
           Crew_Container.To_Index(Position => I) > Crew_Index;
         Button_Name := Null_Unbounded_String;
      end loop Find_Active_Button_Loop;
      if Button_Name = Null_Unbounded_String then
         Button_Name := To_Unbounded_String(Source => ".moduledialog.button");
      end if;
      Button :=
        Get_Widget
          (pathName => To_String(Source => Button_Name), Interp => Interp);
      Focus(Widgt => Button);
      return TCL_OK;
   end Get_Active_Button_Command;

   procedure Update_Modules_Info(Page: Positive := 1) is
      --## rule off TYPE_INITIAL_VALUES
      type Modules_Array is array(0 .. 50) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      M_Array: Modules_Array := (others => 0);
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      procedure Update_Ada_Modules_Info
        (P: Positive; M: Modules_Array; W: out Nim_Width) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaModulesInfo";
   begin
      if Modules_Indexes.Length /= Player_Ship.Modules.Length then
         Modules_Indexes.Clear;
         Update_Modules_Indexes_Loop :
         for I in Player_Ship.Modules.Iterate loop
            Modules_Indexes.Append
              (New_Item => Modules_Container.To_Index(Position => I));
         end loop Update_Modules_Indexes_Loop;
      end if;
      Convert_Modules_Indexes_Loop :
      for M_Index of Modules_Indexes loop
         M_Array(Index) := M_Index;
         Index := Index + 1;
      end loop Convert_Modules_Indexes_Loop;
      Update_Ada_Modules_Info(P => Page, M => M_Array, W => N_Width);
      Index := 1;
      Convert_Headers_Width_Loop :
      for Width of N_Width loop
         exit Convert_Headers_Width_Loop when Width = 0;
         Modules_Table.Columns_Width(Index) := Width;
         Index := Index + 1;
      end loop Convert_Headers_Width_Loop;
   end Update_Modules_Info;

   -- ****o* SUModules/SUModules.Show_Modules_Command
   -- FUNCTION
   -- Show the list of the player's ship modules to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModules ?page?
   -- Page parameter is a index of page from which starts showing
   -- modules.
   -- SOURCE
   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Update_Modules_Info
        (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      return TCL_OK;
   end Show_Modules_Command;

   -- ****it* SUModules/SUModules.Modules_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the ship modules list
   -- OPTIONS
   -- NAMEASC    - Sort modules by name ascending
   -- NAMEDESC   - Sort modules by name descending
   -- DAMAGEASC  - Sort modules by damage ascending
   -- DAMAGEDESC - Sort modules by damage descending
   -- INFOASC    - Sort modules by info ascending
   -- INFODESC   - Sort modules by info descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.9 - Added sorting by info column
   -- SOURCE
   type Modules_Sort_Orders is
     (NAMEASC, NAMEDESC, DAMAGEASC, DAMAGEDESC, INFOASC, INFODESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUModules/SUModules.Default_Modules_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's modules
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Modules_Sort_Order: constant Modules_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUModules/SUModules.Modules_Sort_Order
   -- FUNCTION
   -- The current sorting order for modules list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUModules/SUModules.Sort_Modules_Command
   -- FUNCTION
   -- Sort the player's ship's modules list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Modules_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Module_Data is record
         Name: Bounded_String;
         Damage: Float;
         Id: Positive;
         Info: Unbounded_String;
      end record;
      type Modules_Array is array(Positive range <>) of Local_Module_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Modules: Modules_Array(1 .. Positive(Player_Ship.Modules.Length));
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function Get_Modules_Sort_Order return Modules_Sort_Orders is
      begin
         return Modules_Sort_Order;
      end Get_Modules_Sort_Order;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Get_Modules_Sort_Order = NAMEASC
           and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFOASC
           and then Left.Info < Right.Info then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFODESC
           and then Left.Info > Right.Info then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Modules is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Modules_Array);
      function Get_Module_Info(Module_Index: Positive) return String is
         use Interfaces.C.Strings;
         function Get_Ada_Module_Info(M_Index: Positive) return chars_ptr with
            Import => True,
            Convention => C,
            External_Name => "getAdaModuleInfo";
      begin
         return Value(Item => Get_Ada_Module_Info(M_Index => Module_Index));
      end Get_Module_Info;
   begin
      case Column is
         when 1 =>
            if Get_Modules_Sort_Order = NAMEASC then
               Modules_Sort_Order := NAMEDESC;
            else
               Modules_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Get_Modules_Sort_Order = DAMAGEASC then
               Modules_Sort_Order := DAMAGEDESC;
            else
               Modules_Sort_Order := DAMAGEASC;
            end if;
         when 3 =>
            if Get_Modules_Sort_Order = INFOASC then
               Modules_Sort_Order := INFODESC;
            else
               Modules_Sort_Order := INFOASC;
            end if;
         when others =>
            null;
      end case;
      if Get_Modules_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Modules_Loop :
      for I in Player_Ship.Modules.Iterate loop
         Local_Modules(Modules_Container.To_Index(Position => I)) :=
           (Name => Player_Ship.Modules(I).Name,
            Damage =>
              Float(Player_Ship.Modules(I).Durability) /
              Float(Player_Ship.Modules(I).Max_Durability),
            Id => Modules_Container.To_Index(Position => I),
            Info =>
              To_Unbounded_String
                (Source =>
                   Get_Module_Info
                     (Module_Index =>
                        Modules_Container.To_Index(Position => I))));
      end loop Fill_Local_Modules_Loop;
      Sort_Modules(Container => Local_Modules);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Modules_Indexes.Clear;
      Fill_Modules_Indexes_Loop :
      for Module of Local_Modules loop
         Modules_Indexes.Append(New_Item => Module.Id);
      end loop Fill_Modules_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Update_Modules_Info;
      return TCL_OK;
   end Sort_Modules_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Ammo_Command
   -- FUNCTION
   -- Show the list of available ammo for the selected gun
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssingAmmo index
   -- Index is the module index of the selected gun which will be have
   -- assigned a new ammo
   -- SOURCE
   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use ShipModules;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Ammo_Index: constant Natural :=
        (if Player_Ship.Modules(Module_Index).M_Type = GUN then
           Player_Ship.Modules(Module_Index).Ammo_Index
         else Player_Ship.Modules(Module_Index).Harpoon_Index);
      Ammo_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".ammomenu", Title => "Available ammo", Parent_Name => ".");
      Row: Positive := 1;
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Ammo_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Ammo_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Ammo_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Ammo_Menu & ".ammo1;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Find_Ammo_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         if Get_Proto_Item
             (Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Proto_Index)
             .I_Type =
           Get_Ada_Item_Type
             (Item_Index =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Value -
                1) and
           I /= Ammo_Index then
            Add_Button
              (Name =>
                 ".ammo" & Trim(Source => Positive'Image(Row), Side => Left),
               Label =>
                 To_String
                   (Source =>
                      Get_Proto_Item
                        (Index =>
                           Inventory_Container.Element
                             (Container => Player_Ship.Cargo, Index => I)
                             .Proto_Index)
                        .Name),
               Command =>
                 "AssignModule ammo " & CArgv.Arg(Argv => Argv, N => 1) &
                 Positive'Image(I));
            Row := Row + 1;
         end if;
      end loop Find_Ammo_Loop;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Ammo_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Assign_Ammo_Command;

   procedure Add_Modules_Commands is
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaModulesCommands";
   begin
      Add_Ada_Commands;
--      Add_Command
--        (Name => "ShowAssignCrew",
--         Ada_Command => Show_Assign_Crew_Command'Access);
      Add_Command
        (Name => "ShowAssignSkill",
         Ada_Command => Show_Assign_Skill_Command'Access);
      Add_Command
        (Name => "CancelOrder", Ada_Command => Cancel_Order_Command'Access);
      Add_Command
        (Name => "GetActiveButton",
         Ada_Command => Get_Active_Button_Command'Access);
      Add_Command
        (Name => "ShowModules", Ada_Command => Show_Modules_Command'Access);
      Add_Command
        (Name => "SortShipModules",
         Ada_Command => Sort_Modules_Command'Access);
      Add_Command
        (Name => "ShowAssignAmmo",
         Ada_Command => Show_Assign_Ammo_Command'Access);
   end Add_Modules_Commands;

end Ships.UI.Modules;
