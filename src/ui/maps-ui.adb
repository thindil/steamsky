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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Bases;
with Bases.LootUI;
with Bases.RecruitUI;
with Bases.SchoolUI;
with Bases.ShipyardUI;
with Bases.UI;
with BasesTypes;
with Config;
with Crafts.UI;
with CoreUI;
with DebugUI;
with GameOptions;
with Help.UI;
with Knowledge;
with Log;
with Messages;
with Messages.UI;
with Missions.UI;
with OrdersMenu;
with Ships;
with Ships.UI;
with Statistics;
with Statistics.UI;
with Trades.UI;
with Themes;
with Utils.UI;
with WaitMenu;
with Maps.UI.Commands;

package body Maps.UI is

   -- ****iv* MUI/MUI.MapView
   -- FUNCTION
   -- Text widget with the sky map
   -- SOURCE
   Map_View: Tk_Text;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* MUI/MUI.Get_Map_View
   -- FUNCTION
   -- Get the text widget with the sky map
   -- RESULT
   -- Returns text widget with the sky map
   -- SOURCE
   function Get_Map_View return Tk_Text is
      -- ****
   begin
      return Map_View;
   end Get_Map_View;
   --## rule on REDUCEABLE_SCOPE

   procedure Create_Game_Ui is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use GNAT.Directory_Operations;
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Winfo;
      use Tcl.Tk.Ada.Wm;
      use Bases;
      use BasesTypes;
      use Config;
      use CoreUI;
      use DebugUI;
      use Log;
      use Ships;
      use Themes;
      use Tiny_String;
      use Utils.UI;
      use Maps.UI.Commands;

      Game_Frame: constant Ttk_Frame := Get_Widget(pathName => ".gameframe");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Game_Frame & ".paned");
      Button: constant Ttk_Button :=
        Get_Widget(pathName => Paned & ".mapframe.buttons.hide");
      Steam_Sky_Map_Error: exception;
      Header: constant Ttk_Frame :=
        Get_Widget(pathName => Game_Frame & ".header");
      Messages_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Paned & ".controls.messages");
      Paned_Position: Natural := 0;
      New_Start: Boolean := False;
      procedure Update_Move_Buttons with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMoveButtons";
      procedure Update_Map_Info
        (X: Positive := Player_Ship.Sky_X;
         Y: Positive := Player_Ship.Sky_Y) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMapInfo";
      procedure Set_Keys with
         Import => True,
         Convention => C,
         External_Name => "setAdaKeys";
      function Get_Menu_Accelerator(Index: Positive) return String is
         function Get_Ada_Menu_Accelerator(I: Positive) return chars_ptr with
            Import => True,
            Convention => C,
            External_Name => "getAdaMenuAccelerator";
      begin
         return Value(Item => Get_Ada_Menu_Accelerator(I => Index));
      end Get_Menu_Accelerator;
   begin
      Map_View := Get_Widget(pathName => Paned & ".mapframe.map");
      if Winfo_Get(Widgt => Get_Map_View, Info => "exists") = "0" then
         New_Start := True;
         Load_Keys_Block :
         declare
            use Ada.Text_IO;

            Keys_File: File_Type;
            Raw_Data, Field_Name, Value: Unbounded_String :=
              Null_Unbounded_String;
            Equal_Index: Natural := 0;
            procedure Set_Menu_Accelerator(Index: Positive; Value2: String) is
               procedure Set_Ada_Menu_Accelerator
                 (I: Positive; Val: chars_ptr) with
                  Import => True,
                  Convention => C,
                  External_Name => "setAdaMenuAccelerator";
            begin
               Set_Ada_Menu_Accelerator
                 (I => Index, Val => New_String(Str => Value2));
            end Set_Menu_Accelerator;
            procedure Set_Map_Accelerator(Index: Positive; Value3: String) is
               procedure Set_Ada_Map_Accelerator
                 (I: Positive; Val: chars_ptr) with
                  Import => True,
                  Convention => C,
                  External_Name => "setAdaMapAccelerator";
            begin
               Set_Ada_Map_Accelerator
                 (I => Index, Val => New_String(Str => Value3));
            end Set_Map_Accelerator;
            procedure Set_General_Accelerator
              (Index: Positive; Value4: String) is
               procedure Set_Ada_General_Accelerator
                 (I: Positive; Val: chars_ptr) with
                  Import => True,
                  Convention => C,
                  External_Name => "setAdaGeneralAccelerator";
            begin
               Set_Ada_General_Accelerator
                 (I => Index, Val => New_String(Str => Value4));
            end Set_General_Accelerator;
            procedure Set_Full_Screen_Accel(Value5: String) is
               procedure Set_Ada_Full_Screen_Accel(Val: chars_ptr) with
                  Import => True,
                  Convention => C,
                  External_Name => "setAdaFullScreenAccel";
            begin
               Set_Ada_Full_Screen_Accel(Val => New_String(Str => Value5));
            end Set_Full_Screen_Accel;
         begin
            Open
              (File => Keys_File, Mode => In_File,
               Name => To_String(Source => Save_Directory) & "keys.cfg");
            Load_Accelerators_Loop :
            while not End_Of_File(File => Keys_File) loop
               Raw_Data :=
                 To_Unbounded_String(Source => Get_Line(File => Keys_File));
               if Length(Source => Raw_Data) = 0 then
                  goto End_Of_Loop;
               end if;
               Equal_Index := Index(Source => Raw_Data, Pattern => "=");
               Field_Name :=
                 Head(Source => Raw_Data, Count => Equal_Index - 2);
               Value :=
                 Tail
                   (Source => Raw_Data,
                    Count => Length(Source => Raw_Data) - Equal_Index - 1);
               if Field_Name = To_Unbounded_String(Source => "ShipInfo") then
                  Set_Menu_Accelerator
                    (Index => 1, Value2 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Orders") then
                  Set_Menu_Accelerator
                    (Index => 2, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "Crafting") then
                  Set_Menu_Accelerator
                    (Index => 3, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "LastMessages") then
                  Set_Menu_Accelerator
                    (Index => 4, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "Knowledge") then
                  Set_Menu_Accelerator
                    (Index => 5, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitOrders") then
                  Set_Menu_Accelerator
                    (Index => 6, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameStats") then
                  Set_Menu_Accelerator
                    (Index => 7, Value2 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Help") then
                  Set_Menu_Accelerator
                    (Index => 8, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameOptions") then
                  Set_Menu_Accelerator
                    (Index => 9, Value2 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Quit") then
                  Set_Menu_Accelerator
                    (Index => 10, Value2 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Resign") then
                  Set_Menu_Accelerator
                    (Index => 11, Value2 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameMenu") then
                  Set_Map_Accelerator
                    (Index => 1, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MapOptions") then
                  Set_Map_Accelerator
                    (Index => 2, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomInMap") then
                  Set_Map_Accelerator
                    (Index => 3, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomOutMap") then
                  Set_Map_Accelerator
                    (Index => 4, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpLeft") then
                  Set_Map_Accelerator
                    (Index => 5, Value3 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "MoveUp") then
                  Set_Map_Accelerator
                    (Index => 6, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpRight") then
                  Set_Map_Accelerator
                    (Index => 7, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveLeft") then
                  Set_Map_Accelerator
                    (Index => 8, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitInPlace") then
                  Set_Map_Accelerator
                    (Index => 10, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveRight") then
                  Set_Map_Accelerator
                    (Index => 9, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownLeft") then
                  Set_Map_Accelerator
                    (Index => 11, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDown") then
                  Set_Map_Accelerator
                    (Index => 12, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownRight") then
                  Set_Map_Accelerator
                    (Index => 13, Value3 => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "MoveTo") then
                  Set_Map_Accelerator
                    (Index => 14, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMap") then
                  Set_Map_Accelerator
                    (Index => 15, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMapOnHomeBase") then
                  Set_Map_Accelerator
                    (Index => 16, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpLeft") then
                  Set_Map_Accelerator
                    (Index => 17, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUp") then
                  Set_Map_Accelerator
                    (Index => 18, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpRight") then
                  Set_Map_Accelerator
                    (Index => 19, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapLeft") then
                  Set_Map_Accelerator
                    (Index => 20, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapRight") then
                  Set_Map_Accelerator
                    (Index => 21, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownLeft") then
                  Set_Map_Accelerator
                    (Index => 22, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDown") then
                  Set_Map_Accelerator
                    (Index => 23, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownRight") then
                  Set_Map_Accelerator
                    (Index => 24, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpLeft") then
                  Set_Map_Accelerator
                    (Index => 25, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUp") then
                  Set_Map_Accelerator
                    (Index => 26, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpRight") then
                  Set_Map_Accelerator
                    (Index => 27, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorLeft") then
                  Set_Map_Accelerator
                    (Index => 28, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorRight") then
                  Set_Map_Accelerator
                    (Index => 29, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownLeft") then
                  Set_Map_Accelerator
                    (Index => 30, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDown") then
                  Set_Map_Accelerator
                    (Index => 31, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownRight") then
                  Set_Map_Accelerator
                    (Index => 32, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "LeftClickMouse") then
                  Set_Map_Accelerator
                    (Index => 33, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullStop") then
                  Set_Map_Accelerator
                    (Index => 34, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "QuarterSpeed") then
                  Set_Map_Accelerator
                    (Index => 35, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "HalfSpeed") then
                  Set_Map_Accelerator
                    (Index => 36, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullSpeed") then
                  Set_Map_Accelerator
                    (Index => 37, Value3 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullScreen") then
                  Set_Full_Screen_Accel(Value5 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeFirst") then
                  Set_General_Accelerator
                    (Index => 1, Value4 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeSecond") then
                  Set_General_Accelerator
                    (Index => 2, Value4 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeThird") then
                  Set_General_Accelerator
                    (Index => 3, Value4 => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeForth") then
                  Set_General_Accelerator
                    (Index => 4, Value4 => To_String(Source => Value));
               end if;
               <<End_Of_Loop>>
            end loop Load_Accelerators_Loop;
            Close(File => Keys_File);
         exception
            when others =>
               if Dir_Separator = '\' then
                  Set_Map_Accelerator(Index => 5, Value3 => "Home");
                  Set_Map_Accelerator(Index => 6, Value3 => "Up");
                  Set_Map_Accelerator(Index => 7, Value3 => "Prior");
                  Set_Map_Accelerator(Index => 8, Value3 => "Left");
                  Set_Map_Accelerator(Index => 9, Value3 => "Clear");
                  Set_Map_Accelerator(Index => 10, Value3 => "Right");
                  Set_Map_Accelerator(Index => 11, Value3 => "End");
                  Set_Map_Accelerator(Index => 12, Value3 => "Down");
                  Set_Map_Accelerator(Index => 13, Value3 => "Next");
                  Set_Map_Accelerator(Index => 14, Value3 => "slash");
                  Set_Map_Accelerator(Index => 17, Value3 => "Shift-Home");
                  Set_Map_Accelerator(Index => 18, Value3 => "Shift-Up");
                  Set_Map_Accelerator(Index => 19, Value3 => "Shift-Prior");
                  Set_Map_Accelerator(Index => 20, Value3 => "Shift-Left");
                  Set_Map_Accelerator(Index => 21, Value3 => "Shift-Right");
                  Set_Map_Accelerator(Index => 22, Value3 => "Shift-End");
                  Set_Map_Accelerator(Index => 23, Value3 => "Shift-Down");
                  Set_Map_Accelerator(Index => 24, Value3 => "Shift-Next");
                  Set_Map_Accelerator(Index => 25, Value3 => "Control-Home");
                  Set_Map_Accelerator(Index => 26, Value3 => "Control-Up");
                  Set_Map_Accelerator(Index => 27, Value3 => "Control-Prior");
                  Set_Map_Accelerator(Index => 28, Value3 => "Control-Left");
                  Set_Map_Accelerator(Index => 29, Value3 => "Control-Right");
                  Set_Map_Accelerator(Index => 30, Value3 => "Control-End");
                  Set_Map_Accelerator(Index => 31, Value3 => "Control-Down");
                  Set_Map_Accelerator(Index => 32, Value3 => "Control-Next");
               end if;
         end Load_Keys_Block;
         Add_Maps_Commands;
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "game.tcl");
         Main_Paned := Paned;
         Game_Header := Header;
         Close_Button := Get_Widget(pathName => Game_Header & ".closebutton");
         Set_Theme;
         OrdersMenu.Add_Commands;
         WaitMenu.Add_Commands;
         Help.UI.Add_Commands;
         Ships.UI.Add_Commands;
         Crafts.UI.Add_Commands;
         Messages.UI.Add_Commands;
         GameOptions.Add_Commands;
         Trades.UI.Add_Commands;
         SchoolUI.Add_Commands;
         RecruitUI.Add_Commands;
         Bases.UI.Add_Commands;
         ShipyardUI.Add_Commands;
         LootUI.Add_Commands;
         Knowledge.Add_Commands;
         Missions.UI.Add_Commands;
         Statistics.UI.Add_Commands;
         Bind
           (Widgt => Messages_Frame, Sequence => "<Configure>",
            Script => "ResizeLastMessages");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Configure>",
            Script => "DrawMap");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Motion>",
            Script => "{UpdateMapInfo %x %y}");
         Bind
           (Widgt => Get_Map_View,
            Sequence =>
              "<Button-" &
              (if Get_Boolean_Setting(Name => "rightButton") then "3"
               else "1") &
              ">",
            Script => "{ShowDestinationMenu %X %Y}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<MouseWheel>",
            Script => "{if {%D > 0} {ZoomMap raise} else {ZoomMap lower}}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Button-4>",
            Script => "{ZoomMap raise}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Button-5>",
            Script => "{ZoomMap lower}");
         Set_Keys;
         if Log.Debug_Mode = Log.MENU then
            Show_Debug_Ui;
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Game_Frame, Options => "-fill both -expand true");
      end if;
      Tcl_SetVar
        (interp => Get_Context, varName => "refreshmap", newValue => "1");
      Wm_Set
        (Widgt => Get_Main_Window(Interp => Get_Context), Action => "title",
         Options => "{Steam Sky}");
      if Get_Boolean_Setting(Name => "fullScreen") then
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Get_Context),
            Action => "attributes", Options => "-fullscreen 1");
      end if;
      Set_Accelerators_Loop :
      for Accelerator in 1 .. 11 loop
         Bind_To_Main_Window
           (Interp => Get_Context,
            Sequence =>
              "<" &
              To_String
                (Source =>
                   Insert
                     (Source =>
                        To_Unbounded_String
                          (Source =>
                             Get_Menu_Accelerator(Index => Accelerator)),
                      Before =>
                        Index
                          (Source =>
                             To_Unbounded_String
                               (Source =>
                                  Get_Menu_Accelerator(Index => Accelerator)),
                           Pattern => "-", Going => Backward) +
                        1,
                      New_Item => "KeyPress-")) &
              ">",
            Script =>
              "{InvokeMenu " & Get_Menu_Accelerator(Index => Accelerator) &
              "}");
      end loop Set_Accelerators_Loop;
      if Index
          (Source =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Get_Main_Window(Interp => Get_Context)),
           Pattern => ".gameframe.header") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Slave => Header);
      end if;
      Update_Header;
      Set_Center_Point(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      Set_Tags_Loop :
      for Base_Type of Bases_Types loop
         exit Set_Tags_Loop when Length(Source => Base_Type) = 0;
         Tag_Configure
           (TextWidget => Get_Map_View,
            TagName => To_String(Source => Base_Type),
            Options =>
              "-foreground #" & Get_Base_Type_Color(Base_Type => Base_Type));
      end loop Set_Tags_Loop;
      Paned_Position :=
        (if
           Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition") <
           0
         then Get_Integer_Setting(Name => "windowHeight")
         else Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition"));
      SashPos
        (Paned => Paned, Index => "0",
         NewPos => Natural'Image(Paned_Position));
      if Index
          (Source =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Get_Main_Window(Interp => Get_Context)),
           Pattern => ".gameframe.paned") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Slave => Paned);
      end if;
      Tcl_Eval(interp => Get_Context, strng => "update");
      if Invoke(Buttn => Button) /= "" then
         raise Steam_Sky_Map_Error with "Can't hide map buttons";
      end if;
      Bind_To_Main_Window
        (Interp => Get_Context, Sequence => "<Escape>",
         Script => "{InvokeButton " & Close_Button & "}");
      Update_Messages;
      if not New_Start then
         Tcl_Eval(interp => Get_Context, strng => "DrawMap");
      end if;
      Update_Move_Buttons;
      Update_Map_Info;
      if not Get_Boolean_Setting(Name => "showLastMessages") then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Messages_Frame);
      end if;
      Tcl_SetVar
        (interp => Get_Context, varName => "shipname",
         newValue => To_String(Source => Player_Ship.Name));
      Tcl_SetVar
        (interp => Get_Context, varName => "gamestate", newValue => "general");
   end Create_Game_Ui;

   procedure Show_Sky_Map(Clear: Boolean := False) is
      procedure Show_Ada_Sky_Map(Cle: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaSkyMap";
   begin
      Show_Ada_Sky_Map(Cle => (if Clear then 1 else 0));
   end Show_Sky_Map;

   function Get_General_Accelerator(Index: Positive) return String is

      function Get_Ada_General_Accelerator(I: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaGeneralAccelerator";
   begin
      return Value(Item => Get_Ada_General_Accelerator(I => Index));
   end Get_General_Accelerator;

end Maps.UI;
