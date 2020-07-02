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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.MenuButton; use Tcl.Tk.Ada.Widgets.MenuButton;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Crew.UI is

   -- ****f* CUI2/Show_Crew_Info_Command
   -- FUNCTION
   -- Show information about the player's ship crew
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Crew_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CrewCanvas: Tk_Canvas;
      CrewFrame, Item: Ttk_Frame;
      CloseButton, CrewButton: Ttk_Button;
      Tokens: Slice_Set;
      Rows, Row: Natural := 0;
      NeedClean, NeedRepair: Boolean;
      OrdersButton: Tk_MenuButton;
      OrdersMenu: Tk_Menu;
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"),
         To_Unbounded_String("Gunner"), To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading module"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleans ship"), To_Unbounded_String("On break"),
         To_Unbounded_String("Defends ship"), To_Unbounded_String("Boarding"),
         To_Unbounded_String("Trains"));
      SteamSky_Crew_Exception: exception;
      function IsWorking
        (Owners: Natural_Container.Vector; MemberIndex: Positive)
         return Boolean is
      begin
         for Owner of Owners loop
            if Owner = MemberIndex then
               return True;
            end if;
         end loop;
         return False;
      end IsWorking;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      CrewFrame.Interp := Interp;
      CrewFrame.Name := New_String(Widget_Image(Paned) & ".crewframe");
      CrewCanvas.Interp := Interp;
      CrewCanvas.Name := New_String(Widget_Image(CrewFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name := New_String(Widget_Image(CrewCanvas) & ".crew.crew.name");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "crew.tcl");
         Bind(CrewFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      CrewFrame.Name := New_String(Widget_Image(CrewCanvas) & ".crew.crew");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(CrewFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      Row := 1;
      for I in PlayerShip.Crew.Iterate loop
         NeedClean := False;
         NeedRepair := False;
         CrewButton :=
           Create
             (Widget_Image(CrewFrame) & ".name" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) &
              "} -command {ShowMemberInfo" &
              Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
         Tcl.Tk.Ada.Grid.Grid(CrewButton, "-row" & Natural'Image(Row));
         OrdersButton :=
           Create
             (Widget_Image(CrewFrame) & ".orders" &
              Trim(Natural'Image(Row), Left),
              "-text {" &
              To_String
                (OrdersNames(Crew_Orders'Pos(PlayerShip.Crew(I).Order) + 1)) &
              "} -menu " & Widget_Image(CrewFrame) & ".orders" &
              Trim(Natural'Image(Row), Left) & ".menu");
         Tcl.Tk.Ada.Grid.Grid
           (OrdersButton, "-row" & Natural'Image(Row) & " -column 1");
         OrdersMenu.Create
           (Widget_Image(OrdersButton) & ".menu", "-tearoff false");
         if
           ((PlayerShip.Crew(I).Tired = 100 or
             PlayerShip.Crew(I).Hunger = 100 or
             PlayerShip.Crew(I).Thirst = 100) and
            PlayerShip.Crew(I).Order /= Rest) or
           (PlayerShip.Crew(I).Skills.Length = 0 or
            PlayerShip.Crew(I).ContractLength = 0) then
            Add
              (OrdersMenu, "command",
               "-label {Go on break} -command {SetCrewOrder Rest" &
               Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
         else
            if PlayerShip.Crew(I).Order /= Pilot then
               Add
                 (OrdersMenu, "command",
                  "-label {Piloting} -command {SetCrewOrder Pilot" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Engineer then
               Add
                 (OrdersMenu, "command",
                  "-label {Engineering} -command {SetCrewOrder Engineer" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
            for J in PlayerShip.Modules.Iterate loop
               if PlayerShip.Modules(J).Durability > 0 then
                  case Modules_List(PlayerShip.Modules(J).ProtoIndex).MType is
                     when GUN | HARPOON_GUN =>
                        if PlayerShip.Modules(J).Owner(1) /=
                          Crew_Container.To_Index(I) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Operate " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Gunner" &
                              Positive'Image
                                (Positive(Crew_Container.To_Index(I))) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when ALCHEMY_LAB .. GREENHOUSE =>
                        if not IsWorking
                            (PlayerShip.Modules(J).Owner,
                             Crew_Container.To_Index(I)) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Work in " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Craft" &
                              Positive'Image
                                (Positive(Crew_Container.To_Index(I))) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when CABIN =>
                        if PlayerShip.Modules(J).Cleanliness <
                          PlayerShip.Modules(J).Quality and
                          PlayerShip.Crew(I).Order /= Clean and NeedClean then
                           Add
                             (OrdersMenu, "command",
                              "-label {Clean ship} -command {SetCrewOrder Clean" &
                              Positive'Image
                                (Positive(Crew_Container.To_Index(I))) &
                              "}");
                           NeedClean := False;
                        end if;
                     when TRAINING_ROOM =>
                        if not IsWorking
                            (PlayerShip.Modules(J).Owner,
                             Crew_Container.To_Index(I)) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Go on training in " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Train" &
                              Positive'Image
                                (Positive(Crew_Container.To_Index(I))) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when others =>
                        null;
                  end case;
                  if PlayerShip.Modules(J).Durability <
                    PlayerShip.Modules(J).MaxDurability and
                    NeedRepair then
                     Add
                       (OrdersMenu, "command",
                        "-label {Repair ship} -command {SetCrewOrder Repair" &
                        Positive'Image(Positive(Crew_Container.To_Index(I))) &
                        "}");
                     NeedRepair := False;
                  end if;
               end if;
            end loop;
            for J in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(J).Health < 100 and
                 Crew_Container.To_Index(J) /= Crew_Container.To_Index(I) and
                 PlayerShip.Crew(J).Order /= Heal then
                  Add
                    (OrdersMenu, "command",
                     "-label {Heal wounded crew members} -command {SetCrewOrder Heal" &
                     Positive'Image(Positive(Crew_Container.To_Index(I))) &
                     "}");
                  exit;
               end if;
            end loop;
            if PlayerShip.UpgradeModule > 0 and
              PlayerShip.Crew(I).Order /= Upgrading then
               Add
                 (OrdersMenu, "command",
                  "-label {Upgrade module} -command {SetCrewOrder Upgrading" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Talk then
               Add
                 (OrdersMenu, "command",
                  "-label {Talking in bases} -command {SetCrewOrder Talk" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Rest then
               Add
                 (OrdersMenu, "command",
                  "-label {Go on break} -command {SetCrewOrder Rest" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
         end if;
         if Row = 1 then
            if Invoke(CrewButton) /= "" then
               raise SteamSky_Crew_Exception
                 with "Can't show player character info";
            end if;
         end if;
         Row := Row + 1;
      end loop;
      -- End of fill
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      CrewFrame.Name := New_String(Widget_Image(CrewCanvas) & ".crew");
      configure
        (CrewCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CrewCanvas, "window",
         "[expr " & Winfo_Get(CrewFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CrewFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CrewFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CrewCanvas, "-scrollregion [list " & BBox(CrewCanvas, "all") & "]");
      ShowScreen("crewframe");
      return TCL_OK;
   end Show_Crew_Info_Command;

   -- ****f* CUI2/Set_Crew_Order_Command
   -- FUNCTION
   -- Set order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Crew_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crew_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ModuleIndex: Natural := 0;
   begin
      if Argc = 4 then
         ModuleIndex := Natural'Value(CArgv.Arg(Argv, 3));
      end if;
      GiveOrders
        (PlayerShip, Positive'Value(CArgv.Arg(Argv, 2)),
         Crew_Orders'Value(CArgv.Arg(Argv, 1)), ModuleIndex);
      UpdateHeader;
      UpdateMessages;
      return Show_Crew_Info_Command(ClientData, Interp, Argc, Argv);
   end Set_Crew_Order_Command;

   -- ****f* CUI2/Show_Member_Info_Command
   -- FUNCTION
   -- Show detailed information about the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Member_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Member: constant Member_Data :=
        PlayerShip.Crew(Positive'Value(CArgv.Arg(Argv, 1)));
      MemberInfo: Unbounded_String;
      MemberLabel: Ttk_Label;
   begin
      if Factions_List(Member.Faction).Flags.Find_Index
          (To_Unbounded_String("nogender")) =
        UnboundedString_Container.No_Index then
         if Member.Gender = 'M' then
            MemberInfo := To_Unbounded_String("Gender: Male");
         else
            MemberInfo := To_Unbounded_String("Gender: Female");
         end if;
      end if;
      Append(MemberInfo, LF & "Faction: ");
      Append(MemberInfo, Factions_List(Member.Faction).Name);
      Append(MemberInfo, LF & "Home base: ");
      Append(MemberInfo, SkyBases(Member.HomeBase).Name);
--      if Member.Skills.Length = 0 or Member.ContractLength = 0 then
--         Hide(Gtk_Widget(Get_Object(Object, "boxcrewstats")));
--         Hide(Gtk_Widget(Get_Object(Object, "btninventory")));
--         Hide(Gtk_Widget(Get_Object(Object, "exppriorities")));
--         Hide(Gtk_Widget(Get_Object(Object, "lblstats1")));
--         Hide(Gtk_Widget(Get_Object(Object, "lblskills")));
--         Hide(Gtk_Widget(Get_Object(Object, "boxcrewskills")));
--         Append(MemberInfo, LF & "Passenger");
--         if Member.ContractLength > 0 then
--            Append(MemberInfo, LF & "Time limit:");
--            MinutesToDate(Member.ContractLength, MemberInfo);
--         end if;
--      else
--         Show_All(Gtk_Widget(Get_Object(Object, "btninventory")));
--         Show_All(Gtk_Widget(Get_Object(Object, "exppriorities")));
--         Show_All(Gtk_Widget(Get_Object(Object, "lblstats1")));
--         Show_All(Gtk_Widget(Get_Object(Object, "lblskills")));
--         if MemberIndex > 1 then
--            Append(MemberInfo, LF & "Contract length:");
--            if Member.ContractLength > 0 then
--               Append
--                 (MemberInfo, Integer'Image(Member.ContractLength) & " days.");
--            else
--               Append(MemberInfo, " pernament.");
--            end if;
--            Append
--              (MemberInfo,
--               LF & "Payment:" & Natural'Image(Member.Payment(1)) & " " &
--               To_String(MoneyName) & " each day");
--            if Member.Payment(2) > 0 then
--               Append
--                 (MemberInfo,
--                  " and " & Natural'Image(Member.Payment(2)) &
--                  " percent of profit from each trade");
--            end if;
--            Append(MemberInfo, ".");
--         end if;
--      end if;
      MemberLabel.Interp := Interp;
      MemberLabel.Name :=
        New_String(".paned.crewframe.canvas.crew.info.info.label");
      configure(MemberLabel, "-text {" & To_String(MemberInfo) & "}");
--      Set_Label
--        (Gtk_Label(Get_Object(Object, "lblcrewinfo")), To_String(MemberInfo));
--      if PlayerShip.Speed = DOCKED and MemberIndex > 1 then
--         Show_All(Gtk_Widget(Get_Object(Object, "btndismiss")));
--      else
--         Hide(Gtk_Widget(Get_Object(Object, "btndismiss")));
--      end if;
--      ProgressBar := Gtk_Progress_Bar(Get_Object(Object, "progresshealth"));
--      Show_All(Gtk_Widget(ProgressBar));
--      Set_Fraction(ProgressBar, Gdouble(Member.Health) / 100.0);
--      if Member.Health = 100 then
--         Hide(Gtk_Widget(ProgressBar));
--      end if;
--      if GameSettings.ShowNumbers then
--         Set_Text(ProgressBar, "Health:" & Natural'Image(Member.Health) & "%");
--      else
--         case Member.Health is
--            when 81 .. 99 =>
--               Set_Text(ProgressBar, "Slightly wounded");
--            when 51 .. 80 =>
--               Set_Text(ProgressBar, "Wounded");
--            when 1 .. 50 =>
--               Set_Text(ProgressBar, "Heavily wounded");
--            when others =>
--               null;
--         end case;
--      end if;
--      TiredPoints := Member.Tired - Member.Attributes(ConditionIndex)(1);
--      if TiredPoints < 0 then
--         TiredPoints := 0;
--      end if;
--      ProgressBar := Gtk_Progress_Bar(Get_Object(Object, "progresstired"));
--      Show_All(Gtk_Widget(ProgressBar));
--      Set_Fraction(ProgressBar, Gdouble(TiredPoints) / 100.0);
--      if TiredPoints = 0 then
--         Hide(Gtk_Widget(ProgressBar));
--      end if;
--      if GameSettings.ShowNumbers then
--         Set_Text
--           (ProgressBar, "Tiredness:" & Natural'Image(TiredPoints) & "%");
--      else
--         case TiredPoints is
--            when 1 .. 40 =>
--               Set_Text(ProgressBar, "Bit tired");
--            when 41 .. 80 =>
--               Set_Text(ProgressBar, "Tired");
--            when 81 .. 99 =>
--               Set_Text(ProgressBar, "Very tired");
--            when 100 =>
--               Set_Text(ProgressBar, "Unconscious");
--            when others =>
--               null;
--         end case;
--      end if;
--      ProgressBar := Gtk_Progress_Bar(Get_Object(Object, "progressthirst"));
--      Show_All(Gtk_Widget(ProgressBar));
--      Set_Fraction(ProgressBar, Gdouble(Member.Thirst) / 100.0);
--      if Member.Thirst = 0 then
--         Hide(Gtk_Widget(ProgressBar));
--      end if;
--      if GameSettings.ShowNumbers then
--         Set_Text(ProgressBar, "Thirst:" & Natural'Image(Member.Thirst) & "%");
--      else
--         case Member.Thirst is
--            when 1 .. 40 =>
--               Set_Text(ProgressBar, "Bit thirsty");
--            when 41 .. 80 =>
--               Set_Text(ProgressBar, "Thirsty");
--            when 81 .. 99 =>
--               Set_Text(ProgressBar, "Very thirsty");
--            when 100 =>
--               Set_Text(ProgressBar, "Dehydrated");
--            when others =>
--               null;
--         end case;
--      end if;
--      ProgressBar := Gtk_Progress_Bar(Get_Object(Object, "progresshunger"));
--      Show_All(Gtk_Widget(ProgressBar));
--      Set_Fraction(ProgressBar, Gdouble(Member.Hunger) / 100.0);
--      if Member.Hunger = 0 then
--         Hide(Gtk_Widget(ProgressBar));
--      end if;
--      if GameSettings.ShowNumbers then
--         Set_Text(ProgressBar, "Hunger:" & Natural'Image(Member.Hunger) & "%");
--      else
--         case Member.Hunger is
--            when 1 .. 40 =>
--               Set_Text(ProgressBar, "Bit hungry");
--            when 41 .. 80 =>
--               Set_Text(ProgressBar, "Hungry");
--            when 81 .. 99 =>
--               Set_Text(ProgressBar, "Very hungry");
--            when 100 =>
--               Set_Text(ProgressBar, "Starving");
--            when others =>
--               null;
--         end case;
--      end if;
--      ProgressBar := Gtk_Progress_Bar(Get_Object(Object, "progressmorale"));
--      Show_All(Gtk_Widget(ProgressBar));
--      Set_Fraction(ProgressBar, Gdouble(Member.Morale(1)) / 100.0);
--      if Member.Morale(1) = 50 then
--         Hide(Gtk_Widget(ProgressBar));
--      end if;
--      if GameSettings.ShowNumbers then
--         Set_Text
--           (ProgressBar, "Morale:" & Natural'Image(Member.Morale(1)) & "%");
--      else
--         case Member.Morale(1) is
--            when 0 .. 24 =>
--               Set_Text(ProgressBar, "Upset");
--            when 25 .. 49 =>
--               Set_Text(ProgressBar, "Unhappy");
--            when 51 .. 74 =>
--               Set_Text(ProgressBar, "Happy");
--            when 75 .. 100 =>
--               Set_Text(ProgressBar, "Excited");
--            when others =>
--               null;
--         end case;
--      end if;
--      if Member.Skills.Length > 0 and Member.ContractLength /= 0 then
--         declare
--            StatisticBar, ExperienceBar: Gtk_Progress_Bar;
--            StatsBox: constant Gtk_Container :=
--              Gtk_Container(Get_Object(Object, "boxcrewstats"));
--         begin
--            Foreach(StatsBox, RemoveWidget'Access);
--            for I in Member.Attributes.Iterate loop
--               Gtk_New(StatisticBar);
--               Set_Name(Gtk_Widget(StatisticBar), "redbar");
--               Set_Show_Text(StatisticBar, True);
--               Set_Fraction
--                 (StatisticBar, Gdouble(Member.Attributes(I)(1) * 2) / 100.0);
--               Set_Text
--                 (StatisticBar,
--                  To_String
--                    (Attributes_List(Attributes_Container.To_Index(I)).Name) &
--                  ": " & GetAttributeLevelName(Member.Attributes(I)(1)));
--               Set_Tooltip_Text
--                 (Gtk_Widget(StatisticBar),
--                  To_String
--                    (Attributes_List(Attributes_Container.To_Index(I))
--                       .Description));
--               Add(StatsBox, Gtk_Widget(StatisticBar));
--               Gtk_New(ExperienceBar);
--               Set_Name(Gtk_Widget(ExperienceBar), "experience");
--               Set_Margin_Bottom(Gtk_Widget(ExperienceBar), 10);
--               Set_Fraction
--                 (ExperienceBar,
--                  Gdouble(Member.Attributes(I)(2)) /
--                  (Gdouble(Member.Attributes(I)(1) * 250)));
--               Set_Tooltip_Text
--                 (Gtk_Widget(ExperienceBar),
--                  "Experience needed to reach next level in " &
--                  To_String
--                    (Attributes_List(Attributes_Container.To_Index(I)).Name));
--               Add(StatsBox, Gtk_Widget(ExperienceBar));
--            end loop;
--            Show_All(Gtk_Widget(StatsBox));
--         end;
--         declare
--            SkillBar, ExperienceBar: Gtk_Progress_Bar;
--            SkillBox: constant Gtk_Container :=
--              Gtk_Container(Get_Object(Object, "boxcrewskills"));
--            ItemIndex, TooltipText: Unbounded_String;
--            Quality: Natural;
--         begin
--            Foreach(SkillBox, RemoveWidget'Access);
--            for Skill of Member.Skills loop
--               Gtk_New(SkillBar);
--               Set_Name(Gtk_Widget(SkillBar), "goldbar");
--               Set_Show_Text(SkillBar, True);
--               Set_Fraction(SkillBar, Gdouble(Skill(2)) / 100.0);
--               Set_Text
--                 (SkillBar,
--                  To_String(Skills_List(Skill(1)).Name) & ": " &
--                  GetSkillLevelName(Skill(2)));
--               TooltipText := Null_Unbounded_String;
--               Append(TooltipText, "Related statistic: ");
--               Append
--                 (TooltipText,
--                  Attributes_List(Skills_List(Skill(1)).Attribute).Name);
--               if Skills_List(Skill(1)).Tool /= Null_Unbounded_String then
--                  Append(TooltipText, ". Training tool: ");
--                  Quality := 0;
--                  for I in Items_List.Iterate loop
--                     if Items_List(I).IType = Skills_List(Skill(1)).Tool
--                       and then
--                       (Items_List(I).Value.Length > 0
--                        and then Items_List(I).Value(1) <=
--                          GetTrainingToolQuality(MemberIndex, Skill(1))) then
--                        if Items_List(I).Value(1) > Quality then
--                           ItemIndex := Objects_Container.Key(I);
--                           Quality := Items_List(I).Value(1);
--                        end if;
--                     end if;
--                  end loop;
--                  Append(TooltipText, Items_List(ItemIndex).Name);
--               end if;
--               Append(TooltipText, ". ");
--               Append(TooltipText, Skills_List(Skill(1)).Description);
--               Set_Tooltip_Text(Gtk_Widget(SkillBar), To_String(TooltipText));
--               Add(SkillBox, Gtk_Widget(SkillBar));
--               Gtk_New(ExperienceBar);
--               Set_Name(Gtk_Widget(ExperienceBar), "experience");
--               Set_Margin_Bottom(Gtk_Widget(ExperienceBar), 10);
--               Set_Fraction
--                 (ExperienceBar, Gdouble(Skill(3)) / (Gdouble(Skill(2) * 25)));
--               Set_Tooltip_Text
--                 (Gtk_Widget(ExperienceBar),
--                  "Experience needed to reach next level in " &
--                  To_String(Skills_List(Skill(1)).Name));
--               Add(SkillBox, Gtk_Widget(ExperienceBar));
--            end loop;
--            Show_All(Gtk_Widget(SkillBox));
--         end;
--      end if;
--      Foreach
--        (Gtk_List_Store(Get_Object(Builder, "prioritieslist")),
--         UpdatePriorities'Access);
      return TCL_OK;
   end Show_Member_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCrewInfo", Show_Crew_Info_Command'Access);
      AddCommand("SetCrewOrder", Set_Crew_Order_Command'Access);
      AddCommand("ShowMemberInfo", Show_Member_Info_Command'Access);
   end AddCommands;

end Crew.UI;
