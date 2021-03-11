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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Config; use Config;
with Game; use Game;

package body Themes is

   procedure LoadThemes is
      Directories, Files: Search_Type;
      FoundDirectory, FoundFile: Directory_Entry_Type;
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      TempRecord: ThemeRecord;
      procedure SetDefaultValues is
      begin
         TempRecord :=
           (Name => Null_Unbounded_String, FileName => Null_Unbounded_String,
            EnemyShipIcon => Wide_Character'Val(16#f51c#),
            AttackOnBaseIcon => Wide_Character'Val(16#f543#),
            DiseaseIcon => Wide_Character'Val(16#f5a6#),
            DoublePriceIcon => Wide_Character'Val(16#f0d6#),
            FullDocksIcon => Wide_Character'Val(16#f057#),
            EnemyPatrolIcon => Wide_Character'Val(16#f51b#),
            TraderIcon => Wide_Character'Val(16#f197#),
            FriendlyShipIcon => Wide_Character'Val(16#f197#),
            DeliverIcon => Wide_Character'Val(16#f53b#),
            DestroyIcon => Wide_Character'Val(16#fc6a#),
            PatrolIcon => Wide_Character'Val(16#f540#),
            ExploreIcon => Wide_Character'Val(16#f707#),
            PassengerIcon => Wide_Character'Val(16#f183#),
            PilotIcon => Wide_Character'Val(16#f655#),
            EngineerIcon => Wide_Character'Val(16#f013#),
            GunnerIcon => Wide_Character'Val(16#f4fb#),
            CrewTraderIcon => Wide_Character'Val(16#f651#),
            RepairIcon => Wide_Character'Val(16#f54a#),
            UpgradeIcon => Wide_Character'Val(16#f6e3#),
            CleanIcon => Wide_Character'Val(16#f458#),
            ManufactureIcon => Wide_Character'Val(16#f0e3#),
            MoveMapUpIcon => Wide_Character'Val(16#2191#),
            MoveMapDownIcon => Wide_Character'Val(16#2193#),
            MoveMapLeftIcon => Wide_Character'Val(16#2190#),
            MoveMapRightIcon => Wide_Character'Val(16#2192#),
            NoFuelIcon => Wide_Character'Val(16#f2ca#),
            NoFoodIcon => Wide_Character'Val(16#f787#),
            NoDrinksIcon => Wide_Character'Val(16#f72f#),
            NotVisitedBaseIcon => Wide_Character'Val(16#229b#),
            PlayerShipIcon => Wide_Character'Val(16#f135#),
            EmptyMapIcon => Wide_Character'Val(16#f0c8#),
            TargetIcon => Wide_Character'Val(16#f05b#),
            StoryIcon => Wide_Character'Val(16#f059#),
            OverloadedIcon => Wide_Character'Val(16#f55b#));
      end SetDefaultValues;
   begin
      SetDefaultValues;
      TempRecord.Name := To_Unbounded_String("Default theme");
      TempRecord.FileName :=
        Data_Directory &
        To_Unbounded_String("ui" & Dir_Separator & "theme.tcl");
      Themes_Container.Include(Themes_List, "steamsky", TempRecord);
      SetDefaultValues;
      Start_Search
        (Directories, To_String(Themes_Directory), "",
         (Directory => True, others => False));
      while More_Entries(Directories) loop
         Get_Next_Entry(Directories, FoundDirectory);
         if Simple_Name(FoundDirectory) in "." | ".." then
            goto End_Of_Load_Themes_Loop;
         end if;
         Start_Search(Files, Full_Name(FoundDirectory), "*.cfg");
         while More_Entries(Files) loop
            Get_Next_Entry(Files, FoundFile);
            Open(ConfigFile, In_File, Full_Name(FoundFile));
            while not End_Of_File(ConfigFile) loop
               RawData := To_Unbounded_String(Get_Line(ConfigFile));
               if Length(RawData) = 0 then
                  goto End_Of_Load_Config_Loop;
               end if;
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Name") then
                  TempRecord.Name := Value;
               elsif FieldName = To_Unbounded_String("FileName") then
                  TempRecord.FileName :=
                    To_Unbounded_String
                      (Full_Name(FoundDirectory) & Dir_Separator) &
                    Value;
               elsif FieldName = To_Unbounded_String("EnemyShipIcon") then
                  TempRecord.EnemyShipIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("AttackOnBaseIcon") then
                  TempRecord.AttackOnBaseIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DiseaseIcon") then
                  TempRecord.DiseaseIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DoublePriceIcon") then
                  TempRecord.DoublePriceIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("FullDocksIcon") then
                  TempRecord.FullDocksIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EnemyPatrolIcon") then
                  TempRecord.EnemyPatrolIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("TraderIcon") then
                  TempRecord.TraderIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("FriendlyShipIcon") then
                  TempRecord.FriendlyShipIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DeliverIcon") then
                  TempRecord.DeliverIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("DestroyIcon") then
                  TempRecord.DestroyIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PatrolIcon") then
                  TempRecord.PatrolIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("ExploreIcon") then
                  TempRecord.ExploreIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PassengerIcon") then
                  TempRecord.PassengerIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("PilotIcon") then
                  TempRecord.PilotIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EngineerIcon") then
                  TempRecord.EngineerIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("GunnerIcon") then
                  TempRecord.GunnerIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("CrewTraderIcon") then
                  TempRecord.CrewTraderIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("RepairIcon") then
                  TempRecord.RepairIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("UpgradeIcon") then
                  TempRecord.UpgradeIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("CleanIcon") then
                  TempRecord.CleanIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("ManufactureIcon") then
                  TempRecord.ManufactureIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapUpIcon") then
                  TempRecord.MoveMapUpIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapDownIcon") then
                  TempRecord.MoveMapDownIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapLeftIcon") then
                  TempRecord.MoveMapLeftIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("MoveMapRightIcon") then
                  TempRecord.MoveMapRightIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoFuelIcon") then
                  TempRecord.NoFuelIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoFoodIcon") then
                  TempRecord.NoFoodIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NoDrinksIcon") then
                  TempRecord.NoDrinksIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("NotVisitedBaseIcon") then
                  TempRecord.NotVisitedBaseIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("EmptyMapIcon") then
                  TempRecord.EmptyMapIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("TargetIcon") then
                  TempRecord.TargetIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("StoryIcon") then
                  TempRecord.StoryIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               elsif FieldName = To_Unbounded_String("OverloadedIcon") then
                  TempRecord.OverloadedIcon :=
                    Wide_Character'Val
                      (Natural'Value("16#" & To_String(Value) & "#"));
               end if;
               <<End_Of_Load_Config_Loop>>
            end loop;
            Close(ConfigFile);
            Themes_Container.Include
              (Themes_List, Simple_Name(FoundDirectory), TempRecord);
            SetDefaultValues;
         end loop;
         End_Search(Files);
         <<End_Of_Load_Themes_Loop>>
      end loop;
      End_Search(Directories);
      if not Themes_List.Contains(To_String(GameSettings.InterfaceTheme)) then
         GameSettings.InterfaceTheme := To_Unbounded_String("steamsky");
      end if;
   end LoadThemes;

   procedure SetTheme is
      Label: Ttk_Label;
      Button: Ttk_Button;
   begin
      Label.Interp := Get_Context;
      Button.Interp := Get_Context;
      for I in Themes_List.Iterate loop
         if Themes_Container.Key(I) /= GameSettings.InterfaceTheme then
            goto End_Of_Set_Theme_Loop;
         end if;
         Label.Name := New_String(".gameframe.header.nofuel");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).NoFuelIcon) & "}");
         Label.Name := New_String(".gameframe.header.nofood");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).NoFoodIcon) & "}");
         Label.Name := New_String(".gameframe.header.nodrink");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).NoDrinksIcon) & "}");
         Label.Name := New_String(".gameframe.header.overloaded");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).OverloadedIcon) & "}");
         Label.Name := New_String(".gameframe.header.pilot");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).PilotIcon) & "}");
         Label.Name := New_String(".gameframe.header.engineer");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).EngineerIcon) & "}");
         Label.Name := New_String(".gameframe.header.gunner");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).GunnerIcon) & "}");
         Label.Name := New_String(".gameframe.header.talk");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).CrewTraderIcon) & "}");
         Label.Name := New_String(".gameframe.header.repairs");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).RepairIcon) & "}");
         Label.Name := New_String(".gameframe.header.upgrade");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).UpgradeIcon) & "}");
         Label.Name := New_String(".gameframe.header.clean");
         configure
           (Label, "-text {" & Encode("" & Themes_List(I).CleanIcon) & "}");
         Label.Name := New_String(".gameframe.header.crafting");
         configure
           (Label,
            "-text {" & Encode("" & Themes_List(I).ManufactureIcon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.show");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).MoveMapUpIcon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.hide");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).MoveMapDownIcon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.left");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).MoveMapLeftIcon) & "}");
         Button.Name := New_String(".gameframe.paned.mapframe.buttons.right");
         configure
           (Button,
            "-text {" & Encode("" & Themes_List(I).MoveMapRightIcon) & "}");
         <<End_Of_Set_Theme_Loop>>
      end loop;
   end SetTheme;

end Themes;
