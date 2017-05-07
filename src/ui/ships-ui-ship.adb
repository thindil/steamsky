--    Copyright 2016-2017 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Items; use Items;
with ShipModules; use ShipModules;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Crafts; use Crafts;
with Maps; use Maps;
with Help.UI; use Help.UI;
with Ships.Cargo; use Ships.Cargo;
with Ships.Upgrade; use Ships.Upgrade;

package body Ships.UI.Ship is

   OptionsMenu: Menu;
   MenuWindow, MenuWindow2: Window;
   CurrentMenuIndex: Positive := 1;

   procedure ShowModuleInfo is
      InfoWindow: Window;
      ModuleIndex: constant Positive := Get_Index(Current(ShipsMenu));
      DamagePercent: Natural;
      MAmount, TextLength: Natural := 0;
      CurrentLine, StartLine: Line_Position;
      MaxUpgrade, UpgradePercent: Natural;
      MaxValue: Positive;
      HaveAmmo, HaveMaterial: Boolean := False;
      StartColumn, EndColumn: Column_Position;
      Module: constant ModuleData := PlayerShip.Modules(ModuleIndex);
   begin
      InfoWindow := Create((Lines - 3), (Columns / 2), 2, (Columns / 2));
      Add(Win => InfoWindow, Str => "Status: ");
      DamagePercent :=
        100 -
        Natural
          ((Float(Module.Durability) / Float(Module.MaxDurability)) * 100.0);
      if DamagePercent = 0 then
         Add(Win => InfoWindow, Str => "Ok");
      elsif DamagePercent > 0 and DamagePercent < 20 then
         Add(Win => InfoWindow, Str => "Slightly damaged");
      elsif DamagePercent > 19 and DamagePercent < 50 then
         Add(Win => InfoWindow, Str => "Damaged");
      elsif DamagePercent > 49 and DamagePercent < 80 then
         Add(Win => InfoWindow, Str => "Heavily damaged");
      elsif DamagePercent > 79 and DamagePercent < 100 then
         Add(Win => InfoWindow, Str => "Almost destroyed");
      else
         Add(Win => InfoWindow, Str => "Destroyed");
      end if;
      MaxValue :=
        Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
      if Module.MaxDurability = MaxValue then
         Add(Win => InfoWindow, Str => " (max upgrade)");
      end if;
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Weight:" & Integer'Image(Module.Weight) & " kg");
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
      Add(Win => InfoWindow, Str => "Repair/Upgrade material: ");
      for Item of Items_List loop
         if Item.IType = Modules_List(Module.ProtoIndex).RepairMaterial then
            if MAmount > 0 then
               Add(Win => InfoWindow, Str => " or ");
            end if;
            Get_Cursor_Position
              (Win => InfoWindow,
               Line => StartLine,
               Column => StartColumn);
            Add(Win => InfoWindow, Str => To_String(Item.Name));
            Get_Cursor_Position
              (Win => InfoWindow,
               Line => CurrentLine,
               Column => EndColumn);
            if FindCargo(ItemType => Item.IType) > 0 then
               HaveMaterial := True;
            end if;
            if not HaveMaterial then
               if StartLine = CurrentLine then
                  TextLength := Natural(EndColumn - StartColumn);
                  Change_Attributes
                    (Win => InfoWindow,
                     Line => StartLine,
                     Column => StartColumn,
                     Count => Integer(StartColumn) + TextLength,
                     Color => 3);
               else
                  TextLength := Natural((Columns / 2) - StartColumn);
                  Change_Attributes
                    (Win => InfoWindow,
                     Line => StartLine,
                     Column => StartColumn,
                     Count => Integer(StartColumn) + TextLength,
                     Color => 3);
                  Change_Attributes
                    (Win => InfoWindow,
                     Line => CurrentLine,
                     Column => 0,
                     Count => Integer(EndColumn),
                     Color => 3);
               end if;
               Move_Cursor
                 (Win => InfoWindow,
                  Line => CurrentLine,
                  Column => EndColumn);
            end if;
            HaveMaterial := False;
            MAmount := MAmount + 1;
         end if;
      end loop;
      CurrentLine := 5;
      Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Repair/Upgrade skill: " &
           To_String
             (Skills_Names(Modules_List(Module.ProtoIndex).RepairSkill)));
      Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
      case Modules_List(Module.ProtoIndex).MType is
         when ENGINE =>
            Add
              (Win => InfoWindow,
               Str => "Max power:" & Integer'Image(Module.Max_Value));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Max_Value = MaxValue then
               Add(Win => InfoWindow, Str => " (max upgrade)");
            end if;
            Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
            Add
              (Win => InfoWindow,
               Str => "Fuel usage:" & Integer'Image(Module.Current_Value));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).Value) / 2.0);
            if Module.Current_Value = MaxValue then
               Add(Win => InfoWindow, Str => " (max upgrade)");
            end if;
            CurrentLine := CurrentLine + 1;
         when ShipModules.CARGO =>
            Add
              (Win => InfoWindow,
               Str => "Max cargo:" & Integer'Image(Module.Max_Value) & " kg");
         when HULL =>
            Add
              (Win => InfoWindow,
               Str =>
                 "Modules space:" &
                 Integer'Image(Module.Current_Value) &
                 " /" &
                 Integer'Image(Module.Max_Value));
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Max_Value = MaxValue then
               Add(Win => InfoWindow, Str => " (max upgrade)");
            end if;
         when CABIN =>
            if Module.Owner > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Owner: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Add(Win => InfoWindow, Str => "Owner: none");
            end if;
            Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
            Add(Win => InfoWindow, Str => "Quality: ");
            if Module.Max_Value < 30 then
               Add(Win => InfoWindow, Str => "minimal");
            elsif Module.Max_Value > 29 and Module.Max_Value < 60 then
               Add(Win => InfoWindow, Str => "basic");
            elsif Module.Max_Value > 59 and Module.Max_Value < 80 then
               Add(Win => InfoWindow, Str => "extended");
            else
               Add(Win => InfoWindow, Str => "luxury");
            end if;
            MaxValue :=
              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
            if Module.Max_Value = MaxValue then
               Add(Win => InfoWindow, Str => " (max upgrade)");
            end if;
            DamagePercent :=
              100 -
              Natural
                ((Float(Module.Current_Value) / Float(Module.Max_Value)) *
                 100.0);
            Move_Cursor(Win => InfoWindow, Line => 6, Column => 0);
            Add(Win => InfoWindow, Str => "State: ");
            if DamagePercent = 0 then
               Add(Win => InfoWindow, Str => "clean");
            elsif DamagePercent > 0 and DamagePercent < 20 then
               Add(Win => InfoWindow, Str => "bit dusty");
            elsif DamagePercent > 19 and DamagePercent < 50 then
               Add(Win => InfoWindow, Str => "dusty");
            elsif DamagePercent > 49 and DamagePercent < 80 then
               Add(Win => InfoWindow, Str => "dirty");
            elsif DamagePercent > 79 and DamagePercent < 100 then
               Add(Win => InfoWindow, Str => "very dirty");
            else
               Add(Win => InfoWindow, Str => "ruined");
            end if;
            CurrentLine := CurrentLine + 2;
         when GUN =>
            Add(Win => InfoWindow, Str => "Ammunition: ");
            if Module.Current_Value >= PlayerShip.Cargo.First_Index and
              Module.Current_Value <= PlayerShip.Cargo.Last_Index then
               if Items_List(PlayerShip.Cargo(Module.Current_Value).ProtoIndex)
                   .IType =
                 Items_Types(Modules_List(Module.ProtoIndex).Value) then
                  Add
                    (Win => InfoWindow,
                     Str =>
                       To_String
                         (Items_List
                            (PlayerShip.Cargo(Module.Current_Value).ProtoIndex)
                            .Name) &
                       " (assigned)");
                  HaveAmmo := True;
               end if;
            end if;
            if not HaveAmmo then
               MAmount := 0;
               for I in Items_List.Iterate loop
                  if Items_List(I).IType =
                    Items_Types(Modules_List(Module.ProtoIndex).Value) then
                     if MAmount > 0 then
                        Add(Win => InfoWindow, Str => " or ");
                     end if;
                     Get_Cursor_Position
                       (Win => InfoWindow,
                        Line => StartLine,
                        Column => StartColumn);
                     Add
                       (Win => InfoWindow,
                        Str => To_String(Items_List(I).Name));
                     Get_Cursor_Position
                       (Win => InfoWindow,
                        Line => CurrentLine,
                        Column => EndColumn);
                     if FindCargo(Objects_Container.To_Index(I)) > 0 then
                        HaveAmmo := True;
                     end if;
                     if not HaveAmmo then
                        if StartLine = CurrentLine then
                           TextLength := Natural(EndColumn - StartColumn);
                           Change_Attributes
                             (Win => InfoWindow,
                              Line => StartLine,
                              Column => StartColumn,
                              Count => Integer(StartColumn) + TextLength,
                              Color => 3);
                        else
                           TextLength := Natural((Columns / 2) - StartColumn);
                           Change_Attributes
                             (Win => InfoWindow,
                              Line => StartLine,
                              Column => StartColumn,
                              Count => Integer(StartColumn) + TextLength,
                              Color => 3);
                           Change_Attributes
                             (Win => InfoWindow,
                              Line => CurrentLine,
                              Column => 0,
                              Count => Integer(EndColumn),
                              Color => 3);
                        end if;
                        Move_Cursor
                          (Win => InfoWindow,
                           Line => CurrentLine,
                           Column => EndColumn);
                     end if;
                     HaveAmmo := False;
                     MAmount := MAmount + 1;
                  end if;
               end loop;
               CurrentLine := CurrentLine + 1;
            end if;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            if Module.Owner > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Gunner: " &
                    To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Add(Win => InfoWindow, Str => "Gunner: none");
            end if;
            CurrentLine := CurrentLine + 1;
         when TURRET =>
            if Module.Current_Value > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Weapon: " &
                    To_String(PlayerShip.Modules(Module.Current_Value).Name));
            else
               Add(Win => InfoWindow, Str => "Weapon: none");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if Module.Owner > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Worker: " &
                    To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Add(Win => InfoWindow, Str => "Worker: none");
            end if;
            Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
            if Module.Current_Value /= 0 then
               if Module.Current_Value > 0 then
                  Add
                    (Win => InfoWindow,
                     Str =>
                       "Manufacturing: " &
                       To_String
                         (Items_List
                            (Recipes_List(Module.Current_Value).ResultIndex)
                            .Name));
               else
                  Add
                    (Win => InfoWindow,
                     Str =>
                       "Deconstructing " &
                       To_String(Items_List(abs (Module.Current_Value)).Name));
               end if;
               Move_Cursor(Win => InfoWindow, Line => 6, Column => 0);
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Time to complete:" &
                    Positive'Image(Module.Max_Value) &
                    " minutes");
               CurrentLine := CurrentLine + 1;
            else
               Add(Win => InfoWindow, Str => "Manufacturing: nothing");
            end if;
            CurrentLine := CurrentLine + 1;
         when MEDICAL_ROOM =>
            if Module.Owner > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Medic: " & To_String(PlayerShip.Crew(Module.Owner).Name));
            else
               Add(Win => InfoWindow, Str => "Medic: none");
            end if;
         when others =>
            CurrentLine := CurrentLine - 1;
      end case;
      if Modules_List(Module.ProtoIndex).Size > 0 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Size:" & Natural'Image(Modules_List(Module.ProtoIndex).Size));
         CurrentLine := CurrentLine + 1;
      end if;
      if Module.UpgradeAction /= NONE then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Str => "Upgrading: ");
         case Module.UpgradeAction is
            when DURABILITY =>
               Add(Win => InfoWindow, Str => "durability");
               MaxUpgrade := 10;
            when MAX_VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Add(Win => InfoWindow, Str => "power");
                     MaxUpgrade := 10;
                  when CABIN =>
                     Add(Win => InfoWindow, Str => "quality");
                     MaxUpgrade := 100;
                  when GUN | BATTERING_RAM =>
                     Add(Win => InfoWindow, Str => "damage");
                     MaxUpgrade := 100;
                  when HULL =>
                     Add(Win => InfoWindow, Str => "enlarge");
                     MaxUpgrade := 500;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List(Module.ProtoIndex).MType is
                  when ENGINE =>
                     Add(Win => InfoWindow, Str => "fuel usage");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Str => "Upgrade progress: ");
         UpgradePercent :=
           100 -
           Natural
             ((Float(Module.UpgradeProgress) / Float(MaxUpgrade)) * 100.0);
         if UpgradePercent < 11 then
            Add(Win => InfoWindow, Str => "started");
         elsif UpgradePercent < 31 then
            Add(Win => InfoWindow, Str => "designing");
         elsif UpgradePercent < 51 then
            Add(Win => InfoWindow, Str => "base upgrades");
         elsif UpgradePercent < 80 then
            Add(Win => InfoWindow, Str => "advanced upgrades");
         else
            Add(Win => InfoWindow, Str => "final upgrades");
         end if;
         CurrentLine := CurrentLine + 1;
      end if;
      if Modules_List(Module.ProtoIndex).Description /=
        Null_Unbounded_String then
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str => To_String(Modules_List(Module.ProtoIndex).Description));
         Get_Cursor_Position
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => StartColumn);
         CurrentLine := CurrentLine + 1;
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Press Enter to see selected module options");
      Change_Attributes
        (Win => InfoWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowModuleInfo;

   procedure ShowShipInfo is
      Weight: Integer;
      Modules_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (PlayerShip.Modules.Last_Index + 1));
      MenuHeight, CurrentLine: Line_Position;
      MenuLength: Column_Position;
      UpgradePercent, MaxUpgrade: Natural;
   begin
      Weight := CountShipWeight(PlayerShip);
      Move_Cursor(Line => 2, Column => 2);
      Add(Str => "Name: " & To_String(PlayerShip.Name));
      Change_Attributes(Line => 2, Column => 2, Count => 1, Color => 1);
      Move_Cursor(Line => 3, Column => 2);
      Add(Str => "Upgrading: ");
      if PlayerShip.UpgradeModule = 0 then
         Add(Str => "Nothing");
         CurrentLine := 4;
      else
         Add
           (Str =>
              To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
              " ");
         case PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction is
            when DURABILITY =>
               Add(Str => "(durability)");
               MaxUpgrade := 10;
            when MAX_VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Add(Str => "(power)");
                     MaxUpgrade := 10;
                  when CABIN =>
                     Add(Str => "(quality)");
                     MaxUpgrade := 100;
                  when GUN | BATTERING_RAM =>
                     Add(Str => "(damage)");
                     MaxUpgrade := 100;
                  when HULL =>
                     Add(Str => "(enlarge)");
                     MaxUpgrade := 500;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Add(Str => "(fuel usage)");
                     MaxUpgrade := 100;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Move_Cursor(Line => 4, Column => 2);
         Add(Str => "Upgrade progress: ");
         UpgradePercent :=
           100 -
           Natural
             ((Float
                 (PlayerShip.Modules(PlayerShip.UpgradeModule)
                    .UpgradeProgress) /
               Float(MaxUpgrade)) *
              100.0);
         if UpgradePercent < 11 then
            Add(Str => "started");
         elsif UpgradePercent < 31 then
            Add(Str => "designing");
         elsif UpgradePercent < 51 then
            Add(Str => "base upgrades");
         elsif UpgradePercent < 80 then
            Add(Str => "advanced upgrades");
         else
            Add(Str => "final upgrades");
         end if;
         CurrentLine := 5;
      end if;
      Move_Cursor(Line => CurrentLine, Column => 2);
      Add(Str => "Repair first: ");
      if PlayerShip.RepairModule = 0 then
         Add(Str => "Any module");
      else
         Add
           (Str =>
              To_String(PlayerShip.Modules(PlayerShip.RepairModule).Name));
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => 2);
      Add(Str => "Destination: ");
      if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
         Add(Str => "None");
      else
         if SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
             .BaseIndex >
           0 then
            Add
              (Str =>
                 To_String
                   (SkyBases
                      (SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
                         .BaseIndex)
                      .Name));
         else
            Add
              (Str =>
                 "X:" &
                 Positive'Image(PlayerShip.DestinationX) &
                 " Y:" &
                 Positive'Image(PlayerShip.DestinationY));
         end if;
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => 2);
      Add(Str => "Weight:" & Integer'Image(Weight) & "kg");
      for I in
        PlayerShip.Modules.First_Index .. PlayerShip.Modules.Last_Index loop
         Modules_Items.all(I) :=
           New_Item(To_String(PlayerShip.Modules(I).Name));
      end loop;
      Modules_Items.all(Modules_Items'Last) := Null_Item;
      ShipsMenu := New_Menu(Modules_Items);
      Set_Format(ShipsMenu, Lines - 10, 1);
      Set_Mark(ShipsMenu, "");
      Scale(ShipsMenu, MenuHeight, MenuLength);
      CurrentLine := CurrentLine + 2;
      MenuWindow := Create(MenuHeight, MenuLength, CurrentLine, 2);
      Set_Window(ShipsMenu, MenuWindow);
      Set_Sub_Window
        (ShipsMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(ShipsMenu);
      Set_Current(ShipsMenu, Modules_Items.all(CurrentMenuIndex));
      ShowModuleInfo;
      Refresh(MenuWindow);
   end ShowShipInfo;

   procedure ShowModuleOptions is
      ModuleIndex: constant Positive := Get_Index(Current(ShipsMenu));
      Options_Items: constant Item_Array_Access := new Item_Array(1 .. 11);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MaxValue: Positive;
      MenuIndex: Positive := 1;
   begin
      MaxValue :=
        Natural
          (Float
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability) *
           1.5);
      if PlayerShip.Modules(ModuleIndex).MaxDurability < MaxValue then
         Options_Items.all(MenuIndex) := New_Item("Upgrade durability", "1");
         MenuIndex := MenuIndex + 1;
      end if;
      case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
         when ENGINE =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Max_Value < MaxValue then
               Options_Items.all(MenuIndex) :=
                 New_Item("Upgrade engine power", "2");
               MenuIndex := MenuIndex + 1;
            end if;
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Value) /
                 2.0);
            if PlayerShip.Modules(ModuleIndex).Current_Value > MaxValue then
               Options_Items.all(MenuIndex) :=
                 New_Item("Reduce fuel usage", "3");
               MenuIndex := MenuIndex + 1;
            end if;
         when CABIN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Max_Value < MaxValue then
               Options_Items.all(MenuIndex) :=
                 New_Item("Upgrade quality", "2");
               MenuIndex := MenuIndex + 1;
            end if;
            Options_Items.all(MenuIndex) := New_Item("Assign owner", "7");
            MenuIndex := MenuIndex + 1;
         when GUN =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Max_Value < MaxValue then
               Options_Items.all(MenuIndex) := New_Item("Upgrade damage", "2");
               MenuIndex := MenuIndex + 1;
            end if;
            Options_Items.all(MenuIndex) := New_Item("Assign gunner", "7");
            MenuIndex := MenuIndex + 1;
            Options_Items.all(MenuIndex) := New_Item("Assign ammo", "9");
            MenuIndex := MenuIndex + 1;
         when BATTERING_RAM =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Max_Value < MaxValue then
               Options_Items.all(MenuIndex) := New_Item("Upgrade damage", "2");
               MenuIndex := MenuIndex + 1;
            end if;
         when HULL =>
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).Max_Value < MaxValue then
               Options_Items.all(MenuIndex) := New_Item("Enlarge hull", "2");
               MenuIndex := MenuIndex + 1;
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            if PlayerShip.Modules(ModuleIndex).Current_Value > 0 then
               Options_Items.all(MenuIndex) := New_Item("Assign worker", "7");
               MenuIndex := MenuIndex + 1;
            end if;
         when MEDICAL_ROOM =>
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 and
                 FindCargo(ItemType => HealingTools) > 0 then
                  Options_Items.all(MenuIndex) :=
                    New_Item("Assign medic", "7");
                  MenuIndex := MenuIndex + 1;
                  exit;
               end if;
            end loop;
         when others =>
            null;
      end case;
      if PlayerShip.Modules(ModuleIndex).UpgradeAction /= NONE then
         Options_Items.all(MenuIndex) := New_Item("Continue upgrade", "4");
         MenuIndex := MenuIndex + 1;
      end if;
      if PlayerShip.UpgradeModule > 0 then
         Options_Items.all(MenuIndex) := New_Item("Stop upgrading", "8");
         MenuIndex := MenuIndex + 1;
      end if;
      if PlayerShip.RepairModule /= ModuleIndex then
         Options_Items.all(MenuIndex) := New_Item("Repair as first", "10");
         MenuIndex := MenuIndex + 1;
      end if;
      if PlayerShip.RepairModule > 0 then
         Options_Items.all(MenuIndex) :=
           New_Item("Remove repair priority", "11");
         MenuIndex := MenuIndex + 1;
      end if;
      Options_Items.all(MenuIndex) := New_Item("Rename", "5");
      MenuIndex := MenuIndex + 1;
      Options_Items.all(MenuIndex) := New_Item("Quit", "6");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Options_Items'Last loop
         Options_Items.all(I) := Null_Item;
      end loop;
      OptionsMenu := New_Menu(Options_Items);
      Set_Mark(OptionsMenu, "");
      Set_Options(OptionsMenu, (Show_Descriptions => False, others => True));
      Scale(OptionsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow2);
      Set_Window(OptionsMenu, MenuWindow2);
      Set_Sub_Window
        (OptionsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OptionsMenu);
      Refresh;
      Refresh(MenuWindow2);
   end ShowModuleOptions;

   procedure ShowAssignMenu is
      ModuleIndex: constant Positive := Get_Index(Current(ShipsMenu));
      Assign_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (PlayerShip.Crew.Last_Index + 2));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
   begin
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Modules(ModuleIndex).Owner /= I then
            case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
              .MType is
               when MEDICAL_ROOM =>
                  if PlayerShip.Crew(I).Health = 100 then
                     Assign_Items.all(MenuIndex) :=
                       New_Item
                         ("Assign " & To_String(PlayerShip.Crew(I).Name),
                          Positive'Image(I));
                     MenuIndex := MenuIndex + 1;
                  end if;
               when others =>
                  Assign_Items.all(MenuIndex) :=
                    New_Item
                      ("Assign " & To_String(PlayerShip.Crew(I).Name),
                       Positive'Image(I));
                  MenuIndex := MenuIndex + 1;
            end case;
         end if;
      end loop;
      Assign_Items.all(MenuIndex) := New_Item("Quit", "0");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Assign_Items'Last loop
         Assign_Items.all(I) := Null_Item;
      end loop;
      OptionsMenu := New_Menu(Assign_Items);
      Set_Options(OptionsMenu, (Show_Descriptions => False, others => True));
      Set_Mark(OptionsMenu, "");
      Scale(OptionsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow2);
      Set_Window(OptionsMenu, MenuWindow2);
      Set_Sub_Window
        (OptionsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OptionsMenu);
      Refresh;
      Refresh(MenuWindow2);
   end ShowAssignMenu;

   function ShowAssignAmmoMenu return GameStates is
      ModuleIndex: constant Positive := Get_Index(Current(ShipsMenu));
      Assign_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (PlayerShip.Cargo.Last_Index + 2));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
   begin
      for I in PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
         if Items_List(PlayerShip.Cargo(I).ProtoIndex).IType =
           Items_Types
             (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Value) and
           I /= PlayerShip.Modules(ModuleIndex).Current_Value then
            Assign_Items.all(MenuIndex) :=
              New_Item
                (To_String(Items_List(PlayerShip.Cargo(I).ProtoIndex).Name),
                 Positive'Image(I));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      if MenuIndex = 1 then
         ShowDialog("You don't have any ammo to this gun.");
         DrawGame(Ship_Info);
         return Ship_Info;
      end if;
      Assign_Items.all(MenuIndex) := New_Item("Quit", "0");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Assign_Items'Last loop
         Assign_Items.all(I) := Null_Item;
      end loop;
      OptionsMenu := New_Menu(Assign_Items);
      Set_Options(OptionsMenu, (Show_Descriptions => False, others => True));
      Set_Mark(OptionsMenu, "");
      Scale(OptionsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow2);
      Set_Window(OptionsMenu, MenuWindow2);
      Set_Sub_Window
        (OptionsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OptionsMenu);
      Refresh;
      Refresh(MenuWindow2);
      return Assign_Ammo;
   end ShowAssignAmmoMenu;

   function ShipInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when Character'Pos('q') |
           Character'Pos('Q') => -- Back to sky map or combat screen
            CurrentMenuIndex := 1;
            DrawGame(OldState);
            return OldState;
         when 56 | KEY_UP => -- Select previous module
            Result := Driver(ShipsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               ShowModuleInfo;
               Refresh(MenuWindow);
            end if;
         when 50 | KEY_DOWN => -- Select next module
            Result := Driver(ShipsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               ShowModuleInfo;
               Refresh(MenuWindow);
            end if;
         when 10 => -- Show module options menu
            ShowModuleOptions;
            return Module_Options;
         when Character'Pos('n') | Character'Pos('N') => -- Rename ship
            ShowShipForm("New name for ship:");
            return Rename_Ship;
         when Key_F1 => -- Show help
            Erase;
            ShowGameHeader(Help_Topic);
            ShowHelp(Ship_Info, 6);
            return Help_Topic;
         when others =>
            Result := Driver(ShipsMenu, Key);
            if Result = Menu_Ok then
               ShowModuleInfo;
               Refresh(MenuWindow);
            else
               Result := Driver(ShipsMenu, M_Clear_Pattern);
               Result := Driver(ShipsMenu, Key);
               if Result = Menu_Ok then
                  ShowModuleInfo;
                  Refresh(MenuWindow);
               end if;
            end if;
      end case;
      CurrentMenuIndex := Get_Index(Current(ShipsMenu));
      return Ship_Info;
   end ShipInfoKeys;

   function ModuleOptionsKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Positive :=
        Positive'Value(Description(Current(OptionsMenu)));
      ModuleName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 10 => -- Select option from menu
            Post(OptionsMenu, False);
            if OptionIndex /= 5 and OptionIndex < 7 then
               if OptionIndex < 5 then
                  StartUpgrading(CurrentMenuIndex, OptionIndex);
                  UpdateOrders;
               end if;
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 5 then
               DrawGame(Ship_Info);
               ShowShipForm("New name for " & ModuleName & ":");
               return Rename_Module;
            elsif OptionIndex = 7 then
               DrawGame(Ship_Info);
               ShowAssignMenu;
               return Assign_Owner;
            elsif OptionIndex = 8 then
               PlayerShip.UpgradeModule := 0;
               for I in
                 PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
                  if PlayerShip.Crew(I).Order = Upgrading then
                     GiveOrders(I, Rest);
                     exit;
                  end if;
               end loop;
               AddMessage("You stopped current upgrade.", OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 9 then
               DrawGame(Ship_Info);
               return ShowAssignAmmoMenu;
            elsif OptionIndex = 10 then
               PlayerShip.RepairModule := CurrentMenuIndex;
               AddMessage
                 ("You assigned " & ModuleName & " as repair priority.",
                  OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 11 then
               PlayerShip.RepairModule := 0;
               AddMessage("You removed repair priority.", OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            end if;
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            else
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
               if Result = Menu_Ok then
                  Refresh(MenuWindow2);
               end if;
            end if;
      end case;
      return Module_Options;
   end ModuleOptionsKeys;

   function AssignOwnerKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Natural :=
        Positive'Value(Description(Current(OptionsMenu)));
      ModuleName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 10 => -- Select new module owner
            Post(OptionsMenu, False);
            if OptionIndex /= 0 then
               case Modules_List
                 (PlayerShip.Modules(CurrentMenuIndex).ProtoIndex)
                 .MType is
                  when CABIN =>
                     for I in PlayerShip.Modules.Iterate loop
                        if PlayerShip.Modules(I).Owner = OptionIndex and
                          Modules_List(PlayerShip.Modules(I).ProtoIndex)
                              .MType =
                            CABIN then
                           PlayerShip.Modules(I).Owner := 0;
                        end if;
                     end loop;
                     UpdateModule
                       (PlayerShip,
                        CurrentMenuIndex,
                        "Owner",
                        Positive'Image(OptionIndex));
                     AddMessage
                       ("You assigned " &
                        ModuleName &
                        " to " &
                        To_String(PlayerShip.Crew(OptionIndex).Name) &
                        ".",
                        OrderMessage);
                  when GUN =>
                     GiveOrders(OptionIndex, Gunner, CurrentMenuIndex);
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     GiveOrders(OptionIndex, Craft, CurrentMenuIndex);
                  when MEDICAL_ROOM =>
                     GiveOrders(OptionIndex, Heal, CurrentMenuIndex);
                  when others =>
                     null;
               end case;
            end if;
            DrawGame(Ship_Info);
            return Ship_Info;
         when others =>
            null;
      end case;
      return Assign_Owner;
   end AssignOwnerKeys;

   function AssignAmmoKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Natural :=
        Positive'Value(Description(Current(OptionsMenu)));
      GunName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            end if;
         when 10 => -- Select new ammo for gun
            Post(OptionsMenu, False);
            if OptionIndex /= 0 then
               UpdateModule
                 (PlayerShip,
                  CurrentMenuIndex,
                  "Current_Value",
                  Positive'Image(OptionIndex));
               AddMessage
                 ("You assigned " &
                  To_String
                    (Items_List(PlayerShip.Cargo(OptionIndex).ProtoIndex)
                       .Name) &
                  " to " &
                  GunName &
                  ".",
                  OrderMessage);
            end if;
            DrawGame(Ship_Info);
            return Ship_Info;
         when others =>
            null;
      end case;
      return Assign_Owner;
   end AssignAmmoKeys;

end Ships.UI.Ship;
