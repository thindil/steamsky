--    Copyright 2016-2018 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Bases.Ship; use Bases.Ship;
with ShipModules; use ShipModules;
with Utils.UI; use Utils.UI;
with Trades; use Trades;

package body Bases.UI.Shipyard is

   TypesMenu: Menu;
   MenuWindow2: Window;
   InstallView: Boolean := True;
   ModulesType: ModuleType := ANY;
   ModulesNames: constant array(Natural range <>) of Unbounded_String :=
     (To_Unbounded_String("Any"),
      To_Unbounded_String("Engines"),
      To_Unbounded_String("Cabins"),
      To_Unbounded_String("Cockpits"),
      To_Unbounded_String("Turrets"),
      To_Unbounded_String("Guns"),
      To_Unbounded_String("Cargo bays"),
      To_Unbounded_String("Hulls"),
      To_Unbounded_String("Armors"),
      To_Unbounded_String("Battering rams"),
      To_Unbounded_String("Alchemy labs"),
      To_Unbounded_String("Furnaces"),
      To_Unbounded_String("Water collectors"),
      To_Unbounded_String("Workshops"),
      To_Unbounded_String("Greenhouses"),
      To_Unbounded_String("Medical rooms"),
      To_Unbounded_String("Harpoon guns"));

   procedure ShowModuleInfo(ClearInfo: Boolean := False) is
      ModuleIndex: constant Positive :=
        Positive'Value(Description(Current(TradeMenu)));
      InfoWindow, BoxWindow, ClearWindow, ActionWindow: Window;
      TextCost, TextTime, AmmoText, MaterialText: Unbounded_String;
      CurrentLine: Line_Position := 3;
      MTime: Positive;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      MAmount, MoneyIndex2, Cost: Natural;
      StartColumn: Column_Position;
      WindowHeight: Line_Position;
      WindowWidth: Column_Position := 1;
   begin
      if ClearInfo then
         ClearWindow := Create((Lines - 1), (Columns / 2), 3, (Columns / 2));
         Refresh_Without_Update(ClearWindow);
         Delete(ClearWindow);
      end if;
      if InstallView then
         TextCost := To_Unbounded_String("Install cost:");
         TextTime := To_Unbounded_String("Installation time:");
         Cost := Modules_List(ModuleIndex).Price;
         CountPrice(Cost, FindMember(Talk));
         MTime := Modules_List(ModuleIndex).InstallTime;
         WindowHeight := 10;
      else
         TextCost := To_Unbounded_String("Remove gain:");
         TextTime := To_Unbounded_String("Removing time:");
         Damage :=
           1.0 -
           DamageFactor
             (Float(PlayerShip.Modules(ModuleIndex).Durability) /
              Float(PlayerShip.Modules(ModuleIndex).MaxDurability));
         Cost :=
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Price -
           Integer
             (Float
                (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .Price) *
              Float(Damage));
         if Cost = 0 then
            Cost := 1;
         end if;
         CountPrice(Cost, FindMember(Talk), False);
         MTime :=
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
             .InstallTime;
         WindowWidth := 8;
      end if;
      Append(TextCost, Positive'Image(Cost));
      Append(TextCost, " ");
      Append(TextCost, MoneyName);
      Append(TextTime, Positive'Image(MTime));
      Append(TextTime, " minutes");
      if WindowWidth < Column_Position(Length(TextCost)) + 2 then
         WindowWidth := Column_Position(Length(TextCost)) + 2;
      end if;
      if WindowWidth < Column_Position(Length(TextTime)) + 2 then
         WindowWidth := Column_Position(Length(TextTime)) + 2;
      end if;
      BoxWindow := Create(Lines - 7, (Columns / 2), 4, (Columns / 2));
      InfoWindow := Create(Lines - 9, (Columns / 2) - 2, 5, (Columns / 2) + 1);
      Add(Win => InfoWindow, Str => To_String(TextCost));
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      Add(Win => InfoWindow, Str => To_String(TextTime));
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
      if InstallView then
         case Modules_List(ModuleIndex).MType is
            when HULL =>
               Add
                 (Win => InfoWindow,
                  Str => "Ship hull can be only replaced.");
               if WindowWidth < 33 then
                  WindowWidth := 33;
               end if;
               Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Modules space:" &
                    Positive'Image(Modules_List(ModuleIndex).MaxValue));
               CurrentLine := 5;
            when ENGINE =>
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Max power:" &
                    Positive'Image(Modules_List(ModuleIndex).MaxValue));
               Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Fuel usage:" &
                    Positive'Image(Modules_List(ModuleIndex).Value));
               CurrentLine := 5;
            when ShipModules.CARGO =>
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Max cargo:" &
                    Positive'Image(Modules_List(ModuleIndex).MaxValue) &
                    " kg");
               CurrentLine := 4;
            when CABIN =>
               Add(Win => InfoWindow, Str => "Quality: ");
               if Modules_List(ModuleIndex).MaxValue < 30 then
                  Add(Win => InfoWindow, Str => "minimal");
               elsif Modules_List(ModuleIndex).MaxValue > 29 and
                 Modules_List(ModuleIndex).MaxValue < 60 then
                  Add(Win => InfoWindow, Str => "basic");
               elsif Modules_List(ModuleIndex).MaxValue > 59 and
                 Modules_List(ModuleIndex).MaxValue < 80 then
                  Add(Win => InfoWindow, Str => "extended");
               else
                  Add(Win => InfoWindow, Str => "luxury");
               end if;
               CurrentLine := 4;
            when GUN | HARPOON_GUN =>
               AmmoText := To_Unbounded_String("Ammunition: ");
               MAmount := 0;
               for Item of Items_List loop
                  if Item.IType =
                    Items_Types(Modules_List(ModuleIndex).Value) then
                     if MAmount > 0 then
                        Append(AmmoText, " or ");
                     end if;
                     Append(AmmoText, Item.Name);
                     MAmount := MAmount + 1;
                  end if;
               end loop;
               if Length(AmmoText) > (Natural((Columns) / 2) - 2) then
                  WindowWidth := (Columns / 2) - 2;
                  WindowHeight :=
                    WindowHeight +
                    (Line_Position(Length(AmmoText)) /
                     (Line_Position(Columns / 2) - 2));
                  CurrentLine :=
                    4 +
                    (Line_Position(Length(AmmoText)) /
                     (Line_Position(Columns / 2) - 2));
               else
                  if WindowWidth < Column_Position(Length(AmmoText)) + 2 then
                     WindowWidth := Column_Position(Length(AmmoText)) + 2;
                  end if;
                  CurrentLine := 4;
               end if;
               Add(Win => InfoWindow, Str => To_String(AmmoText));
            when others =>
               null;
         end case;
         if Modules_List(ModuleIndex).Size > 0 then
            Move_Cursor
              (Win => InfoWindow,
               Line => CurrentLine - 1,
               Column => 0);
            Add
              (Win => InfoWindow,
               Str => "Size:" & Natural'Image(Modules_List(ModuleIndex).Size));
            CurrentLine := CurrentLine + 1;
         end if;
         if Modules_List(ModuleIndex).Weight > 0 then
            Move_Cursor
              (Win => InfoWindow,
               Line => CurrentLine - 1,
               Column => 0);
            Add
              (Win => InfoWindow,
               Str =>
                 "Weight:" &
                 Natural'Image(Modules_List(ModuleIndex).Weight) &
                 " kg");
            CurrentLine := CurrentLine + 1;
         end if;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine - 1, Column => 0);
         MaterialText := To_Unbounded_String("Repair/Upgrade material: ");
         MAmount := 0;
         for Item of Items_List loop
            if Item.IType = Modules_List(ModuleIndex).RepairMaterial then
               if MAmount > 0 then
                  Append(MaterialText, " or ");
               end if;
               Append(MaterialText, Item.Name);
               MAmount := MAmount + 1;
            end if;
         end loop;
         if Length(MaterialText) > (Natural((Columns) / 2) - 2) then
            WindowWidth := (Columns / 2) - 2;
            WindowHeight :=
              WindowHeight +
              (Line_Position(Length(MaterialText)) /
               (Line_Position(Columns / 2) - 2));
            CurrentLine :=
              CurrentLine +
              (Line_Position(Length(MaterialText)) /
               (Line_Position(Columns / 2) - 2));
         else
            if WindowWidth < Column_Position(Length(MaterialText)) + 2 then
               WindowWidth := Column_Position(Length(MaterialText)) + 2;
            end if;
         end if;
         Add(Win => InfoWindow, Str => To_String(MaterialText));
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Repair/Upgrade skill: " &
              To_String
                (Skills_List(Modules_List(ModuleIndex).RepairSkill).Name) &
              "/" &
              To_String
                (Attributes_Names
                   (Skills_List(Modules_List(ModuleIndex).RepairSkill)
                      .Attribute)));
         Get_Cursor_Position
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => StartColumn);
         if WindowWidth < StartColumn + 2 then
            WindowWidth := StartColumn + 2;
         end if;
         if Modules_List(ModuleIndex).Description /= Null_Unbounded_String then
            CurrentLine := CurrentLine + 2;
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add
              (Win => InfoWindow,
               Str => To_String(Modules_List(ModuleIndex).Description));
            Get_Cursor_Position
              (Win => InfoWindow,
               Line => WindowHeight,
               Column => StartColumn);
            if Length(Modules_List(ModuleIndex).Description) >=
              Natural(Columns / 2) - 2 then
               WindowWidth := Columns / 2;
            elsif WindowWidth < StartColumn + 2 then
               WindowWidth := StartColumn + 2;
            end if;
         end if;
      else
         case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
            when ENGINE =>
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Max power:" &
                    Positive'Image(PlayerShip.Modules(ModuleIndex).Data(2)));
               CurrentLine := 4;
            when ShipModules.CARGO =>
               Add
                 (Win => InfoWindow,
                  Str =>
                    "Max cargo:" &
                    Positive'Image(PlayerShip.Modules(ModuleIndex).Data(2)) &
                    " kg");
               CurrentLine := 4;
            when CABIN =>
               Add(Win => InfoWindow, Str => "Quality: ");
               if PlayerShip.Modules(ModuleIndex).Data(2) < 30 then
                  Add(Win => InfoWindow, Str => "minimal");
               elsif PlayerShip.Modules(ModuleIndex).Data(2) > 29 and
                 PlayerShip.Modules(ModuleIndex).Data(2) < 60 then
                  Add(Win => InfoWindow, Str => "basic");
               elsif PlayerShip.Modules(ModuleIndex).Data(2) > 59 and
                 PlayerShip.Modules(ModuleIndex).Data(2) < 80 then
                  Add(Win => InfoWindow, Str => "extended");
               else
                  Add(Win => InfoWindow, Str => "luxury");
               end if;
               CurrentLine := 4;
            when GUN | HARPOON_GUN =>
               AmmoText := To_Unbounded_String("Ammunition: ");
               MAmount := 0;
               for I in Items_List.First_Index .. Items_List.Last_Index loop
                  if Items_List(I).IType =
                    Items_Types
                      (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                         .Value) then
                     if MAmount > 0 then
                        Append(AmmoText, " or ");
                     end if;
                     Append(AmmoText, Items_List(I).Name);
                     MAmount := MAmount + 1;
                  end if;
               end loop;
               if Length(AmmoText) > (Natural((Columns) / 2) - 2) then
                  WindowWidth := (Columns / 2) - 2;
                  WindowHeight :=
                    WindowHeight +
                    (Line_Position(Length(AmmoText)) /
                     (Line_Position(Columns / 2) - 2));
                  CurrentLine :=
                    4 +
                    (Line_Position(Length(AmmoText)) /
                     (Line_Position(Columns / 2) - 2));
               else
                  if WindowWidth < Column_Position(Length(AmmoText)) + 2 then
                     WindowWidth := Column_Position(Length(AmmoText)) + 2;
                  end if;
                  CurrentLine := 4;
               end if;
               Add(Win => InfoWindow, Str => To_String(AmmoText));
            when others =>
               null;
         end case;
         if Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Size >
           0 then
            Move_Cursor
              (Win => InfoWindow,
               Line => CurrentLine - 1,
               Column => 0);
            Add
              (Win => InfoWindow,
               Str =>
                 "Size:" &
                 Natural'Image
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Size));
            CurrentLine := CurrentLine + 1;
         end if;
         if PlayerShip.Modules(ModuleIndex).Weight > 0 then
            Move_Cursor
              (Win => InfoWindow,
               Line => CurrentLine - 1,
               Column => 0);
            Add
              (Win => InfoWindow,
               Str =>
                 "Weight:" &
                 Natural'Image(PlayerShip.Modules(ModuleIndex).Weight) &
                 " kg");
            CurrentLine := CurrentLine + 1;
         end if;
         if Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
             .Description /=
           Null_Unbounded_String then
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
            Add
              (Win => InfoWindow,
               Str =>
                 To_String
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Description));
            Get_Cursor_Position
              (Win => InfoWindow,
               Line => WindowHeight,
               Column => StartColumn);
            if Length
                (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .Description) >=
              Natural(Columns / 2) - 2 then
               WindowWidth := Columns / 2;
            elsif WindowWidth < StartColumn + 2 then
               WindowWidth := StartColumn + 2;
            end if;
         end if;
      end if;
      if WindowHeight > (Lines - 10) then
         WindowHeight := Lines - 10;
      end if;
      Resize(BoxWindow, WindowHeight + 3, WindowWidth);
      WindowFrame(BoxWindow, 2, "Module info");
      Resize(InfoWindow, WindowHeight + 1, WindowWidth - 2);
      ActionWindow :=
        Create(5, (Columns / 2), WindowHeight + 7, (Columns / 2));
      if InstallView then
         Add(Win => ActionWindow, Str => "Press Enter to install module.");
      else
         Add(Win => ActionWindow, Str => "Press Enter to remove module.");
      end if;
      Change_Attributes
        (Win => ActionWindow,
         Line => 0,
         Column => 6,
         Count => 5,
         Color => 1,
         Attr => BoldCharacters);
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      Move_Cursor(Win => ActionWindow, Line => 1, Column => 0);
      if MoneyIndex2 > 0 then
         Add
           (Win => ActionWindow,
            Str =>
              "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
              " " &
              To_String(MoneyName) &
              ".");
      elsif InstallView then
         Add
           (Win => ActionWindow,
            Str =>
              "You don't have any " &
              To_String(MoneyName) &
              " to install anything.");
      end if;
      Move_Cursor(Win => ActionWindow, Line => 2, Column => 0);
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = HULL then
            Add
              (Win => ActionWindow,
               Str =>
                 "You have used" &
                 Natural'Image(Module.Data(1)) &
                 " modules space from max" &
                 Natural'Image(Module.Data(2)) &
                 " allowed.");
            exit;
         end if;
      end loop;
      Get_Cursor_Position
        (Win => ActionWindow,
         Line => CurrentLine,
         Column => StartColumn);
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => ActionWindow, Line => CurrentLine, Column => 0);
      Add(Win => ActionWindow, Str => "Press Escape to back to sky map.");
      Change_Attributes
        (Win => ActionWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 6,
         Attr => BoldCharacters,
         Color => 1);
      Refresh_Without_Update;
      Refresh_Without_Update(BoxWindow);
      Delete(BoxWindow);
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(ActionWindow);
      Delete(ActionWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowModuleInfo;

   procedure ShowShipyard is
      Modules_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      MenuOptions: Menu_Option_Set;
      procedure AddMenuItems(MType: ModuleType) is
      begin
         for I in Modules_List.Iterate loop
            if Modules_List(I).Price > 0 and Modules_List(I).MType = MType then
               Modules_Items.all(MenuIndex) :=
                 New_Item
                   (To_String(Modules_List(I).Name),
                    Positive'Image(BaseModules_Container.To_Index(I)));
               MenuIndex := MenuIndex + 1;
            end if;
         end loop;
      end AddMenuItems;
   begin
      Move_Cursor(Line => 2, Column => 2);
      if InstallView then
         Add(Str => "[Install] [F2 Remove]");
         Change_Attributes
           (Line => 2,
            Column => 13,
            Count => 2,
            Color => 1,
            Attr => BoldCharacters);
         Move_Cursor(Line => 2, Column => 24);
         Add
           (Str =>
              "[F3 Show modules: " &
              To_Lower(To_String(ModulesNames(ModuleType'Pos(ModulesType)))) &
              "]");
         Change_Attributes
           (Line => 2,
            Column => 25,
            Count => 2,
            Color => 1,
            Attr => BoldCharacters);
         Modules_Items :=
           new Item_Array
           (Modules_List.First_Index .. (Modules_List.Last_Index + 1));
         if ModulesType = ANY then
            for I in ModuleType'Range loop
               AddMenuItems(I);
            end loop;
         else
            AddMenuItems(ModulesType);
         end if;
      else
         Add(Str => "[F2 Install] [Remove]");
         Change_Attributes
           (Line => 2,
            Column => 3,
            Count => 2,
            Color => 1,
            Attr => BoldCharacters);
         Modules_Items :=
           new Item_Array
           (PlayerShip.Modules.First_Index ..
                (PlayerShip.Modules.Last_Index + 1));
         for I in PlayerShip.Modules.Iterate loop
            if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType /=
              HULL then
               Modules_Items.all(MenuIndex) :=
                 New_Item
                   (To_String(PlayerShip.Modules(I).Name),
                    Positive'Image(Modules_Container.To_Index(I)));
               MenuIndex := MenuIndex + 1;
            end if;
         end loop;
      end if;
      for I in MenuIndex .. Modules_Items'Last loop
         Modules_Items.all(I) := Null_Item;
      end loop;
      TradeMenu := New_Menu(Modules_Items);
      MenuOptions := Get_Options(TradeMenu);
      MenuOptions.Show_Descriptions := False;
      Set_Options(TradeMenu, MenuOptions);
      Set_Format(TradeMenu, Lines - 4, 1);
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if CurrentMenuIndex >= Modules_Items'Last then
         CurrentMenuIndex := 1;
      end if;
      if Modules_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Modules_Items.all(CurrentMenuIndex));
      ShowModuleInfo;
   end ShowShipyard;

   procedure ShowTypesMenu is
      Types_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (ModulesNames'Last + 3));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      for I in ModulesNames'Range loop
         Types_Items.all(I + 1) := New_Item(To_String(ModulesNames(I)));
      end loop;
      Types_Items.all(Types_Items'Last - 1) := New_Item("Close");
      Types_Items.all(Types_Items'Last) := Null_Item;
      TypesMenu := New_Menu(Types_Items);
      Scale(TypesMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow2);
      WindowFrame(MenuWindow2, 5, "Modules type");
      Set_Window(TypesMenu, MenuWindow2);
      Set_Sub_Window
        (TypesMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(TypesMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow2);
      Update_Screen;
   end ShowTypesMenu;

   function ShipyardKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when 27 => -- Back to sky map
            CurrentMenuIndex := 1;
            InstallView := True;
            ModulesType := ANY;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous repair option
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next repair option
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when Key_F2 => -- Switch modules to install/remove
            if not InstallView then
               InstallView := True;
            else
               InstallView := False;
            end if;
            CurrentMenuIndex := 1;
            DrawGame(Shipyard_View);
            return Shipyard_View;
         when Key_F3 => -- Show select modules type menu
            if InstallView then
               ShowTypesMenu;
               return ShipyardTypesMenu;
            end if;
         when 10 => -- Install/remove module
            Bases.Ship.UpgradeShip
              (InstallView,
               Positive'Value(Description(Current(TradeMenu))));
            DrawGame(Shipyard_View);
            Result := Request_Denied;
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowModuleInfo(True);
      end if;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      return Shipyard_View;
   exception
      when Trade_No_Money =>
         ShowDialog
           ("You don't have " & To_String(MoneyName) & " to pay for modules.");
         DrawGame(Shipyard_View);
         return Shipyard_View;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " &
            To_String(MoneyName) &
            " to pay for " &
            Exception_Message(An_Exception) &
            ".");
         DrawGame(Shipyard_View);
         return Shipyard_View;
      when An_Exception : BasesShip_Unique_Module =>
         ShowDialog
           ("You can't install another " &
            Exception_Message(An_Exception) &
            " because you have installed one module that type. Remove old first.");
         DrawGame(Shipyard_View);
         return Shipyard_View;
      when An_Exception : BasesShip_Installation_Error |
        BasesShip_Removing_Error =>
         ShowDialog(Exception_Message(An_Exception));
         DrawGame(Shipyard_View);
         return Shipyard_View;
      when Trade_No_Free_Cargo =>
         ShowDialog
           ("You don't have enough free space for " &
            To_String(MoneyName) &
            " in ship cargo.");
         DrawGame(Shipyard_View);
         return Shipyard_View;
      when Trade_No_Money_In_Base =>
         ShowDialog
           ("Base don't have enough " &
            To_String(MoneyName) &
            " for buy this module.");
         DrawGame(Shipyard_View);
         return Shipyard_View;
   end ShipyardKeys;

   function ShipyardTypesKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous type option
            Result := Driver(TypesMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TypesMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next type option
            Result := Driver(TypesMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TypesMenu, M_First_Item);
            end if;
         when 10 => -- Set modules type to show
            if Name(Current(TypesMenu)) /= "Close" then
               CurrentMenuIndex := 1;
               ModulesType :=
                 ModuleType'Val(Get_Index(Current(TypesMenu)) - 1);
            end if;
            DrawGame(Shipyard_View);
            return Shipyard_View;
         when 27 => -- Esc select close option, used second time, close menu
            if Name(Current(TypesMenu)) = "Close" then
               DrawGame(Shipyard_View);
               return Shipyard_View;
            else
               Result := Driver(TypesMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(TypesMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TypesMenu, M_Clear_Pattern);
               Result := Driver(TypesMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return ShipyardTypesMenu;
   end ShipyardTypesKeys;

end Bases.UI.Shipyard;
