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

with Ada.Exceptions; use Ada.Exceptions;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Messages; use Messages;
with Bases; use Bases;
with Maps; use Maps;
with ShipModules; use ShipModules;
with Help.UI; use Help.UI;
with Ships.Crew; use Ships.Crew;
with Header; use Header;
with Utils.UI; use Utils.UI;

package body Crew.UI is

   CrewMenu, OrdersMenu, PrioritiesMenu: Menu;
   MenuWindow, MenuWindow2, SkillsPad: Window;
   MemberIndex, PriorityIndex: Positive := 1;
   NeedClean, NeedRepairs: Boolean := False;
   StartIndex, EndIndex: Integer := 0;

   procedure ShowMemberInfo is
      InfoWindow, ClearWindow, ActionsWindow: Window;
      Member: constant Member_Data := PlayerShip.Crew(MemberIndex);
      CurrentLine: Line_Position := 2;
      Health,
      Tired,
      Hungry,
      Thirsty,
      OrderName: Unbounded_String :=
        Null_Unbounded_String;
      WindowHeight: Line_Position := 3;
      TextColor: array(1 .. 4) of Color_Pair;
      SkillsLine, SkillsStartsLine: Line_Position := 0;
      WindowWidth: Column_Position := 18;
      CurrentColumn: Column_Position;
      AttributesHeaderLine, SkillsHeaderLine, OrderHeaderLine: Line_Position;
      procedure SubHeader(Caption: String; HeaderLine: Line_Position) is
      begin
         Set_Color(InfoWindow, 2);
         Move_Cursor(Win => InfoWindow, Line => HeaderLine, Column => 0);
         Add(Win => InfoWindow, Ch => ACS_Map(ACS_Left_Tee));
         Horizontal_Line
           (Win => InfoWindow,
            Line_Size => Integer(WindowWidth - 2));
         Move_Cursor(Win => InfoWindow, Line => HeaderLine, Column => 2);
         Add(Win => InfoWindow, Str => "[");
         Set_Color(InfoWindow, Color_Pair'First);
         Add(Win => InfoWindow, Str => Caption);
         Set_Color(InfoWindow, 2);
         Add(Win => InfoWindow, Str => "]");
         Move_Cursor
           (Win => InfoWindow,
            Line => HeaderLine,
            Column => WindowWidth - 1);
         Add(Win => InfoWindow, Ch => ACS_Map(ACS_Right_Tee));
         Set_Color(InfoWindow, Color_Pair'First);
      end SubHeader;
   begin
      ClearWindow := Create((Lines - 3), (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      if Member.Health < 100 and Member.Health > 80 then
         Health := To_Unbounded_String("Slightly wounded");
         TextColor(1) := 2;
      elsif Member.Health < 81 and Member.Health > 50 then
         Health := To_Unbounded_String("Wounded");
         TextColor(1) := 1;
      elsif Member.Health < 51 then
         Health := To_Unbounded_String("Heavily Wounded");
         TextColor(1) := 3;
      end if;
      if Health /= Null_Unbounded_String then
         WindowHeight := WindowHeight + 1;
      end if;
      if Member.Tired > 20 and Member.Tired < 41 then
         Tired := To_Unbounded_String("Bit tired");
         TextColor(2) := 2;
      elsif Member.Tired > 40 and Member.Tired < 81 then
         Tired := To_Unbounded_String("Tired");
         TextColor(2) := 1;
      elsif Member.Tired > 80 and Member.Tired < 100 then
         Tired := To_Unbounded_String("Very tired");
         TextColor(2) := 3;
      elsif Member.Tired = 100 then
         Tired := To_Unbounded_String("Unconscious");
         TextColor(2) := 4;
      end if;
      if Tired /= Null_Unbounded_String then
         WindowHeight := WindowHeight + 1;
      end if;
      if Member.Thirst > 20 and Member.Thirst < 41 then
         Thirsty := To_Unbounded_String("Bit thirsty");
         TextColor(3) := 2;
      elsif Member.Thirst > 40 and Member.Thirst < 81 then
         Thirsty := To_Unbounded_String("Thirsty");
         TextColor(3) := 1;
      elsif Member.Thirst > 80 and Member.Thirst < 100 then
         Thirsty := To_Unbounded_String("Very thirsty");
         TextColor(3) := 3;
      elsif Member.Thirst = 100 then
         Thirsty := To_Unbounded_String("Dehydrated");
         TextColor(3) := 4;
      end if;
      if Thirsty /= Null_Unbounded_String then
         WindowHeight := WindowHeight + 1;
      end if;
      if Member.Hunger > 20 and Member.Hunger < 41 then
         Hungry := To_Unbounded_String("Bit hungry");
         TextColor(4) := 2;
      elsif Member.Hunger > 40 and Member.Hunger < 81 then
         Hungry := To_Unbounded_String("Hungry");
         TextColor(4) := 1;
      elsif Member.Hunger > 80 and Member.Hunger < 100 then
         Hungry := To_Unbounded_String("Very hungry");
         TextColor(4) := 3;
      elsif Member.Hunger = 100 then
         Hungry := To_Unbounded_String("Starving");
         TextColor(4) := 4;
      end if;
      if Hungry /= Null_Unbounded_String then
         WindowHeight := WindowHeight + 1;
      end if;
      if Member.Skills.Length = 0 then
         WindowHeight := WindowHeight + 1;
      else
         WindowHeight :=
           WindowHeight +
           Line_Position(Member.Skills.Length) +
           4 +
           Line_Position(Member.Attributes.Length);
         if WindowHeight > (Lines - 5) then
            WindowHeight := Lines - 5;
         end if;
      end if;
      InfoWindow := Create(WindowHeight, (Columns / 2), 3, (Columns / 2));
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 2);
      if Member.Gender = 'M' then
         Add(Win => InfoWindow, Str => "Gender: Male");
      else
         Add(Win => InfoWindow, Str => "Gender: Female");
      end if;
      if Member.Skills.Length = 0 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => "Passenger");
         CurrentLine := CurrentLine + 1;
      end if;
      if Health /= Null_Unbounded_String then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => To_String(Health));
         Change_Attributes
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => 2,
            Count => Length(Health),
            Color => TextColor(1));
         CurrentLine := CurrentLine + 1;
      end if;
      if Tired /= Null_Unbounded_String then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => To_String(Tired));
         Change_Attributes
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => 2,
            Count => Length(Tired),
            Color => TextColor(2));
         CurrentLine := CurrentLine + 1;
      end if;
      if Thirsty /= Null_Unbounded_String then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => To_String(Thirsty));
         Change_Attributes
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => 2,
            Count => Length(Thirsty),
            Color => TextColor(3));
         CurrentLine := CurrentLine + 1;
      end if;
      if Hungry /= Null_Unbounded_String then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => To_String(Hungry));
         Change_Attributes
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => 2,
            Count => Length(Hungry),
            Color => TextColor(4));
         CurrentLine := CurrentLine + 1;
      end if;
      if Member.Skills.Length > 0 then
         AttributesHeaderLine := CurrentLine;
         CurrentLine := CurrentLine + 1;
         for I in Member.Attributes.Iterate loop
            Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
            Add
              (Win => InfoWindow,
               Str =>
                 To_String
                   (Attributes_Names(Attributes_Container.To_Index(I))) &
                 ": " &
                 GetAttributeLevelName(Member.Attributes(I)(1)));
            Get_Cursor_Position
              (Win => InfoWindow,
               Line => CurrentLine,
               Column => CurrentColumn);
            if WindowWidth < (CurrentColumn + 2) then
               WindowWidth := CurrentColumn + 2;
            end if;
            CurrentLine := CurrentLine + 1;
         end loop;
         SkillsHeaderLine := CurrentLine;
         CurrentLine := CurrentLine + 1;
         SkillsPad :=
           New_Pad(Line_Position(Member.Skills.Length), (Columns / 2) - 2);
         SkillsStartsLine := CurrentLine + 3;
         EndIndex :=
           Integer(Member.Skills.Length) -
           Integer(WindowHeight - SkillsStartsLine);
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
         for Skill of Member.Skills loop
            Move_Cursor(Win => SkillsPad, Line => SkillsLine, Column => 0);
            Add
              (Win => SkillsPad,
               Str =>
                 To_String(Skills_Names(Skill(1))) &
                 ": " &
                 GetSkillLevelName(Skill(2)));
            Get_Cursor_Position
              (Win => SkillsPad,
               Line => SkillsLine,
               Column => CurrentColumn);
            SkillsLine := SkillsLine + 1;
            if WindowWidth < (CurrentColumn + 4) then
               WindowWidth := CurrentColumn + 4;
            end if;
         end loop;
         case Member.Order is
            when Pilot =>
               OrderName := To_Unbounded_String("Piloting");
            when Engineer =>
               OrderName := To_Unbounded_String("Engineering");
            when Gunner =>
               OrderName := To_Unbounded_String("Gunner");
            when Rest =>
               OrderName := To_Unbounded_String("On break");
            when Repair =>
               OrderName := To_Unbounded_String("Repair ship");
            when Craft =>
               OrderName := To_Unbounded_String("Manufacturing");
            when Upgrading =>
               OrderName := To_Unbounded_String("Upgrading module");
            when Talk =>
               OrderName := To_Unbounded_String("Talking in bases");
            when Heal =>
               OrderName := To_Unbounded_String("Healing wounded");
            when Clean =>
               OrderName := To_Unbounded_String("Cleans ship");
         end case;
         CurrentLine := WindowHeight - 3;
         OrderHeaderLine := CurrentLine;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add(Win => InfoWindow, Ch => ACS_Map(ACS_Left_Tee));
         Horizontal_Line
           (Win => InfoWindow,
            Line_Size => Natural(Columns / 2) - 2);
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add(Win => InfoWindow, Str => "[Order]");
         Move_Cursor
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => (Columns / 2) - 1);
         Add(Win => InfoWindow, Ch => ACS_Map(ACS_Right_Tee));
         Move_Cursor(Win => InfoWindow, Line => CurrentLine + 1, Column => 2);
         Add(Win => InfoWindow, Str => To_String(OrderName));
      end if;
      Resize(InfoWindow, WindowHeight, WindowWidth);
      WindowFrame(InfoWindow, 2, "Member info");
      if Member.Skills.Length > 0 then
         SubHeader("Attributes", AttributesHeaderLine);
         SubHeader("Skills", SkillsHeaderLine);
         SubHeader("Order", OrderHeaderLine);
         Resize
           (SkillsPad,
            Line_Position(Member.Skills.Length),
            WindowWidth - 4);
      end if;
      CurrentLine := WindowHeight + 3;
      if CurrentLine >= Lines - 4 then
         CurrentLine := Lines - 5;
      end if;
      ActionsWindow := Create(5, (Columns / 2), CurrentLine, (Columns / 2));
      if Member.Tired < 100 and
        Member.Hunger < 100 and
        Member.Thirst < 100 then
         Add
           (Win => ActionsWindow,
            Str => "Press Enter to give orders to crew member");
         Get_Cursor_Position
           (Win => ActionsWindow,
            Line => CurrentLine,
            Column => CurrentColumn);
         Change_Attributes
           (Win => ActionsWindow,
            Line => 0,
            Column => 6,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
      end if;
      for Module of PlayerShip.Modules loop
         if Module.Durability > 0 and
           Modules_List(Module.ProtoIndex).MType = CABIN and
           Module.Data(1) < Module.Data(2) and
           not NeedClean then
            NeedClean := True;
         end if;
         if not NeedRepairs and Module.Durability < Module.MaxDurability then
            if FindCargo
                (ItemType => Modules_List(Module.ProtoIndex).RepairMaterial) >
              0 then
               NeedRepairs := True;
            end if;
         end if;
         if NeedRepairs and NeedClean then
            exit;
         end if;
      end loop;
      if NeedClean or NeedRepairs then
         CurrentLine := CurrentLine + 1;
         Move_Cursor(Win => ActionsWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => ActionsWindow,
            Str => "Press Space to give orders to all crew");
         Change_Attributes
           (Win => ActionsWindow,
            Line => CurrentLine,
            Column => 6,
            Count => 5,
            Color => 1,
            Attr => BoldCharacters);
      end if;
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => ActionsWindow, Line => CurrentLine, Column => 0);
      Add(Win => ActionsWindow, Str => "Press Escape to back to sky map");
      Change_Attributes
        (Win => ActionsWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 6,
         Attr => BoldCharacters,
         Color => 1);
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => ActionsWindow, Line => CurrentLine, Column => 0);
      Add(Win => ActionsWindow, Str => "Press F1 for help");
      Change_Attributes
        (Win => ActionsWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 2,
         Attr => BoldCharacters,
         Color => 1);
      Refresh_Without_Update;
      Refresh_Without_Update(InfoWindow);
      Refresh_Without_Update(ActionsWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
      Delete(InfoWindow);
      Delete(ActionsWindow);
      if Member.Skills.Length > 0 then
         Refresh
           (SkillsPad,
            Line_Position(StartIndex),
            0,
            SkillsStartsLine,
            (Columns / 2) + 2,
            WindowHeight - 1,
            Columns - 2);
      end if;
   end ShowMemberInfo;

   procedure ShowCrewInfo is
      Crew_Items: constant Item_Array_Access :=
        new Item_Array(1 .. (PlayerShip.Crew.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      Move_Cursor(Line => 3, Column => 2);
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         Crew_Items.all(I) := New_Item(To_String(PlayerShip.Crew(I).Name));
      end loop;
      Crew_Items.all(Crew_Items'Last) := Null_Item;
      CrewMenu := New_Menu(Crew_Items);
      Set_Format(CrewMenu, Lines - 10, 1);
      Scale(CrewMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(CrewMenu, MenuWindow);
      Set_Sub_Window
        (CrewMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(CrewMenu);
      if Crew_Items.all(MemberIndex) = Null_Item then
         MemberIndex := 1;
      end if;
      Set_Current(CrewMenu, Crew_Items.all(MemberIndex));
      ShowMemberInfo;
   end ShowCrewInfo;

   procedure ShowOrdersMenu is
      Orders_Items: Item_Array_Access;
      OrdersAmount: Positive := 2;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
      NeedHealer, HealOrder: Boolean := False;
   begin
      if
        (PlayerShip.Crew(MemberIndex).Tired = 100 or
         PlayerShip.Crew(MemberIndex).Hunger = 100 or
         PlayerShip.Crew(MemberIndex).Thirst = 100) and
        PlayerShip.Crew(MemberIndex).Order /= Rest then
         Orders_Items := new Item_Array(1 .. 3);
         Orders_Items.all(1) := New_Item("Go on break", "0");
         MenuIndex := 2;
      elsif PlayerShip.Crew(MemberIndex).Skills.Length = 0 then
         if PlayerShip.Speed = DOCKED then
            if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                .Owner /=
              Abandoned then
               Orders_Items := new Item_Array(1 .. 3);
               Orders_Items.all(1) := New_Item("Dismiss", "0");
               MenuIndex := 2;
            end if;
         else
            Orders_Items := new Item_Array(1 .. 2);
         end if;
      else
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 and
              Crew_Container.To_Index(I) /= MemberIndex then
               NeedHealer := True;
               exit;
            end if;
         end loop;
         for Module of PlayerShip.Modules loop
            if Module.Durability > 0 then
               case Modules_List(Module.ProtoIndex).MType is
                  when GUN =>
                     if Module.Owner /= MemberIndex then
                        OrdersAmount := OrdersAmount + 1;
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if Module.Owner /= MemberIndex and
                       Module.Data(1) /= 0 then
                        OrdersAmount := OrdersAmount + 1;
                     end if;
                  when MEDICAL_ROOM =>
                     if NeedHealer then
                        if FindCargo(ItemType => HealingTools) > 0 and
                          PlayerShip.Crew(MemberIndex).Order /= Heal and
                          PlayerShip.Crew(MemberIndex).Health = 100 then
                           HealOrder := True;
                           OrdersAmount := OrdersAmount + 1;
                        end if;
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end loop;
         if NeedRepairs and PlayerShip.Crew(MemberIndex).Order /= Repair then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Rest then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if PlayerShip.UpgradeModule > 0 and
           PlayerShip.Crew(MemberIndex).Order /= Upgrading then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if PlayerShip.Speed = DOCKED and MemberIndex > 1 then
            if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                .Owner /=
              Abandoned then
               OrdersAmount := OrdersAmount + 1;
            end if;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Pilot then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Engineer then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Talk then
            OrdersAmount := OrdersAmount + 1;
         end if;
         if NeedClean and PlayerShip.Crew(MemberIndex).Order /= Clean then
            OrdersAmount := OrdersAmount + 1;
         end if;
         Orders_Items := new Item_Array(1 .. (OrdersAmount + 1));
         if PlayerShip.Crew(MemberIndex).Order /= Pilot then
            Orders_Items.all(MenuIndex) := New_Item("Piloting", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Engineer then
            Orders_Items.all(MenuIndex) := New_Item("Engineering", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         for I in PlayerShip.Modules.Iterate loop
            if PlayerShip.Modules(I).Durability > 0 then
               case Modules_List(PlayerShip.Modules(I).ProtoIndex).MType is
                  when GUN =>
                     if PlayerShip.Modules(I).Owner /= MemberIndex then
                        Orders_Items.all(MenuIndex) :=
                          New_Item
                            ("Operate " &
                             To_String(PlayerShip.Modules(I).Name),
                             Positive'Image(Modules_Container.To_Index(I)));
                        MenuIndex := MenuIndex + 1;
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if PlayerShip.Modules(I).Owner /= MemberIndex and
                       PlayerShip.Modules(I).Data(1) /= 0 then
                        Orders_Items.all(MenuIndex) :=
                          New_Item
                            ("Work in " &
                             To_String(PlayerShip.Modules(I).Name),
                             Positive'Image(Modules_Container.To_Index(I)));
                        MenuIndex := MenuIndex + 1;
                     end if;
                  when MEDICAL_ROOM =>
                     if HealOrder then
                        Orders_Items.all(MenuIndex) :=
                          New_Item
                            ("Heal wounded in " &
                             To_String(PlayerShip.Modules(I).Name),
                             Positive'Image(Modules_Container.To_Index(I)));
                        MenuIndex := MenuIndex + 1;
                     end if;
                  when CABIN =>
                     if NeedClean and
                       PlayerShip.Crew(MemberIndex).Order /= Clean then
                        Orders_Items.all(MenuIndex) :=
                          New_Item("Clean ship", "0");
                        MenuIndex := MenuIndex + 1;
                        NeedClean := False;
                     end if;
                  when others =>
                     null;
               end case;
            end if;
         end loop;
         if NeedRepairs and PlayerShip.Crew(MemberIndex).Order /= Repair then
            Orders_Items.all(MenuIndex) := New_Item("Repair ship", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         if PlayerShip.UpgradeModule > 0 and
           PlayerShip.Crew(MemberIndex).Order /= Upgrading then
            Orders_Items.all(MenuIndex) := New_Item("Upgrade module", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Talk then
            Orders_Items.all(MenuIndex) := New_Item("Talking in bases", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         if PlayerShip.Crew(MemberIndex).Order /= Rest then
            Orders_Items.all(MenuIndex) := New_Item("Go on break", "0");
            MenuIndex := MenuIndex + 1;
         end if;
         if PlayerShip.Speed = DOCKED and MemberIndex > 1 then
            if SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
                .Owner /=
              Abandoned then
               Orders_Items.all(MenuIndex) := New_Item("Dismiss", "0");
               MenuIndex := MenuIndex + 1;
            end if;
         end if;
      end if;
      if PlayerShip.Crew(MemberIndex).Skills.Length > 0 then
         Orders_Items.all(MenuIndex) := New_Item("Set orders priorities", "0");
         MenuIndex := MenuIndex + 1;
      end if;
      Orders_Items.all(MenuIndex) := New_Item("Close", "0");
      Orders_Items.all(Orders_Items'Last) := Null_Item;
      OrdersMenu := New_Menu(Orders_Items);
      Set_Options(OrdersMenu, (Show_Descriptions => False, others => True));
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Give order");
      Set_Window(OrdersMenu, MenuWindow2);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh(MenuWindow2);
   end ShowOrdersMenu;

   procedure DismissMember is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if PlayerShip.Speed /= DOCKED then
         ShowDialog
           ("You can't dismiss crew members if you are not docked to base.");
         return;
      end if;
      if MemberIndex = 1 then
         ShowDialog("You can't dismiss self.");
         return;
      end if;
      AddMessage
        ("You dismissed " & To_String(PlayerShip.Crew(MemberIndex).Name) & ".",
         OrderMessage);
      DeleteMember(MemberIndex, PlayerShip);
      SkyBases(BaseIndex).Population := SkyBases(BaseIndex).Population + 1;
      MemberIndex := 1;
      DrawGame(Crew_Info);
   end DismissMember;

   procedure ShowOrdersForAll is
      Orders_Items: constant Item_Array_Access := new Item_Array(1 .. 4);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
   begin
      if NeedClean then
         Orders_Items.all(MenuIndex) := New_Item("Clean ship everyone");
         MenuIndex := MenuIndex + 1;
      end if;
      if NeedRepairs then
         Orders_Items.all(MenuIndex) := New_Item("Repair ship everyone");
         MenuIndex := MenuIndex + 1;
      end if;
      Orders_Items.all(MenuIndex) := New_Item("Close");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Orders_Items'Last loop
         Orders_Items.all(I) := Null_Item;
      end loop;
      OrdersMenu := New_Menu(Orders_Items);
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuLength := 25;
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Give order whole crew");
      Set_Window(OrdersMenu, MenuWindow2);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh(MenuWindow2);
   end ShowOrdersForAll;

   procedure ShowPrioritiesMenu is
      Orders_Items: constant Item_Array_Access :=
        new Item_Array(Orders_Array'First .. (Orders_Array'Last + 2));
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"),
         To_Unbounded_String("Engineering"),
         To_Unbounded_String("Operating guns"),
         To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading ship"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleaning ship"));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      OrderPriority: Unbounded_String;
   begin
      for I in OrdersNames'Range loop
         case PlayerShip.Crew(MemberIndex).Orders(I) is
            when 0 =>
               OrderPriority := To_Unbounded_String("None");
            when 1 =>
               OrderPriority := To_Unbounded_String("Normal");
            when 2 =>
               OrderPriority := To_Unbounded_String("Highest");
            when others =>
               null;
         end case;
         Orders_Items.all(I) :=
           New_Item(To_String(OrdersNames(I)), To_String(OrderPriority));
      end loop;
      Orders_Items.all(Orders_Items'Last - 1) := New_Item("Close");
      Orders_Items.all(Orders_Items'Last) := Null_Item;
      PrioritiesMenu := New_Menu(Orders_Items);
      Scale(PrioritiesMenu, MenuHeight, MenuLength);
      MenuLength := 25;
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Set orders priorities");
      Set_Window(PrioritiesMenu, MenuWindow2);
      Set_Sub_Window
        (PrioritiesMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(PrioritiesMenu);
      Set_Current(PrioritiesMenu, Orders_Items.all(PriorityIndex));
      Refresh(MenuWindow2);
   end ShowPrioritiesMenu;

   function CrewInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Driver_Result;
      RefreshSkills: Boolean := False;
   begin
      case Key is
         when 27 => -- Back to sky map or combat screen
            MemberIndex := 1;
            NeedRepairs := False;
            NeedClean := False;
            DrawGame(OldState);
            return OldState;
         when 56 | KEY_UP => -- Select previous crew member
            Result := Driver(CrewMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(CrewMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next crew member
            Result := Driver(CrewMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(CrewMenu, M_First_Item);
            end if;
         when 51 | KEY_NPAGE => -- Scroll skills one line down
            StartIndex := StartIndex + 1;
            RefreshSkills := True;
         when 57 | KEY_PPAGE => -- Scroll skills one line up
            StartIndex := StartIndex - 1;
            RefreshSkills := True;
         when 10 => -- Give orders to selected crew member
            ShowOrdersMenu;
            return Giving_Orders;
         when 32 => -- Give orders to all crew
            if NeedRepairs or NeedClean then
               ShowOrdersForAll;
               return Orders_For_All;
            end if;
         when Key_F1 => -- Show help
            Erase;
            ShowGameHeader(Help_Topic);
            ShowHelp(Crew_Info, 7);
            return Help_Topic;
         when others =>
            Result := Driver(CrewMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(CrewMenu, M_Clear_Pattern);
               Result := Driver(CrewMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         MemberIndex := Get_Index(Current(CrewMenu));
         ShowMemberInfo;
      end if;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      if RefreshSkills then
         ShowMemberInfo;
      end if;
      return Crew_Info;
   end CrewInfoKeys;

   function CrewOrdersKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      ModuleIndex: constant Natural :=
        Natural'Value(Description(Current(OrdersMenu)));
      OrderName: constant String := Name(Current(OrdersMenu));
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous order
            Result := Driver(OrdersMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OrdersMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next order
            Result := Driver(OrdersMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OrdersMenu, M_First_Item);
            end if;
         when 10 => -- Select order
            if OrderName = "Piloting" then
               GiveOrders(MemberIndex, Pilot);
            elsif OrderName = "Engineering" then
               GiveOrders(MemberIndex, Engineer);
            elsif OrderName = "Go on break" then
               GiveOrders(MemberIndex, Rest);
            elsif OrderName = "Repair ship" then
               GiveOrders(MemberIndex, Repair);
            elsif OrderName = "Upgrade module" then
               GiveOrders(MemberIndex, Upgrading);
            elsif OrderName = "Talking in bases" then
               GiveOrders(MemberIndex, Talk);
            elsif OrderName = "Heal wounded crew members" then
               GiveOrders(MemberIndex, Heal, ModuleIndex);
            elsif OrderName = "Clean ship" then
               GiveOrders(MemberIndex, Clean);
            elsif OrderName = "Dismiss" then
               DrawGame(Dismiss_Confirm);
               return Dismiss_Confirm;
            elsif OrderName = "Set orders priorities" then
               DrawGame(Crew_Info);
               ShowPrioritiesMenu;
               return Orders_Priorities;
            elsif OrderName /= "Close" then
               if Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .MType =
                 GUN then
                  GiveOrders(MemberIndex, Gunner, ModuleIndex);
               else
                  GiveOrders(MemberIndex, Craft, ModuleIndex);
               end if;
            end if;
            DrawGame(Crew_Info);
            return Crew_Info;
         when 27 => -- Esc select close option, used second time, close menu
            if OrderName = "Close" then
               DrawGame(Crew_Info);
               return Crew_Info;
            else
               Result := Driver(OrdersMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(OrdersMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OrdersMenu, M_Clear_Pattern);
               Result := Driver(OrdersMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Giving_Orders;
   exception
      when An_Exception : Crew_Order_Error =>
         ShowDialog(Exception_Message(An_Exception));
         DrawGame(Crew_Info);
         return Crew_Info;
   end CrewOrdersKeys;

   function CrewOrdersAllKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      OrderName: constant String := Name(Current(OrdersMenu));
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous order
            Result := Driver(OrdersMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OrdersMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next order
            Result := Driver(OrdersMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OrdersMenu, M_First_Item);
            end if;
         when 10 => -- Select order
            if OrderName = "Repair ship everyone" then
               for I in
                 PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
                  if PlayerShip.Crew(I).Skills.Length > 0 then
                     GiveOrders(I, Repair);
                  end if;
               end loop;
            elsif OrderName = "Clean ship everyone" then
               for I in
                 PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
                  if PlayerShip.Crew(I).Skills.Length > 0 then
                     GiveOrders(I, Clean);
                  end if;
               end loop;
            end if;
            DrawGame(Crew_Info);
            return Crew_Info;
         when 27 => -- Esc select close option, used second time, close menu
            if OrderName = "Close" then
               DrawGame(Crew_Info);
               return Crew_Info;
            else
               Result := Driver(OrdersMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(OrdersMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OrdersMenu, M_Clear_Pattern);
               Result := Driver(OrdersMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Orders_For_All;
   exception
      when An_Exception : Crew_Order_Error =>
         ShowDialog(Exception_Message(An_Exception));
         DrawGame(Crew_Info);
         return Crew_Info;
   end CrewOrdersAllKeys;

   function OrdersPrioritiesKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      OptionIndex: Positive := PriorityIndex;
      NewPriority: Integer := -1;
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous order
            Result := Driver(PrioritiesMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(PrioritiesMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next order
            Result := Driver(PrioritiesMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(PrioritiesMenu, M_First_Item);
            end if;
         when 52 | KEY_LEFT => -- Set lower priority
            NewPriority :=
              PlayerShip.Crew(MemberIndex).Orders(OptionIndex) - 1;
            if NewPriority > -1 then
               DrawGame(Crew_Info);
               PlayerShip.Crew(MemberIndex).Orders(OptionIndex) := NewPriority;
            end if;
         when 54 | KEY_RIGHT => -- Set higher priority
            NewPriority :=
              PlayerShip.Crew(MemberIndex).Orders(OptionIndex) + 1;
            if NewPriority = 1 then
               DrawGame(Crew_Info);
               PlayerShip.Crew(MemberIndex).Orders(OptionIndex) := NewPriority;
            elsif NewPriority = 2 then
               DrawGame(Crew_Info);
               for I in PlayerShip.Crew.Element(MemberIndex).Orders'Range loop
                  if PlayerShip.Crew(MemberIndex).Orders(I) = 2 then
                     NewPriority := 1;
                     OptionIndex := I;
                     PlayerShip.Crew(MemberIndex).Orders(OptionIndex) :=
                       NewPriority;
                     exit;
                  end if;
               end loop;
               NewPriority := 2;
               OptionIndex := Get_Index(Current(PrioritiesMenu));
               PlayerShip.Crew(MemberIndex).Orders(OptionIndex) := NewPriority;
            else
               NewPriority := -1;
            end if;
         when 10 => -- Quit or show hint about setting
            if OptionIndex > Orders_Array'Last then
               PriorityIndex := 1;
               UpdateOrders;
               DrawGame(Crew_Info);
               return Crew_Info;
            else
               ShowDialog
                 ("Use Left arrow to lower order priority or Right arrow to raise order priority.");
               DrawGame(Crew_Info);
               ShowPrioritiesMenu;
            end if;
         when 27 => -- Esc select close option, used second time, close menu
            if Name(Current(PrioritiesMenu)) = "Close" then
               DrawGame(Crew_Info);
               return Crew_Info;
            else
               Result := Driver(PrioritiesMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(PrioritiesMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(PrioritiesMenu, M_Clear_Pattern);
               Result := Driver(PrioritiesMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         PriorityIndex := Get_Index(Current(PrioritiesMenu));
         Refresh(MenuWindow2);
      end if;
      if NewPriority > -1 then
         ShowPrioritiesMenu;
      end if;
      return Orders_Priorities;
   end OrdersPrioritiesKeys;

end Crew.UI;
