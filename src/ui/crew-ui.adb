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

with Terminal_Interface.Curses.Forms.Field_Types.IntField;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Messages; use Messages;
with Bases; use Bases;
with Maps; use Maps;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;
with Items.UI; use Items.UI;
with Config; use Config;
with Crew.Inventory; use Crew.Inventory;

package body Crew.UI is

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
      TiredPoints: constant Integer :=
        Member.Tired - Member.Attributes(ConditionIndex)(1);
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
      if TiredPoints > 20 and TiredPoints < 41 then
         Tired := To_Unbounded_String("Bit tired");
         TextColor(2) := 2;
      elsif TiredPoints > 40 and TiredPoints < 81 then
         Tired := To_Unbounded_String("Tired");
         TextColor(2) := 1;
      elsif TiredPoints > 80 and TiredPoints < 100 then
         Tired := To_Unbounded_String("Very tired");
         TextColor(2) := 3;
      elsif TiredPoints = 100 then
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
                 To_String(Skills_List(Skill(1)).Name) &
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
            when Boarding =>
               OrderName := To_Unbounded_String("Boarding");
            when Defend =>
               OrderName := To_Unbounded_String("Defends ship");
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
      ActionsWindow := Create(6, (Columns / 2), CurrentLine, (Columns / 2));
      CurrentLine := -1;
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
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => ActionsWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => ActionsWindow,
         Str => "Press F2 to show crew member inventory");
      Change_Attributes
        (Win => ActionsWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 2,
         Color => 1,
         Attr => BoldCharacters);
      for Module of PlayerShip.Modules loop
         if Module.Durability > 0 and
           Modules_List(Module.ProtoIndex).MType = CABIN and
           Module.Data(1) < Module.Data(2) and
           not NeedClean then
            NeedClean := True;
         end if;
         if not NeedRepairs and Module.Durability < Module.MaxDurability then
            if FindItem
                (Inventory => PlayerShip.Cargo,
                 ItemType => Modules_List(Module.ProtoIndex).RepairMaterial) >
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
      Add
        (Win => ActionsWindow,
         Str =>
           "Press " &
           GetKeyName(Key_Code(GameSettings.Keys(33))) &
           " for help");
      Change_Attributes
        (Win => ActionsWindow,
         Line => CurrentLine,
         Column => 6,
         Count => GetKeyName(Key_Code(GameSettings.Keys(33)))'Length,
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
      NeedHealer: Boolean := False;
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
         if PlayerShip.Crew(MemberIndex).Order /= Heal then
            for I in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(I).Health < 100 and
                 Crew_Container.To_Index(I) /= MemberIndex then
                  OrdersAmount := OrdersAmount + 1;
                  NeedHealer := True;
                  exit;
               end if;
            end loop;
         end if;
         for Module of PlayerShip.Modules loop
            if Module.Durability > 0 then
               case Modules_List(Module.ProtoIndex).MType is
                  when GUN | HARPOON_GUN =>
                     if Module.Owner /= MemberIndex then
                        OrdersAmount := OrdersAmount + 1;
                     end if;
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     if Module.Owner /= MemberIndex and
                       Module.Data(1) /= 0 then
                        OrdersAmount := OrdersAmount + 1;
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
                  when GUN | HARPOON_GUN =>
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
         if NeedHealer and PlayerShip.Crew(MemberIndex).Order /= Heal then
            Orders_Items.all(MenuIndex) :=
              New_Item("Heal wounded crew members", "0");
            MenuIndex := MenuIndex + 1;
         end if;
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

   procedure ShowItemInfo is
      ClearWindow: Window;
      CurrentLine: Line_Position := 1;
      FreeSpace: constant Integer := FreeInventory(MemberIndex, 0);
   begin
      ClearWindow := Create(Lines - 3, (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      CurrentLine :=
        Items.UI.ShowItemInfo
          (PlayerShip.Crew(MemberIndex).Inventory,
           ItemIndex);
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Free inventory space:" & Integer'Image(FreeSpace) & " kg");
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Press Enter to see item options");
      Change_Attributes
        (Line => CurrentLine,
         Column => (Columns / 2) + 6,
         Count => 5,
         Color => 1,
         Attr => BoldCharacters);
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Line => CurrentLine, Column => (Columns / 2));
      Add(Str => "Press Escape to back to crew informations");
      Change_Attributes
        (Line => CurrentLine,
         Column => (Columns / 2) + 6,
         Count => 6,
         Color => 1,
         Attr => BoldCharacters);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowItemInfo;

   procedure ShowInventory is
      Cargo_Items: constant Item_Array_Access :=
        new Item_Array
        (1 .. (PlayerShip.Crew(MemberIndex).Inventory.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      ItemName: Unbounded_String;
   begin
      if PlayerShip.Crew(MemberIndex).Inventory.Length = 0 then
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 4));
         Add(Str => "Empty, press Escape to back to crew list.");
         Change_Attributes
           (Line => (Lines / 3),
            Column => (Columns / 4) + 13,
            Count => 6,
            Color => 1,
            Attr => BoldCharacters);
         Refresh;
         return;
      end if;
      if MoveForm /= Null_Form then
         Post(MoveForm, False);
         Delete(MoveForm);
      end if;
      if OrdersMenu /= Null_Menu then
         Post(OrdersMenu, False);
         Delete(OrdersMenu);
      end if;
      for I in
        PlayerShip.Crew(MemberIndex).Inventory.First_Index ..
            PlayerShip.Crew(MemberIndex).Inventory.Last_Index loop
         ItemName :=
           To_Unbounded_String
             (GetItemName(PlayerShip.Crew(MemberIndex).Inventory(I)));
         for J in PlayerShip.Crew(MemberIndex).Equipment'Range loop
            if PlayerShip.Crew(MemberIndex).Equipment(J) = I then
               ItemName := ItemName & "(used)";
               exit;
            end if;
         end loop;
         Cargo_Items.all(I) := New_Item(To_String(ItemName));
      end loop;
      Cargo_Items.all(Cargo_Items'Last) := Null_Item;
      CrewMenu := New_Menu(Cargo_Items);
      Set_Format(CrewMenu, Lines - 10, 1);
      Scale(CrewMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(CrewMenu, MenuWindow);
      Set_Sub_Window
        (CrewMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(CrewMenu);
      if Cargo_Items.all(ItemIndex) = Null_Item then
         ItemIndex := 1;
      end if;
      Set_Current(CrewMenu, Cargo_Items.all(ItemIndex));
      ShowItemInfo;
   end ShowInventory;

   procedure ShowMoveForm is
      Move_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      MaxAmount: constant Positive :=
        PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount;
      FieldText: constant String :=
        "Enter amount (max" & Positive'Image(MaxAmount) & "): ";
   begin
      if MoveForm = Null_Form then
         Set_Cursor_Visibility(Visibility);
         Move_Fields.all(1) :=
           New_Field(1, Column_Position(FieldText'Length), 0, 0, 0, 0);
         FieldOptions := Get_Options(Move_Fields.all(1));
         Set_Buffer(Move_Fields.all(1), 0, FieldText);
         FieldOptions.Active := False;
         Set_Options(Move_Fields.all(1), FieldOptions);
         Move_Fields.all(2) :=
           New_Field(1, 20, 0, Column_Position(FieldText'Length), 0, 0);
         FieldOptions := Get_Options(Move_Fields.all(2));
         FieldOptions.Auto_Skip := False;
         Set_Options(Move_Fields.all(2), FieldOptions);
         Set_Foreground(Move_Fields.all(2), BoldCharacters, 11);
         Set_Background(Move_Fields.all(2), BoldCharacters, 11);
         Terminal_Interface.Curses.Forms.Field_Types.IntField.Set_Field_Type
           (Move_Fields.all(2),
            (0, 0, MaxAmount));
         Move_Fields.all(3) :=
           New_Field(1, 8, 2, (Column_Position(FieldText'Length) / 2), 0, 0);
         Set_Buffer(Move_Fields.all(3), 0, "[Cancel]");
         FieldOptions := Get_Options(Move_Fields.all(3));
         FieldOptions.Edit := False;
         Set_Options(Move_Fields.all(3), FieldOptions);
         Move_Fields.all(4) :=
           New_Field
             (1,
              4,
              2,
              (Column_Position(FieldText'Length) / 2) + 10,
              0,
              0);
         FieldOptions := Get_Options(Move_Fields.all(4));
         FieldOptions.Edit := False;
         Set_Options(Move_Fields.all(4), FieldOptions);
         Set_Buffer(Move_Fields.all(4), 0, "[Ok]");
         Move_Fields.all(5) := Null_Field;
         MoveForm := New_Form(Move_Fields);
         Scale(MoveForm, FormHeight, FormLength);
         FormWindow :=
           Create
             (FormHeight + 2,
              FormLength + 2,
              ((Lines / 3) - (FormHeight / 2)),
              ((Columns / 2) - (FormLength / 2)));
         Box(FormWindow);
         WindowFrame(FormWindow, 5, "Move item to ship cargo");
         Set_Window(MoveForm, FormWindow);
         Set_Sub_Window
           (MoveForm,
            Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
         Post(MoveForm);
      end if;
      Refresh_Without_Update;
      Refresh_Without_Update(FormWindow);
      Update_Screen;
   end ShowMoveForm;

   procedure ShowInventoryMenu is
      Options_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      ItemType: constant Unbounded_String :=
        Items_List
          (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
          .IType;
      Worn: constant Boolean := ItemIsUsed(MemberIndex, ItemIndex);
      OptionText: Unbounded_String := Null_Unbounded_String;
   begin
      if not Worn then
         if ItemType = WeaponType then
            OptionText := To_Unbounded_String("Use as weapon");
         elsif ItemType = ShieldType then
            OptionText := To_Unbounded_String("Use as shield");
         elsif ItemType = HeadArmor then
            OptionText := To_Unbounded_String("Use as helmet");
         elsif ItemType = ChestArmor then
            OptionText := To_Unbounded_String("Use as torso armor");
         elsif ItemType = ArmsArmor then
            OptionText := To_Unbounded_String("Use as arms armor");
         elsif ItemType = LegsArmor then
            OptionText := To_Unbounded_String("Use as legs armor");
         elsif Tools_List.Find_Index(Item => ItemType) /=
           UnboundedString_Container.No_Index then
            OptionText := To_Unbounded_String("Use as tool");
         end if;
      else
         OptionText := To_Unbounded_String("Take off item");
      end if;
      if OptionText = Null_Unbounded_String then
         Options_Items := new Item_Array(1 .. 3);
         Options_Items.all :=
           (New_Item("Move item to ship cargo"), New_Item("Close"), Null_Item);
      else
         Options_Items := new Item_Array(1 .. 4);
         Options_Items.all :=
           (New_Item(To_String(OptionText)),
            New_Item("Move item to ship cargo"),
            New_Item("Close"),
            Null_Item);
      end if;
      OrdersMenu := New_Menu(Options_Items);
      Set_Format(OrdersMenu, Lines - 10, 1);
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Item options");
      Set_Window(OrdersMenu, MenuWindow2);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow2);
      Update_Screen;
   end ShowInventoryMenu;

end Crew.UI;
