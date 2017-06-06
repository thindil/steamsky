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

with Maps; use Maps;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Items; use Items;

package body Bases.UI.Recruits is

   procedure ShowRecruitInfo is
      InfoWindow, ClearWindow: Window;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      RecruitIndex: constant Positive := Get_Index(Current(TradeMenu));
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      CurrentLine: Line_Position := 3;
      SkillLevel: Unbounded_String;
      WindowWidth: Line_Position;
      MoneyIndex2: Natural := 0;
   begin
      ClearWindow := Create((Lines - 1), (Columns / 2), 3, (Columns / 2));
      Refresh(ClearWindow);
      Delete(ClearWindow);
      WindowWidth := 4 + Line_Position(Recruit.Skills.Length);
      if WindowWidth > (Lines - 5) then
         WindowWidth := Lines - 5;
      end if;
      InfoWindow := Create(WindowWidth, (Columns / 2), 3, (Columns / 2));
      Box(InfoWindow);
      Move_Cursor(Win => InfoWindow, Line => 0, Column => 2);
      Add(Win => InfoWindow, Str => "[Recruit info]");
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 2);
      if Recruit.Gender = 'M' then
         Add(Win => InfoWindow, Str => "Male");
      else
         Add(Win => InfoWindow, Str => "Female");
      end if;
      for Skill of Recruit.Skills loop
         case Skill(2) is
            when 1 .. 10 =>
               SkillLevel := To_Unbounded_String("Beginner");
            when 11 .. 20 =>
               SkillLevel := To_Unbounded_String("Novice");
            when 21 .. 30 =>
               SkillLevel := To_Unbounded_String("Apprentice");
            when 31 .. 40 =>
               SkillLevel := To_Unbounded_String("Practitioner");
            when 41 .. 50 =>
               SkillLevel := To_Unbounded_String("Competent");
            when 51 .. 60 =>
               SkillLevel := To_Unbounded_String("Respected");
            when 61 .. 70 =>
               SkillLevel := To_Unbounded_String("Renowned");
            when 71 .. 80 =>
               SkillLevel := To_Unbounded_String("Master");
            when 81 .. 90 =>
               SkillLevel := To_Unbounded_String("Grand-Master");
            when 91 .. 99 =>
               SkillLevel := To_Unbounded_String("Legendary");
            when others =>
               SkillLevel := To_Unbounded_String("Ultimate");
         end case;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 2);
         Add
           (Win => InfoWindow,
            Str =>
              To_String(Skills_Names(Skill(1))) &
              ": " &
              To_String(SkillLevel));
         CurrentLine := CurrentLine + 1;
      end loop;
      MoneyIndex2 := FindCargo(FindProtoItem(MoneyIndex));
      Move_Cursor(Line => WindowWidth + 3, Column => (Columns / 2));
      if MoneyIndex2 > 0 then
         Add
           (Str =>
              "Press ENTER to hire for" &
              Positive'Image(Recruit.Price) &
              " " &
              To_String(MoneyName) &
              ".");
         Change_Attributes
           (Line => WindowWidth + 3,
            Column => (Columns / 2) + 6,
            Count => 5,
            Color => 1);
         Move_Cursor(Line => WindowWidth + 4, Column => (Columns / 2));
         Add
           (Str =>
              "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
              " " &
              To_String(MoneyName) &
              ".");
      else
         Add
           (Str =>
              "You don't have any " &
              To_String(MoneyName) &
              " to hire anyone.");
      end if;
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowRecruitInfo;

   procedure ShowRecruits is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruits_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      if SkyBases(BaseIndex).Recruits.Length = 0 then
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
         Add(Str => "Here no recruits to hire.");
         Refresh;
         return;
      end if;
      Recruits_Items :=
        new Item_Array
        (SkyBases(BaseIndex).Recruits.First_Index ..
             (SkyBases(BaseIndex).Recruits.Last_Index + 1));
      for I in
        SkyBases(BaseIndex).Recruits.First_Index ..
            SkyBases(BaseIndex).Recruits.Last_Index loop
         Recruits_Items.all(I) :=
           New_Item(To_String(SkyBases(BaseIndex).Recruits(I).Name));
      end loop;
      Recruits_Items.all(Recruits_Items'Last) := Null_Item;
      TradeMenu := New_Menu(Recruits_Items);
      Set_Format(TradeMenu, Lines - 10, 1);
      Set_Mark(TradeMenu, "");
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if Recruits_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Recruits_Items.all(CurrentMenuIndex));
      ShowRecruitInfo;
      Refresh(MenuWindow);
   end ShowRecruits;

   function RecruitKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            CurrentMenuIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous recruit to hire
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               ShowRecruitInfo;
               Refresh(MenuWindow);
            end if;
         when 50 | KEY_DOWN => -- Select next recruit to hire
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               ShowRecruitInfo;
               Refresh(MenuWindow);
            end if;
         when 10 => -- Hire recruit
            HireRecruit(Get_Index(Current(TradeMenu)));
            DrawGame(Recruits_View);
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result = Menu_Ok then
               ShowRecruitInfo;
               Refresh(MenuWindow);
            else
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
               if Result = Menu_Ok then
                  ShowRecruitInfo;
                  Refresh(MenuWindow);
               end if;
            end if;
      end case;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      return Recruits_View;
   end RecruitKeys;

end Bases.UI.Recruits;
