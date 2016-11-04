--    Copyright 2016 Bartek thindil Jasicki
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Bases; use Bases;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Maps; use Maps;

package body BasesList is

    BasesMenu : Menu;
    MenuWindow : Window;
    CurrentMenuIndex : Positive := 1;

    procedure ShowBaseInfo is
        InfoWindow : Window;
        BaseIndex : constant Positive := Positive'Value(Description(Current(BasesMenu)));
    begin
        InfoWindow := Create(6, (Columns / 2), 3, (Columns / 2));
        if SkyBases(BaseIndex).Visited.Year > 0 then
            Add(Win => InfoWindow, Str => "X:" & Positive'Image(SkyBases(BaseIndex).SkyX) & " Y:" &
                Positive'Image(SkyBases(BaseIndex).SkyX));
            Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
            Add(Win => InfoWindow, Str => "Type: " & To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
            Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
            Add(Win => InfoWindow, Str => "Size: ");
            if SkyBases(BaseIndex).Population < 150 then
                Add(Win => InfoWindow, Str => "small");
            elsif SkyBases(BaseIndex).Population > 149 and SkyBases(BaseIndex).Population < 300 then
                Add(Win => InfoWindow, Str => "medium");
            else
                Add(Win => InfoWindow, Str => "large");
            end if;
            Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
            Add(Win => InfoWindow, Str => "Last visited: " & FormatedTime(SkyBases(BaseIndex).Visited));
        else
            Add(Win => InfoWindow, Str => "Not visited yet.");
        end if;
        Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
        Add(Win => InfoWindow, Str => "Press SPACE to show base on map");
        Change_Attributes(Win => InfoWindow, Line => 5, Column => 6, Count => 5, Color => 1);
        Refresh;
        Refresh(InfoWindow);
        Delete(InfoWindow);
    end ShowBaseInfo;

    procedure ShowBasesList is
        Bases_Items : constant Item_Array_Access := new Item_Array(1..(SkyBases'Last + 1));
        MenuHeight : Line_Position;
        MenuLength : Column_Position;
        MenuIndex : Positive := 1;
    begin
        for I in SkyBases'Range loop
            if SkyBases(I).Known then
                Bases_Items.all(MenuIndex) := New_Item(To_String(SkyBases(I).Name), Positive'Image(I));
                MenuIndex := MenuIndex + 1;
            end if;
        end loop;
        for I in MenuIndex..Bases_Items'Last loop
            Bases_Items.all(I) := Null_Item;
        end loop;
        BasesMenu := New_Menu(Bases_Items);
        Set_Options(BasesMenu, (Show_Descriptions => False, others => True));
        Set_Format(BasesMenu, Lines - 10, 1);
        Set_Mark(BasesMenu, "");
        Scale(BasesMenu, MenuHeight, MenuLength);
        MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
        Set_Window(BasesMenu, MenuWindow);
        Set_Sub_Window(BasesMenu, Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
        Post(BasesMenu);
        Set_Current(BasesMenu, Bases_Items.all(CurrentMenuIndex));
        ShowBaseInfo;
        Refresh(MenuWindow);
    end ShowBasesList;

    function BasesListKeys(Key : Key_Code) return GameStates is
        Result : Menus.Driver_Result;
        BaseIndex : constant Positive := Positive'Value(Description(Current(BasesMenu)));
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                CurrentMenuIndex := 1;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when KEY_UP => -- Select previous item to Bases
                Result := Driver(BasesMenu, M_Up_Item);
                if Result = Request_Denied then
                    Result := Driver(BasesMenu, M_Last_Item);
                end if;
            when KEY_DOWN => -- Select next item to Bases
                Result := Driver(BasesMenu, M_Down_Item);
                if Result = Request_Denied then
                    Result := Driver(BasesMenu, M_First_Item);
                end if;
            when KEY_NPAGE => -- Scroll list one screen down
                Result := Driver(BasesMenu, M_ScrollUp_Page);
            when KEY_PPAGE => -- Scroll list one screen up
                Result := Driver(BasesMenu, M_ScrollDown_Page);
            when KEY_HOME => -- Scroll list to start
                Result := Driver(BasesMenu, M_First_Item);
            when KEY_END => -- Scroll list to end
                Result := Driver(BasesMenu, M_Last_Item);
            when 32 => -- Show selected base on map
                MoveMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY);
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when others =>
                null;
        end case;
        if Result = Menu_Ok then
            ShowBaseInfo;
            Refresh(MenuWindow);
        end if;
        CurrentMenuIndex := Get_Index(Current(BasesMenu));
        return Bases_List;
    end BasesListKeys;

end BasesList;
