--    Copyright 2017 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Maps; use Maps;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Help.UI; use Help.UI;
with Events; use Events;
with Header; use Header;
with Utils.UI; use Utils.UI;
with Bases.UI; use Bases.UI;
with Config; use Config;

package body Trades.UI.Keys is

   function TradeResult return GameStates is
      Visibility: Cursor_Visibility := Invisible;
      FieldIndex: constant Positive := Get_Index(Current(TradeForm));
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseItemIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      procedure ShowErrorMessage(Message: String) is
      begin
         Set_Cursor_Visibility(Visibility);
         Post(TradeForm, False);
         Delete(TradeForm);
         ShowDialog(Message);
         DrawGame(Trade_View);
      end ShowErrorMessage;
   begin
      if BaseIndex > 0 then
         Trader := "base";
      else
         Trader := "ship";
      end if;
      if FieldIndex < 3 then
         return Trade_Form;
      elsif FieldIndex > 3 then
         if Integer'Value(Description(Current(TradeMenu))) > 0 then
            CargoIndex := Integer'Value(Description(Current(TradeMenu)));
            if BaseIndex > 0 then
               BaseItemIndex :=
                 FindBaseCargo
                   (PlayerShip.Cargo(CargoIndex).ProtoIndex,
                    PlayerShip.Cargo(CargoIndex).Durability);
               if BaseItemIndex = 0 then
                  BaseItemIndex :=
                    FindBaseCargo(PlayerShip.Cargo(CargoIndex).ProtoIndex);
               end if;
            else
               for I in TraderCargo.Iterate loop
                  if TraderCargo(I).ProtoIndex =
                    PlayerShip.Cargo(CargoIndex).ProtoIndex and
                    TraderCargo(I).Durability =
                      PlayerShip.Cargo(CargoIndex).Durability then
                     BaseItemIndex := BaseCargo_Container.To_Index(I);
                     exit;
                  end if;
               end loop;
               if BaseItemIndex = 0 then
                  for I in TraderCargo.Iterate loop
                     if TraderCargo(I).ProtoIndex =
                       PlayerShip.Cargo(CargoIndex).ProtoIndex then
                        BaseItemIndex := BaseCargo_Container.To_Index(I);
                        exit;
                     end if;
                  end loop;
               end if;
            end if;
         else
            BaseItemIndex :=
              Integer'Value(Description(Current(TradeMenu))) * (-1);
            if BaseIndex > 0 then
               CargoIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ProtoIndex =>
                      SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex,
                    Durability =>
                      SkyBases(BaseIndex).Cargo(BaseItemIndex).Durability);
               if CargoIndex = 0 then
                  CargoIndex :=
                    FindItem
                      (PlayerShip.Cargo,
                       SkyBases(BaseIndex).Cargo(BaseItemIndex).ProtoIndex);
               end if;
            else
               CargoIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ProtoIndex => TraderCargo(BaseItemIndex).ProtoIndex,
                    Durability => TraderCargo(BaseItemIndex).Durability);
               if CargoIndex = 0 then
                  CargoIndex :=
                    FindItem
                      (PlayerShip.Cargo,
                       TraderCargo(BaseItemIndex).ProtoIndex);
               end if;
            end if;
         end if;
         if not Buy then
            if FieldIndex = 4 then
               SellItems(CargoIndex, Get_Buffer(Fields(TradeForm, 2)));
            else
               SellItems
                 (CargoIndex,
                  Positive'Image(PlayerShip.Cargo.Element(CargoIndex).Amount));
            end if;
         else
            BuyItems(BaseItemIndex, Get_Buffer(Fields(TradeForm, 2)));
         end if;
      end if;
      Set_Cursor_Visibility(Visibility);
      Post(TradeForm, False);
      Delete(TradeForm);
      DrawGame(Trade_View);
      return Trade_View;
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowErrorMessage
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this " &
            Trader &
            ".");
         return Trade_View;
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowErrorMessage
           ("You can't buy " &
            Exception_Message(An_Exception) &
            " in this base at this moment.");
         return Trade_View;
      when An_Exception : Trade_Buying_Too_Much =>
         ShowErrorMessage
           (Trader &
            " don't have that much " &
            Exception_Message(An_Exception) &
            " for sale.");
         return Trade_View;
      when Trade_No_Free_Cargo =>
         ShowErrorMessage
           ("You don't have that much free space in your ship cargo.");
         return Trade_View;
      when An_Exception : Trade_No_Money =>
         ShowErrorMessage
           ("You don't have any " &
            To_String(MoneyName) &
            " to buy " &
            Exception_Message(An_Exception) &
            ".");
         return Trade_View;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowErrorMessage
           ("You don't have enough " &
            To_String(MoneyName) &
            " to buy so much " &
            Exception_Message(An_Exception) &
            ".");
         return Trade_View;
      when Trade_Invalid_Amount =>
         if Buy then
            ShowErrorMessage("You entered invalid amount to buy.");
         else
            ShowErrorMessage("You entered invalid amount to sell.");
         end if;
         return Trade_View;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowErrorMessage
           ("You dont have that much " &
            Exception_Message(An_Exception) &
            " in ship cargo.");
         return Trade_View;
      when An_Exception : Trade_No_Money_In_Base =>
         ShowErrorMessage
           ("You can't sell so much " &
            Exception_Message(An_Exception) &
            " because " &
            Trader &
            " don't have that much " &
            To_String(MoneyName) &
            " to buy it.");
         return Trade_View;
      when Trade_No_Trader =>
         ShowErrorMessage
           ("You don't have assigned anyone in crew to talk in bases duty.");
         return Trade_View;
   end TradeResult;

   function TradeKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
   begin
      if Key = Key_Code(GameSettings.Keys(33)) then -- Show help
         Erase;
         ShowGameHeader(Help_Topic);
         ShowHelp(Trade_View, 3);
         return Help_Topic;
      end if;
      case Key is
         when 27 => -- Back to sky map
            if BaseIndex = 0 and EventIndex > 0 then
               DeleteEvent(EventIndex);
            end if;
            CurrentMenuIndex := 1;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous item to trade
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item to trade
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when 32 => -- Sell item
            Buy := False;
            return ShowTradeForm;
         when 10 => -- Buy item
            Buy := True;
            return ShowTradeForm;
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowItemInfo;
      end if;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      return Trade_View;
   end TradeKeys;

   function TradeFormKeys(Key: Key_Code) return GameStates is
      Result: Forms.Driver_Result;
      FieldIndex: Positive := Get_Index(Current(TradeForm));
      Visibility: Cursor_Visibility := Invisible;
      MaxIndex: Positive;
   begin
      case Key is
         when KEY_UP => -- Select previous field
            Result := Driver(TradeForm, F_Previous_Field);
            FieldIndex := Get_Index(Current(TradeForm));
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_End_Line);
            end if;
         when KEY_DOWN => -- Select next field
            Result := Driver(TradeForm, F_Next_Field);
            FieldIndex := Get_Index(Current(TradeForm));
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_End_Line);
            end if;
         when 10 => -- quit/buy/sell
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Next_Field);
               if Result = Form_Ok then
                  if Get_Buffer(Fields(TradeForm, 2)) =
                    "                    " then
                     if Buy then
                        FieldIndex := 3;
                     else
                        FieldIndex := 5;
                     end if;
                  else
                     FieldIndex := 4;
                  end if;
                  Set_Current(TradeForm, Fields(TradeForm, FieldIndex));
               end if;
            else
               return TradeResult;
            end if;
         when Key_Backspace => -- delete last character
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Delete_Previous);
               if Result = Form_Ok then
                  FieldIndex := Get_Index(Current(TradeForm));
                  if FieldIndex /= 2 then
                     FieldIndex := 2;
                     Set_Current(TradeForm, Fields(TradeForm, 2));
                  end if;
               end if;
            end if;
         when KEY_DC => -- delete character at cursor
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Delete_Char);
            end if;
         when KEY_RIGHT => -- Move cursor right
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Right_Char);
            end if;
         when KEY_LEFT => -- Move cursor left
            if FieldIndex = 2 then
               Result := Driver(TradeForm, F_Left_Char);
            end if;
         when 27 => -- Escape select cancel button, second time closes form
            if FieldIndex /= 3 then
               FieldIndex := 3;
               Set_Current(TradeForm, Fields(TradeForm, 3));
               Result := Form_Ok;
            else
               return TradeResult;
            end if;
         when others =>
            Result := Driver(TradeForm, Key);
      end case;
      if Result = Form_Ok then
         if Buy then
            MaxIndex := 4;
         else
            MaxIndex := 5;
         end if;
         for I in 2 .. MaxIndex loop
            Set_Foreground(Fields(TradeForm, I));
            Set_Background(Fields(TradeForm, I));
         end loop;
         Set_Foreground(Current(TradeForm), BoldCharacters, 11);
         Set_Background(Current(TradeForm), BoldCharacters, 11);
         if FieldIndex = 2 then
            Visibility := Normal;
         end if;
         Set_Cursor_Visibility(Visibility);
         Refresh(FormWindow);
      end if;
      return Trade_Form;
   end TradeFormKeys;

end Trades.UI.Keys;
