--    Copyright 2018 Bartek thindil Jasicki
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Window; use Gtk.Window;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Container; use Gtk.Container;
with Gtk.Adjustment; use Gtk.Adjustment;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Pango.Font; use Pango.Font;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Device; use Gdk.Device;
with Gdk.Window; use Gdk.Window;
with Gdk.Types; use Gdk.Types;
with Gdk; use Gdk;
with Game; use Game;
with MainMenu; use MainMenu;
with Utils.UI; use Utils.UI;
with Ships; use Ships;
with Messages; use Messages;
with Crew; use Crew;
with ShipModules; use ShipModules;
with Events; use Events;
with Items; use Items;
with Config; use Config;
with Bases; use Bases;
with Missions; use Missions;

package body Maps.UI is

   Builder: Gtkada_Builder;
   MapWidth,
   MapHeight,
   CenterX,
   CenterY,
   MapCellWidth,
   MapCellHeight,
   MapX,
   MapY: Positive;
   StartX, StartY: Integer;

   function QuitGame
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      SkyMapWindow: constant Gtk_Window :=
        Gtk_Window(Get_Object(Object, "skymapwindow"));
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (SkyMapWindow,
           Modal,
           Message_Question,
           Buttons_Yes_No,
           "Are you sure want to quit game?");
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         EndGame(True);
         Destroy(MessageDialog);
         ShowMainMenu;
         return Hide_On_Delete(Gtk_Widget(SkyMapWindow));
      end if;
      Destroy(MessageDialog);
      return True;
   end QuitGame;

   procedure QuitGameMenu(Object: access Gtkada_Builder_Record'Class) is
   begin
      if not QuitGame(Object) then
         ShowDialog
           ("Can't quit game.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      end if;
   end QuitGameMenu;

   procedure UpdateHeader is
      HaveWorker, HaveGunner: Boolean := True;
      NeedCleaning,
      NeedRepairs,
      NeedWorker,
      HavePilot,
      HaveEngineer,
      HaveTrader,
      HaveUpgrader,
      HaveCleaner,
      HaveRepairman: Boolean :=
        False;
      ItemIndex, ItemAmount: Natural := 0;
   begin
      Set_Text(Gtk_Label(Get_Object(Builder, "lbltime")), FormatedTime);
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnofuel"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnodrink"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "lblnofood"))) then
         Hide(Gtk_Widget(Get_Object(Builder, "lblnofood")));
      end if;
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
      if ItemIndex > 0 then
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = FuelType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowFuel;
         end loop;
         if ItemAmount < GameSettings.LowFuel then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnofuel")),
               "[<span foreground=""yellow"">[Low Fuel]</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnofuel")),
               "Low level of fuel on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
         end if;
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnofuel")),
            "[<span foreground=""red"">[No Fuel]</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnofuel")),
            "You can't travel anymore.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnofuel")));
      end if;
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => DrinksType);
      if ItemIndex = 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnodrink")),
            "[<span foreground=""red"">[No Drinks]</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnodrink")),
            "You don't have any drinks in ship.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = DrinksType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowDrinks;
         end loop;
         if ItemAmount < GameSettings.LowDrinks then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnodrink")),
               "[<span foreground=""yellow"">[Low Drinks]</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnodrink")),
               "Low level of drinks on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnodrink")));
         end if;
      end if;
      for FoodType of FoodTypes loop
         ItemIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => FoodType);
         exit when ItemIndex > 0;
      end loop;
      if ItemIndex = 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblnofood")),
            "[<span foreground=""red"">[No Food]</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblnofood")),
            "You don't have any food in ship.");
         Show_All(Gtk_Widget(Get_Object(Builder, "lblnofood")));
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if FoodTypes.Find_Index
              (Item => Items_List(Item.ProtoIndex).IType) /=
              UnboundedString_Container.No_Index then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > GameSettings.LowFood;
         end loop;
         if ItemAmount < GameSettings.LowFood then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblnofood")),
               "[<span foreground=""yellow"">[Low Food]</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblnofood")),
               "Low level of food on ship.");
            Show_All(Gtk_Widget(Get_Object(Builder, "lblnofood")));
         end if;
      end if;
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when GUN | HARPOON_GUN =>
               if Module.Owner = 0 then
                  HaveGunner := False;
               elsif PlayerShip.Crew(Module.Owner).Order /= Gunner then
                  HaveGunner := False;
               end if;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Data(1) > 0 then
                  NeedWorker := True;
                  if Module.Owner = 0 then
                     HaveWorker := False;
                  elsif PlayerShip.Crew(Module.Owner).Order /= Craft then
                     HaveWorker := False;
                  end if;
               end if;
            when CABIN =>
               if Module.Data(1) /= Module.Data(2) then
                  NeedCleaning := True;
               end if;
            when others =>
               null;
         end case;
         if Module.Durability /= Module.MaxDurability then
            NeedRepairs := True;
         end if;
      end loop;
      for Member of PlayerShip.Crew loop
         case Member.Order is
            when Pilot =>
               HavePilot := True;
            when Engineer =>
               HaveEngineer := True;
            when Talk =>
               HaveTrader := True;
            when Upgrading =>
               HaveUpgrader := True;
            when Clean =>
               HaveCleaner := True;
            when Repair =>
               HaveRepairman := True;
            when others =>
               null;
         end case;
      end loop;
      if HavePilot then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblpilot")),
            "[<span foreground=""green"">P</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblpilot")),
            "Pilot is on position.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblpilot")),
            "[<span foreground=""red"">P</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblpilot")),
            "No pilot assigned. Ship can't move.");
      end if;
      if HaveEngineer then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblengineer")),
            "[<span foreground=""green"">E</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblengineer")),
            "Engineer is on position.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblengineer")),
            "[<span foreground=""red"">E</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblengineer")),
            "No engineer assigned. Ship can't move.");
      end if;
      if HaveGunner then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblgunners")),
            "[<span foreground=""green"">G</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblgunners")),
            "All guns are manned.");
      else
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lblgunners")),
            "[<span foreground=""red"">G</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblgunners")),
            "One or more guns don't have gunner.");
      end if;
      if NeedRepairs then
         if HaveRepairman then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblrepairs")),
               "[<span foreground=""green"">R</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
               "Ship is being repaired.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblrepairs")),
               "[<span foreground=""red"">R</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
               "Ship needs repairs but no one is working on it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblrepairs")), "[R]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblrepairs")),
            "Ship no need repairs.");
      end if;
      if NeedWorker then
         if HaveWorker then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblcrafting")),
               "[<span foreground=""green"">M</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
               "All crafting orders are executed.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblcrafting")),
               "[<span foreground=""red"">M</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
               "You need to assing some crew members to manufacturing.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblcrafting")), "[M]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblcrafting")),
            "No crafting orders were set.");
      end if;
      if PlayerShip.UpgradeModule > 0 then
         if HaveUpgrader then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblupgrade")),
               "[<span foreground=""green"">U</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
               "Ship module upgrade in progress.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblupgrade")),
               "[<span foreground=""red"">U</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
               "Ship module upgrade in progress but no one is working on it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblupgrade")), "[U]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblupgrade")),
            "No ship module upgrade was set.");
      end if;
      if HaveTrader then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lbltalk")),
            "[<span foreground=""green"">T</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "Trader is on position.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         Set_Markup
           (Gtk_Label(Get_Object(Builder, "lbltalk")),
            "[<span foreground=""red"">T</span>]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "No trader assigned. You need one for talk/trade.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
         if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .EType =
           FriendlyShip then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lbltalk")),
               "[<span foreground=""red"">T</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lbltalk")),
               "No trader assigned. You need one for talk/trade.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lbltalk")), "[T]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lbltalk")),
            "No trader needed.");
      end if;
      if NeedCleaning then
         if HaveCleaner then
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblclean")),
               "[<span foreground=""green"">C</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblclean")),
               "Ship is cleaned.");
         else
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblclean")),
               "[<span foreground=""red"">C</span>]");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "lblclean")),
               "Ship is dirty but no one is cleaning it.");
         end if;
      else
         Set_Text(Gtk_Label(Get_Object(Builder, "lblclean")), "[C]");
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "lblclean")),
            "Ship no need cleaning.");
      end if;
   end UpdateHeader;

   procedure UpdateMoveButtons is
   begin
      if PlayerShip.Speed = DOCKED then
         Hide(Gtk_Widget(Get_Object(Builder, "cmbspeed")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
         Set_Label(Gtk_Button(Get_Object(Builder, "btnmove")), "Wait");
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupleft")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnup")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupright")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnleft")), False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnright")), False);
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "btnbottomleft")),
            False);
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottom")), False);
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "btnbottomright")),
            False);
      else
         Set_Active
           (Gtk_Combo_Box(Get_Object(Builder, "cmdspeed")),
            Gint(ShipSpeed'Pos(PlayerShip.Speed)));
         Show_All(Gtk_Widget(Get_Object(Builder, "cmbspeed")));
         if PlayerShip.DestinationX > 0 and PlayerShip.DestinationY > 0 then
            Show_All(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Label(Gtk_Button(Get_Object(Builder, "btnmove")), "Move");
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Text(Gtk_Label(Get_Object(Builder, "btnmove")), "Wait");
         end if;
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnup")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnupright")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnright")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottomleft")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottom")));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnbottomright")));
      end if;
   end UpdateMoveButtons;

   procedure DrawMap is
      Iter: Gtk_Text_Iter;
      MapBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmap"));
      EndY, EndX: Integer;
      Tags: constant Gtk_Text_Tag_Table := Get_Tag_Table(MapBuffer);
      WhiteColor: constant Gtk_Text_Tag := Lookup(Tags, "white");
      GrayColor: constant Gtk_Text_Tag := Lookup(Tags, "gray");
      RedColor: constant Gtk_Text_Tag := Lookup(Tags, "red");
      GreenColor: constant Gtk_Text_Tag := Lookup(Tags, "green");
      BlueColor: constant Gtk_Text_Tag := Lookup(Tags, "blue");
      CyanColor: constant Gtk_Text_Tag := Lookup(Tags, "cyan");
      MapChar: Character;
      MapColor: Gtk_Text_Tag;
   begin
      StartY := CenterY - (MapHeight / 2);
      StartX := CenterX - (MapWidth / 2);
      EndY := CenterY + (MapHeight / 2);
      EndX := CenterX + (MapWidth / 2);
      if StartY < 1 then
         StartY := 1;
         EndY := MapHeight;
      end if;
      if StartX < 1 then
         StartX := 1;
         EndX := MapWidth;
      end if;
      if EndY > 1024 then
         EndY := 1024;
         StartY := 1024 - MapHeight;
      end if;
      if EndX > 1024 then
         EndX := 1024;
         StartX := 1024 - MapWidth;
      end if;
      Get_Start_Iter(MapBuffer, Iter);
      for Y in StartY .. EndY loop
         for X in StartX .. EndX loop
            if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
               MapChar := '+';
               MapColor := WhiteColor;
            else
               MapChar := ' ';
               if SkyMap(X, Y).Visited then
                  MapColor := WhiteColor;
               else
                  MapColor := GrayColor;
               end if;
               if X = PlayerShip.DestinationX and
                 Y = PlayerShip.DestinationY then
                  MapChar := 'X';
               elsif SkyMap(X, Y).MissionIndex > 0 then
                  MapChar := '!';
               elsif SkyMap(X, Y).EventIndex > 0 then
                  MapChar := '?';
               elsif SkyMap(X, Y).BaseIndex > 0 then
                  if SkyBases(SkyMap(X, Y).BaseIndex).Known then
                     MapChar := 'o';
                     if SkyBases(SkyMap(X, Y).BaseIndex).Visited.Year > 0 then
                        case SkyBases(SkyMap(X, Y).BaseIndex).BaseType is
                           when Industrial =>
                              MapColor := RedColor;
                           when Agricultural =>
                              MapColor := GreenColor;
                           when Refinery =>
                              MapColor := BlueColor;
                           when Shipyard =>
                              MapColor := CyanColor;
                           when others =>
                              null;
                        end case;
                     end if;
                  end if;
               end if;
            end if;
            Insert_With_Tags(MapBuffer, Iter, "" & MapChar, MapColor);
         end loop;
         if Y < EndY then
            Insert(MapBuffer, Iter, "" & ASCII.LF);
         end if;
      end loop;
   end DrawMap;

   procedure GetMapSize(Object: access Gtkada_Builder_Record'Class) is
      MapBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Object, "txtmap"));
      MapView: constant Gtk_Text_View :=
        Gtk_Text_View(Get_Object(Object, "mapview"));
      Iter: Gtk_Text_Iter;
      Location: Gdk_Rectangle;
      Result: Boolean;
   begin
      Get_Start_Iter(MapBuffer, Iter);
      Forward_Line(Iter, Result);
      Forward_Char(Iter, Result);
      Get_Iter_Location(MapView, Iter, Location);
      MapWidth :=
        Positive(Get_Allocated_Width(Gtk_Widget(MapView)) / Location.X) - 1;
      MapHeight :=
        Positive(Get_Allocated_Height(Gtk_Widget(MapView)) / Location.Y) - 1;
      MapCellWidth := Positive(Location.X);
      MapCellHeight := Positive(Location.Y);
      Set_Text(MapBuffer, "");
      DrawMap;
   end GetMapSize;

   function ShowMapCellInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      MouseX, MouseY: Gint;
      DeviceManager: constant Gdk_Device_Manager :=
        Get_Device_Manager
          (Get_Display(Gtk_Widget(Get_Object(Builder, "mapview"))));
      Mouse: constant Gdk_Device := Get_Client_Pointer(DeviceManager);
      Mask: Gdk_Modifier_Type;
      Window: Gdk_Window;
      MapInfoText: Unbounded_String;
   begin
      Get_Device_Position
        (Get_Window(Gtk_Widget(Get_Object(Builder, "mapview"))),
         Mouse,
         MouseX,
         MouseY,
         Mask,
         Window);
      MapX := (Positive(MouseX) / MapCellWidth) + StartX;
      MapY := (Positive(MouseY) / MapCellHeight) + StartY;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblmapx")),
         "X:" & Positive'Image(MapX));
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblmapy")),
         "Y:" & Positive'Image(MapY));
      if SkyMap(MapX, MapY).BaseIndex > 0 then
         declare
            BaseIndex: constant Positive := SkyMap(MapX, MapY).BaseIndex;
         begin
            Append(MapInfoText, "Base info");
            Append(MapInfoText, ASCII.LF);
            Append
              (MapInfoText,
               To_Unbounded_String("Name: ") & SkyBases(BaseIndex).Name);
            if SkyBases(SkyMap(MapX, MapY).BaseIndex).Visited.Year > 0 then
               Append(MapInfoText, ASCII.LF);
               Append
                 (MapInfoText,
                  "Type: " &
                  To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
               Append(MapInfoText, ASCII.LF);
               if SkyBases(BaseIndex).Population > 0 and
                 SkyBases(BaseIndex).Population < 150 then
                  Append(MapInfoText, "Population: small");
               elsif SkyBases(BaseIndex).Population > 149 and
                 SkyBases(BaseIndex).Population < 300 then
                  Append(MapInfoText, "Population: medium");
               elsif SkyBases(BaseIndex).Population > 299 then
                  Append(MapInfoText, "Population: large");
               end if;
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, ASCII.LF);
               end if;
               if SkyBases(BaseIndex).Owner = Abandoned then
                  Append(MapInfoText, "Base is abandoned");
               else
                  Append
                    (MapInfoText,
                     "Owner: " &
                     To_Lower(Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
               end if;
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, ASCII.LF);
                  case SkyBases(BaseIndex).Reputation(1) is
                     when -100 .. -75 =>
                        Append(MapInfoText, "You are hated here");
                     when -74 .. -50 =>
                        Append(MapInfoText, "You are outlaw here");
                     when -49 .. -25 =>
                        Append(MapInfoText, "You are hostile here");
                     when -24 .. -1 =>
                        Append(MapInfoText, "They are unfriendly to you");
                     when 0 =>
                        Append(MapInfoText, "You are unknown here");
                     when 1 .. 25 =>
                        Append(MapInfoText, "You are know here as visitor");
                     when 26 .. 50 =>
                        Append(MapInfoText, "You are know here as trader");
                     when 51 .. 75 =>
                        Append(MapInfoText, "You are know here as friend");
                     when 76 .. 100 =>
                        Append(MapInfoText, "You are well know here");
                     when others =>
                        null;
                  end case;
               end if;
            end if;
         end;
      end if;
      if SkyMap(MapX, MapY).EventIndex > 0 then
         declare
            EventIndex: constant Positive := SkyMap(MapX, MapY).EventIndex;
         begin
            if Events_List(EventIndex).EType /= BaseRecovery then
               Append(MapInfoText, ASCII.LF & ASCII.LF);
            end if;
            case Events_List(EventIndex).EType is
               when EnemyShip | Trader | FriendlyShip =>
                  Append
                    (MapInfoText,
                     ProtoShips_List(Events_List(EventIndex).Data).Name);
               when FullDocks =>
                  Append(MapInfoText, "Full docks in base");
               when AttackOnBase =>
                  Append(MapInfoText, "Base is under attack");
               when Disease =>
                  Append(MapInfoText, "Disease in base");
               when EnemyPatrol =>
                  Append(MapInfoText, "Enemy patrol");
               when DoublePrice =>
                  Append
                    (MapInfoText,
                     "Double price for " &
                     To_String(Items_List(Events_List(EventIndex).Data).Name));
               when None | BaseRecovery =>
                  null;
            end case;
         end;
      end if;
      if SkyMap(MapX, MapY).MissionIndex > 0 then
         declare
            MissionIndex: constant Positive := SkyMap(MapX, MapY).MissionIndex;
         begin
            Append(MapInfoText, ASCII.LF & ASCII.LF);
            case PlayerShip.Missions(MissionIndex).MType is
               when Deliver =>
                  Append
                    (MapInfoText,
                     "Deliver " &
                     To_String
                       (Items_List(PlayerShip.Missions(MissionIndex).Target)
                          .Name));
               when Destroy =>
                  Append
                    (MapInfoText,
                     "Destroy " &
                     To_String
                       (ProtoShips_List
                          (PlayerShip.Missions(MissionIndex).Target)
                          .Name));
               when Patrol =>
                  Append(MapInfoText, "Patrol area");
               when Explore =>
                  Append(MapInfoText, "Explore area");
               when Passenger =>
                  Append(MapInfoText, "Transport passenger");
            end case;
         end;
      end if;
      if MapX /= PlayerShip.SkyX or MapY /= PlayerShip.SkyY then
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btndestination")));
         if Length(MapInfoText) > 0 then
            Append(MapInfoText, ASCII.LF);
         end if;
         Append
           (MapInfoText,
            "Distance:" & Positive'Image(CountDistance(MapX, MapY)));
      else
         Set_Sensitive
           (Gtk_Widget(Get_Object(Object, "btndestination")),
            False);
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblmapinfo")),
         To_String(MapInfoText));
      Show_All(Gtk_Widget(Get_Object(Builder, "mapinfowindow")));
      return True;
   end ShowMapCellInfo;

   procedure HideMapInfoWindow(User_Data: access GObject_Record'Class) is
   begin
      Hide(Gtk_Window(User_Data));
   end HideMapInfoWindow;

   procedure ShowWindow(User_Data: access GObject_Record'Class) is
   begin
      Show_All(Gtk_Widget(User_Data));
   end ShowWindow;

   procedure UpdateMessages is
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      Iter: Gtk_Text_Iter;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"),
         To_Unbounded_String("green"),
         To_Unbounded_String("red"),
         To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
   begin
      Set_Text(MessagesBuffer, "");
      Get_Start_Iter(MessagesBuffer, Iter);
      if LoopStart = 0 then
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      for I in reverse LoopStart .. -1 loop
         Message := GetMessage(I + 1);
         if Message.Color = 0 then
            Insert(MessagesBuffer, Iter, To_String(Message.Message));
         else
            Insert_With_Tags
              (MessagesBuffer,
               Iter,
               To_String(Message.Message),
               Lookup
                 (Get_Tag_Table(MessagesBuffer),
                  To_String(TagNames(Message.Color))));
         end if;
         if I > LoopStart then
            Insert(MessagesBuffer, Iter, "" & ASCII.LF);
         end if;
      end loop;
      if LastMessage = Null_Unbounded_String then
         Hide(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
      else
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         Check_Resize(Gtk_Container(Get_Object(Builder, "skymapwindow")));
      end if;
   end UpdateMessages;

   procedure SetDestination(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.DestinationX := MapX;
      PlayerShip.DestinationY := MapY;
      AddMessage("You set travel destination for your ship.", OrderMessage);
      Hide(Gtk_Window(Get_Object(Object, "mapinfowindow")));
      UpdateMessages;
   end SetDestination;

   procedure HideLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      if not HideWindow(Get_Object(Object, "infolastmessage")) then
         ShowDialog
           ("Can't hide last message.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      else
         LastMessage := Null_Unbounded_String;
      end if;
      Check_Resize(Gtk_Container(Get_Object(Object, "skymapwindow")));
   end HideLastMessage;

   procedure MoveMap(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btncenter") then
         CenterX := PlayerShip.SkyX;
         CenterY := PlayerShip.SkyY;
      elsif User_Data = Get_Object(Builder, "btnmovemapok") then
         CenterX :=
           Positive(Get_Value(Gtk_Adjustment(Get_Object(Builder, "mapxadj"))));
         CenterY :=
           Positive(Get_Value(Gtk_Adjustment(Get_Object(Builder, "mapyadj"))));
      elsif User_Data = Get_Object(Builder, "btnmapup") then
         if CenterY - (MapHeight / 3) < 1 then
            CenterY := 1;
         else
            CenterY := CenterY - (MapHeight / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapdown") then
         if CenterY + (MapHeight / 3) > 1024 then
            CenterY := 1024;
         else
            CenterY := CenterY + (MapHeight / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapleft") then
         if CenterX - (MapWidth / 3) < 1 then
            CenterX := 1;
         else
            CenterX := CenterX - (MapWidth / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapright") then
         if CenterX + (MapWidth / 3) > 1024 then
            CenterX := 1024;
         else
            CenterX := CenterX + (MapWidth / 3);
         end if;
      end if;
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmap")), "");
      DrawMap;
      Hide(Gtk_Widget(Get_Object(Builder, "movemapwindow")));
   end MoveMap;

   procedure CreateSkyMap is
      Error: aliased GError;
      FontDescription: constant Pango_Font_Description :=
        Pango_Font_Description_New;
   begin
      CenterX := PlayerShip.SkyX;
      CenterY := PlayerShip.SkyY;
      if Builder = null then
         Gtk_New(Builder);
         if Add_From_File
             (Builder,
              To_String(DataDirectory) & "ui" & Dir_Separator & "skymap.glade",
              Error'Access) =
           Guint(0) then
            Put_Line("Error : " & Get_Message(Error));
            return;
         end if;
         Register_Handler(Builder, "Quit_Game", QuitGame'Access);
         Register_Handler(Builder, "Quit_Game_Menu", QuitGameMenu'Access);
         Register_Handler
           (Builder,
            "Hide_Last_Message",
            HideLastMessage'Access);
         Register_Handler(Builder, "Get_New_Size", GetMapSize'Access);
         Register_Handler(Builder, "Show_Map_Info", ShowMapCellInfo'Access);
         Register_Handler(Builder, "Hide_Map_Info", HideMapInfoWindow'Access);
         Register_Handler(Builder, "Show_Window", ShowWindow'Access);
         Register_Handler(Builder, "Set_Destination", SetDestination'Access);
         Register_Handler(Builder, "Hide_Window", HideWindow'Access);
         Register_Handler(Builder, "Move_Map", MoveMap'Access);
         Do_Connect(Builder);
         Set_Family(FontDescription, "monospace");
         Override_Font
           (Gtk_Widget(Get_Object(Builder, "mapview")),
            FontDescription);
      end if;
      UpdateMessages;
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "txtmap")),
         "X" & ASCII.LF & "X");
      Show_All(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
      UpdateHeader;
      UpdateMoveButtons;
      Hide(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
   end CreateSkyMap;

end Maps.UI;
