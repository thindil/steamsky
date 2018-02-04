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
with Ada.Containers; use Ada.Containers;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
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
with Gdk; use Gdk;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Device; use Gdk.Device;
with Gdk.Window; use Gdk.Window;
with Gdk.Types; use Gdk.Types;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Utils.UI; use Utils.UI;
with Ships; use Ships;
with Ships.Movement; use Ships.Movement;
with Ships.Crew; use Ships.Crew;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Crew; use Crew;
with ShipModules; use ShipModules;
with Events; use Events;
with Items; use Items;
with Config; use Config;
with Bases; use Bases;
with Missions; use Missions;
with Crafts; use Crafts;
with Combat.UI; use Combat.UI;
with Help.UI; use Help.UI;

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
   ButtonsVisible: Boolean := False;

   procedure QuitGameMenu(Object: access Gtkada_Builder_Record'Class) is
   begin
      if not QuitGame(Gtk_Window(Get_Object(Object, "skymapwindow"))) then
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
         Set_Label(Gtk_Button(Get_Object(Builder, "btnmovewait")), "Wait");
         Set_Label(Gtk_Button(Get_Object(Builder, "btndock")), "Undock");
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
           (Gtk_Combo_Box(Get_Object(Builder, "cmbspeed")),
            Gint(ShipSpeed'Pos(PlayerShip.Speed) - 1));
         Show_All(Gtk_Widget(Get_Object(Builder, "cmbspeed")));
         if PlayerShip.DestinationX > 0 and PlayerShip.DestinationY > 0 then
            Show_All(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Label(Gtk_Button(Get_Object(Builder, "btnmovewait")), "Move");
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btnmoveto")));
            Set_Label(Gtk_Button(Get_Object(Builder, "btnmovewait")), "Wait");
         end if;
         Set_Label(Gtk_Button(Get_Object(Builder, "btndock")), "Dock");
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            Show_All(Gtk_Widget(Get_Object(Builder, "btndock")));
         else
            Hide(Gtk_Widget(Get_Object(Builder, "btndock")));
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

   procedure UpdateMenu is
      NeedHealing, NeedRest: Boolean := False;
   begin
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            NeedRest := True;
         end if;
         if PlayerShip.Crew(I).Health < 100 and
           PlayerShip.Crew(I).Health > 0 and
           PlayerShip.Crew(I).Order = Rest then
            for Module of PlayerShip.Modules loop
               if Modules_List(Module.ProtoIndex).MType = CABIN and
                 Module.Owner = I then
                  NeedHealing := True;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      Set_Visible
        (Gtk_Widget(Get_Object(Builder, "itemwaitheal")),
         NeedHealing);
      Set_Visible(Gtk_Widget(Get_Object(Builder, "itemwaitrest")), NeedRest);
   end UpdateMenu;

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
      TextWindow: constant Gdk_Window :=
        Get_Window
          (Gtk_Text_View(Get_Object(Builder, "mapview")),
           Text_Window_Text);
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
      if TextWindow /= null then
         Set_Cursor(TextWindow, Gdk_Cursor_New(Cross));
      end if;
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
            if Events_List(EventIndex).EType /= BaseRecovery and
              SkyMap(MapX, MapY).BaseIndex > 0 then
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
            if SkyMap(MapX, MapY).BaseIndex > 0 or
              SkyMap(MapX, MapY).EventIndex > 0 then
               Append(MapInfoText, ASCII.LF & ASCII.LF);
            end if;
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
         LastMessage := Null_Unbounded_String;
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

   procedure BtnDockClicked(Object: access Gtkada_Builder_Record'Class) is
      Message: Unbounded_String := Null_Unbounded_String;
   begin
      if PlayerShip.Speed = DOCKED then
         Message := To_Unbounded_String(DockShip(False));
         if Length(Message) > 0 then
            ShowDialog
              (To_String(Message),
               Gtk_Window(Get_Object(Object, "skymapwindow")));
            return;
         end if;
      else
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            ShowDialog
              (To_String(Message),
               Gtk_Window(Get_Object(Object, "skymapwindow")));
            return;
         end if;
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      UpdateMenu;
   end BtnDockClicked;

   procedure ChangeSpeed(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.Speed :=
        ShipSpeed'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbspeed"))) + 1);
   end ChangeSpeed;

   procedure MoveShip(User_Data: access GObject_Record'Class) is
      Message: Unbounded_String;
      Result: Natural;
      StartsCombat: Boolean;
      NewX, NewY: Integer;
   begin
      if User_Data = Get_Object(Builder, "btnup") then -- Move up
         Result := MoveShip(0, 0, -1, Message);
      elsif User_Data = Get_Object(Builder, "btnbottom") then -- Move down
         Result := MoveShip(0, 0, 1, Message);
      elsif User_Data = Get_Object(Builder, "btnright") then -- Move right
         Result := MoveShip(0, 1, 0, Message);
      elsif User_Data = Get_Object(Builder, "btnleft") then -- Move left
         Result := MoveShip(0, -1, 0, Message);
      elsif User_Data =
        Get_Object(Builder, "btnbottomleft") then -- Move down/left
         Result := MoveShip(0, -1, 1, Message);
      elsif User_Data =
        Get_Object(Builder, "btnbottomright") then -- Move down/right
         Result := MoveShip(0, 1, 1, Message);
      elsif User_Data = Get_Object(Builder, "btnupleft") then -- Move up/left
         Result := MoveShip(0, -1, -1, Message);
      elsif User_Data = Get_Object(Builder, "btnupright") then -- Move up/right
         Result := MoveShip(0, 1, -1, Message);
      elsif User_Data =
        Get_Object
          (Builder,
           "btnmovewait") then -- Move to destination or wait 1 game minute
         if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
            Result := 1;
            UpdateGame(1);
         else
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(0, NewX, NewY, Message);
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.",
                  OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
            end if;
         end if;
      elsif User_Data =
        Get_Object(Builder, "btnmoveto") then -- Move to destination
         loop
            NewX := 0;
            NewY := 0;
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(0, NewX, NewY, Message);
            exit when Result = 0;
            StartsCombat := CheckForEvent;
            if StartsCombat then
               Result := 4;
               exit;
            end if;
            if Result = 8 then
               WaitForRest;
               Result := 1;
               StartsCombat := CheckForEvent;
               if StartsCombat then
                  Result := 4;
                  exit;
               end if;
            end if;
            if GameSettings.AutoMoveStop /= NEVER and
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
               declare
                  EventIndex: constant Positive :=
                    SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
               begin
                  case GameSettings.AutoMoveStop is
                     when ANY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when FRIENDLY =>
                        if Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip then
                           Result := 0;
                           exit;
                        end if;
                     when ENEMY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when NEVER =>
                        null;
                  end case;
               end;
            end if;
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.",
                  OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
               exit;
            end if;
            exit when Result = 6 or Result = 7;
         end loop;
      end if;
      case Result is
         when 1 => -- Ship moved, check for events
            StartsCombat := CheckForEvent;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when 6 => -- Ship moved, but pilot needs rest, confirm
            if ShowConfirmDialog
                ("You don't have pilot on duty. Did you want to wait until your pilot rest?",
                 Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 7 => -- Ship moved, but engineer needs rest, confirm
            if ShowConfirmDialog
                ("You don't have engineer on duty. Did you want to wait until your engineer rest?",
                 Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 8 => -- Ship moved, but crew needs rest, autorest
            StartsCombat := CheckForEvent;
            if not StartsCombat then
               WaitForRest;
               StartsCombat := CheckForEvent;
            end if;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when others =>
            null;
      end case;
      if Message /= Null_Unbounded_String then
         ShowDialog
           (To_String(Message),
            Gtk_Window(Get_Object(Builder, "skymapwindow")));
      end if;
      if Result > 0 then
         CenterX := PlayerShip.SkyX;
         CenterY := PlayerShip.SkyY;
      end if;
      if StartsCombat then
         Hide(Gtk_Window(Get_Object(Builder, "skymapwindow")));
         ShowCombatUI;
      end if;
      if Result > 0 then
         UpdateHeader;
         UpdateMessages;
         UpdateMoveButtons;
         UpdateMenu;
         DrawMap;
      end if;
   end MoveShip;

   procedure HideButtons(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      Set_No_Show_All(Widget, True);
      Hide(Widget);
   end HideButtons;

   procedure CheckButtons(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      if not Get_No_Show_All(Widget) and not ButtonsVisible then
         ButtonsVisible := True;
      end if;
   end CheckButtons;

   procedure ShowOrders(Object: access Gtkada_Builder_Record'Class) is
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
   begin
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxorders")),
         HideButtons'Access);
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
      if PlayerShip.Speed = DOCKED then
         if HaveTrader and SkyBases(BaseIndex).Owner /= Abandoned then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btntrade")), False);
            Set_No_Show_All
              (Gtk_Widget(Get_Object(Object, "btnschool")),
               False);
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnrecruit")),
                  False);
            end if;
            if DaysDifference(SkyBases(BaseIndex).AskedForEvents) > 6 then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnaskevents")),
                  False);
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnaskbases")),
                  False);
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnheal")),
                     False);
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnrepair")),
                     False);
                  exit;
               end if;
            end loop;
            if SkyBases(BaseIndex).BaseType = Shipyard then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnshipyard")),
                  False);
            end if;
            for I in Recipes_List.First_Index .. Recipes_List.Last_Index loop
               if Known_Recipes.Find_Index(Item => I) =
                 Positive_Container.No_Index and
                 Recipes_List(I).BaseType =
                   Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1 then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnrecipes")),
                     False);
                  exit;
               end if;
            end loop;
            if SkyBases(BaseIndex).Missions.Length > 0 then
               case SkyBases(BaseIndex).Reputation(1) is
                  when 0 .. 25 =>
                     MissionsLimit := 1;
                  when 26 .. 50 =>
                     MissionsLimit := 3;
                  when 51 .. 75 =>
                     MissionsLimit := 5;
                  when 76 .. 100 =>
                     MissionsLimit := 10;
                  when others =>
                     MissionsLimit := 0;
               end case;
               for Mission of PlayerShip.Missions loop
                  if (Mission.Finished and Mission.StartBase = BaseIndex) or
                    (Mission.TargetX = PlayerShip.SkyX and
                     Mission.TargetY = PlayerShip.SkyY) then
                     case Mission.MType is
                        when Deliver =>
                           Set_Label
                             (Gtk_Button
                                (Get_Object(Object, "btnfinishmission")),
                              "Complete delivery of " &
                              To_String(Items_List(Mission.Target).Name));
                        when Destroy =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "Complete destroy " &
                                 To_String
                                   (ProtoShips_List(Mission.Target).Name));
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "Complete Patrol area mission");
                           end if;
                        when Explore =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "Complete Explore area mission");
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "Complete Transport passenger mission");
                           end if;
                     end case;
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnfinishmission")),
                        False);
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop;
               if MissionsLimit > 0 then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnmissions")),
                     False);
               end if;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnsethome")),
                  False);
            end if;
         end if;
         if SkyBases(BaseIndex).Owner = Abandoned then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnloot")), False);
         end if;
      else
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            Event :=
              Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType;
         end if;
         case Event is
            when EnemyShip | EnemyPatrol =>
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")),
                  False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")),
                  "Attack");
            when FullDocks =>
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")),
                  False);
               Set_Label(Gtk_Button(Get_Object(Object, "btnattack")), "Wait");
            when AttackOnBase =>
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")),
                  False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")),
                  "Defend");
            when Disease =>
               if HaveTrader then
                  ItemIndex :=
                    FindItem
                      (Inventory => PlayerShip.Cargo,
                       ItemType => HealingTools);
                  if ItemIndex > 0 then
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnfreemedicines")),
                        False);
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnpricedmedicines")),
                        False);
                  end if;
               end if;
            when None | DoublePrice | BaseRecovery =>
               if BaseIndex > 0 then
                  for Mission of PlayerShip.Missions loop
                     if HaveTrader then
                        case Mission.MType is
                           when Deliver =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "Complete delivery of " &
                                 To_String(Items_List(Mission.Target).Name));
                           when Destroy =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "Complete destroy " &
                                    To_String
                                      (ProtoShips_List(Mission.Target).Name));
                              end if;
                           when Patrol =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "Complete Patrol area mission");
                              end if;
                           when Explore =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "Complete Explore area mission");
                              end if;
                           when Passenger =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "Complete Transport passenger mission");
                              end if;
                        end case;
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btnfinishmission")),
                           False);
                     end if;
                  end loop;
               else
                  for Mission of PlayerShip.Missions loop
                     if Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver | Passenger =>
                              null;
                           when Destroy =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "Search for " &
                                 To_String
                                   (ProtoShips_List(Mission.Target).Name));
                           when Patrol =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "Patrol area");
                           when Explore =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "Explore area");
                        end case;
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btncurrentmission")),
                           False);
                     end if;
                  end loop;
               end if;
            when Trader =>
               if HaveTrader then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btntrade")),
                     False);
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskevents")),
                     False);
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskbases")),
                     False);
               end if;
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")),
                  False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")),
                  "Attack");
            when FriendlyShip =>
               if HaveTrader then
                  if Index
                      (ProtoShips_List
                         (Events_List
                            (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                               .EventIndex)
                            .Data)
                         .Name,
                       To_String(TradersName)) >
                    0 then
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btntrade")),
                        False);
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnaskbases")),
                        False);
                  end if;
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskevents")),
                     False);
               end if;
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")),
                  False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")),
                  "Attack");
         end case;
      end if;
      ButtonsVisible := False;
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxorders")),
         CheckButtons'Access);
      if ButtonsVisible then
         Show_All(Gtk_Widget(Get_Object(Object, "orderswindow")));
      else
         ShowDialog
           ("Here are no available ship orders at this moment.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      end if;
   end ShowOrders;

   procedure WaitOrder(User_Data: access GObject_Record'Class) is
      TimeNeeded: Natural := 0;
   begin
      if User_Data = Get_Object(Builder, "item1min") then
         UpdateGame(1);
      elsif User_Data = Get_Object(Builder, "item5min") then
         UpdateGame(5);
      elsif User_Data = Get_Object(Builder, "item10min") then
         UpdateGame(10);
      elsif User_Data = Get_Object(Builder, "item15min") then
         UpdateGame(15);
      elsif User_Data = Get_Object(Builder, "item30min") then
         UpdateGame(30);
      elsif User_Data = Get_Object(Builder, "item1hour") then
         UpdateGame(60);
      elsif User_Data = Get_Object(Builder, "itemwaitrest") then
         WaitForRest;
      elsif User_Data = Get_Object(Builder, "itemwaitheal") then
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 and
              PlayerShip.Crew(I).Health > 0 and
              PlayerShip.Crew(I).Order = Rest then
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = CABIN and
                    Module.Owner = Crew_Container.To_Index(I) then
                     if TimeNeeded <
                       (100 - PlayerShip.Crew(I).Health) * 15 then
                        TimeNeeded := (100 - PlayerShip.Crew(I).Health) * 15;
                     end if;
                     exit;
                  end if;
               end loop;
            end if;
         end loop;
         if TimeNeeded > 0 then
            UpdateGame(TimeNeeded);
         else
            return;
         end if;
      elsif User_Data = Get_Object(Builder, "waitxadj") then
         Hide(Gtk_Widget(Get_Object(Builder, "waitxwindow")));
         UpdateGame(Positive(Get_Value(Gtk_Adjustment(User_Data))));
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      UpdateMenu;
      DrawMap;
   end WaitOrder;

   procedure AttackOrder(Object: access Gtkada_Builder_Record'Class) is
      BtnAttack: constant Gtk_Button :=
        Gtk_Button(Get_Object(Object, "btnattack"));
      Label: constant String := Get_Label(BtnAttack);
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
      if Label = "Wait" then
         Show_All(Gtk_Widget(Get_Object(Builder, "waitxwindow")));
      else
         Hide(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
         ShowCombatUI;
      end if;
   end AttackOrder;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(1);
   end ShowHelp;

   procedure ShowMessages(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "skymapwindow")));
      ShowMessagesUI(SkyMap_View);
   end ShowMessages;

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
         Register_Handler(Builder, "Dock_Ship", BtnDockClicked'Access);
         Register_Handler(Builder, "Change_Speed", ChangeSpeed'Access);
         Register_Handler(Builder, "Move_Ship", MoveShip'Access);
         Register_Handler(Builder, "Show_Orders", ShowOrders'Access);
         Register_Handler(Builder, "Wait_Order", WaitOrder'Access);
         Register_Handler(Builder, "Attack_Order", AttackOrder'Access);
         Register_Handler(Builder, "Show_Help", ShowHelp'Access);
         Register_Handler(Builder, "Show_Messages", ShowMessages'Access);
         Do_Connect(Builder);
         Set_Family(FontDescription, "monospace");
         Override_Font
           (Gtk_Widget(Get_Object(Builder, "mapview")),
            FontDescription);
         Override_Background_Color
           (Gtk_Widget(Get_Object(Builder, "messagesview")),
            0,
            Black_RGBA);
         Override_Color
           (Gtk_Widget(Get_Object(Builder, "messagesview")),
            0,
            White_RGBA);
      end if;
      UpdateMessages;
      Set_Text
        (Gtk_Text_Buffer(Get_Object(Builder, "txtmap")),
         "X" & ASCII.LF & "X");
      Show_All(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
      UpdateHeader;
      UpdateMoveButtons;
      UpdateMenu;
      Hide(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
   end CreateSkyMap;

end Maps.UI;
