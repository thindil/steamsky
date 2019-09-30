--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Window; use Gtk.Window;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Expander; use Gtk.Expander;
with Gtk.Main; use Gtk.Main;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Game; use Game;
with Items; use Items;
with Factions; use Factions;
with Config; use Config;
with Utils.UI; use Utils.UI;
with BasesTypes; use BasesTypes;

package body Help.UI is

   -- ****iv* Help.UI/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****
   -- ****iv* Help.UI/Setting
   -- FUNCTION
   -- If true, UI is in setting mode. Default is false
   -- SOURCE
   Setting: Boolean := False;
   -- ****

   -- ****if* Help.UI/SelectTopic
   -- FUNCTION
   -- Show selected from the list help topic
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SelectTopic(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      TopicName: Unbounded_String;
   begin
      if Setting then
         return;
      end if;
      declare
         TopicIter: Gtk_Tree_Iter;
         TopicModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treetopics"))),
            TopicModel, TopicIter);
         if TopicIter = Null_Iter then
            return;
         end if;
         TopicName :=
           To_Unbounded_String(Get_String(TopicModel, TopicIter, 0));
      end;
      for I in Help_List.Iterate loop
         if TopicName = Help_Container.Key(I) then
            ShowHelpUI(Help_List(I).Index);
            exit;
         end if;
      end loop;
   end SelectTopic;

   -- ****if* Help.UI/DisableMouse
   -- FUNCTION
   -- Disable mouse buttons on help text view
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI (unused)
   -- RESULT
   -- This function always return True
   -- SOURCE
   function DisableMouse
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      pragma Unreferenced(Object);
      -- ****
   begin
      return True;
   end DisableMouse;

   -- ****if* Help.UI/HideHelpWindow
   -- FUNCTION
   -- Hide help window instead of destroying it
   -- PARAMETERS
   -- User_Data - Window to hide
   -- SOURCE
   function HideHelpWindow
     (User_Data: access GObject_Record'Class) return Boolean is
   -- ****
   begin
      GameSettings.TopicsPosition :=
        Natural(Get_Position(Gtk_Paned(Get_Object(Builder, "helppaned"))));
      SaveConfig;
      return Hide_On_Delete(Gtk_Widget(User_Data));
   end HideHelpWindow;

   -- ****if* Help.UI/ToggleTopics
   -- FUNCTION
   -- Hide or show help topics list
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ToggleTopics(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      if not Get_Expanded(Gtk_Expander(Get_Object(Object, "helpexpander"))) and
        GameSettings.TopicsPosition > 0 then
         Set_Position
           (Gtk_Paned(Get_Object(Builder, "helppaned")),
            Gint(GameSettings.TopicsPosition));
      else
         GameSettings.TopicsPosition :=
           Natural(Get_Position(Gtk_Paned(Get_Object(Builder, "helppaned"))));
         Set_Position(Gtk_Paned(Get_Object(Builder, "helppaned")), 60);
      end if;
   end ToggleTopics;

   procedure CreateHelpUI is
      Error: aliased GError;
      TopicsIter: Gtk_Tree_Iter;
      TopicsList: Gtk_List_Store;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "help.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      TopicsList := Gtk_List_Store(Get_Object(Builder, "topicslist"));
      Clear(TopicsList);
      for I in Help_List.Iterate loop
         Append(TopicsList, TopicsIter);
         Set(TopicsList, TopicsIter, 0, To_String(Help_Container.Key(I)));
      end loop;
      Register_Handler(Builder, "Hide_Window", HideHelpWindow'Access);
      Register_Handler(Builder, "Select_Topic", SelectTopic'Access);
      Register_Handler(Builder, "Disable_Mouse", DisableMouse'Access);
      Register_Handler(Builder, "Toggle_Topics", ToggleTopics'Access);
      Do_Connect(Builder);
      On_Key_Release_Event
        (Gtk_Widget(Get_Object(Builder, "helpwindow")), CloseWindow'Access);
   end CreateHelpUI;

   procedure ShowHelpUI(HelpIndex: Unbounded_String) is
      NewText, TagText: Unbounded_String;
      StartIndex, EndIndex, OldIndex, TopicIndex: Natural;
      Key: Gtk_Accel_Key;
      Found: Boolean;
      type Variables_Data is record
         Name: Unbounded_String;
         Value: Unbounded_String;
      end record;
      Variables: constant array(Positive range <>) of Variables_Data :=
        (1 => (Name => To_Unbounded_String("MoneyName"), Value => MoneyName),
         2 =>
           (Name => To_Unbounded_String("FuelName"),
            Value => Items_List(FindProtoItem(ItemType => FuelType)).Name),
         3 =>
           (Name => To_Unbounded_String("StrengthName"),
            Value => Attributes_List(StrengthIndex).Name),
         4 =>
           (Name => To_Unbounded_String("PilotingSkill"),
            Value => Skills_List(PilotingSkill).Name),
         5 =>
           (Name => To_Unbounded_String("EngineeringSkill"),
            Value => Skills_List(EngineeringSkill).Name),
         6 =>
           (Name => To_Unbounded_String("GunnerySkill"),
            Value => Skills_List(GunnerySkill).Name),
         7 =>
           (Name => To_Unbounded_String("TalkingSkill"),
            Value => Skills_List(TalkingSkill).Name),
         8 =>
           (Name => To_Unbounded_String("PerceptionSkill"),
            Value => Skills_List(PerceptionSkill).Name),
         9 =>
           (Name => To_Unbounded_String("ConditionName"),
            Value => Attributes_List(ConditionIndex).Name),
         10 =>
           (Name => To_Unbounded_String("DodgeSkill"),
            Value => Skills_List(DodgeSkill).Name),
         11 =>
           (Name => To_Unbounded_String("UnarmedSkill"),
            Value => Skills_List(UnarmedSkill).Name));
      AccelNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("<skymapwindow>/btnupleft"),
         To_Unbounded_String("<skymapwindow>/btnup"),
         To_Unbounded_String("<skymapwindow>/btnupright"),
         To_Unbounded_String("<skymapwindow>/btnleft"),
         To_Unbounded_String("<skymapwindow>/btnmovewait"),
         To_Unbounded_String("<skymapwindow>/btnright"),
         To_Unbounded_String("<skymapwindow>/btnbottomleft"),
         To_Unbounded_String("<skymapwindow>/btnbottom"),
         To_Unbounded_String("<skymapwindow>/btnbottomright"),
         To_Unbounded_String("<skymapwindow>/btnmoveto"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipCargoInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/CrewInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/ShipOrders"),
         To_Unbounded_String("<skymapwindow>/Menu/CraftInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MessagesInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/BasesInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/EventsInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MissionsInfo"),
         To_Unbounded_String("<skymapwindow>/Menu/MoveMap"),
         To_Unbounded_String("<skymapwindow>/Menu/GameStats"),
         To_Unbounded_String("<skymapwindow>/Menu/Help"),
         To_Unbounded_String("<skymapwindow>/Menu/GameOptions"),
         To_Unbounded_String("<skymapwindow>/Menu/QuitGame"),
         To_Unbounded_String("<skymapwindow>/Menu/ResignFromGame"),
         To_Unbounded_String("<skymapwindow>/Menu"),
         To_Unbounded_String("<skymapwindow>/Menu/WaitOrders"),
         To_Unbounded_String("<skymapwindow>/zoomin"),
         To_Unbounded_String("<skymapwindow>/zoomout"));
      HelpBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "helpbuffer"));
      Iter: Gtk_Text_Iter;
      Tags: constant Gtk_Text_Tag_Table := Get_Tag_Table(HelpBuffer);
      SpecialText: constant Gtk_Text_Tag := Lookup(Tags, "special");
      type FontTag is record
         Tag: String(1 .. 1);
         TextTag: Gtk_Text_Tag;
      end record;
      FontTags: constant array(Positive range <>) of FontTag :=
        (1 => (Tag => "b", TextTag => Lookup(Tags, "bold")),
         2 => (Tag => "u", TextTag => Lookup(Tags, "underline")),
         3 => (Tag => "i", TextTag => Lookup(Tags, "italic")));
      FlagsTags: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("diseaseimmune"),
         To_Unbounded_String("nofatigue"), To_Unbounded_String("nomorale"),
         To_Unbounded_String("naturalarmor"),
         To_Unbounded_String("toxicattack"),
         To_Unbounded_String("sentientships"),
         To_Unbounded_String("fanaticism"), To_Unbounded_String("loner"));
      FactionsWithFlag: Unbounded_String;
      BasesFlags: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("shipyard"), To_Unbounded_String("temple"));
      BasesWithFlag: Unbounded_String;
   begin
      if Setting then
         return;
      end if;
      Setting := True;
      for Help of Help_List loop
         if Help.Index = HelpIndex then
            NewText := Help.Text;
            exit;
         end if;
      end loop;
      OldIndex := 1;
      Set_Text(HelpBuffer, "");
      Get_Start_Iter(HelpBuffer, Iter);
      loop
         StartIndex := Index(NewText, "{", OldIndex);
         if StartIndex > 0 then
            Insert(HelpBuffer, Iter, Slice(NewText, OldIndex, StartIndex - 1));
         else
            Insert
              (HelpBuffer, Iter, Slice(NewText, OldIndex, Length(NewText)));
            exit;
         end if;
         EndIndex := Index(NewText, "}", StartIndex) - 1;
         TagText := Unbounded_Slice(NewText, StartIndex + 1, EndIndex);
         for I in Variables'Range loop
            if TagText = Variables(I).Name then
               Insert_With_Tags
                 (HelpBuffer, Iter, To_String(Variables(I).Value),
                  SpecialText);
               exit;
            end if;
         end loop;
         for I in AccelNames'Range loop
            if TagText =
              To_Unbounded_String("GameKey") &
                To_Unbounded_String(Positive'Image(I)) then
               Lookup_Entry(To_String(AccelNames(I)), Key, Found);
               Insert_With_Tags
                 (HelpBuffer, Iter,
                  "'" & Accelerator_Get_Label(Key.Accel_Key, Key.Accel_Mods) &
                  "'",
                  SpecialText);
               exit;
            end if;
         end loop;
         for I in FontTags'Range loop
            if TagText = To_Unbounded_String(FontTags(I).Tag) then
               StartIndex := Index(NewText, "{", EndIndex) - 1;
               Insert_With_Tags
                 (HelpBuffer, Iter, Slice(NewText, EndIndex + 2, StartIndex),
                  FontTags(I).TextTag);
               EndIndex := Index(NewText, "}", StartIndex) - 1;
               exit;
            end if;
         end loop;
         for I in FlagsTags'Range loop
            if TagText = FlagsTags(I) then
               FactionsWithFlag := Null_Unbounded_String;
               for Faction of Factions_List loop
                  if Faction.Flags.Contains(TagText) then
                     if FactionsWithFlag /= Null_Unbounded_String then
                        Append(FactionsWithFlag, " and ");
                     end if;
                     Append(FactionsWithFlag, Faction.Name);
                  end if;
               end loop;
               while Ada.Strings.Unbounded.Count(FactionsWithFlag, " and ") >
                 1 loop
                  Replace_Slice
                    (FactionsWithFlag, Index(FactionsWithFlag, " and "),
                     Index(FactionsWithFlag, " and ") + 4, ", ");
               end loop;
               Insert(HelpBuffer, Iter, To_String(FactionsWithFlag));
               exit;
            end if;
         end loop;
         for BaseFlag of BasesFlags loop
            if TagText /= BaseFlag then
               goto Bases_Flags_Loop_End;
            end if;
            BasesWithFlag := Null_Unbounded_String;
            for BaseType of BasesTypes_List loop
               if BaseType.Flags.Contains(TagText) then
                  if BasesWithFlag /= Null_Unbounded_String then
                     Append(BasesWithFlag, " and ");
                  end if;
                  Append(BasesWithFlag, BaseType.Name);
               end if;
            end loop;
            while Ada.Strings.Unbounded.Count(BasesWithFlag, " and ") > 1 loop
               Replace_Slice
                 (BasesWithFlag, Index(BasesWithFlag, " and "),
                  Index(BasesWithFlag, " and ") + 4, ", ");
            end loop;
            Insert(HelpBuffer, Iter, To_String(BasesWithFlag));
            exit;
            <<Bases_Flags_Loop_End>>
         end loop;
         OldIndex := EndIndex + 2;
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "helpwindow")));
      Resize
        (Gtk_Window(Get_Object(Builder, "helpwindow")),
         Gint(GameSettings.WindowWidth), Gint(GameSettings.WindowHeight));
      while Events_Pending loop
         if Main_Iteration_Do(False) then
            return;
         end if;
      end loop;
      TopicIndex := 0;
      for I in Help_List.Iterate loop
         if HelpIndex = Help_List(I).Index then
            exit;
         end if;
         TopicIndex := TopicIndex + 1;
      end loop;
      if GameSettings.TopicsPosition > 0 then
         Set_Position
           (Gtk_Paned(Get_Object(Builder, "helppaned")),
            Gint(GameSettings.TopicsPosition));
      end if;
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treetopics")),
         Gtk_Tree_Path_New_From_String(Natural'Image(TopicIndex)), null,
         False);
      Setting := False;
   end ShowHelpUI;

end Help.UI;
