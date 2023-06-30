--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with BasesTypes;
with Careers;
with Factions;
with Game;
with Log;

package body Help is

   procedure Load_Help(File_Name: String) is
      use Ada.Characters.Latin_1;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use BasesTypes;
      use Factions;
      use Game;
      use Log;
      use Tiny_String;

      Tmp_Help: Help_Data := Empty_Help;
      Help_Title: Unbounded_String := Null_Unbounded_String;
      Result, Help_Index2, Help_Title2, Help_Text: chars_ptr;
      Index: Natural := 0;
      function Load_Ada_Help(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaHelp";
      procedure Get_Ada_Help
        (I: Natural; H_Index, H_Title, H_Text: out chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "getAdaHelp";
   begin
      Result := Load_Ada_Help(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
      Load_Help_Loop :
      loop
         Get_Ada_Help
           (I => Index, H_Index => Help_Index2, H_Title => Help_Title2,
            H_Text => Help_Text);
         exit Load_Help_Loop when Strlen(Item => Help_Index2) = 0;
         Tmp_Help :=
           (Index => To_Unbounded_String(Source => Value(Item => Help_Index2)),
            Text => To_Unbounded_String(Source => Value(Item => Help_Text)));
         Help_Container.Include
           (Container => Help_List,
            Key => To_Unbounded_String(Source => Value(Item => Help_Title2)),
            New_Item => Tmp_Help);
         Index := Index + 1;
      end loop Load_Help_Loop;
      -- Add help page about available careers and factions
      Tmp_Help.Index := To_Unbounded_String(Source => "factions2");
      Help_Title :=
        To_Unbounded_String
          (Source =>
             Trim
               (Source => Positive'Image(Positive(Help_List.Length) + 1),
                Side => Left) &
             ". Factions and careers");
      Tmp_Help.Text :=
        To_Unbounded_String
          (Source =>
             "Here you will find information about all available factions and careers in the game" &
             LF & LF & "{u}Factions{/u}" & LF & LF);
      Load_Factions_Info_Loop :
      for I in 1 .. Get_Factions_Amount loop
         Load_Faction_Block :
         declare
            Faction: constant Faction_Record := Get_Faction(Number => I);
         begin
            if Faction.Careers.Length > 0 then
               Append
                 (Source => Tmp_Help.Text,
                  New_Item =>
                    "{b}" & To_String(Source => Faction.Name) & "{/b}" & LF &
                    "    " & To_String(Source => Faction.Description) & LF &
                    "    {i}Relations{/b}" & LF);
               Show_Relations_Loop :
               for J in Faction.Relations.Iterate loop
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item =>
                       "        " &
                       To_String
                         (Source =>
                            Get_Faction
                              (Index => Relations_Container.Key(Position => J))
                              .Name) &
                       ": " &
                       (if Faction.Relations(J).Friendly then "Friendly"
                        else "Enemies") &
                       LF);
               end loop Show_Relations_Loop;
               Append(Source => Tmp_Help.Text, New_Item => LF);
            end if;
         end Load_Faction_Block;
      end loop Load_Factions_Info_Loop;
      Append
        (Source => Tmp_Help.Text, New_Item => LF & "{u}Careers{/u}" & LF & LF);
      Show_Careers_Block :
      declare
         use Careers;

         Faction: constant Faction_Record :=
           Get_Faction(Index => To_Bounded_String(Source => "POLEIS"));
      begin
         Load_Careers_Info_Loop :
         for I in Careers_List.Iterate loop
            Append
              (Source => Tmp_Help.Text,
               New_Item =>
                 "{b}" & To_String(Source => Careers_List(I).Name) & "{/b}" &
                 LF &
                 Faction.Careers(Careers.Careers_Container.Key(Position => I))
                   .Description &
                 LF);
            if Careers_List(I).Skills.Length > 0 then
               Append
                 (Source => Tmp_Help.Text,
                  New_Item => "    {i}Bonus to skills{/b}" & LF);
               Show_Skills_Loop :
               for Skill of Careers_List(I).Skills loop
                  Append
                    (Source => Tmp_Help.Text,
                     New_Item => "        " & To_String(Source => Skill) & LF);
               end loop Show_Skills_Loop;
            end if;
            Append(Source => Tmp_Help.Text, New_Item => LF);
         end loop Load_Careers_Info_Loop;
      end Show_Careers_Block;
      Help_List.Include(Key => Help_Title, New_Item => Tmp_Help);
      Log_Message
        (Message => "Help added: " & To_String(Source => Help_Title),
         Message_Type => EVERYTHING);
      -- Add help page about available careers and factions
      Tmp_Help.Index := To_Unbounded_String(Source => "basestypes");
      Help_Title :=
        To_Unbounded_String
          (Source =>
             Trim
               (Source => Positive'Image(Positive(Help_List.Length) + 1),
                Side => Left) &
             ". Bases Types");
      Tmp_Help.Text :=
        To_Unbounded_String
          (Source =>
             "Here you will find information about all available bases types in the game" &
             LF & LF);
      Load_Bases_Types_Info_Loop :
      for Base_Index of Bases_Types loop
         exit Load_Bases_Types_Info_Loop when Get_Base_Type_Name
             (Base_Type => Base_Index)'
             Length =
           0;
         Append
           (Source => Tmp_Help.Text,
            New_Item =>
              "{b}" & Get_Base_Type_Name(Base_Type => Base_Index) & "{/b}" &
              LF & "    " &
              Get_Base_Type_Description(Base_Type => Base_Index) & LF & LF);
      end loop Load_Bases_Types_Info_Loop;
      Help_List.Include(Key => Help_Title, New_Item => Tmp_Help);
      Log_Message
        (Message => "Help added: " & To_String(Source => Help_Title),
         Message_Type => EVERYTHING);
   end Load_Help;

end Help;
