--    Copyright 2018-2021 Bartek thindil Jasicki
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
with Interfaces.C.Strings;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Factions; use Factions;

package body Careers is

   procedure Load_Careers(Reader: Tree_Reader; File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;

      Temp_Record: Career_Record;
      Nodes_List, Child_Nodes: Node_List;
      Careers_Data: Document;
      Skill_Name, Career_Index: Unbounded_String;
      Tmp_Skills: UnboundedString_Container.Vector;
      Delete_Index: Positive;
      Career_Node: Node;
      Action, Skill_Action: Data_Action;
      type Nim_Career_Array is array(0 .. 1) of chars_ptr;
      Temp_Nim_Career: Nim_Career_Array;
      Index: Positive := 1;
      procedure Load_Ada_Careers(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaCareers";
      procedure Get_Ada_Career
        (Career_Index: Integer; Ada_Career: out Nim_Career_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCareer";
   begin
      Load_Ada_Careers(Name => New_String(Str => File_Name));
      Load_Careers_Data_Loop :
      loop
         Get_Ada_Career
           (Career_Index => Index, Ada_Career => Temp_Nim_Career);
         exit Load_Careers_Data_Loop when Strlen(Item => Temp_Nim_Career(0)) = 0;
         Career_Index := To_Unbounded_String(Source => Value(Item => Temp_Nim_Career(0)));
         Temp_Record.Name := To_Unbounded_String(Source => Value(Item => Temp_Nim_Career(1)));
         Temp_Record.Skills.Clear;
         Careers_List.Include(Key => Career_Index, New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Careers_Data_Loop;
      Careers_Data := Get_Tree(Read => Reader);
      Nodes_List :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name
          (Doc => Careers_Data, Tag_Name => "career");
      Load_Careers_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Temp_Record := (Name => Null_Unbounded_String, Skills => Tmp_Skills);
         Career_Node := Item(List => Nodes_List, Index => I);
         Career_Index :=
           To_Unbounded_String
             (Source => Get_Attribute(Elem => Career_Node, Name => "index"));
         Action :=
           (if Get_Attribute(Elem => Career_Node, Name => "action")'Length > 0
            then
              Data_Action'Value
                (Get_Attribute(Elem => Career_Node, Name => "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Careers_Container.Contains
                (Container => Careers_List, Key => Career_Index) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Item => Data_Action'Image(Action)) &
                 " career '" & To_String(Source => Career_Index) &
                 "', there is no career with that index.";
            end if;
         elsif Careers_Container.Contains
             (Container => Careers_List, Key => Career_Index) then
            raise Data_Loading_Error
              with "Can't add career '" & To_String(Source => Career_Index) &
              "', there is already a career with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               Temp_Record := Careers_List(Career_Index);
            end if;
            if Get_Attribute(Elem => Career_Node, Name => "name") /= "" then
               Temp_Record.Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Career_Node, Name => "name"));
            end if;
            Child_Nodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Elem => Career_Node, Name => "skill");
            Read_Skills_Loop :
            for J in 0 .. Length(List => Child_Nodes) - 1 loop
               Skill_Name :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute
                        (Elem => Item(List => Child_Nodes, Index => J),
                         Name => "name"));
               Skill_Action :=
                 (if
                    Get_Attribute
                      (Elem => Item(List => Child_Nodes, Index => J),
                       Name => "action")'
                      Length >
                    0
                  then
                    Data_Action'Value
                      (Get_Attribute
                         (Elem => Item(List => Child_Nodes, Index => J),
                          Name => "action"))
                  else ADD);
               if Find_Skill_Index
                   (Skill_Name => To_String(Source => Skill_Name)) =
                 0 then
                  raise Data_Loading_Error
                    with "Can't " &
                    To_Lower(Item => Data_Action'Image(Action)) & "career '" &
                    To_String(Source => Career_Index) & "', skill '" &
                    To_String(Source => Skill_Name) & "' not exists";
               end if;
               if Skill_Action /= REMOVE then
                  Temp_Record.Skills.Append(New_Item => Skill_Name);
               else
                  Delete_Index := Temp_Record.Skills.First_Index;
                  Remove_Skills_Loop :
                  while Delete_Index <= Temp_Record.Skills.Last_Index loop
                     if Temp_Record.Skills(Delete_Index) = Skill_Name then
                        Temp_Record.Skills.Delete(Index => Delete_Index);
                        exit Remove_Skills_Loop;
                     end if;
                     Delete_Index := Delete_Index + 1;
                  end loop Remove_Skills_Loop;
               end if;
            end loop Read_Skills_Loop;
            if Action /= UPDATE then
               Careers_Container.Include
                 (Container => Careers_List, Key => Career_Index,
                  New_Item => Temp_Record);
               Log_Message
                 (Message =>
                    "Career added: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            else
               Careers_List(Career_Index) := Temp_Record;
               Log_Message
                 (Message =>
                    "Career updated: " & To_String(Source => Temp_Record.Name),
                  Message_Type => EVERYTHING);
            end if;
         else
            Careers_Container.Exclude
              (Container => Careers_List, Key => Career_Index);
            Remove_Careers_Loop :
            for Faction of Factions_List loop
               Factions.Careers_Container.Exclude
                 (Container => Faction.Careers, Key => Career_Index);
            end loop Remove_Careers_Loop;
            Log_Message
              (Message =>
                 "Career removed: " & To_String(Source => Career_Index),
               Message_Type => EVERYTHING);
         end if;
      end loop Load_Careers_Loop;
   end Load_Careers;

end Careers;
