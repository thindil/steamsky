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
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Log; use Log;
with Factions; use Factions;

package body Careers is

   procedure LoadCareers(Reader: Tree_Reader) is
      TempRecord: CareerRecord;
      NodesList, ChildNodes: Node_List;
      CareersData: Document;
      SkillName, CareerIndex: Unbounded_String;
      TmpSkills: UnboundedString_Container.Vector;
      DeleteIndex: Positive;
      CareerNode: Node;
      Action, SkillAction: DataAction;
   begin
      CareersData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(CareersData, "career");
      Load_Careers_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord := (Name => Null_Unbounded_String, Skills => TmpSkills);
         CareerNode := Item(NodesList, I);
         CareerIndex :=
           To_Unbounded_String(Get_Attribute(CareerNode, "index"));
         Action :=
           (if Get_Attribute(CareerNode, "action")'Length > 0 then
              DataAction'Value(Get_Attribute(CareerNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Careers_Container.Contains(Careers_List, CareerIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " career '" & To_String(CareerIndex) &
                 "', there is no career with that index.";
            end if;
         elsif Careers_Container.Contains(Careers_List, CareerIndex) then
            raise Data_Loading_Error
              with "Can't add career '" & To_String(CareerIndex) &
              "', there is already a career with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Careers_List(CareerIndex);
            end if;
            if Get_Attribute(CareerNode, "name") /= "" then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(CareerNode, "name"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(CareerNode, "skill");
            Read_Skills_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               SkillName :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "name"));
               SkillAction :=
                 (if Get_Attribute(Item(ChildNodes, J), "action")'Length > 0
                  then
                    DataAction'Value
                      (Get_Attribute(Item(ChildNodes, J), "action"))
                  else ADD);
               if FindSkillIndex(SkillName) = 0 then
                  raise Data_Loading_Error
                    with "Can't " & To_Lower(DataAction'Image(Action)) &
                    "career '" & To_String(CareerIndex) & "', skill '" &
                    To_String(SkillName) & "' not exists";
               end if;
               if SkillAction /= REMOVE then
                  TempRecord.Skills.Append(New_Item => SkillName);
               else
                  DeleteIndex := TempRecord.Skills.First_Index;
                  Remove_Skills_Loop :
                  while DeleteIndex <= TempRecord.Skills.Last_Index loop
                     if TempRecord.Skills(DeleteIndex) = SkillName then
                        TempRecord.Skills.Delete(Index => DeleteIndex);
                        exit Remove_Skills_Loop;
                     end if;
                     DeleteIndex := DeleteIndex + 1;
                  end loop Remove_Skills_Loop;
               end if;
            end loop Read_Skills_Loop;
            if Action /= UPDATE then
               Careers_Container.Include
                 (Careers_List, CareerIndex, TempRecord);
               LogMessage
                 ("Career added: " & To_String(TempRecord.Name), Everything);
            else
               Careers_List(CareerIndex) := TempRecord;
               LogMessage
                 ("Career updated: " & To_String(TempRecord.Name), Everything);
            end if;
         else
            Careers_Container.Exclude(Careers_List, CareerIndex);
            Remove_Careers_Loop :
            for Faction of Factions_List loop
               Factions.Careers_Container.Exclude
                 (Faction.Careers, CareerIndex);
            end loop Remove_Careers_Loop;
            LogMessage
              ("Career removed: " & To_String(CareerIndex), Everything);
         end if;
      end loop Load_Careers_Loop;
   end LoadCareers;

end Careers;
