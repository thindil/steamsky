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
      Action, SkillName: Unbounded_String;
      DeleteIndex: Natural := 0;
      TmpSkills: UnboundedString_Container.Vector;
      CareerIndex: Positive;
      CareerNode: Node;
   begin
      TempRecord :=
        (Index => Null_Unbounded_String, Name => Null_Unbounded_String,
         Skills => TmpSkills);
      CareersData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(CareersData, "career");
      for I in 0 .. Length(NodesList) - 1 loop
         CareerNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(CareerNode, "index"));
         Action := To_Unbounded_String(Get_Attribute(CareerNode, "action"));
         if Action = Null_Unbounded_String or
           Action = To_Unbounded_String("add") then
            for Career of Careers_List loop
               if Career.Index = TempRecord.Index then
                  raise Careers_Adding_Error
                    with "Can't add career '" & To_String(TempRecord.Index) &
                    "', there is one with that index.";
               end if;
            end loop;
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(CareerNode, "name"));
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(CareerNode, "skill");
            for J in 0 .. Length(ChildNodes) - 1 loop
               SkillName :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "name"));
               if FindSkillIndex(SkillName) = 0 then
                  raise Careers_Adding_Error
                    with "Can't add career '" & To_String(TempRecord.Index) &
                    "', skill '" & To_String(SkillName) & "' not exists";
               end if;
               TempRecord.Skills.Append(New_Item => SkillName);
            end loop;
            Careers_List.Append(New_Item => TempRecord);
            LogMessage
              ("Career added: " & To_String(TempRecord.Name), Everything);
         else
            DeleteIndex := 0;
            for J in Careers_List.Iterate loop
               if Careers_List(J).Index = TempRecord.Index then
                  DeleteIndex := Careers_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            if DeleteIndex = 0 then
               raise Careers_Remove_Error
                 with "Can't delete career '" & To_String(TempRecord.Index) &
                 "', no career with that index.";
            end if;
            Careers_List.Delete(Index => DeleteIndex);
            for Faction of Factions_List loop
               CareerIndex := Faction.Careers.First_Index;
               while CareerIndex <= Faction.Careers.Last_Index loop
                  if Faction.Careers(CareerIndex).Index = TempRecord.Index then
                     Faction.Careers.Delete(Index => CareerIndex);
                     exit;
                  end if;
                  CareerIndex := CareerIndex + 1;
               end loop;
            end loop;
            LogMessage
              ("Career removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Index => Null_Unbounded_String, Name => Null_Unbounded_String,
            Skills => TmpSkills);
      end loop;
   end LoadCareers;

end Careers;
