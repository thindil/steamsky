--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ships.Crew.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Ships.Crew.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Get_Skill_Level_413ae3_359a68
     (Member: Member_Data; Skill_Index: Skills_Amount_Range)
      return Skill_Range is
   begin
      begin
         pragma Assert(Skill_Index in 1 .. Skills_Amount);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_GetSkillLevel test requirement violated");
      end;
      declare
         Test_Get_Skill_Level_413ae3_359a68_Result: constant Skill_Range :=
           GNATtest_Generated.GNATtest_Standard.Ships.Crew.Get_Skill_Level
             (Member, Skill_Index);
      begin
         begin
            pragma Assert(True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-crew.ads:0:):Test_GetSkillLevel test commitment violated");
         end;
         return Test_Get_Skill_Level_413ae3_359a68_Result;
      end;
   end Wrap_Test_Get_Skill_Level_413ae3_359a68;
--  end read only

--  begin read only
   procedure Test_Get_Skill_Level_test_getskilllevel(Gnattest_T: in out Test);
   procedure Test_Get_Skill_Level_413ae3_359a68
     (Gnattest_T: in out Test) renames
     Test_Get_Skill_Level_test_getskilllevel;
--  id:2.2/413ae334836fc9be/Get_Skill_Level/1/0/test_getskilllevel/
   procedure Test_Get_Skill_Level_test_getskilllevel
     (Gnattest_T: in out Test) is
      function Get_Skill_Level
        (Member: Member_Data; Skill_Index: Skills_Amount_Range)
         return Skill_Range renames
        Wrap_Test_Get_Skill_Level_413ae3_359a68;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Skill_Level(Player_Ship.Crew(1), 1) = 0,
         "Failed to get real level of not owned skill.");
      Assert
        (Get_Skill_Level(Player_Ship.Crew(1), 4) = 9,
         "Failed to get real level of skill.");

--  begin read only
   end Test_Get_Skill_Level_test_getskilllevel;
--  end read only

--  begin read only
   procedure Wrap_Test_Death_af2fea_e5df10
     (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
      Ship: in out Ship_Record; Create_Body: Boolean := True) is
   begin
      begin
         pragma Assert
           ((Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
             Reason /= Null_Unbounded_String));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_Death test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Death
        (Member_Index, Reason, Ship, Create_Body);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_Death test commitment violated");
      end;
   end Wrap_Test_Death_af2fea_e5df10;
--  end read only

--  begin read only
   procedure Test_Death_test_death(Gnattest_T: in out Test);
   procedure Test_Death_af2fea_e5df10(Gnattest_T: in out Test) renames
     Test_Death_test_death;
--  id:2.2/af2fea911992db88/Death/1/0/test_death/
   procedure Test_Death_test_death(Gnattest_T: in out Test) is
      procedure Death
        (Member_Index: Crew_Container.Extended_Index; Reason: Unbounded_String;
         Ship: in out Ship_Record; Create_Body: Boolean := True) renames
        Wrap_Test_Death_af2fea_e5df10;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Crew: constant Crew_Container.Vector := Player_Ship.Crew;
      Amount: constant Positive :=
        Positive(Inventory_Container.Length(Container => Player_Ship.Cargo));

   begin

      Death(2, To_Unbounded_String("Test death"), Player_Ship);
      Assert
        (Player_Ship.Crew.Length + 1 = Crew.Length,
         "Failed to remove crew member on death.");
      Assert
        (Amount + 1 =
         Positive(Inventory_Container.Length(Container => Player_Ship.Cargo)),
         "Failed to add body of dead crew member.");
      Player_Ship.Crew := Crew;

--  begin read only
   end Test_Death_test_death;
--  end read only

--  begin read only
   procedure Wrap_Test_Delete_Member_f48a2f_e91d14
     (Member_Index: Crew_Container.Extended_Index; Ship: in out Ship_Record) is
   begin
      begin
         pragma Assert
           (Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_DeleteMember test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Delete_Member
        (Member_Index, Ship);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_DeleteMember test commitment violated");
      end;
   end Wrap_Test_Delete_Member_f48a2f_e91d14;
--  end read only

--  begin read only
   procedure Test_Delete_Member_test_deletemember(Gnattest_T: in out Test);
   procedure Test_Delete_Member_f48a2f_e91d14(Gnattest_T: in out Test) renames
     Test_Delete_Member_test_deletemember;
--  id:2.2/f48a2f4f9504972a/Delete_Member/1/0/test_deletemember/
   procedure Test_Delete_Member_test_deletemember(Gnattest_T: in out Test) is
      procedure Delete_Member
        (Member_Index: Crew_Container.Extended_Index;
         Ship: in out Ship_Record) renames
        Wrap_Test_Delete_Member_f48a2f_e91d14;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Crew: constant Crew_Container.Vector := Player_Ship.Crew;

   begin

      Delete_Member(2, Player_Ship);
      Assert
        (Crew.Length = Player_Ship.Crew.Length + 1,
         "Failed to delete member from the player ship crew.");
      Player_Ship.Crew := Crew;

--  begin read only
   end Test_Delete_Member_test_deletemember;
--  end read only

--  begin read only
   function Wrap_Test_Find_Member_4eb5a6_a89aa1
     (Order: Crew_Orders; Crew: Crew_Container.Vector := Player_Ship.Crew)
      return Crew_Container.Extended_Index is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_FindMember test requirement violated");
      end;
      declare
         Test_Find_Member_4eb5a6_a89aa1_Result: constant Crew_Container
           .Extended_Index :=
           GNATtest_Generated.GNATtest_Standard.Ships.Crew.Find_Member
             (Order, Crew);
      begin
         begin
            pragma Assert
              (Test_Find_Member_4eb5a6_a89aa1_Result <= Crew.Last_Index);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(ships-crew.ads:0:):Test_FindMember test commitment violated");
         end;
         return Test_Find_Member_4eb5a6_a89aa1_Result;
      end;
   end Wrap_Test_Find_Member_4eb5a6_a89aa1;
--  end read only

--  begin read only
   procedure Test_Find_Member_test_findmember(Gnattest_T: in out Test);
   procedure Test_Find_Member_4eb5a6_a89aa1(Gnattest_T: in out Test) renames
     Test_Find_Member_test_findmember;
--  id:2.2/4eb5a63fb81e1abd/Find_Member/1/0/test_findmember/
   procedure Test_Find_Member_test_findmember(Gnattest_T: in out Test) is
      function Find_Member
        (Order: Crew_Orders; Crew: Crew_Container.Vector := Player_Ship.Crew)
         return Crew_Container.Extended_Index renames
        Wrap_Test_Find_Member_4eb5a6_a89aa1;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Find_Member(Talk) = 1,
         "Failed to find crew member with selected order.");
      Assert
        (Find_Member(Defend) = 0,
         "Failed to not find crew member with selected order.");

--  begin read only
   end Test_Find_Member_test_findmember;
--  end read only

--  begin read only
   procedure Wrap_Test_Give_Orders_2026b6_23bded
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Given_Order: Crew_Orders;
      Module_Index: Modules_Container.Extended_Index := 0;
      Check_Priorities: Boolean := True) is
   begin
      begin
         pragma Assert
           ((Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index and
             Module_Index <= Ship.Modules.Last_Index));
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_GiveOrders test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Give_Orders
        (Ship, Member_Index, Given_Order, Module_Index, Check_Priorities);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_GiveOrders test commitment violated");
      end;
   end Wrap_Test_Give_Orders_2026b6_23bded;
--  end read only

--  begin read only
   procedure Test_Give_Orders_test_giveorders(Gnattest_T: in out Test);
   procedure Test_Give_Orders_2026b6_23bded(Gnattest_T: in out Test) renames
     Test_Give_Orders_test_giveorders;
--  id:2.2/2026b6880e668a94/Give_Orders/1/0/test_giveorders/
   procedure Test_Give_Orders_test_giveorders(Gnattest_T: in out Test) is
      procedure Give_Orders
        (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
         Given_Order: Crew_Orders;
         Module_Index: Modules_Container.Extended_Index := 0;
         Check_Priorities: Boolean := True) renames
        Wrap_Test_Give_Orders_2026b6_23bded;
--  end read only

      pragma Unreferenced(Gnattest_T);
      use Tiny_String;

      EnemyShip: Ship_Record :=
        Create_Ship(2, Null_Bounded_String, 10, 10, FULL_SPEED);

   begin

      Give_Orders(Player_Ship, 1, REST);
      Assert
        (Player_Ship.Crew(1).Order = Talk, "Failed to give order to player.");
      Give_Orders(Player_Ship, 4, REST);
      Assert
        (Player_Ship.Crew(4).Order = Rest, "Failed to give order to gunner.");
      EnemyShip.Crew(1).Morale(1) := 5;
      Give_Orders(EnemyShip, 1, TALK);
      Assert(True, "This test can only crash");

--  begin read only
   end Test_Give_Orders_test_giveorders;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Orders_9d8659_cad1b0
     (Ship: in out Ship_Record; Combat: Boolean := False) is
   begin
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Update_Orders
        (Ship, Combat);
   end Wrap_Test_Update_Orders_9d8659_cad1b0;
--  end read only

--  begin read only
   procedure Test_Update_Orders_test_updateorders(Gnattest_T: in out Test);
   procedure Test_Update_Orders_9d8659_cad1b0(Gnattest_T: in out Test) renames
     Test_Update_Orders_test_updateorders;
--  id:2.2/9d865998be69128c/Update_Orders/1/0/test_updateorders/
   procedure Test_Update_Orders_test_updateorders(Gnattest_T: in out Test) is
      procedure Update_Orders
        (Ship: in out Ship_Record; Combat: Boolean := False) renames
        Wrap_Test_Update_Orders_9d8659_cad1b0;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Give_Orders(Player_Ship, 1, REST, 0, False);
      Update_Orders(Player_Ship);
      Assert
        (Player_Ship.Crew(1).Order = Talk,
         "Failed to update orders for player ship crew.");

--  begin read only
   end Test_Update_Orders_test_updateorders;
--  end read only

--  begin read only
   procedure Wrap_Test_Update_Morale_4b2bdd_63517c
     (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
      Value: Integer) is
   begin
      begin
         pragma Assert
           (Member_Index in Ship.Crew.First_Index .. Ship.Crew.Last_Index);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(ships-crew.ads:0):Test_UpdateMorale test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Ships.Crew.Update_Morale
        (Ship, Member_Index, Value);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(ships-crew.ads:0:):Test_UpdateMorale test commitment violated");
      end;
   end Wrap_Test_Update_Morale_4b2bdd_63517c;
--  end read only

--  begin read only
   procedure Test_Update_Morale_test_updatemorale(Gnattest_T: in out Test);
   procedure Test_Update_Morale_4b2bdd_63517c(Gnattest_T: in out Test) renames
     Test_Update_Morale_test_updatemorale;
--  id:2.2/4b2bddb9d7147aed/Update_Morale/1/0/test_updatemorale/
   procedure Test_Update_Morale_test_updatemorale(Gnattest_T: in out Test) is
      procedure Update_Morale
        (Ship: in out Ship_Record; Member_Index: Crew_Container.Extended_Index;
         Value: Integer) renames
        Wrap_Test_Update_Morale_4b2bdd_63517c;
--  end read only

      pragma Unreferenced(Gnattest_T);
      OldMorale: constant Natural := Player_Ship.Crew(1).Morale(2);
      OldLevel: constant Natural := Player_Ship.Crew(1).Morale(1);

   begin

      Update_Morale(Player_Ship, 1, 1);
      Assert
        (Player_Ship.Crew(1).Morale(2) - 1 = OldMorale or
         Player_Ship.Crew(1).Morale(1) - 1 = OldLevel,
         "Failed to raise player morale.");
      Update_Morale(Player_Ship, 1, -1);
      Assert
        (Player_Ship.Crew(1).Morale(2) = OldMorale,
         "Failed to lower player morale.");

--  begin read only
   end Test_Update_Morale_test_updatemorale;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Ships.Crew.Test_Data.Tests;
