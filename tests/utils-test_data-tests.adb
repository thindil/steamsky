--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Utils.Test_Data.

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
package body Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Get_Random_254206_4c55ca
     (Min, Max: Integer) return Integer is
   begin
      begin
         pragma Assert(Min <= Max);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(utils.ads:0):Test_GetRandom test requirement violated");
      end;
      declare
         Test_Get_Random_254206_4c55ca_Result: constant Integer :=
           GNATtest_Generated.GNATtest_Standard.Utils.Get_Random(Min, Max);
      begin
         begin
            pragma Assert(Test_Get_Random_254206_4c55ca_Result in Min .. Max);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(utils.ads:0:):Test_GetRandom test commitment violated");
         end;
         return Test_Get_Random_254206_4c55ca_Result;
      end;
   end Wrap_Test_Get_Random_254206_4c55ca;
--  end read only

--  begin read only
   procedure Test_Get_Random_test_getrandom(Gnattest_T: in out Test);
   procedure Test_Get_Random_254206_4c55ca(Gnattest_T: in out Test) renames
     Test_Get_Random_test_getrandom;
--  id:2.2/2542065c792cecb1/Get_Random/1/0/test_getrandom/
   procedure Test_Get_Random_test_getrandom(Gnattest_T: in out Test) is
      function Get_Random(Min, Max: Integer) return Integer renames
        Wrap_Test_Get_Random_254206_4c55ca;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      for I in 1 .. 5 loop
         Assert(Get_Random(1, 5) in 1 .. 5, "Wrong random number returned.");
      end loop;
      Assert(Get_Random(5, 5) = 5, "Wrong random number from 5 returned.");

--  begin read only
   end Test_Get_Random_test_getrandom;
--  end read only

--  begin read only
   function Wrap_Test_Days_Difference_3eb9cd_fd50f2
     (Date_To_Compare: Date_Record) return Integer is
   begin
      declare
         Test_Days_Difference_3eb9cd_fd50f2_Result: constant Integer :=
           GNATtest_Generated.GNATtest_Standard.Utils.Days_Difference
             (Date_To_Compare);
      begin
         return Test_Days_Difference_3eb9cd_fd50f2_Result;
      end;
   end Wrap_Test_Days_Difference_3eb9cd_fd50f2;
--  end read only

--  begin read only
   procedure Test_Days_Difference_test_daysdifference(Gnattest_T: in out Test);
   procedure Test_Days_Difference_3eb9cd_fd50f2
     (Gnattest_T: in out Test) renames
     Test_Days_Difference_test_daysdifference;
--  id:2.2/3eb9cd623ef6a20f/Days_Difference/1/0/test_daysdifference/
   procedure Test_Days_Difference_test_daysdifference
     (Gnattest_T: in out Test) is
      function Days_Difference
        (Date_To_Compare: Date_Record) return Integer renames
        Wrap_Test_Days_Difference_3eb9cd_fd50f2;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Game_Date := (1_600, 1, 2, 0, 0);
      Assert
        (Days_Difference((1_600, 1, 1, 0, 0)) = 1,
         "Invalid count of days difference between game dates.");

--  begin read only
   end Test_Days_Difference_test_daysdifference;
--  end read only

--  begin read only
   function Wrap_Test_Generate_Robotic_Name_eb65d6_cad966
      return Unbounded_String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(utils.ads:0):Test_GenerateRoboticName test requirement violated");
      end;
      declare
         Test_Generate_Robotic_Name_eb65d6_cad966_Result: constant Unbounded_String :=
           GNATtest_Generated.GNATtest_Standard.Utils.Generate_Robotic_Name;
      begin
         begin
            pragma Assert
              (Length
                 (Source => Test_Generate_Robotic_Name_eb65d6_cad966_Result) >
               0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(utils.ads:0:):Test_GenerateRoboticName test commitment violated");
         end;
         return Test_Generate_Robotic_Name_eb65d6_cad966_Result;
      end;
   end Wrap_Test_Generate_Robotic_Name_eb65d6_cad966;
--  end read only

--  begin read only
   procedure Test_Generate_Robotic_Name_test_generateroboticname
     (Gnattest_T: in out Test);
   procedure Test_Generate_Robotic_Name_eb65d6_cad966
     (Gnattest_T: in out Test) renames
     Test_Generate_Robotic_Name_test_generateroboticname;
--  id:2.2/eb65d6968733e831/Generate_Robotic_Name/1/0/test_generateroboticname/
   procedure Test_Generate_Robotic_Name_test_generateroboticname
     (Gnattest_T: in out Test) is
      function Generate_Robotic_Name return Unbounded_String renames
        Wrap_Test_Generate_Robotic_Name_eb65d6_cad966;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Length(Generate_Robotic_Name) > 0, "Failed to generate robotic name.");

--  begin read only
   end Test_Generate_Robotic_Name_test_generateroboticname;
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
end Utils.Test_Data.Tests;
