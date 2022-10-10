--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Messages.Test_Data.

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
package body Messages.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Get_Message_6b9b1e_0b2a8d
     (Message_Index: Integer; M_Type: Message_Type := DEFAULT)
      return Message_Data is
   begin
      declare
         Test_Get_Message_6b9b1e_0b2a8d_Result: constant Message_Data :=
           GNATtest_Generated.GNATtest_Standard.Messages.Get_Message
             (Message_Index, M_Type);
      begin
         return Test_Get_Message_6b9b1e_0b2a8d_Result;
      end;
   end Wrap_Test_Get_Message_6b9b1e_0b2a8d;
--  end read only

--  begin read only
   procedure Test_Get_Message_test_getmessage(Gnattest_T: in out Test);
   procedure Test_Get_Message_6b9b1e_0b2a8d(Gnattest_T: in out Test) renames
     Test_Get_Message_test_getmessage;
--  id:2.2/6b9b1ecd4648f27e/Get_Message/1/0/test_getmessage/
   procedure Test_Get_Message_test_getmessage(Gnattest_T: in out Test) is
      function Get_Message
        (Message_Index: Integer; M_Type: Message_Type := DEFAULT)
         return Message_Data renames
        Wrap_Test_Get_Message_6b9b1e_0b2a8d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Get_Message(1).Message /= Null_Unbounded_String,
         "Failed to get message.");
      Assert
        (Get_Message(1_000).Message = Null_Unbounded_String,
         "Failed to not get non-existing message.");

--  begin read only
   end Test_Get_Message_test_getmessage;
--  end read only

--  begin read only
   procedure Wrap_Test_Clear_Messages_c573e1_267040 is
   begin
      GNATtest_Generated.GNATtest_Standard.Messages.Clear_Messages;
   end Wrap_Test_Clear_Messages_c573e1_267040;
--  end read only

--  begin read only
   procedure Test_Clear_Messages_test_clearmessages(Gnattest_T: in out Test);
   procedure Test_Clear_Messages_c573e1_267040(Gnattest_T: in out Test) renames
     Test_Clear_Messages_test_clearmessages;
--  id:2.2/c573e1b25d985259/Clear_Messages/1/0/test_clearmessages/
   procedure Test_Clear_Messages_test_clearmessages(Gnattest_T: in out Test) is
      procedure Clear_Messages renames Wrap_Test_Clear_Messages_c573e1_267040;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Clear_Messages;
      Assert(Messages_Amount = 0, "Failed to clear all messages.");
      Add_Message("Test message.", DEFAULT);

--  begin read only
   end Test_Clear_Messages_test_clearmessages;
--  end read only

--  begin read only
   function Wrap_Test_Messages_Amount_a330a8_8e4cbf
     (M_Type: Message_Type := DEFAULT) return Natural is
   begin
      declare
         Test_Messages_Amount_a330a8_8e4cbf_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Messages.Messages_Amount
             (M_Type);
      begin
         return Test_Messages_Amount_a330a8_8e4cbf_Result;
      end;
   end Wrap_Test_Messages_Amount_a330a8_8e4cbf;
--  end read only

--  begin read only
   procedure Test_Messages_Amount_test_messagesamount(Gnattest_T: in out Test);
   procedure Test_Messages_Amount_a330a8_8e4cbf
     (Gnattest_T: in out Test) renames
     Test_Messages_Amount_test_messagesamount;
--  id:2.2/a330a8b491b0f340/Messages_Amount/1/0/test_messagesamount/
   procedure Test_Messages_Amount_test_messagesamount
     (Gnattest_T: in out Test) is
      function Messages_Amount
        (M_Type: Message_Type := DEFAULT) return Natural renames
        Wrap_Test_Messages_Amount_a330a8_8e4cbf;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (Messages_Amount = Natural(Messages_List.Length),
         "Failed to count properly amount of messages.");

--  begin read only
   end Test_Messages_Amount_test_messagesamount;
--  end read only

--  begin read only
   procedure Wrap_Test_Restore_Message_2b3d02_0f207a
     (Message: Unbounded_String; M_Type: Message_Type := DEFAULT;
      Color: Message_Color := WHITE) is
   begin
      begin
         pragma Assert(Message /= Null_Unbounded_String);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(messages.ads:0):Test_RestoreMessage test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Messages.Restore_Message
        (Message, M_Type, Color);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(messages.ads:0:):Test_RestoreMessage test commitment violated");
      end;
   end Wrap_Test_Restore_Message_2b3d02_0f207a;
--  end read only

--  begin read only
   procedure Test_Restore_Message_test_restoremessage(Gnattest_T: in out Test);
   procedure Test_Restore_Message_2b3d02_0f207a
     (Gnattest_T: in out Test) renames
     Test_Restore_Message_test_restoremessage;
--  id:2.2/2b3d023fedadb3fa/Restore_Message/1/0/test_restoremessage/
   procedure Test_Restore_Message_test_restoremessage
     (Gnattest_T: in out Test) is
      procedure Restore_Message
        (Message: Unbounded_String; M_Type: Message_Type := DEFAULT;
         Color: Message_Color := WHITE) renames
        Wrap_Test_Restore_Message_2b3d02_0f207a;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: constant Natural := Messages_Amount;

   begin

      Restore_Message(To_Unbounded_String("Test message"));
      Assert
        (Messages_Amount = Amount + 1, "Failed to restore the game message.");

--  begin read only
   end Test_Restore_Message_test_restoremessage;
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
end Messages.Test_Data.Tests;
