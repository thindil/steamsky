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
   function Wrap_Test_FormatedTime_5bb1ad_97dea1
     (Time: Date_Record := Game_Date) return String is
   begin
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(messages.ads:0):Test_FormattedTime test requirement violated");
      end;
      declare
         Test_FormatedTime_5bb1ad_97dea1_Result: constant String :=
           GNATtest_Generated.GNATtest_Standard.Messages.FormatedTime(Time);
      begin
         begin
            pragma Assert(Test_FormatedTime_5bb1ad_97dea1_Result'Length > 0);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(messages.ads:0:):Test_FormattedTime test commitment violated");
         end;
         return Test_FormatedTime_5bb1ad_97dea1_Result;
      end;
   end Wrap_Test_FormatedTime_5bb1ad_97dea1;
--  end read only

--  begin read only
   procedure Test_FormatedTime_test_formattedtime(Gnattest_T: in out Test);
   procedure Test_FormatedTime_5bb1ad_97dea1(Gnattest_T: in out Test) renames
     Test_FormatedTime_test_formattedtime;
--  id:2.2/5bb1ad5dbd52690f/FormatedTime/1/0/test_formattedtime/
   procedure Test_FormatedTime_test_formattedtime(Gnattest_T: in out Test) is
      function FormatedTime
        (Time: Date_Record := Game_Date) return String renames
        Wrap_Test_FormatedTime_5bb1ad_97dea1;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert(FormatedTime /= "", "Failed to get formated game time.");

--  begin read only
   end Test_FormatedTime_test_formattedtime;
--  end read only

--  begin read only
   procedure Wrap_Test_AddMessage_508d2e_c15a00
     (Message: String; MType: Message_Type; Color: Message_Color := WHITE) is
   begin
      begin
         pragma Assert(Message'Length > 0);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "req_sloc(messages.ads:0):Test_AddMessage test requirement violated");
      end;
      GNATtest_Generated.GNATtest_Standard.Messages.AddMessage
        (Message, MType, Color);
      begin
         pragma Assert(True);
         null;
      exception
         when System.Assertions.Assert_Failure =>
            AUnit.Assertions.Assert
              (False,
               "ens_sloc(messages.ads:0:):Test_AddMessage test commitment violated");
      end;
   end Wrap_Test_AddMessage_508d2e_c15a00;
--  end read only

--  begin read only
   procedure Test_AddMessage_test_addmessage(Gnattest_T: in out Test);
   procedure Test_AddMessage_508d2e_c15a00(Gnattest_T: in out Test) renames
     Test_AddMessage_test_addmessage;
--  id:2.2/508d2ebb71c5d14a/AddMessage/1/0/test_addmessage/
   procedure Test_AddMessage_test_addmessage(Gnattest_T: in out Test) is
      procedure AddMessage
        (Message: String; MType: Message_Type;
         Color: Message_Color := WHITE) renames
        Wrap_Test_AddMessage_508d2e_c15a00;
--  end read only

      pragma Unreferenced(Gnattest_T);
      Amount: constant Natural := MessagesAmount;

   begin

      AddMessage("Test message.", Default);
      Assert(Amount < MessagesAmount, "Failed to add new message.");

--  begin read only
   end Test_AddMessage_test_addmessage;
--  end read only

--  begin read only
   function Wrap_Test_GetMessage_56cd5a_0b2a8d
     (MessageIndex: Integer; MType: Message_Type := Default)
      return Message_Data is
   begin
      declare
         Test_GetMessage_56cd5a_0b2a8d_Result: constant Message_Data :=
           GNATtest_Generated.GNATtest_Standard.Messages.GetMessage
             (MessageIndex, MType);
      begin
         return Test_GetMessage_56cd5a_0b2a8d_Result;
      end;
   end Wrap_Test_GetMessage_56cd5a_0b2a8d;
--  end read only

--  begin read only
   procedure Test_GetMessage_test_getmessage(Gnattest_T: in out Test);
   procedure Test_GetMessage_56cd5a_0b2a8d(Gnattest_T: in out Test) renames
     Test_GetMessage_test_getmessage;
--  id:2.2/56cd5ac704cead2b/GetMessage/1/0/test_getmessage/
   procedure Test_GetMessage_test_getmessage(Gnattest_T: in out Test) is
      function GetMessage
        (MessageIndex: Integer; MType: Message_Type := Default)
         return Message_Data renames
        Wrap_Test_GetMessage_56cd5a_0b2a8d;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetMessage(1).Message /= Null_Unbounded_String,
         "Failed to get message.");
      Assert
        (GetMessage(1_000).Message = Null_Unbounded_String,
         "Failed to not get non-existing message.");

--  begin read only
   end Test_GetMessage_test_getmessage;
--  end read only

--  begin read only
   procedure Wrap_Test_ClearMessages_aeb026_267040 is
   begin
      GNATtest_Generated.GNATtest_Standard.Messages.ClearMessages;
   end Wrap_Test_ClearMessages_aeb026_267040;
--  end read only

--  begin read only
   procedure Test_ClearMessages_test_clearmessages(Gnattest_T: in out Test);
   procedure Test_ClearMessages_aeb026_267040(Gnattest_T: in out Test) renames
     Test_ClearMessages_test_clearmessages;
--  id:2.2/aeb0266c09a96d71/ClearMessages/1/0/test_clearmessages/
   procedure Test_ClearMessages_test_clearmessages(Gnattest_T: in out Test) is
      procedure ClearMessages renames Wrap_Test_ClearMessages_aeb026_267040;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      ClearMessages;
      Assert(MessagesAmount = 0, "Failed to clear all messages.");
      AddMessage("Test message.", Default);

--  begin read only
   end Test_ClearMessages_test_clearmessages;
--  end read only

--  begin read only
   function Wrap_Test_MessagesAmount_922f17_8e4cbf
     (MType: Message_Type := Default) return Natural is
   begin
      declare
         Test_MessagesAmount_922f17_8e4cbf_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Messages.MessagesAmount(MType);
      begin
         return Test_MessagesAmount_922f17_8e4cbf_Result;
      end;
   end Wrap_Test_MessagesAmount_922f17_8e4cbf;
--  end read only

--  begin read only
   procedure Test_MessagesAmount_test_messagesamount(Gnattest_T: in out Test);
   procedure Test_MessagesAmount_922f17_8e4cbf(Gnattest_T: in out Test) renames
     Test_MessagesAmount_test_messagesamount;
--  id:2.2/922f1712ec778778/MessagesAmount/1/0/test_messagesamount/
   procedure Test_MessagesAmount_test_messagesamount
     (Gnattest_T: in out Test) is
      function MessagesAmount
        (MType: Message_Type := Default) return Natural renames
        Wrap_Test_MessagesAmount_922f17_8e4cbf;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (MessagesAmount = Natural(Messages_List.Length),
         "Failed to count properly amount of messages.");

--  begin read only
   end Test_MessagesAmount_test_messagesamount;
--  end read only

--  begin read only
   function Wrap_Test_GetLastMessageIndex_ee1f16_517343 return Natural is
   begin
      declare
         Test_GetLastMessageIndex_ee1f16_517343_Result: constant Natural :=
           GNATtest_Generated.GNATtest_Standard.Messages.GetLastMessageIndex;
      begin
         return Test_GetLastMessageIndex_ee1f16_517343_Result;
      end;
   end Wrap_Test_GetLastMessageIndex_ee1f16_517343;
--  end read only

--  begin read only
   procedure Test_GetLastMessageIndex_test_getlastmessageindex
     (Gnattest_T: in out Test);
   procedure Test_GetLastMessageIndex_ee1f16_517343
     (Gnattest_T: in out Test) renames
     Test_GetLastMessageIndex_test_getlastmessageindex;
--  id:2.2/ee1f163ccc085b43/GetLastMessageIndex/1/0/test_getlastmessageindex/
   procedure Test_GetLastMessageIndex_test_getlastmessageindex
     (Gnattest_T: in out Test) is
      function GetLastMessageIndex return Natural renames
        Wrap_Test_GetLastMessageIndex_ee1f16_517343;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetLastMessageIndex = Messages_List.Last_Index,
         "Failed to get last message index.");

--  begin read only
   end Test_GetLastMessageIndex_test_getlastmessageindex;
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
