' To set up an Xpert/9210 to output the current satlink message over Iridium:
'  1. Use Xterm to copy this file to Xpert/9210 \Flash Disk
'  2. Fully configure system for Satlink transmission as you normally would,
'     but then disable both self-timed and random transmissions
'  3. On Setup tab, navigate to Basic - Custom Formatting - Iridium, and
'     select "STFORMATTER" as custom formatter for Iridium transmitter
 
' Declare external function used to get current satlink self-timed message
' extern "C" _declspec(dllexport) LPCTSTR GetSTCurrentMessage()
Declare Function GetSTCurrentMessage Lib "\Windows\Satlink.sll" As String
 
' This function is called by Iridium when time to format. Simply return the
' current satlink self-timed message.
Public Function SELFTIMED_STFormatter
	   'Clears the alarm from the iATON formatter.
   ClearAlarm
   
   SELFTIMED_STFormatter = GetSTCurrentMessage
   
End Function

'----- ADCP Message Formatter -----------------------------------------------
'
'     This basic code is the self timed formatter for Nortek ADCP'S
'     In this case, we capture RECENT.DAT and store to a working output.dat.
'     The GOES message is stored in SAT.Dat. That way, the user can define a
'     special log in name and password and retrieve the message that way.
'
'     The function SELFTIMED_STFormatter handles the message capture.  Most of this
'     routine is straight from the XPert BASIC manual.
'
'------------------------------------------------------------------------------
'

Public FUNCTION ALARM_iATONFormatter(Group)
   'Note here that Selftime_STFormatter is the current message built by the C code
   F1 = FREEFILE
   strDatFileName = "\Flash Disk\RECENT.DAT"
   F2 = FREEFILE
   OPEN "Output.Dat" FOR OUTPUT AS F2
   DATASTR = ""
   OPEN strDatFileName FOR INPUT AS F1                                                                                         
   result = ReadB(F1, DATASTR, 1000)
   N = WriteB(F2,DATASTR,result) 'TRANSFER RECENT.DAT TO WORKING FILE
   CLOSE F1
   CLOSE F2
   F3 = FREEFILE
   OPEN "Output.Dat" FOR INPUT AS F2
   OPEN "SAT.Dat" FOR OUTPUT AS F3
   TStr = ""
   'PRINT F3, "CA";SYSTAT(0);      'PRINT STATION ID
   PRINT F3, "CAaa0301xx";
'12 01 2011 16 26 51 00000000 00110001  11.8 1486.8 132.5  -5.4   0.5   2.022  10.18     0 14868
   LINE INPUT F2, TStr   'read in HEADER line
   PRINT F3, "-";BIN6(VAL(MID(TStr, 1,2)),2);Bin6(VAL(Mid(TStr,4,2)),2);Bin6(VAL(Mid(TStr,7,4)),2);Bin6(VAL(Mid(TStr,12,2)),2);Bin6(VAL(Mid(TStr,15,2)),2);Bin6(VAL(Mid(TStr,18,2)),2);
   PRINT F3, Bin6(VAL(MID(TStr,21,8)),3);Bin6(VAL(MID(TStr,30,8)),3);
   PRINT F3, Bin6(VAL(MID(TStr,38,6))*10,2);Bin6(VAL(MID(TStr,44,7))*10,3);Right(BIN6(VAL(MID(TStr,51,6))*10,3),2);Bin6(VAL(MID(TStr,57,6))*10,2);Bin6(VAL(MID(TStr,63,6))*10,2);
   PRINT F3, Right(BIN6(VAL(MID(TStr,69,8))*1000,3),2);Right(BIN6(VAL(MID(TStr,77,7))*100,3),2);BIN6(VAL(MID(TStr,84,6)),3);Bin6(VAL(Mid(TStr,90,6)),3);
'  -0.129  -0.068  -0.004 140.0 139.0 139.0
   Do While Not Eof(F2) 'read in all bin lines
	TStr = ""
   	LINE INPUT F2, TStr   'read in profile line
	If Len(TStr) > 2 then
	   PRINT F3,"+";
  	   PRINT F3, BIN6(VAL(MID(TStr,1,8))*1000,3);
	   PRINT F3, BIN6(VAL(MID(TStr,9,8))*1000,3);
	   PRINT F3, BIN6(VAL(MID(TStr,17,8))*1000,3);
	   PRINT F3, BIN6(VAL(MID(TStr,26,3)),2);BIN6(VAL(MID(TStr,32,3)),2);BIN6(VAL(MID(TStr,38,3)),2);
	End if
   End Loop
   CLOSE F2
   CLOSE F3
   OPEN "SAT.Dat" FOR INPUT AS F3
   result = ReadB(F3, DATASTR, 2000)
   CLOSE F3
   ALARM_iATONFormatter = DATASTR
      
END FUNCTION
