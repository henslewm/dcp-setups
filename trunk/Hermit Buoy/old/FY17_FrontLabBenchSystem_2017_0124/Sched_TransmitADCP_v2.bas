'-------------------------------------------------------------------------------
'     NOAA / COOPS IridumSBDRoutines.BAS - 
'     BASIC code for Iridium hourly hydro project that handles Iridium tasks
' 	Modified code from Sutron software for Bhutan COE Concord project.
'                Designed to run on version Xpert OS version 3.11.0.14 without
'                a SatLink2 connected.
'
'     Routines defined in this file are:
'
'     Sub SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
'         This routine sends a message (TXMsg) via SBD.  The program first
'         sends at ATE0 to turn echo off.  Next AT+SBDD2 to clear the input and
'         output buffers of the iridium modem.  It send the message to the 
'         modem using AT+SBDWB command and then issues AT+SBDIX command.
'         The program parses the SBDIX response to see if there are any msgs
'         queued at the gateway.  The info is passed back out and handled by
'         the main SBDHandler program 
'
'
'     Function GMTTime(RemainingMilliseconds)
'         Routine added at NOAA in Chesapeake VA which is called when SendSBD transmits data.
'         Iridium system time is polled from the Gateway and then converted to GMT Time. If 
'         clock has drifted by a second or more the time is synced to the GMT Time from
'         the Gateway.
'
'     Sub SyncDCPClock
'         This is a function which contains an algorithm for converting the 90ms increments
'         from Iridium into ms values for comparing with the system time. It works with the
'         GMTTime function and it could be consolidated to one function I think.
'
'     Function GetSTCurrentMessage 
'         Added when upgraded to Xpert OS 3.11.0.14 so that we have access to the GOES
'         message when the SatLink is not connected.
'
'     Right now, this is set up for an Iridium modem on COM4: This is defined in 
' 	the Constants below
'
'     NOTE:  ATE0 has been wired into the code as of 7/18/07 TNK
'
'
'-------------------------------------------------------------------------------
Const LoggingIn=2
Const LoggedIn=4
Const LF = Chr(10)
Const CR = Chr(13)
Const CRLF = Chr(13) & Chr(10)
Const RegTimeout = 600              'timeout to register (seconds)
Const SendTimeout = 300             'timeout to send
Const CharQuote = Chr(34)           'Quote (to send quote character inside of a string)

Const IridiumPort = "COM4:"         'Com port iridium is connected to.  If com3 make sure NO 5 volts.
Const SecretPhrase = "How 'bout Dem Os?"

TxWhileRecordingOff = False

Const MaxModemInitFail = 25         'Set max number of allowed failures it initialize modem 
Const RebootOnModemFail = True      'Set true if want to auto-reboot after max modem init failures reached


Static ModemRegistered
Static IridiumHandle
Static MsgInSBD            'indicates if there are messages are queued at the gateway
Static MsgBytes            'number of bytes in current message transferred from iridium modem
Static NumMsgQueued        'number of msgs queued at gateway as a result of SBDIXA cmd
Static ModemInitFail

'Init variable for first start recording
COMPortInUse = False


Static CurrentDay          'variables to track diagnostics
Static LastDay
Static NumBytesTxTotal     'number to total bytes transmitted by iridium
Static NumBytesTxToday     'number of total bytes transmitted today
Static NumBytesRxTotal     'number to total bytes received by Iridium
Static NumBytesRxToday     'number of total bytes received today
Static NumTxSuccessTotal   'number of total successful transmissions 
Static NumTxSuccessToday   'number of successul transmissions today
Static NumTxFailTotal      'number of total failed transmissions
Static NumTxFailToday      'number of failed transmissions today


Public Declare Sub SendSBD(TxMsg,SendOK)
Public Declare Function CRCOK(RxMsg)
Public Declare Function AddCRC(TxMsg)
Public Declare Function FormatIridiumWXData

Public Declare Sub FlushPort(Handle)
Public Declare Sub ClosePort(Handle)
Public Declare Sub WritePort(Handle, Data)
Public Declare Sub SetDTRPort(Handle)
Public Declare Sub ClrDTRPort(Handle)
Public Declare Sub SetRTSPort(Handle)
Public Declare Sub ClrRTSPort(Handle)
Public Declare Sub SetBreakPort(Handle)
Public Declare Sub ClrBreakPort(Handle)
Public Declare Function CDPort(Handle)
Public Declare Function OpenPort(ComPort, RejectWhen)
Public Declare Function Trim(S)
Public Declare Function HayesCommand(Handle, Command, TimeoutSec)
Public Declare Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)

'********************* SUB SendSBDData ******************************************
Public Sub SendSBD(TxMsg, SendOK)
  
  'Data transmission routine
  'This sets up the modem and pushes out a data string -- in this case,
  'it will be the GOES Message which would have been pushed out by the
  'SatLink2 

    On Error Resume Next
    NumMsg = 0
    StatusMsg "Iridium SendSBD - SBD transmission started"

    'Make sure Echo is off -- parser fails if echo is on
    'StatusMsg "Iridium - ATE0"
    txt = HayesCommand(IridiumHandle, "ATE0", 10)

    'Clear SBD Message Buffer
    iTry = 0
    BufferCleared = false
    Do
       iTry = iTry + 1
       StatusMsg "Iridium - AT+SBDD2 - Clears MO/MT Buffers"
       txt = HayesCommand(IridiumHandle, "AT+SBDD2", 3)   '0=MO, 1=MT, 2=both
       If Right(txt,3) = "0OK" then
         BufferCleared = true
       Else
          Sleep 5
       End If
       If iTry > 5 Then
          ErrorMsg "Iridium - Command AT+SBDD failed on retries " &txt
          goto ErrorHandler
       End If
    Loop Until (BufferCleared OR iTry > 5)


    StatusMsg "Tx Data = "&(TxMsg)
    BinString = TxMsg

    LenData = Format("%d", Len(BinString))
    Result = ComputeCRC(0,0,BinString) Mod 65536
    StatusMsg "Iridium - CRC " &(Result>>8) &" " &(Result Mod 256)
    BinString = BinString & Chr(Result>>8) & Chr(Result Mod 256)

    If Len(BinString) > 270 Then
       ErrorMsg "SBD message too long"
       TxMsg = ""
       GoTo ErrorHandler
    End If   

    StartTime = Time
    DataSent = False
    iTry = 0
    Do
       iTry = iTry + 1
       StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to write SBD to buffer"
       Cmd = "AT+SBDWB="&LenData
       Txt = HayesCommand(IridiumHandle, Cmd, 5)
       StatusMsg "Iridium - SBDWB status=" & txt
       If Left(Txt, 5)="READY" then
          Txt = HayesCommand(IridiumHandle, BinString, 2)
          StatusMsg "Iridium - SBDWB: " &txt
          If Left(Txt,3) <> "0OK" then
             ErrorMsg "Iridium - Command SBDWB failed " &Txt
             goto ErrorHandler
          Else
             DataSent = True
             TxMsg = ""
          End If
       End If
       SLEEP 10
    Loop Until (DataSent OR ((Time - StartTime) > SendTimeout))

    SLEEP 1
    'Initiate SBD Session to send the data
    StartTime = Time
    DataSent = false
    iTry = 0
    Do
       Statusmsg "Iridium - AT+SBDIX"
       iTry = iTry + 1
       StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to send SBD Message"
       txt = HayesCommand(IridiumHandle, "AT+SBDIX", 90)
       StatusMsg "Iridium - SBDIX=" &txt
       If (Left(txt, 10) = "+SBDIX: 0,") Or (Left(txt, 10) = "+SBDIX: 1,") Or (Left(txt, 10) = "+SBDIX: 2,") Then
          DataSent = True
          NumBytesTxTotal = NumBytesTxTotal + LenData
          NumBytesTxToday = NumBytesTxToday + LenData
          NumTxSuccessTotal = NumTxSuccessTotal + 1
          NumTxSuccessToday = NumTxSuccessToday + 1

          MsgBytes = 0
          NumMsgQueued = 0
          MsgInSBD = False

       Else
          SLEEP 30
          nRxBytes = 0
          NumMsgQueued = 0
          If iTry > 10 Then
             StatusMsg "Iridium - SBDI retry count exceeded"
             NumTxFailTotal = NumTxFailTotal + 1
             NumTxFailToday = NumTxFailToday + 1

             goto ErrorHandler
          End If
       End If

    Loop Until (DataSent OR ((Time - StartTime)> SendTimeout))

    StatusMsg "Clear buffer after SBDIX"
    If MsgInSBD Then
       txt = HayesCommand(IridiumHandle, "AT+SBDD0", 3)   '0=MO, 1=MT, 2=both
       StatusMsg "SBDD0 response: " + txt
    Else
       txt = HayesCommand(IridiumHandle, "AT+SBDD2", 3)   '0=MO, 1=MT, 2=both
       StatusMsg "SBDD2 response: " + txt
    End If


    SendOK = DataSent
    StatusMsg "Iridium - SBD transmission done"

    ' Check the signal strength and record with status
    txt = HayesCommand(IridiumHandle, "AT+CSQ", 8)
    StatusMsg "Iridium - Signal Strength - ending = " & txt

ErrorHandler:   'Label for branch from errors

End Sub

'********************* FUNCTION CRCOK *******************************************
Public Function CRCOK(RxMsg)
  
  
  'Ind = InStr(1, RxMsg, "!")
  '   A = ASC(Mid(RxMsg, Ind+1, 1))
  '   B = ASC(Mid(RxMsg, Ind+2, 1))
  '   A = 69
  '   B = 7
  '   StatusMsg "A = "&(A)
  '   StatusMsg "B = "&(B)
  '   A = A << 8
  '   B = B Mod 256
  '   C = A Or B
  '   StatusMsg "Shifted Computed CRC: "&(c)
  '   hex  45 07

  On Error Resume Next
  Ind = InStr(1, RxMsg, "!")
  nLen = Len(RxMsg)
  If Ind+3 <= nLen Then
     Msg = Mid(RxMsg, 1, Ind-1)
     StatusMsg "Msg = "+Msg
     aCRC = Mid(RxMsg, Ind+1, 3)
     StatusMsg "Received CRC = "+aCRC
     nCRC = ComputeCRC(0, &h38005, Msg+SecretPhrase)
     StatusMsg "Computed SSP CRC: "+nCRC
     sCRC = Bin6(nCRC, 3)
     StatusMsg "6-bit CRC: "+sCRC

     If sCRC <> aCRC Then
        ErrorMsg "CRCs do not match"
        CRCOK = False
     Else
        StatusMsg "CRCs Ok"
        RxMsg = Msg
        CRCOK = True
     End If    
  Else
    ErrorMsg "Complete 2-byte CRC not present in msg"
    CRCOK = False  
  End If

End Function  

'********************* FUNCTION AddCRC ******************************************
Public Function AddCRC(TxMsg)
  
  StatusMsg "Computing CRC "
  nCRC = ComputeCRC(0, &h38005, TxMsg+SecretPhrase)
  StatusMsg "Computed SSP CRC: "+nCRC
  sCRC = Bin6(nCRC, 3)
  AddCRC = "!"+sCRC
  StatusMsg "6-bit CRC: "+AddCRC
  
End Function  

'********************* SUB Sched_CRCTest*********************************************
Public Sub Sched_CRCTest
    RxMsg = "siren=1!E"
    If CRCOK(RxMsg) Then
       StatusMsg "CRC OK: "+RxMsg
    Else
       StatusMsg "CRC check failed: "+RxMsg
    End If
  
End Sub  

'********************* SUB Sched_ATTest*********************************************
Public Sub Sched_ATTest
   IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
   StatusMsg "Opening port - handle = " &(IridiumHandle)

   If IridiumHandle Then
      StatusMsg "Iridium - SBD Registration started"

      'Power 1, 1
      'Sleep 10 ' Allow the modem time to wake up
      ' wait for the modem to be registered

      StatusMsg "Iridium - Sending ATE0"
      txt = HayesCommand(IridiumHandle, "ATE0", 10)
      StatusMsg "ATE0 reply - "+txt

      ' Configure ISU to listen for ring alerts
      StatusMsg "Iridium - Sending AT+SBDMTA=1"
      txt = HayesCommand(IridiumHandle, "AT+SBDMTA=1", 10)
      StatusMsg "AT+SBDMTA=1 reply - "+txt
      If Right(txt,2) <> "OK" then
         'Try one more time
         txt = HayesCommand(IridiumHandle, "AT+SBDMTA=1", 10)
         StatusMsg "AT+SBDMTA=1 try #2 reply - "+txt
         If Right(txt,2) <> "OK" then
            ErrorMsg "Iridium - Failed to set enable SBD Ring Alerts = " & txt
         End If
      End If
   End If
  If IridiumHandle <> 0 Then
     Call ClosePort(IridiumHandle)
     Sleep 1
     'Power 1, 0
  End If
        
End Sub  


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
Public Sub Sched_SBDTxADCP

   IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
   StatusMsg "Opening port - handle = " &(IridiumHandle)

   If IridiumHandle Then

	   StatusMsg "In SBDTxADCP"
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
	   PRINT F3, "CA";SYSTAT(0);      'PRINT STATION ID


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
	   
	   SendOK = FALSE
	   TxMsg = DATASTR   
	   MsgInSBD = 0 
	   
	   Call SendSBD(TxMsg, SendOK)
	End If

   If IridiumHandle <> 0 Then
     Call ClosePort(IridiumHandle)
     Sleep 1
     'Power 1, 0
   End If
END SUB
