REM SerialLib.bas (C) 2005 Sutron Corporation
REM
REM This is a library of functions that may be used to communicate with
REM a serial port which is under the control of the Coms Manager and/or Remote.
REM For ports which are not inuse, the standard file I/O api built-in to Xpert Basic
REM should be used.
REM
REM The library uses TCP/IP socket communications to take control of the port from
REM Remote. Strings may be read or written, control lines can be toggled, and
REM there is a command to issue Hayes commands and get back the result.
REM
REM This code may be freely modified or adapted as long as it is used on Sutron
REM equipment and this notice is not removed.

REM Constants that may be reused by other Basic Programs using this Library
Const True = -1
Const False = 0
Const LoggingIn=2
Const LoggedIn=4

REM Subroutines and Function declarations that may be reused by other Basic Programs using this Library
Public Declare Function OpenPort(ComPort, RejectWhen)
Public Declare Sub ClosePort(Handle)
Public Declare Sub FlushPort(Handle)
Public Declare Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)
Public Declare Sub WritePort(Handle, Data)
Public Declare Function HayesCommand(Handle, Command, TimeoutSec)
Public Declare Sub SetDTRPort(Handle)
Public Declare Sub ClrDTRPort(Handle)
Public Declare Sub SetRTSPort(Handle)
Public Declare Sub ClrRTSPort(Handle)
Public Declare Sub SetBreakPort(Handle)
Public Declare Sub ClrBreakPort(Handle)
Public Declare Function CDPort(Handle)
Public Declare Function DSRPort(Handle)
Public Declare Function CTSPort(Handle)

' Documentation:
'
' There is a sample called SerialLibTest at the end of this file that may be
' commented in, and used to test the library. There is also a seperate program
' called SerialTest.Bas that demonstrates how these functions can be called from
' a seperate program.
'
' Do not expect fast performance when using this library. A lot of indirection is involved
' and reading data specifically requires a lot of overhead. Sending data to a port will
' be efficient if the data can be built up in to strings.
'
' OpenPort:      Call this to gain access to a port. 0 is returned if access could
'                not be obtained, otherwise an I/O handle is returned. The RejectWhen
'                parameter determines whether the port should be grabbed only when the
'                the port is disconnected (6), only when someone isn't logged in (4),
'                or without restriction (0).
'
' ClosePort:     Call this when you are done with the port.
'
' WritePort:     Writes bytes to a port.
'
' ReadPort:      Reads bytes from a port. BytesToRead specifies how many bytes are desired.
'                BytesRead indicates how many were read, BytesRemain indicates how many
'                can be read, and TimeoutSec is how long to wait.
'
' HayesCommand:  Appends a CR to a command and sends it to a port. It then waits up to
'                TimeoutSec for a reply and returns any data. The returned data is trimmed
'                of control-codes. No retries are performed.
'
' SetDtrPort:    Enables DTR on the port
'
' ClrDtrPort:    Disables DTR on the port
'
' SetRtsPort:    Enables RTS on the port
'
' ClrRtsPort:    Disables RTS on the port
'
' SetBreakPort:  Creates a break condition on the port.
'
' ClrBreakPort:  Clears a break condition on the port
'
' CDPort:        Returns the status of CD, any incoming data is lost.
'
' DSRPort:       Returns the status of DSR, any incoming data is lost.
'
' CTSPort:       Returns the status of CTS, any incoming data is lost.

REM Private constants
Const OpAck = 0
Const OpEOT = 16
Const OpComOpen = 82
Const OpComClose = 83
Const OpComLock = 84
Const OpComUnLock = 85
Const OpComData = 86
Const OpComControl = 90
Const OpComGetStatus = 91
Const OpComStatus = 92
Const OpComCapture = 93
Const OpComGetOptions = 97
Const OpComOptions = 98
Const OpComSetCommState = 102
Const ThreeNulls = Chr(0)&Chr(0)&Chr(0)

REM Private variables
Dim DataBuffer

Sub SendOp(F, Opcode, Data)
   L = Len(Data)
   Print F, Chr(Opcode); Chr(L); Chr(L >> 8); Data;
End Sub

Function GetOp(F, Opcode, L, Data)
   L = 0
   Opcode = OpEOT
   LL = ""
   LH = ""
   OP = ""
   GetOp = False
   If (ReadB(F, OP, 1) = 1) Then
      Opcode = Asc(OP)
      If (ReadB(F, LL, 1) = 1) Then
         If (ReadB(F, LH, 1) = 1) Then
            L = Asc(LH)*256 + Asc(LL)
            If (ReadB(F, Data, L) = L) Then
               GetOp = True
            End If
          End If
      End If
   End If
End Function

Function Send(F, Opcode, Data)
   Call SendOp(F, Opcode, Data)
   Send = False
   O = 0
   D = ""
   L = 0
   If GetOp(F, O, L, D) Then
      If O=OpAck And Opcode=Asc(D) Then
         Send = True
      End If
   End If
End Function

' Return true if someone is logged in or in the process of logging in to the port
Function IsLoggedIn(F, RejectWhen)
   Call SendOp(F, OpComGetOptions, "")
   IsLoggedIn = False
   O = 0
   D = ""
   L = 0
   If GetOp(F, O, L, D) Then
      If (O=OpComOptions) And (Asc(D) And RejectWhen) Then
         IsLoggedIn = True
      End If
   End If
End Function

' Return true if the port is not inuse and we can get exclusive access
Function MakeConnect(F, RejectWhen)
   MakeConnect = False
   If Not IsLoggedIn(F, RejectWhen) Then
      If Send(F, OpComLock, "") Then
         MakeConnect = True
      End If
   End If
End Function

' Erase the input buffer
Public Sub FlushPort(Handle)
   DataBuffer(Handle) = ""
   FlushInput Handle
End Sub

' Open's a a COM port in the specified mode
' RejectWhen is used to prevent accessing a port which is already inuse and
' can take the value: 0, LoggingIn, LoggedIn, or Logging Or LoggedIn
Public Function OpenPort(ComPort, RejectWhen)
   OpenPort = 0
   PortIndex = Asc(Mid(ComPort, 4, 1)) - Asc("0")
   Handle = FreeFile
   ' 52733 is the socket server in Remote
   Open "localhost:52733" As Handle
   SetTimeout Handle, 5.0
   If Send(Handle, OpComOpen, Chr(PortIndex)) Then
      StartTick = GetTickCount
      ' Try for up to 5 seconds to get exclusive access to the port
      Do While Not MakeConnect(Handle, RejectWhen)
         Sleep(0.1)
         If (Now - StartTick) >= 5000 Then
            Close Handle
            Exit Function
         End If
      End Loop
      Call SendOp(Handle, OpComCapture, Chr(1))
      OpenPort = Handle
      Call FlushPort(Handle)
   Else
      Close Handle
   End If
End Function

Public Sub ClosePort(Handle)
   If Handle<>0 Then Close Handle
End Sub

Public Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)
   Result = ""
   O = 0
   D = ""
   L = 0
   StartTick = GetTickCount
   TimeoutMS = TimeoutSec * 1000
   Do While Len(DataBuffer(Handle)) < BytesToRead
      If Loc(Handle) >= 3 Then
         If GetOp(Handle, O, L, D) Then
            If O=OpComData Then
               DataBuffer(Handle) = DataBuffer(Handle) + D
            End If
         Else
            Exit Do
         End If
      ElseIf (GetTickCount-StartTick) > TimeoutMS Then
         Exit Do
      Else
         Sleep(1)
      End If
   End Loop
   BytesRead = Len(DataBuffer(Handle))
   If BytesRead > BytesToRead Then
      BytesRead = BytesToRead
   End If
   ReadPort = Left(DataBuffer(Handle), BytesRead)
   DataBuffer(Handle) = Mid(DataBuffer(Handle), BytesRead+1)
   BytesRemain = Len(DataBuffer(Handle))
End Function

Public Sub WritePort(Handle, Data)
   Call SendOp(Handle, OpComData, Data)
End Sub

Public Sub SetDTRPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(5)+ThreeNulls)
End Sub

Public Sub ClrDTRPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(6)+ThreeNulls)
End Sub

Public Sub SetRTSPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(3)+ThreeNulls)
End Sub

Public Sub ClrRTSPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(4)+ThreeNulls)
End Sub

Public Sub SetBreakPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(8)+ThreeNulls)
End Sub

Public Sub ClrBreakPort(Handle)
   Call SendOp(Handle, OpComControl, Chr(9)+ThreeNulls)
End Sub

Function GetStatus(F)
   Result = 0
   Call SendOp(F, OpComGetStatus, "")
   IsLoggedIn = False
   O = 0
   D = ""
   L = 0
   Do While GetOp(F, O, L, D)
      If O=OpComData Then ' Process a data packet that might slip in
         DataBuffer(F) = DataBuffer(F) + D
      ElseIf O=OpComStatus Then
         Result = Asc(D)
         Exit Do
      End If
   End Loop
   GetStatus = Result
End Function

Public Function CDPort(Handle)
   Const MS_RLSD_ON = &h80
   CDPort = (GetStatus(Handle) And MS_RLSD_ON) <> 0
End Function

Public Function DSRPort(Handle)
   Const MS_DSR_ON = &h20
   DSRPort = (GetStatus(Handle) And MS_DSR_ON) <> 0
End Function

Public Function CTSPort(Handle)
   Const MS_CTS_ON = &h10
   CTSPort = (GetStatus(Handle) And MS_CTS_ON) <> 0
End Function

' Eliminate control characters from a string
Function Trim(S)
   Result = ""
   For i = 1 To Len(S)
      Ch = Mid(S, i, 1)
      If (" " <= Ch) And (Ch <= "~") Then
         Result = Result + Ch
      End If
   Next i
   Trim = Result
End Function

' Sends a Hayes commands to the modem and returns the reply (empty if a timeout occured)
Public Function HayesCommand(Handle, Command, TimeoutSec)
   Result = ""
   Call FlushPort(Handle)
   ' Tack on a CR and send the command to the modem
   Call WritePort(Handle, Command + Chr(13))
   BytesRead = 0
   BytesRemain = 0
   ' Wait up to the timeout for 1 character to come in
   Result = ReadPort(Handle, 1, BytesRead, BytesRemain, TimeoutSec)
   ' Now read in any additional characters but with a very short timeout
   If Result <> "" Then
      Result = Result + ReadPort(Handle, 1000, BytesRead, BytesRemain, 0.01)
   End If
   HayesCommand = Trim(Result)
End Function

' Sample subroutine that may be commented out and scheduled.
' It will open COM1: and prompt for some characters and echo them back
' It requires that noone is logged in to COM1:
'
'Public Sub Sched_SerialLibTest
'   Handle = OpenPort("COM1:", LoggedIn)
'   If Handle = 0 Then
'      ErrorMsg "SerialLib could not open COM1:"
'      Exit Sub
'   End If
'   StatusMsg "SerialLib DSR = " & DSRPort(Handle) & ", CD = " & CDPort(Handle) & ", DSR = " & DSRPort(Handle)
'   CRLF = Chr(13) + Chr(10)
'   Call WritePort(Handle, CRLF+CRLF+"Hello there. This is a test of SerialLib.bas"+CRLF+"Please enter a string: ")
'   BytesRead = 0
'   BytesRemain = 0
'   Reply = ReadPort(Handle, 32, BytesRead, BytesRemain, 10.0)
'   Call WritePort(Handle, CRLF + "You typed: " + Reply + CRLF)
'   Call WritePort(Handle, "BytesRead is "+BytesRead+CRLF)
'   Call ClosePort(Handle)
'End Sub


