'------------------------------------------------------------------------
' Filename: IMO Relay Enable.bas
' Purpose: Set up a COMS tag to turn a digital IO
'      on or off remotely via the command line or
'      Iridium MTM.
'
'          To test MTM capability, send sbd message with the following:
'
'          !set IOEnable 1
'
'------------------------------------------------------------------------

'Digital I/O Vars
Static iOut    = 5    'Edit this number to match the I/O point of the IP Modem Relay
Static iOut2   = 6    'Edit this number to match the I/O point of Wi-Fi Dongle Relay
Static iSleep1 = 3600 'Keeps I/O point set for 1 hour when MTM Turns it on.
Static iSleep2 = 60   'Keeps I/O point set for 1 Minutes on Schedule
Static iModule = 1    'Only change this if you are using I/O module 2 on an XPert
Static IOon    = -1
Static IOoff   = 0

' Coms Tag Vars
Declare Tag IOEnable(1)
Last_IOEnable = Digital(iModule,iOut)

Public Function Get_IOEnable(Value)
   If Value = 1 Then Get_IOEnable = Last_IOEnable
End Function

Public Sub Set_IOEnable(Value, Data)
   If Value = 1 Then
      Last_IOEnable = Data
      Digital iModule , iOut , Data
      Digital iModule , iOut2, Data
      StatusMsg "Trying to set DIOs to: "&Data
   End If
End Sub

Public Sub Eval_IOEnable
   Last_IOEnable = Digital(iModule , iOut)
End Sub

Public Sub Start_IOEnable
   REM called when recording is started
   Call Eval_IOEnable
End Sub

Public Sub Stop_IOEnable
   REM called when recording is stopped
End Sub


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' THESE ROUTINES MUST BE USED IN UNISON. DO NOT FORGET TO TURN I/O OFF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Sub SCHED_TURNRELAYS_ON
' Turn Relay's ON
      Digital iModule , iOut  , IOon
      Digital iModule , iOut2 , IOon
      StatusMsg "Scheduled Attempt: Turn IOs ON"
End Sub

Public Sub SCHED_TURNRELAYS_OFF
' Turn Relay OFF
      Digital iModule , iOut  , IOoff
      Digital iModule , iOut2 , IOoff
      StatusMsg "Scheduled Attempt: Turn IOs OFF"
End Sub
