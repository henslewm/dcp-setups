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
Public Function SELFTIMED_STFormatter_GPS

	If Minute(Now) > 30 And Minute(Now) < 45 Then
		
		LAT = LogReading(“\sd card\gps.log”, “B”, “Lat”, 5)
		LON = LogReading(“\sd card\gps.log”, “B”, “Long”, 5)

      		If (UBound(LAT) > 0) Then
         		LAT = LAT(0)
      		End If

		If (UBound(LON) > 0) Then
         		LON = LON(0)
      		End If

		StatusMsg "Transmitting-> Lat:" & LAT(3) & " Long:" & LON(3) 
      		SELFTIMED_STFormatter_GPS = "Lat:" & LAT(3) & " Long:" & LON(3)
	Else
		SELFTIMED_STFormatter_GPS = GetSTCurrentMessage
	End If

End Function

Public Function SELFTIMED_STFormatter_noGPS
		SELFTIMED_STFormatter_noGPS = GetSTCurrentMessage
End Function
' Test out the functionality for the above formatter.
Public Sub SCHED_TestGPSFormatter

		'Grab the Latitude Reading, I think this returns 2-dimensional
		LAT = LogReading(“\sd card\gps.log”, “B”, “Lat”, 5)
		LON = LogReading(“\sd card\gps.log”, “B”, “Long”, 5)

		' Make sure you have a reading for Lat
      		If (UBound(LAT) > 0) Then
         		LAT = LAT(0)
		Else
			StatusMsg "Lat = 0 : " & LAT
      		End If


		' Make sure you have a reading for Long
		If (UBound(LON) > 0) Then
         		LON = LON(0)
      		End If

		StatusMsg "Lat:" & LAT(3) & " Long:" & LON(3)

		'''''''''''FOR DEBUG'''''''''
		'For LogReadingCtr = 1 To UBound(LAT)

		'	StatusMsg "Transmitting GPS Data " & LAT(LogReadingCtr)

		'Next
		''''''''''''''''''''''''''''''

End Sub