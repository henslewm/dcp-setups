'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Filename: 	AIRMAR.bas
' Author:   	Philip Libraro
' Date:     	Long long ago.
' Modified: 	Winston Hensley - 02/23/2015
' Decription: 	For the AirMar WX150 All-in-One Weather Station. 
'        		All output is suppressed except Wind/Baro/Temp and GPS Lat/Long
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This program supports measuring AIRMAR WEATHER sensor. Assumes sensor has been
' initialized (all strings aside from weather&GPS have been shut off) and measures
' sensor at schedule user assigns to measure blocks.
' On power up, Wind speed and direction will not be available until GPS lock
' is achieved.  This could take a minute or two.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'********'Variables initilized
CONST AIRMAR_CONNECTED = 1       ' Easily disable code for testing.
CONST CHAR_SH = Chr(1)           ' Start of heading character
CONST CHAR_SX = Chr(2)           ' Start of text character
CONST CHAR_EX = Chr(3)           ' End of text character
CONST CHAR_ET = Chr(4)           ' End of transmission character
CONST CHAR_LF = Chr(10)          ' Line feed character
CONST CHAR_CR = Chr(13)          ' Carriage return character
CONST ENTER = CHAR_CR + CHAR_LF
CONST MAXBYTES = 100             ' Max num bytes in sensor response. See manual.


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Port Configuration Parameters
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
CONST PORT = "COM4:"
CONST PORT_TIMEOUT = 0.5
CONST SENSORNAME = "AIRMAR"        ' Name used on block output and sensors page
Const BAUD = 4800
Const NOPARITY = 0
Const NOHANDSHAKE = 0


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Configure Sensor at Program Start
' We want to disable all lines aside from the wind, baro, gps and temperature 
' output line. 
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub START_PROGRAM
	StatusMsg "Starting Program"
	If AIRMAR_CONNECTED THEN
	 	'*** Open port, abort if recording stopped
		If Abort Then Goto ErrorHandler
		On Error Resume Next
		
		StatusMsg "Opening AIRMAR COM Port to configure sensor."
		Open PORT as #1  'iFilenum
		If Err <> 0 Then
			ErrorMsg "Failed to open com port, err " &Err
			GoTo ErrorHandler
		End If
		StatusMsg "AIRMAR port opened"
		SetPort #1, BAUD, NOPARITY, 8, 1, NOHANDSHAKE
		StartTime = Time
		TimeValid = false
		SetTimeout #1,35
		'*** port opened and initialized
		StatusMsg "AIRMAR Init..."
	   'Turn off all lines except weather data.  Runs on restart.
	   InitCmdTbl(1, 0)  = "$PAMTX"           		:  InitCmdTbl(1, 1) = 0 'Turn off line streaming
	   InitCmdTbl(2, 0)  = "$PAMTC,EN,GGA,1"        :  InitCmdTbl(2, 1) = 0 
	   InitCmdTbl(3, 0)  = "$PAMTC,EN,GLL,0"        :  InitCmdTbl(3, 1) = 0 
	   InitCmdTbl(4, 0)  = "$PAMTC,EN,MWVR,0"       :  InitCmdTbl(4, 1) = 0 ' Leave the GPS on. Max Length 82
	   InitCmdTbl(5, 0)  = "$PAMTC,EN,MWVT,0" 	   :  InitCmdTbl(5, 1) = 0
	   InitCmdTbl(6, 0)  = "$PAMTC,EN,MWD,0"		   :  InitCmdTbl(6, 1) = 0
	   InitCmdTbl(7, 0)  = "$PAMTC,EN,ROT,0"	   	:  InitCmdTbl(7, 1) = 0
	   InitCmdTbl(8, 0)  = "$PAMTC,EN,ZDA,0"        :  InitCmdTbl(8, 1) = 0
	   InitCmdTbl(9, 0)  = "$PAMTC,EN,VTG,0"		   :  InitCmdTbl(9, 1) = 0
	   InitCmdTbl(10, 0)  = "$PAMTC,EN,HDT,0"		   :  InitCmdTbl(10, 1) = 0
	   InitCmdTbl(11, 0) = "$PAMTC,EN,XDRA,0"	    	:  InitCmdTbl(11, 1) = 0
	   InitCmdTbl(12, 0) = "$PAMTC,EN,XDRB,0"			:  InitCmdTbl(12, 1) = 0
	   InitCmdTbl(13, 0) = "$PAMTC,EN,S"				:  InitCmdTbl(13, 1) = 0 'Save to EEROM
	   InitCmdTbl(14, 0) = "$PAMTX,1"               :  InitCmdTbl(14, 1) = 0 ' Restart
	   
	   NumBytes = 0
	   DATA = ""
      FlushInput PORT

      ' Output each of the init commands.
      FOR i = 1 TO UBOUND(InitCmdTbl)
         Print #1, (InitCmdTbl(i, 0) + ENTER)
         SLEEP PORT_TIMEOUT
      NEXT i
	ELSE
		ErrorHandler:
	      ErrorMsg Format("AIRMAR: Open port %s failed (configure)", PORT)
	End If

	Close #1	
	StatusMsg "Closing AirMar COM Port"
	If Err <> 0 Then
		ErrorMsg "Failed to close com port, err " &Err
		GoTo ErrorHandler
	End If

End Sub
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Initialize Sensor
' This code runs at recording start. Here we simply open the port.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub Start_Recording
	If AIRMAR_CONNECTED THEN
	 '*** Open port, abort if recording stopped
	  If Abort Then Goto ErrorHandler
	  On Error Resume Next
	  StatusMsg "Opening AIRMAR port"
	  Open PORT as #1  'iFilenum
	  If Err <> 0 Then
	     ErrorMsg "Failed to open com port, err " &Err
	     Goto ErrorHandler
	  End If
	  
	  
	  StatusMsg "AIRMAR port opened"
	  SetPort #1, BAUD, NOPARITY, 8, 1, NOHANDSHAKE
	  StartTime = Time
	  TimeValid = false
	  SetTimeout #1,35
	  
	  'Flush any previous messages from input buffer.
	  FlushInput PORT
	Else
	ErrorHandler:
	      ErrorMsg Format("AIRMAR: Open port %s failed.", PORT)
	End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Measure Sensor
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Sub SENSOR_AIRMAR

'Initialize Variables
NMEA_Messages = 2
Message_Location = 0
'Number of empty strings below corresponds to 
'number of messages in variable above.
sInBuf 	= Array("","")
MSGID 	= ""
TEMPC 	= 0
PRESIN 	= 0.0
PRESMB 	= 0.0
Q1 		= "b"
Q2 		= "b"
Q3 		= "b"
Q4 		= "b"
Q5 		= "b"
Q6 		= "b"
Q7 		= "b"
Q8 		= "b"
Q9 		= "b"
NG 		= ""
N 			= 0.0
COG 		= 0.0
DIRT 		= 0.0
DIRM 		= 0.0
SPDN 		= 0.0
SPDM 		= 0.0
Asterisk = ""
GPSLat  	= 0.0
GPSLong 	= 0.0
NMEA_message = ""

' Output from GPGGA
GGA_UTC 		= 0 
GGA_lat		= 0
GGA_ns		= "N"
GGA_long		= 0
GGA_ew		= "E"
GGA_quality = 0.0
GGA_num 		= 0
GGA_HDOP		= ""
GGA_Alt  	= 0
GGA_M    	= ""
GGA_geoid	= 0

' Output from GPGLL
GLL_lat 			= 0 
GLL_long 		= 0
GLL_ns			= "N"
GLL_ew			= "E"

' NMEA CheckSum Variable 
Cksum 	= ""
On Error Resume Next
  
'------------------------------------------------------------------------------------------
'Start Time
'------------------------------------------------------------------------------------------
tStart = Now

'Synchronization string is $
'Typical string from the AirMar for Weather:
'$WIMDA,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F

'Typical string from the AirMar for GPS:
'$GPGGA,140503.99,3645.8079,N,07615.7310,W,1,5,3.8,628.7,M,-37.4,M,,*67
'$GPGLL,4916.46,N,12311.12,W,,V,N*64
'123451234512345123451234512345123451234512345123451234512345
'----------------10-------------20--------------30--------------40-------------50-------------60
'eg1. $GPGLL,3751.65,S,14507.36,E*77
'eg2. $GPGLL,4916.45,N,12311.12,W,225444,A
'           4916.46,N    Latitude 49 deg. 16.45 min. North
'           12311.12,W   Longitude 123 deg. 11.12 min. West
'           225444       Fix taken at 22:54:44 UTC
'           A            Data valid
'eg3. $GPGLL,5133.81,N,00042.25,W*75
'               1    2     3    4 5
'
'      1    5133.81   Current latitude
'      2    N         North/South
'      3    00042.25  Current longitude
'      4    W         East/West
'      5    *75       checksum

F1 = FreeFile         
Open "DataFile.txt" For Output As F1
	For i = 1 To NMEA_Messages
		WaitFor #1, "$"
		Line Input #1, sInBuf(i) 'Should be a complete line
		Print F1, sInBuf(i)
	Next
Close F1

' Open the file back up for parsing. Non-elegant but necessary.
Open "Datafile.txt" for Input as F1

	' Parse data after reading from instrument to minimize timing conflicts.
	For i = 1 To NMEA_Messages
		
		'Get NMEA message location, pull message, Read the ID, move pointer 
		'back to beginning of line so Line Input can pull the line for 
		'parsing into variables.
		Message_Location = Seek(F1)

		'Read line from file to find MsgID then reset position to start of msg
		Line Input F1, NMEA_message
		Seek F1, Message_Location
		
		' Read Message ID for Further Parsing 
		MsgID = Mid(NMEA_message,1,5)
				
		''''''''''''''''''''''''''''''''''''' QC & Record Winds'''''''''''''''''''''''''''''''''
		If MsgID = "WIMDA" Then
			Input F1, MsgID, PRESIN, Q1, PRESMB, Q2, TEMPC, Q3, NG, NG, NG, NG, NG, NG, DIRT, Q4, DIRM, Q5, SPDN, Q6, SPDM, Cksum
			
			' Don't understand why variables re-defined here.
			dPRES = PRESMB 'numerical PRESSURE
			dTEMP = TEMPC 'numerical LONG
			dDIR = DIRT 'numerical wind direction true
			dSPD = SPDM 'numerical wind speed
			dPRES = dPRES * 1000
			
			  'Set the #2 output to PRESSURE and set the quality
				  SetOutputData 2, dPRES
				  If dPRES = 0.0 Then
				     SetOutputQuality 2, "B"
				  Else
				     SetOutputName 2, "Baro"
				     SetOutputUnits 2, "mb"
				     SetOutputQuality 2, "G"
				  End If
			
			  'Set the #3 output to TEMPERATURE and set the quality
				  SetOutputData 3, dTEMP
				  If dTEMP = 0.0 Then
				     SetOutputQuality 3, "B"
				  Else
				     SetOutputName 3, "ATemp"
				     SetOutputUnits 3, "C"
				     SetOutputQuality 3, "G"
				  End If
			
			  'Set the #4 output to WIND DIRECTION TRUE and set the quality
			     SetOutputName 4, "WDirTrue"
			     SetOutputData 4, dDIR
			     SetOutputUnits 4, "Deg"
			
			  'Set the #5 output to WIND SPEED and set the quality
			     SetOutputName 5, "WSpd"
			     SetOutputData 5, dSPD
			     SetOutputUnits 5, "M/S"
			
		Else
			
			''''''''''''''''''''''''''''''''''''' QC & Record GPS GPGGA''''''''''''''''''''''''''''''''		
			If MsgID = "GPGGA" Then
			Input F1, MsgID, GGA_UTC, GGA_lat, NG, GGA_long, GGA_ew, GGA_quality, NG, NG, NG, GGA_ns, NG, NG, NG, Cksum
			'StatusMsg "Found GPGGA"
		   'Unable to set the GGA_lat and GGA_long directions. The string output N/S/E/W 
		   'seems to come out in a '0 0' form. Perhaps BASIC forces all arguments to be 
		   'same data type as first found when parsing?
			'Set the #6 output to Longitude
			   SetOutputName 6, "Lat"
			   SetOutputData 6, GGA_lat
			   SetOutputUnits 6, "min"
			'Set the #7 output to Latitude
			   SetOutputName 7, "Long"
			   SetOutputData 7, GGA_long
			   SetOutputUnits 7, "min"
				
			Else
			End If
		   ''''''''''''''''''''''''''''''''''''' QC & Record GPS''''''''''''''''''''''''''''''''		
			'$GPGLL,4916.46,N,12311.12,W,,V,N*64
				If MsgID = "GPGLL" Then
 				'StatusMsg "Found GPGLL"
					Input F1, MsgID, GLL_lat, GLL_ns, GLL_long, GLL_ew,NG,NG, Cksum
				    
				    'Unable to set the GGA_lat and GGA_long directions. The string output N/S/E/W 
				    'seems to come out in a '0 0' form. Perhaps BASIC forces all arguments to be 
				    'same data type as first found when parsing?
			  'Set the #6 output to Longitude
			  '   SetOutputName 6, "Lat"
			  '   SetOutputData 6, GLL_lat
			  '   SetOutputUnits 6, "min"
			
			  'Set the #7 output to Latitude
			  '   SetOutputName 7, "Long"
			  '  SetOutputData 7, GLL_long
			  '  SetOutputUnits 7, "min"
					
				Else
				End If
		End If
	Next
Close F1
End Sub

Sub Stop_Recording
	Close #1
	StatusMsg "Closing AirMar COM Port"
End Sub
