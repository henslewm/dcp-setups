' This program supports measuring AIRMAR WEATHER sensor. Assumes sensor has been
' initialized (all strings aside from weather have been shut off) and measures
' sensor at schedule user assigns to measure blocks.
' Set for COM2.
' On power up, Wind speed and direction will not be available until GPS lock
' is achieved.  This could take a minute or two.

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'***'Variables initilized
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
CONST PORT = "COM2:"
CONST PORT_TIMEOUT = 0.5
CONST SENSORNAME = "AIRMAR"        ' Name used on block output and sensors page
Const BAUD = 4800
Const NOPARITY = 0
Const NOHANDSHAKE = 0


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Initialize Sensor
'
' This code runs at recording start. Assumes we have disabled all lines aside
' from the wind, baro, and temperature output line. The Port is opened
' and left open for better performance.
IF AIRMAR_CONNECTED THEN
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
ELSE
ErrorHandler:
      ErrorMsg Format("AIRMAR: Open port %s failed.", PORT)
END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Measure Sensor
'
Public Sub SENSOR_AIRMAR
    'Initialize
 sInBuf = ""
MSGID = 0
TEMPC = 0
PRESIN = 0.0
PRESMB = 0.0
Q1 = "b"
Q2 = "b"
Q3 = "b"
Q4 = "b"
Q5 = "b"
Q6 = "b"
Q7 = "b"
Q8 = "b"
Q9 = "b"
NG = 0.0
N = 0.0
COG = 0.0
DIRT = 0.0
DIRM = 0.0
SPDN = 0.0
SPDM = 0.0
Asterisk = ""
Cksum = 0 
  On Error Resume Next
    '---------------
    'Start
    '---------------
   tStart = Now

  'Read streaming input to allow us to synchronize
      ' Synch string min is $, can also use $WIMDA
       'Here is a typical string from the GPS:
       '$WIMDA,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F
       '123451234512345123451234512345123451234512345123451234512345
       '----------------10-------------20--------------30--------------40-------------50-------------60


Line Input #1, sInBuf 'Should be a complete line
F1 = FreeFile
Open "DataFile.txt" For Output As F1
 Print F1, sInBuf
close F1
Open "Datafile.txt" for Input as F1
'$WIMDA,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F
Input F1, MSGID, PRESIN, Q1, PRESMB, Q2, TEMPC, Q3,  NG, NG, NG, NG, NG, NG, DIRT, Q4, DIRM, Q5, SPDN, Q6, SPDM, Q7, Asterisk, Cksum
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

  Close F1

End Sub

