'-------------------------------------------------------------------------------
'     Iridium SBD transmit.BAS - BASIC code to push GOES messages out via SBD
'
'     This routine must be schedule to run after the GOES message has been sent
'     and the RECENT.DAT file has been posted.  Typically you would schedule
'     this for about one minute after the GOES time.
'
'     Right now, this is set up for an Iridium modem on COM8:
'
'     NOTE:  ATE0 has been wired into the code as of 7/18/07 TNK
'        This version deletes the Wallops-style header
'        but includes the PIN entry.  It also includes retry on buffer clear
'
'-------------------------------------------------------------------------------
CONST True = -1
CONST False = 0
CONST LoggingIn=2
CONST LoggedIn=4
CONST LF = Chr(10)                 ' Line feed character
CONST CR = Chr(13)                 ' Carriage return character
CONST RegTimeout = 300             ' timeout to register
CONST SendTimeout = 300            ' timeout to send
CONST CharQuote = Chr(34)      ' Quote (to send quote character inside of a string)

Public DECLARE SUB FlushPort(HANDLE)
Public DECLARE FUNCTION OpenPort(ComPort, RejectWhen)
Public DECLARE SUB ClosePort(HANDLE)
Public DECLARE FUNCTION ReadPort(HANDLE, BytesToRead, BytesRead, BytesRemain, TimeoutSec)
Public DECLARE SUB WritePort(HANDLE, DATA)
Public DECLARE FUNCTION HayesCommand(HANDLE, Command, TimeoutSec)
Public DECLARE SUB SetDTRPort(HANDLE)
Public DECLARE SUB ClrDTRPort(HANDLE)
Public DECLARE SUB SetRTSPort(HANDLE)
Public DECLARE SUB ClrRTSPort(HANDLE)
Public DECLARE SUB SetBreakPort(HANDLE)
Public DECLARE SUB ClrBreakPort(HANDLE)
Public DECLARE FUNCTION CDPort(HANDLE)

'-------------------------------------------------------------------------------
'     SCHED_SendSBD - scheduled routine that sends Iridium short burst
'                     data string
'
'     This subroutine is a modification of the code used by Sutron for the
'     Corps of Engineers in New Orleans
'
'-------------------------------------------------------------------------------
Public SUB Sched_SendSBD

    'Data transmission routine
    'This sets up the modem and pushes out a data string -- in this case,
    'RECENT.DAT, which is recovered from the flash disk by the
        'GetMostRecent function.

    HANDLE = OpenPort("COM8:", LoggingIn OR LoggedIn)
    'Handle = OpenPort("COM8:", 0) 'use this to openport even if in use.

    IF HANDLE THEN
                'StatusMsg "Iridium transmission started"

                CALL SetDTRPort(HANDLE)
        SLEEP(5) ' Allow the modem time to wake up
        ' wait for the modem to be registered
        '
        'Make sure Echo is off -- parser fails if echo is on
        'StatusMsg "ATE0"
        txt = HayesCommand(HANDLE, "ATE0", 2)


                ' Check the signal strength and record with status
        ' Note that book says not to do this until we are registered
                txt = HayesCommand(HANDLE, "AT+CSQ", 10)
        'The 10 second timeout is because it takes a while to get signal
                StatusMsg "Iridium - Signal Strength - starting = " & txt

        'clear SBD Message Buffer
                iTry = 0
                BufferCleared = false
                DO
                     iTry = iTry + 1
             'StatusMsg "AT+SBDD0"
             txt = HayesCommand(HANDLE, "AT+SBDD0", 3)
                     IF RIGHT(txt,3) = "0OK" THEN
                       BufferCleared = true
                     ELSE
                        SLEEP 1
                     END IF
             IF iTry > 5 THEN
                ErrorMsg "Command AT+SBDD failed on retries " &txt
                GOTO ErrorHandler
             END IF
                LOOP UNTIL (BufferCleared OR iTry > 5)

        IF 0 = 1 THEN
           'send some text to the message buffer
           'StatusMsg "AT+SDBWT"
           txt = HayesCommand(HANDLE, "AT+SBDWT=SBDWT Text Test", 2)
           IF txt="" OR txt <>"OK" THEN
              ErrorMsg "Command AT+SBDWT failed " &txt
              GOTO ErrorHandler
           END IF
        ELSE

                   'Set the binary transmit string to the RECENT.DAT value
                   'returned by GetMostRecent function

                   'Look for Recent.dat
                   iDatFile = FREEFILE
                   strMsg = "No recent data"
                   strDatFileName = "\Flash Disk\RECENT.DAT"
                   OPEN strDatFileName FOR INPUT AS iDatFile
                   ON ERROR GOTO 100
                   LINE INPUT IDatFile, strMsg
               ON ERROR RESUME NEXT
100                CLOSE iDatFile

           BinString = strMsg
           LenData = Format("%d", LEN(BinString))
           Result = ComputeCRC(0,0,BinString) MOD 65536'
           'StatusMsg "CRC " &(Result>>8) &" " &(Result Mod 256)
           BinString = BinString & Chr(Result>>8) & Chr(Result MOD 256)
           Cmd = "AT+SBDWB="&LenData
           Txt = HayesCommand(HANDLE, Cmd, 2)
           IF LEFT(Txt, 5)="READY" THEN
              Txt = HayesCommand(HANDLE, BinString, 2)
              'StatusMsg "SBDWB: " &txt
              IF LEFT(Txt,1) <> "0" THEN
                 ErrorMsg "Command SBDWB failed " &Txt
                 GOTO ErrorHandler
              END IF
           END IF
        END IF
        SLEEP 10
        'Initiate SBD Session to send the data
        StartTime = Time
        DataSent = false
                iTry = 0
        DO
           'statusmsg "AT+SBDI"
                   iTry = iTry + 1
                   StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to send SBD Message"
           txt = HayesCommand(HANDLE, "AT+SBDI", 15)
           'StatusMsg "SBDI=" &txt
           IF LEFT(txt, 8) = "+SBDI: 1" THEN
              DataSent = true
                   ELSE
               SLEEP 1
                       IF iTry > 10 THEN
                  StatusMsg "SBDI retry count exceeded"
                          GOTO errorhandler
                       END IF
           END IF
           IF CDPort(HANDLE) <> 0 THEN
                StatusMsg "CD active on modem ... abort"
                GOTO errorhandler
           END IF
        LOOP UNTIL (DataSent OR ((Time - StartTime)> SendTimeout))
        IF ((Time - StartTime) > SendTimeout) THEN
           ErrorMsg "SBDI timeout " &txt
           GOTO ErrorHandler
        END IF
        StatusMsg "Iridium - SBD transmission done"

                ' Check the signal strength and record with status
                txt = HayesCommand(HANDLE, "AT+CSQ", 8)
                StatusMsg "Iridium - Signal Strength - ending = " & txt

ErrorHandler:   'Label for branch from errors

        'Call ClrDTRPort(Handle)
        CALL ClosePort(HANDLE)

    ELSE    'End of check for valid handle to Iridium

        ErrorMsg "Could not access the Iridium Modem"

    END IF

END SUB
