C     COMMANDS.FOR - Command Handler Module
C
C     Purpose: Parse and execute slash commands
C
C     Commands:
C       /QUIT            - Exit program
C       /SAVE filename   - Save spreadsheet
C       /OPEN filename   - Load spreadsheet
C       /COPY range dest - Copy cells
C       /RECALC          - Force recalculation
C       /RECALC AUTO     - Set auto recalc mode
C       /RECALC MANUAL   - Set manual recalc mode
C       /WIDTH col width - Set column width
C       /IR [n]          - Insert row
C       /DR [n]          - Delete row
C       /IC [col]        - Insert column
C       /DC [col]        - Delete column
C       /HELP or /H or /? - Show help
C
C     Dependencies: UI.FOR, CELLS.FOR, RECALC.FOR, FILES.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C     Updated: 2026-01-23 (Refactored into dispatcher + handlers)
C
C======================================================================

C======================================================================
C     CMDKEY - Handle keystroke in COMMAND mode
C======================================================================
      SUBROUTINE CMDKEY(KEY, RUNNING)
      INTEGER KEY
      INTEGER RUNNING
C     Process keystroke in command mode
C
C     RETURN: Execute command via CMDEXE
C     ESC: Cancel command
C     BACKSPACE: Delete character
C     Printable: Add to buffer

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Check for RETURN (CR=13 or LF=10)
      IF (KEY .EQ. 13 .OR. KEY .EQ. 10) THEN
C       Execute command
        CALL CMDEXE(RUNNING)

C     Check for ESC
      ELSE IF (KEY .EQ. 27) THEN
        CUMODE = 1
        UIBLEN = 0

C     Check for BACKSPACE
      ELSE IF (KEY .EQ. 8 .OR. KEY .EQ. 127) THEN
        IF (UIBLEN .GT. 0) THEN
          UIBLEN = UIBLEN - 1
        END IF

C     Check for printable characters
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
        IF (UIBLEN .LT. 80) THEN
          UIBLEN = UIBLEN + 1
          INBUF(UIBLEN) = KEY
        END IF

      END IF

      RETURN
      END

C======================================================================
C     CMDMCH - Match buffer against command (case-insensitive)
C======================================================================
      SUBROUTINE CMDMCH(BUF, BLEN, CMD, CLEN, MATCH)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER CMD(*)
      INTEGER CLEN
      INTEGER MATCH
C     Compare first CLEN chars of BUF against CMD (case-insensitive)
C     MATCH = 1 if match, 0 otherwise

      INTEGER I, CH

      MATCH = 0
      IF (BLEN .LT. CLEN) RETURN

      DO 10 I = 1, CLEN
        CH = BUF(I)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .NE. CMD(I)) RETURN
10    CONTINUE

      MATCH = 1
      RETURN
      END

C======================================================================
C     CMDEXE - Execute command from buffer
C======================================================================
      SUBROUTINE CMDEXE(RUNNING)
      INTEGER RUNNING

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Command strings
      INTEGER QCMD(4), SCMD(4), OCMD(4), CCMD(4)
      INTEGER RCMDC(6), WCMD(5), HCMD(4)
      INTEGER IRCMD(2), DRCMD(2), ICCMD(2), DCCMD(2)
      DATA QCMD /81,85,73,84/
      DATA SCMD /83,65,86,69/
      DATA OCMD /79,80,69,78/
      DATA CCMD /67,79,80,89/
      DATA RCMDC /82,69,67,65,76,67/
      DATA WCMD /87,73,68,84,72/
      DATA HCMD /72,69,76,80/
      DATA IRCMD /73,82/
      DATA DRCMD /68,82/
      DATA ICCMD /73,67/
      DATA DCCMD /68,67/

      INTEGER MATCH, CH

C     Empty buffer - return to navigation
      IF (UIBLEN .EQ. 0) THEN
        CUMODE = 1
        RETURN
      END IF

C     Try each command in order

C     QUIT (4 chars exactly)
      IF (UIBLEN .EQ. 4) THEN
        CALL CMDMCH(INBUF, UIBLEN, QCMD, 4, MATCH)
        IF (MATCH .EQ. 1) THEN
          CALL CMDQUI(RUNNING)
          RETURN
        END IF
      END IF

C     SAVE filename (SAVE + space + name)
      IF (UIBLEN .GE. 6) THEN
        CALL CMDMCH(INBUF, UIBLEN, SCMD, 4, MATCH)
        IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
          CALL CMDSAV
          RETURN
        END IF
      END IF

C     OPEN filename (OPEN + space + name)
      IF (UIBLEN .GE. 6) THEN
        CALL CMDMCH(INBUF, UIBLEN, OCMD, 4, MATCH)
        IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
          CALL CMDOPN
          RETURN
        END IF
      END IF

C     COPY range dest
      IF (UIBLEN .GE. 10) THEN
        CALL CMDMCH(INBUF, UIBLEN, CCMD, 4, MATCH)
        IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
          CALL CMDCPY
          RETURN
        END IF
      END IF

C     RECALC (with optional AUTO/MANUAL)
      IF (UIBLEN .GE. 6) THEN
        CALL CMDMCH(INBUF, UIBLEN, RCMDC, 6, MATCH)
        IF (MATCH .EQ. 1) THEN
          CALL CMDRCL
          RETURN
        END IF
      END IF

C     WIDTH col width
      IF (UIBLEN .GE. 7) THEN
        CALL CMDMCH(INBUF, UIBLEN, WCMD, 5, MATCH)
        IF (MATCH .EQ. 1 .AND. INBUF(6) .EQ. 32) THEN
          CALL CMDWDT
          RETURN
        END IF
      END IF

C     IR - Insert Row
      IF (UIBLEN .GE. 2) THEN
        CALL CMDMCH(INBUF, UIBLEN, IRCMD, 2, MATCH)
        IF (MATCH .EQ. 1) THEN
          IF (UIBLEN .EQ. 2 .OR. INBUF(3) .EQ. 32) THEN
            CALL CMDIR
            RETURN
          END IF
        END IF
      END IF

C     DR - Delete Row
      IF (UIBLEN .GE. 2) THEN
        CALL CMDMCH(INBUF, UIBLEN, DRCMD, 2, MATCH)
        IF (MATCH .EQ. 1) THEN
          IF (UIBLEN .EQ. 2 .OR. INBUF(3) .EQ. 32) THEN
            CALL CMDDR
            RETURN
          END IF
        END IF
      END IF

C     IC - Insert Column
      IF (UIBLEN .GE. 2) THEN
        CALL CMDMCH(INBUF, UIBLEN, ICCMD, 2, MATCH)
        IF (MATCH .EQ. 1) THEN
          IF (UIBLEN .EQ. 2 .OR. INBUF(3) .EQ. 32) THEN
            CALL CMDIC
            RETURN
          END IF
        END IF
      END IF

C     DC - Delete Column
      IF (UIBLEN .GE. 2) THEN
        CALL CMDMCH(INBUF, UIBLEN, DCCMD, 2, MATCH)
        IF (MATCH .EQ. 1) THEN
          IF (UIBLEN .EQ. 2 .OR. INBUF(3) .EQ. 32) THEN
            CALL CMDDC
            RETURN
          END IF
        END IF
      END IF

C     HELP (4 chars)
      IF (UIBLEN .EQ. 4) THEN
        CALL CMDMCH(INBUF, UIBLEN, HCMD, 4, MATCH)
        IF (MATCH .EQ. 1) THEN
          CALL CMDHLP
          RETURN
        END IF
      END IF

C     H or ? (single char help)
      IF (UIBLEN .EQ. 1) THEN
        CH = INBUF(1)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .EQ. 72 .OR. CH .EQ. 63) THEN
          CALL CMDHLP
          RETURN
        END IF
      END IF

C     Unknown command - return to navigation
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDQUI - Handle QUIT command
C======================================================================
      SUBROUTINE CMDQUI(RUNNING)
      INTEGER RUNNING

      RUNNING = 0
      RETURN
      END

C======================================================================
C     CMDSAV - Handle SAVE command
C======================================================================
      SUBROUTINE CMDSAV

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER FNAME(80), FLEN, I, ERR

C     Extract filename (skip "SAVE ")
      FLEN = UIBLEN - 5
      DO 10 I = 1, FLEN
        FNAME(I) = INBUF(5 + I)
10    CONTINUE

C     Save file
      CALL FLXSAV(FNAME, FLEN, ERR)
      IF (ERR .EQ. 0) THEN
        CALL DSPMSG(1)
      ELSE
        CALL DSPMSG(2)
      END IF

      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDOPN - Handle OPEN command
C======================================================================
      SUBROUTINE CMDOPN

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER FNAME(80), FLEN, I, ERR

C     Extract filename (skip "OPEN ")
      FLEN = UIBLEN - 5
      DO 10 I = 1, FLEN
        FNAME(I) = INBUF(5 + I)
10    CONTINUE

C     Load file
      CALL FLXLOD(FNAME, FLEN, ERR)
      IF (ERR .EQ. 0) THEN
        CALL DSPMSG(3)
        CALL DSPFUL
      ELSE
        CALL DSPMSG(4)
      END IF

      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDCPY - Handle COPY command
C======================================================================
      SUBROUTINE CMDCPY

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER SC1, SR1, SC2, SR2, DC, DR, ERR

C     Parse COPY command
      CALL CPYPRS(INBUF, UIBLEN, SC1, SR1, SC2, SR2, DC, DR, ERR)
      IF (ERR .EQ. 0) THEN
        CALL CPYRNG(SC1, SR1, SC2, SR2, DC, DR)
        CALL DSPMSG(5)
        CALL DSPFUL
      ELSE
        CALL DSPMSG(6)
      END IF

      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDRCL - Handle RECALC command
C======================================================================
      SUBROUTINE CMDRCL

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Mode keywords
      INTEGER ACMD(4), MCMD(6)
      DATA ACMD /65,85,84,79/
      DATA MCMD /77,65,78,85,65,76/

      INTEGER MATCH

C     Just /RECALC - force recalculate all
      IF (UIBLEN .EQ. 6) THEN
        CALL RECALL
        CALL DSPMSG(7)
        CALL DSPFUL
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     Check for /RECALC AUTO
      IF (UIBLEN .GE. 11 .AND. INBUF(7) .EQ. 32) THEN
        CALL CMDMC2(INBUF, UIBLEN, 8, ACMD, 4, MATCH)
        IF (MATCH .EQ. 1) THEN
          CALL UIRCST(1)
          CALL DSPMSG(8)
          CUMODE = 1
          UIBLEN = 0
          RETURN
        END IF
      END IF

C     Check for /RECALC MANUAL
      IF (UIBLEN .GE. 13 .AND. INBUF(7) .EQ. 32) THEN
        CALL CMDMC2(INBUF, UIBLEN, 8, MCMD, 6, MATCH)
        IF (MATCH .EQ. 1) THEN
          CALL UIRCST(2)
          CALL DSPMSG(9)
          CUMODE = 1
          UIBLEN = 0
          RETURN
        END IF
      END IF

C     Unknown subcommand - just do recalc
      CALL RECALL
      CALL DSPMSG(7)
      CALL DSPFUL
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDMC2 - Match buffer at offset against command
C======================================================================
      SUBROUTINE CMDMC2(BUF, BLEN, START, CMD, CLEN, MATCH)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER START
      INTEGER CMD(*)
      INTEGER CLEN
      INTEGER MATCH
C     Compare CLEN chars of BUF starting at START against CMD

      INTEGER I, CH

      MATCH = 0
      IF (BLEN .LT. START + CLEN - 1) RETURN

      DO 10 I = 1, CLEN
        CH = BUF(START + I - 1)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .NE. CMD(I)) RETURN
10    CONTINUE

      MATCH = 1
      RETURN
      END

C======================================================================
C     CMDWDT - Handle WIDTH command
C======================================================================
      SUBROUTINE CMDWDT

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER WCOL, WWID, ERR

C     Parse WIDTH command
      CALL WIDPRS(INBUF, UIBLEN, WCOL, WWID, ERR)
      IF (ERR .EQ. 0) THEN
        CALL CWSET(WCOL, WWID)
        CALL DSPMSG(10)
        CALL DSPFUL
      ELSE
        CALL DSPMSG(11)
      END IF

      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDIR - Handle Insert Row command
C======================================================================
      SUBROUTINE CMDIR

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER IROW, ERR

C     Just /IR - use cursor row
      IF (UIBLEN .EQ. 2) THEN
        CALL ROWINS(UIROW)
        CALL DSPMSG(12)
        CALL DSPFUL
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     /IR n - parse row number
      IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
        CALL PRSNUM(INBUF, UIBLEN, 4, IROW, ERR)
        IF (ERR .EQ. 0 .AND. IROW .GE. 1 .AND. IROW .LE. 255) THEN
          CALL ROWINS(IROW)
          CALL DSPMSG(12)
          CALL DSPFUL
        ELSE
          CALL DSPMSG(13)
        END IF
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     Invalid - show error
      CALL DSPMSG(13)
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDDR - Handle Delete Row command
C======================================================================
      SUBROUTINE CMDDR

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER IROW, ERR

C     Just /DR - use cursor row
      IF (UIBLEN .EQ. 2) THEN
        CALL ROWDEL(UIROW)
        CALL DSPMSG(14)
        CALL DSPFUL
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     /DR n - parse row number
      IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
        CALL PRSNUM(INBUF, UIBLEN, 4, IROW, ERR)
        IF (ERR .EQ. 0 .AND. IROW .GE. 1 .AND. IROW .LE. 255) THEN
          CALL ROWDEL(IROW)
          CALL DSPMSG(14)
          CALL DSPFUL
        ELSE
          CALL DSPMSG(13)
        END IF
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     Invalid - show error
      CALL DSPMSG(13)
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDIC - Handle Insert Column command
C======================================================================
      SUBROUTINE CMDIC

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER ICOL, ERR

C     Just /IC - use cursor column
      IF (UIBLEN .EQ. 2) THEN
        CALL COLINS(UICOL)
        CALL DSPMSG(15)
        CALL DSPFUL
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     /IC col - parse column letter
      IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
        CALL PRSCOL(INBUF, UIBLEN, 4, ICOL, ERR)
        IF (ERR .EQ. 0 .AND. ICOL .GE. 1 .AND. ICOL .LE. 255) THEN
          CALL COLINS(ICOL)
          CALL DSPMSG(15)
          CALL DSPFUL
        ELSE
          CALL DSPMSG(16)
        END IF
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     Invalid - show error
      CALL DSPMSG(16)
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDDC - Handle Delete Column command
C======================================================================
      SUBROUTINE CMDDC

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER ICOL, ERR

C     Just /DC - use cursor column
      IF (UIBLEN .EQ. 2) THEN
        CALL COLDEL(UICOL)
        CALL DSPMSG(17)
        CALL DSPFUL
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     /DC col - parse column letter
      IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
        CALL PRSCOL(INBUF, UIBLEN, 4, ICOL, ERR)
        IF (ERR .EQ. 0 .AND. ICOL .GE. 1 .AND. ICOL .LE. 255) THEN
          CALL COLDEL(ICOL)
          CALL DSPMSG(17)
          CALL DSPFUL
        ELSE
          CALL DSPMSG(16)
        END IF
        CUMODE = 1
        UIBLEN = 0
        RETURN
      END IF

C     Invalid - show error
      CALL DSPMSG(16)
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CMDHLP - Handle HELP command
C======================================================================
      SUBROUTINE CMDHLP

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      CALL HLPSHW
      CUMODE = 1
      UIBLEN = 0

      RETURN
      END

C======================================================================
C     CPYPRS - Parse COPY command arguments
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE CPYPRS(BUF, BLEN, SC1, SR1, SC2, SR2, DC, DR, ERR)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER SC1, SR1, SC2, SR2, DC, DR
      INTEGER ERR

C     Parse: COPY AA1:BB5 CC1 or COPY A1 C1
C     BUF starts with "COPY "

      INTEGER POS, CH

      ERR = 0
      POS = 6       ! Skip "COPY "

C     Parse source start column (1-2 letters)
      SC1 = 0
5     IF (POS .GT. BLEN) GO TO 900
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 7
      SC1 = SC1 * 26 + (CH - 64)
      POS = POS + 1
      GO TO 5
7     IF (SC1 .EQ. 0) GO TO 900

C     Parse source start row
      SR1 = 0
10    IF (POS .GT. BLEN) GO TO 900
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 20
      SR1 = SR1 * 10 + (CH - 48)
      POS = POS + 1
      GO TO 10
20    IF (SR1 .EQ. 0) GO TO 900

C     Check for colon (range) or space (single cell)
      IF (CH .EQ. 58) THEN
C       Range - parse end cell
        POS = POS + 1
        IF (POS .GT. BLEN) GO TO 900

C       Parse source end column (1-2 letters)
        SC2 = 0
25      CH = BUF(POS)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 27
        SC2 = SC2 * 26 + (CH - 64)
        POS = POS + 1
        IF (POS .LE. BLEN) GO TO 25
27      IF (SC2 .EQ. 0) GO TO 900

C       Parse source end row
        SR2 = 0
30      IF (POS .GT. BLEN) GO TO 900
        CH = BUF(POS)
        IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 40
        SR2 = SR2 * 10 + (CH - 48)
        POS = POS + 1
        GO TO 30
40      IF (SR2 .EQ. 0) GO TO 900
      ELSE
C       Single cell - end = start
        SC2 = SC1
        SR2 = SR1
      END IF

C     Expect space before destination
      IF (CH .NE. 32) GO TO 900
      POS = POS + 1

C     Skip extra spaces
50    IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 50
      END IF

C     Parse destination column (1-2 letters)
      DC = 0
55    IF (POS .GT. BLEN) GO TO 57
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 57
      DC = DC * 26 + (CH - 64)
      POS = POS + 1
      GO TO 55
57    IF (DC .EQ. 0) GO TO 900

C     Parse destination row
      DR = 0
60    IF (POS .GT. BLEN) GO TO 70
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 70
      DR = DR * 10 + (CH - 48)
      POS = POS + 1
      GO TO 60

70    IF (DR .EQ. 0) GO TO 900

C     Validate ranges (255 cols = A-IU, 255 rows)
      IF (SC1 .LT. 1 .OR. SC1 .GT. 255) GO TO 900
      IF (SC2 .LT. 1 .OR. SC2 .GT. 255) GO TO 900
      IF (SR1 .LT. 1 .OR. SR1 .GT. 255) GO TO 900
      IF (SR2 .LT. 1 .OR. SR2 .GT. 255) GO TO 900
      IF (DC .LT. 1 .OR. DC .GT. 255) GO TO 900
      IF (DR .LT. 1 .OR. DR .GT. 255) GO TO 900

      RETURN

900   ERR = 1
      RETURN
      END

C======================================================================
C     CPYRNG - Copy range of cells with formula adjustment
C======================================================================
      SUBROUTINE CPYRNG(SC1, SR1, SC2, SR2, DC, DR)
      INTEGER SC1, SR1, SC2, SR2, DC, DR

C     Copy cells from (SC1,SR1):(SC2,SR2) to destination starting at (DC,DR)
C     Adjust relative cell references in formulas

      INTEGER SCOL, SROW, DCOL, DROW
      INTEGER COLDIF, ROWDIF
      INTEGER CTYPE, ALIGN
      REAL VALUE
      INTEGER FSTR(80), FLEN
      INTEGER NFSTR(80), NFLEN
      INTEGER TXTBUF(80), TXTLEN

C     Calculate offset
      COLDIF = DC - SC1
      ROWDIF = DR - SR1

C     Ensure SC1 <= SC2 and SR1 <= SR2
      IF (SC1 .GT. SC2) THEN
        SCOL = SC1
        SC1 = SC2
        SC2 = SCOL
      END IF
      IF (SR1 .GT. SR2) THEN
        SROW = SR1
        SR1 = SR2
        SR2 = SROW
      END IF

C     Copy each cell
      DO 100 SCOL = SC1, SC2
        DO 90 SROW = SR1, SR2
          DCOL = SCOL + COLDIF
          DROW = SROW + ROWDIF

C         Skip if destination out of bounds (255 cols, 255 rows)
          IF (DCOL .LT. 1 .OR. DCOL .GT. 255) GO TO 90
          IF (DROW .LT. 1 .OR. DROW .GT. 255) GO TO 90

C         Get source cell
          CALL CELGET(SCOL, SROW, CTYPE, VALUE)

          IF (CTYPE .EQ. 0) THEN
C           Empty - delete destination
            CALL CELDEL(DCOL, DROW)

          ELSE IF (CTYPE .EQ. 1) THEN
C           Number - copy directly
            CALL CELPUT(DCOL, DROW, 1, VALUE)

          ELSE IF (CTYPE .EQ. 2) THEN
C           Formula - adjust references and copy
            CALL CELGFS(SCOL, SROW, FSTR, 80, FLEN)
            IF (FLEN .GT. 0) THEN
C             Adjust cell references
              CALL CPYADJ(FSTR, FLEN, NFSTR, NFLEN, COLDIF, ROWDIF)
C             Store adjusted formula
              CALL CELPFS(DCOL, DROW, NFSTR, NFLEN)
C             Recalculate
              CALL RECALC(DCOL, DROW)
            END IF

          ELSE IF (CTYPE .EQ. 3) THEN
C           Text - copy directly
            CALL CELGTX(SCOL, SROW, TXTBUF, 80, TXTLEN, ALIGN)
            CALL CELTXT(DCOL, DROW, TXTBUF, TXTLEN, ALIGN)

          END IF

90      CONTINUE
100   CONTINUE

      RETURN
      END

C======================================================================
C     WIDPRS - Parse WIDTH command arguments
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE WIDPRS(BUF, BLEN, COL, WIDTH, ERR)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER COL, WIDTH
      INTEGER ERR

C     Parse: WIDTH A 12 or WIDTH AA 15
C     BUF starts with "WIDTH "

      INTEGER POS, CH

      ERR = 0
      POS = 7       ! Skip "WIDTH "
      COL = 0
      WIDTH = 0

C     Skip extra spaces
5     IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 5
      END IF

C     Parse column letters (1-2 letters, base-26)
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 20
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 10

20    IF (COL .EQ. 0) GO TO 900

C     Skip spaces between column and width
30    IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 30
      END IF

C     Parse width number
40    IF (POS .GT. BLEN) GO TO 50
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 50
      WIDTH = WIDTH * 10 + (CH - 48)
      POS = POS + 1
      GO TO 40

50    IF (WIDTH .EQ. 0) GO TO 900

C     Validate column (1-255) and width (3-40)
      IF (COL .LT. 1 .OR. COL .GT. 255) GO TO 900
      IF (WIDTH .LT. 3 .OR. WIDTH .GT. 40) GO TO 900

      RETURN

900   ERR = 1
      RETURN
      END
