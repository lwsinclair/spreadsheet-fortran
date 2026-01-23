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
C     Updated: 2026-01-23 (Moved from XLMAIN.FOR)
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
C     Commands: QUIT, SAVE filename, OPEN filename, COPY range dest
C     RETURN: Execute command
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

C     Command: QUIT (Q,U,I,T = 81,85,73,84)
      INTEGER QCMD(4)
      DATA QCMD /81,85,73,84/

C     Command: SAVE (S,A,V,E = 83,65,86,69)
      INTEGER SCMD(4)
      DATA SCMD /83,65,86,69/

C     Command: OPEN (O,P,E,N = 79,80,69,78)
      INTEGER OCMD(4)
      DATA OCMD /79,80,69,78/

C     Command: COPY (C,O,P,Y = 67,79,80,89)
      INTEGER CCMD(4)
      DATA CCMD /67,79,80,89/

C     Command: RECALC (R,E,C,A,L,C = 82,69,67,65,76,67)
      INTEGER RCMD(6)
      DATA RCMD /82,69,67,65,76,67/

C     Mode keywords: AUTO (A,U,T,O = 65,85,84,79)
      INTEGER ACMD(4)
      DATA ACMD /65,85,84,79/

C     Mode keywords: MANUAL (M,A,N,U,A,L = 77,65,78,85,65,76)
      INTEGER MCMD(6)
      DATA MCMD /77,65,78,85,65,76/

C     Command: WIDTH (W,I,D,T,H = 87,73,68,84,72)
      INTEGER WCMD(5)
      DATA WCMD /87,73,68,84,72/

C     Command: IR - Insert Row (I,R = 73,82)
      INTEGER IRCMD(2)
      DATA IRCMD /73,82/

C     Command: DR - Delete Row (D,R = 68,82)
      INTEGER DRCMD(2)
      DATA DRCMD /68,82/

C     Command: IC - Insert Column (I,C = 73,67)
      INTEGER ICCMD(2)
      DATA ICCMD /73,67/

C     Command: DC - Delete Column (D,C = 68,67)
      INTEGER DCCMD(2)
      DATA DCCMD /68,67/

C     Command: HELP (H,E,L,P = 72,69,76,80)
      INTEGER HCMD(4)
      DATA HCMD /72,69,76,80/

      INTEGER I, MATCH, ERR, CH
      INTEGER FNAME(80), FLEN
      INTEGER SC1, SR1, SC2, SR2, DC, DR
      INTEGER RMODE
      INTEGER WCOL, WWID
      INTEGER IROW, ICOL

C     Check for RETURN (CR=13 or LF=10, platform-dependent)
      IF (KEY .EQ. 13 .OR. KEY .EQ. 10) THEN
C       Execute command

C       Check for QUIT (4 chars exactly, case insensitive)
        IF (UIBLEN .EQ. 4) THEN
          MATCH = 1
          DO 10 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. QCMD(I)) THEN
              MATCH = 0
            END IF
10        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Quit command
            RUNNING = 0
            RETURN
          END IF
        END IF

C       Check for SAVE filename (case insensitive)
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 20 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. SCMD(I)) MATCH = 0
20        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Extract filename (skip "SAVE ")
            FLEN = UIBLEN - 5
            DO 25 I = 1, FLEN
              FNAME(I) = INBUF(5 + I)
25          CONTINUE

C           Check extension: .xlb = binary, else = JSON
C           Look for .xlb or .XLB at end of filename
            CALL FLXSAV(FNAME, FLEN, ERR)
            IF (ERR .EQ. 0) THEN
              CALL DSPMSG(1)
            ELSE
              CALL DSPMSG(2)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for OPEN filename (case insensitive)
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 30 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. OCMD(I)) MATCH = 0
30        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Extract filename (skip "OPEN ")
            FLEN = UIBLEN - 5
            DO 35 I = 1, FLEN
              FNAME(I) = INBUF(5 + I)
35          CONTINUE

C           Check extension: .xlb = binary, else try both
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
          END IF
        END IF

C       Check for COPY range dest (at least COPY + space + A1:B2 + space + C1 = 14)
        IF (UIBLEN .GE. 10) THEN
          MATCH = 1
          DO 40 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. CCMD(I)) MATCH = 0
40        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Parse COPY command: COPY A1:B5 C1 or COPY A1 C1
            CALL CPYPRS(INBUF, UIBLEN, SC1, SR1, SC2, SR2, DC, DR,
     &                  ERR)
            IF (ERR .EQ. 0) THEN
C             Execute copy
              CALL CPYRNG(SC1, SR1, SC2, SR2, DC, DR)
              CALL DSPMSG(5)
              CALL DSPFUL
            ELSE
              CALL DSPMSG(6)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for RECALC command (case insensitive)
C       /RECALC - force recalculate all
C       /RECALC AUTO - set auto mode
C       /RECALC MANUAL - set manual mode
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 45 I = 1, 6
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. RCMD(I)) MATCH = 0
45        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /RECALC (force recalc)
            IF (UIBLEN .EQ. 6) THEN
C             Force recalculate all cells
              CALL RECALL
              CALL DSPMSG(7)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /RECALC AUTO (7 = space + 4 letters)
            IF (UIBLEN .GE. 11 .AND. INBUF(7) .EQ. 32) THEN
              MATCH = 1
              DO 46 I = 1, 4
                CH = INBUF(7 + I)
                IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
                IF (CH .NE. ACMD(I)) MATCH = 0
46            CONTINUE

              IF (MATCH .EQ. 1) THEN
C               Set AUTO mode
                CALL UIRCST(1)
                CALL DSPMSG(8)
                CUMODE = 1
                UIBLEN = 0
                RETURN
              END IF
            END IF

C           Check for /RECALC MANUAL (7 = space + 6 letters)
            IF (UIBLEN .GE. 13 .AND. INBUF(7) .EQ. 32) THEN
              MATCH = 1
              DO 47 I = 1, 6
                CH = INBUF(7 + I)
                IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
                IF (CH .NE. MCMD(I)) MATCH = 0
47            CONTINUE

              IF (MATCH .EQ. 1) THEN
C               Set MANUAL mode
                CALL UIRCST(2)
                CALL DSPMSG(9)
                CUMODE = 1
                UIBLEN = 0
                RETURN
              END IF
            END IF
          END IF
        END IF

C       Check for WIDTH command (case insensitive)
C       /WIDTH col width - Set column width (e.g., /WIDTH A 12)
        IF (UIBLEN .GE. 7) THEN
          MATCH = 1
          DO 48 I = 1, 5
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. WCMD(I)) MATCH = 0
48        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(6) .EQ. 32) THEN
C           Parse WIDTH command: WIDTH A 12 or WIDTH AA 15
            CALL WIDPRS(INBUF, UIBLEN, WCOL, WWID, ERR)
            IF (ERR .EQ. 0) THEN
C             Set column width
              CALL CWSET(WCOL, WWID)
              CALL DSPMSG(10)
              CALL DSPFUL
            ELSE
              CALL DSPMSG(11)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for IR (Insert Row) command (case insensitive)
C       /IR - Insert row at current cursor position
C       /IR n - Insert row at row n
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 60 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. IRCMD(I)) MATCH = 0
60        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /IR (use cursor row)
            IF (UIBLEN .EQ. 2) THEN
              CALL ROWINS(UIROW)
              CALL DSPMSG(12)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /IR n (parse row number)
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
          END IF
        END IF

C       Check for DR (Delete Row) command (case insensitive)
C       /DR - Delete row at current cursor position
C       /DR n - Delete row n
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 61 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. DRCMD(I)) MATCH = 0
61        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /DR (use cursor row)
            IF (UIBLEN .EQ. 2) THEN
              CALL ROWDEL(UIROW)
              CALL DSPMSG(14)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /DR n (parse row number)
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
          END IF
        END IF

C       Check for IC (Insert Column) command (case insensitive)
C       /IC - Insert column at current cursor position
C       /IC A - Insert column at column A
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 62 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. ICCMD(I)) MATCH = 0
62        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /IC (use cursor column)
            IF (UIBLEN .EQ. 2) THEN
              CALL COLINS(UICOL)
              CALL DSPMSG(15)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /IC col (parse column letter)
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
          END IF
        END IF

C       Check for DC (Delete Column) command (case insensitive)
C       /DC - Delete column at current cursor position
C       /DC A - Delete column A
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 63 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. DCCMD(I)) MATCH = 0
63        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /DC (use cursor column)
            IF (UIBLEN .EQ. 2) THEN
              CALL COLDEL(UICOL)
              CALL DSPMSG(17)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /DC col (parse column letter)
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
          END IF
        END IF

C       Check for HELP command (case insensitive)
        IF (UIBLEN .EQ. 4) THEN
          MATCH = 1
          DO 70 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. HCMD(I)) MATCH = 0
70        CONTINUE

          IF (MATCH .EQ. 1) THEN
            CALL HLPSHW
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for H or ? command (short forms of HELP)
        IF (UIBLEN .EQ. 1) THEN
          CH = INBUF(1)
          IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
          IF (CH .EQ. 72 .OR. CH .EQ. 63) THEN
            CALL HLPSHW
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Unknown command - ignore
        CUMODE = 1
        UIBLEN = 0

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
