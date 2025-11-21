       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SEQ.
      * Tests write to sequential files
      * Writes 3 records into DIAG.DAT and reads them back
      * Tested on MS Cobol v2.20 and Microfocus Visual Cobol v 2.2
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DIAG-FILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD DIAG-FILE
          LABEL RECORD IS STANDARD
          VALUE OF FILE-ID IS 'DIAG.DAT'
          DATA RECORD IS DiagDetails.

       01 DiagDetails.
           02 DiagCode PIC X(5).
           02 DiagName PIC X(70).
      *
       WORKING-STORAGE SECTION.
       01  READ-EOF PIC 9 VALUE 0.
           88 IS-EOF VALUE IS 1.
      *
       PROCEDURE DIVISION.
      *
       p000-Begin.

           OPEN OUTPUT DIAG-FILE.
           MOVE "J01" TO DiagCode.
           MOVE "Acute sinusitis" TO DiagName.
           WRITE DiagDetails.
           MOVE "J03  Acute tonsillitis" TO DiagDetails.
           WRITE DiagDetails.
           MOVE "J00  Acute nasopharyngitis" TO DiagDetails.
           WRITE DiagDetails.
           CLOSE DIAG-FILE.

      * Read data back
           DISPLAY "Code  Name".
           MOVE 0 TO READ-EOF
           OPEN INPUT DIAG-FILE.
           PERFORM p300-ReadItem UNTIL IS-EOF
           CLOSE DIAG-FILE.
           STOP RUN.

       p300-ReadItem.
           READ DIAG-FILE
              AT END MOVE 1 TO READ-EOF.
           IF NOT IS-EOF
              DISPLAY DiagCode " " DiagName
           ELSE
              DISPLAY "*** End of file ***".
