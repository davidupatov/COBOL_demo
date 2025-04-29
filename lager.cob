       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAGERVERWALTUNG.
       AUTHOR. DEEPSEEK-CHAT.
       DATE-WRITTEN. 2023-11-15.
       DATE-COMPILED. 2023-11-15.
       SECURITY. INTERN.

      * Lagerverwaltungssystem für ein Unternehmen mit folgenden Funktionen:
      * - Artikel anlegen/bearbeiten/löschen
      * - Bestände verwalten
      * - Ein- und Ausgänge buchen
      * - Inventur durchführen
      * - Berichte generieren

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARTIKEL-DATEI ASSIGN TO ARTIKELDAT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ART-NUMMER
               ALTERNATE KEY IS ART-EAN WITH DUPLICATES
               FILE STATUS IS ARTIKEL-STATUS.
           SELECT LAGERBEWEGUNGEN ASSIGN TO LAGERBEW
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BEW-NUMMER
               ALTERNATE KEY IS BEW-DATUM WITH DUPLICATES
               ALTERNATE KEY IS BEW-ARTIKEL WITH DUPLICATES
               FILE STATUS IS BEWEGUNG-STATUS.
           SELECT BERICHTE-DATEI ASSIGN TO LAGERBER
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS BERICHT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARTIKEL-DATEI
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 250 CHARACTERS
           DATA RECORD IS ARTIKEL-SATZ.
       01 ARTIKEL-SATZ.
           05 ART-NUMMER              PIC X(15).
           05 ART-EAN                 PIC X(13).
           05 ART-BEZEICHNUNG         PIC X(50).
           05 ART-KATEGORIE           PIC X(20).
           05 ART-LAGERORT           PIC X(10).
           05 ART-EINHEIT            PIC X(5).
           05 ART-MINDESTBESTAND      PIC 9(5).
           05 ART-AKTUELLER-BESTAND  PIC S9(6).
           05 ART-PREIS              PIC 9(5)V99.
           05 ART-EK-PREIS           PIC 9(5)V99.
           05 ART-MWST-SATZ          PIC 99.
           05 ART-LETZTE-AENDERUNG   PIC X(8).
           05 ART-STATUS             PIC X.
              88 AKTIVER-ARTIKEL      VALUE 'A'.
              88 INAKTIVER-ARTIKEL    VALUE 'I'.
              88 GELOESCHTER-ARTIKEL  VALUE 'G'.
           05 ART-HERSTELLER         PIC X(30).
           05 ART-LIEFERANT          PIC X(30).
           05 ART-LIEFERZEIT         PIC 99.

       FD LAGERBEWEGUNGEN
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           DATA RECORD IS BEWEGUNG-SATZ.
       01 BEWEGUNG-SATZ.
           05 BEW-NUMMER             PIC 9(8).
           05 BEW-TYP                PIC X.
              88 EINGANG             VALUE 'E'.
              88 AUSGANG             VALUE 'A'.
              88 INVENTUR            VALUE 'I'.
              88 KORREKTUR           VALUE 'K'.
           05 BEW-ARTIKEL            PIC X(15).
           05 BEW-DATUM              PIC X(8).
           05 BEW-MENGE              PIC S9(6).
           05 BEW-PREIS              PIC 9(5)V99.
           05 BEW-BENUTZER           PIC X(20).
           05 BEW-REFERENZ           PIC X(20).
           05 BEW-BEMERKUNG          PIC X(50).
           05 BEW-PROJEKT            PIC X(20).
           05 BEW-KOSTENSTELLE       PIC X(10).

       FD BERICHTE-DATEI
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS BERICHT-ZEILE.
       01 BERICHT-ZEILE              PIC X(132).

       WORKING-STORAGE SECTION.
       01 DATEI-STATUS-VARIABLEN.
           05 ARTIKEL-STATUS         PIC X(2).
              88 DATEI-OK            VALUE '00'.
              88 DATEI-EOF           VALUE '10'.
              88 DATEI-NICHT-GEFUNDEN VALUE '23'.
           05 BEWEGUNG-STATUS        PIC X(2).
           05 BERICHT-STATUS         PIC X(2).

       01 SYSTEM-VARIABLEN.
           05 AKT-DATUM              PIC X(8).
           05 AKT-ZEIT               PIC X(6).
           05 AKT-BENUTZER           PIC X(20).
           05 NAECHSTE-BEW-NR        PIC 9(8) VALUE 0.
           05 BERICHTS-KOPFZEILE.
              10 FILLER              PIC X(20) VALUE 'LAGERBERICHT'.
              10 FILLER              PIC X(10) VALUE SPACES.
              10 BER-DATUM           PIC X(8).
              10 FILLER              PIC X(10) VALUE SPACES.
              10 BER-ZEIT            PIC X(6).
              10 FILLER              PIC X(10) VALUE SPACES.
              10 BER-BENUTZER        PIC X(20).
              10 FILLER              PIC X(48) VALUE SPACES.

       01 EINGABE-VARIABLEN.
           05 HAUPTMENU-AUSWAHL      PIC X.
              88 NEUER-ARTIKEL       VALUE 'N'.
              88 ARTIKEL-AENDERN     VALUE 'A'.
              88 ARTIKEL-LOESCHEN    VALUE 'L'.
              88 BESTAND-BUCHEN     VALUE 'B'.
              88 INVENTUR-STARTEN    VALUE 'I'.
              88 BERICHT-DRUCKEN    VALUE 'D'.
              88 PROGRAMM-BEENDEN    VALUE 'X'.
           05 ARTIKEL-EINGABE        PIC X(15).
           05 BEWEGUNGS-DATEN.
              10 BEW-TYP-EINGABE     PIC X.
              10 BEW-MENGE-EINGABE   PIC S9(6).
              10 BEW-PREIS-EINGABE    PIC 9(5)V99.
              10 BEW-REF-EINGABE      PIC X(20).
              10 BEW-BEM-EINGABE      PIC X(50).
           05 INVENTUR-DATEN.
              10 INV-ARTIKEL         PIC X(15).
              10 INV-IST-BESTAND     PIC S9(6).
              10 INV-DIFFERENZ       PIC S9(6).
           05 BERICHT-AUSWAHL        PIC X.
              88 BESTANDSLISTE       VALUE 'B'.
              88 MINDBESTANDSLISTE   VALUE 'M'.
              88 BEWEGUNGSLISTE      VALUE 'W'.
              88 ARTIKELSTAMMLISTE   VALUE 'A'.

       01 AUSGABE-VARIABLEN.
           05 MELDUNG                PIC X(100).
           05 ARTIKEL-ANZEIGE.
              10 ANZ-ARTIKEL         PIC X(15).
              10 ANZ-BEZ             PIC X(50).
              10 ANZ-BESTAND         PIC Z(5)9.
              10 ANZ-MINDBESTAND     PIC Z(5)9.
              10 ANZ-LAGERORT        PIC X(10).
              10 ANZ-STATUS          PIC X(15).
           05 BEWEGUNGS-ANZEIGE.
              10 ANZ-BEW-NR          PIC Z(7)9.
              10 ANZ-BEW-DATUM       PIC X(8).
              10 ANZ-BEW-TYP         PIC X(10).
              10 ANZ-BEW-ARTIKEL     PIC X(15).
              10 ANZ-BEW-MENGE       PIC Z(5)9.
              10 ANZ-BEW-PREIS       PIC Z(5)9.99.
           05 BERICHTS-ZEILEN.
              10 BER-ZEILE-1.
                 15 FILLER           PIC X(5) VALUE SPACES.
                 15 BER-ARTIKEL     PIC X(15).
                 15 FILLER           PIC X(3) VALUE SPACES.
                 15 BER-BEZ          PIC X(30).
                 15 FILLER           PIC X(3) VALUE SPACES.
                 15 BER-BESTAND      PIC Z(5)9.
                 15 FILLER           PIC X(3) VALUE SPACES.
                 15 BER-EINHEIT     PIC X(5).
                 15 FILLER           PIC X(3) VALUE SPACES.
                 15 BER-PREIS       PIC Z(5)9.99.
                 15 FILLER           PIC X(3) VALUE SPACES.
                 15 BER-WERT         PIC Z(7)9.99.
                 15 FILLER           PIC X(20) VALUE SPACES.
              10 BER-ZEILE-2         PIC X(132).

       01 HILFSVARIABLEN.
           05 TEMP-ARTIKEL           PIC X(15).
           05 TEMP-MENGE             PIC S9(6).
           05 TEMP-BESTAND           PIC S9(6).
           05 DATEN-GEFUNDEN         PIC X VALUE 'N'.
           05 FEHLER-STATUS          PIC X VALUE 'N'.
           05 BERICHTS-ZAEHLER       PIC 9(3) VALUE 0.
           05 SEITENZAHL            PIC 9(3) VALUE 0.
           05 ZEILENZAHL             PIC 9(2) VALUE 0.
           05 GESAMT-WERT            PIC 9(8)V99 VALUE 0.

       PROCEDURE DIVISION.
       HAUPTSTEUERUNG.
           PERFORM DATEIEN-OEFFNEN
           PERFORM SYSTEMDATEN-HOLEN
           PERFORM NAECHSTE-BEWEGUNGSNUMMER
           PERFORM HAUPTMENU-ANZEIGEN
               UNTIL PROGRAMM-BEENDEN
           PERFORM DATEIEN-SCHLIESSEN
           STOP RUN.

       DATEIEN-OEFFNEN.
           OPEN I-O ARTIKEL-DATEI
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM ÖFFNEN DER ARTIKELDATEI: "
                       ARTIKEL-STATUS
               MOVE 'J' TO FEHLER-STATUS
           END-IF
           OPEN I-O LAGERBEWEGUNGEN
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM ÖFFNEN DER BEWEGUNGSDATEI: "
                       BEWEGUNG-STATUS
               MOVE 'J' TO FEHLER-STATUS
           END-IF
           OPEN OUTPUT BERICHTE-DATEI
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM ÖFFNEN DER BERICHTSDATEI: "
                       BERICHT-STATUS
               MOVE 'J' TO FEHLER-STATUS
           END-IF.

       SYSTEMDATEN-HOLEN.
           ACCEPT AKT-DATUM FROM DATE
           ACCEPT AKT-ZEIT FROM TIME
           ACCEPT AKT-BENUTZER FROM USER-ID
           MOVE AKT-DATUM TO BER-DATUM
           MOVE AKT-ZEIT TO BER-ZEIT
           MOVE AKT-BENUTZER TO BER-BENUTZER.

       NAECHSTE-BEWEGUNGSNUMMER.
           MOVE 0 TO NAECHSTE-BEW-NR
           START LAGERBEWEGUNGEN KEY IS GREATER THAN BEW-NUMMER
           IF DATEI-OK
               READ LAGERBEWEGUNGEN NEXT RECORD
               IF DATEI-OK
                   ADD 1 TO BEW-NUMMER GIVING NAECHSTE-BEW-NR
               ELSE
                   MOVE 1 TO NAECHSTE-BEW-NR
               END-IF
           ELSE
               MOVE 1 TO NAECHSTE-BEW-NR
           END-IF.

       DATEIEN-SCHLIESSEN.
           CLOSE ARTIKEL-DATEI
           CLOSE LAGERBEWEGUNGEN
           CLOSE BERICHTE-DATEI.

       HAUPTMENU-ANZEIGEN.
           DISPLAY " "
           DISPLAY "LAGERVERWALTUNG - HAUPTMENÜ"
           DISPLAY "==========================="
           DISPLAY "N - NEUER ARTIKEL ANLEGEN"
           DISPLAY "A - ARTIKELDATEN ÄNDERN"
           DISPLAY "L - ARTIKEL LÖSCHEN"
           DISPLAY "B - BESTANDSBUCHUNG"
           DISPLAY "I - INVENTUR"
           DISPLAY "D - BERICHTE DRUCKEN"
           DISPLAY "X - PROGRAMM BEENDEN"
           DISPLAY " "
           DISPLAY "IHRE WAHL: " WITH NO ADVANCING
           ACCEPT HAUPTMENU-AUSWAHL
           EVALUATE TRUE
               WHEN NEUER-ARTIKEL    PERFORM ARTIKEL-ANLEGEN
               WHEN ARTIKEL-AENDERN  PERFORM ARTIKEL-AENDERN
               WHEN ARTIKEL-LOESCHEN PERFORM ARTIKEL-LOESCHEN
               WHEN BESTAND-BUCHEN   PERFORM BESTANDSBUCHUNG
               WHEN INVENTUR-STARTEN PERFORM INVENTUR-DURCHFUEHREN
               WHEN BERICHT-DRUCKEN  PERFORM BERICHTSMENU
               WHEN PROGRAMM-BEENDEN CONTINUE
               WHEN OTHER           DISPLAY "UNGÜLTIGE EINGABE"
           END-EVALUATE.

       ARTIKEL-ANLEGEN.
           DISPLAY " "
           DISPLAY "NEUEN ARTIKEL ANLEGEN"
           DISPLAY "====================="
           DISPLAY "ARTIKELNUMMER: " WITH NO ADVANCING
           ACCEPT ARTIKEL-EINGABE
           MOVE ARTIKEL-EINGABE TO ART-NUMMER
           READ ARTIKEL-DATEI
               KEY IS ART-NUMMER
           IF DATEI-OK
               DISPLAY "ARTIKEL EXISTIERT BEREITS"
           ELSE
               PERFORM ARTIKELDATEN-ERFASSEN
               MOVE 'A' TO ART-STATUS
               MOVE 0 TO ART-AKTUELLER-BESTAND
               MOVE AKT-DATUM TO ART-LETZTE-AENDERUNG
               WRITE ARTIKEL-SATZ
               IF DATEI-OK
                   DISPLAY "ARTIKEL ERFOLGREICH ANGELEGT"
                   PERFORM BEWEGUNG-PROTOKOLLIEREN
                       USING 'N' ARTIKEL-EINGABE 0 0 "ARTIKELANLAGE"
               ELSE
                   DISPLAY "FEHLER BEIM ANLEGEN: " ARTIKEL-STATUS
               END-IF
           END-IF.

       ARTIKELDATEN-ERFASSEN.
           DISPLAY "EAN-CODE: " WITH NO ADVANCING
           ACCEPT ART-EAN
           DISPLAY "BEZEICHNUNG: " WITH NO ADVANCING
           ACCEPT ART-BEZEICHNUNG
           DISPLAY "KATEGORIE: " WITH NO ADVANCING
           ACCEPT ART-KATEGORIE
           DISPLAY "LAGERORT: " WITH NO ADVANCING
           ACCEPT ART-LAGERORT
           DISPLAY "MENGENEINHEIT: " WITH NO ADVANCING
           ACCEPT ART-EINHEIT
           DISPLAY "MINDESTBESTAND: " WITH NO ADVANCING
           ACCEPT ART-MINDESTBESTAND
           DISPLAY "VK-PREIS: " WITH NO ADVANCING
           ACCEPT ART-PREIS
           DISPLAY "EK-PREIS: " WITH NO ADVANCING
           ACCEPT ART-EK-PREIS
           DISPLAY "MWST-SATZ (%): " WITH NO ADVANCING
           ACCEPT ART-MWST-SATZ
           DISPLAY "HERSTELLER: " WITH NO ADVANCING
           ACCEPT ART-HERSTELLER
           DISPLAY "LIEFERANT: " WITH NO ADVANCING
           ACCEPT ART-LIEFERANT
           DISPLAY "LIEFERZEIT (TAGE): " WITH NO ADVANCING
           ACCEPT ART-LIEFERZEIT.

       ARTIKEL-AENDERN.
           DISPLAY " "
           DISPLAY "ARTIKELDATEN ÄNDERN"
           DISPLAY "==================="
           PERFORM ARTIKEL-SUCHEN
           IF DATEN-GEFUNDEN = 'J'
               PERFORM ARTIKELDATEN-ANZEIGEN
               PERFORM ARTIKELDATEN-ERFASSEN
               MOVE AKT-DATUM TO ART-LETZTE-AENDERUNG
               REWRITE ARTIKEL-SATZ
               IF DATEI-OK
                   DISPLAY "ÄNDERUNGEN GESPEICHERT"
                   PERFORM BEWEGUNG-PROTOKOLLIEREN
                       USING 'A' ARTIKEL-EINGABE 0 0 "ARTIKELÄNDERUNG"
               ELSE
                   DISPLAY "FEHLER BEIM SPEICHERN: " ARTIKEL-STATUS
               END-IF
           END-IF.

       ARTIKEL-LOESCHEN.
           DISPLAY " "
           DISPLAY "ARTIKEL LÖSCHEN"
           DISPLAY "==============="
           PERFORM ARTIKEL-SUCHEN
           IF DATEN-GEFUNDEN = 'J'
               IF ART-AKTUELLER-BESTAND NOT = 0
                   DISPLAY "KANN NICHT GELÖSCHT WERDEN - BESTAND NICHT NULL"
               ELSE
                   DISPLAY "ARTIKEL: " ART-BEZEICHNUNG
                   DISPLAY "WIRKLICH LÖSCHEN? (J/N): " WITH NO ADVANCING
                   ACCEPT DATEN-GEFUNDEN
                   IF DATEN-GEFUNDEN = 'J' OR 'j'
                       MOVE 'G' TO ART-STATUS
                       MOVE AKT-DATUM TO ART-LETZTE-AENDERUNG
                       REWRITE ARTIKEL-SATZ
                       IF DATEI-OK
                           DISPLAY "ARTIKEL GELÖSCHT"
                           PERFORM BEWEGUNG-PROTOKOLLIEREN
                               USING 'L' ARTIKEL-EINGABE 0 0 "ARTIKEL-LÖSCHUNG"
                       ELSE
                           DISPLAY "FEHLER BEIM LÖSCHEN: " ARTIKEL-STATUS
                       END-IF
                   ELSE
                       DISPLAY "LÖSCHUNG ABGEBROCHEN"
                   END-IF
               END-IF
           END-IF.

       BESTANDSBUCHUNG.
           DISPLAY " "
           DISPLAY "BESTANDSBUCHUNG"
           DISPLAY "==============="
           DISPLAY "EINGANG (E) ODER AUSGANG (A): " WITH NO ADVANCING
           ACCEPT BEW-TYP-EINGABE
           IF EINGANG OR AUSGANG
               PERFORM ARTIKEL-SUCHEN
               IF DATEN-GEFUNDEN = 'J'
                   DISPLAY "ARTIKEL: " ART-BEZEICHNUNG
                   DISPLAY "AKTUELLER BESTAND: " ART-AKTUELLER-BESTAND
                   DISPLAY "MENGE: " WITH NO ADVANCING
                   ACCEPT BEW-MENGE-EINGABE
                   IF BEW-MENGE-EINGABE > 0
                       DISPLAY "PREIS: " WITH NO ADVANCING
                       ACCEPT BEW-PREIS-EINGABE
                       DISPLAY "REFERENZ: " WITH NO ADVANCING
                       ACCEPT BEW-REF-EINGABE
                       DISPLAY "BEMERKUNG: " WITH NO ADVANCING
                       ACCEPT BEW-BEM-EINGABE
                       IF EINGANG
                           ADD BEW-MENGE-EINGABE TO ART-AKTUELLER-BESTAND
                       ELSE
                           IF ART-AKTUELLER-BESTAND >= BEW-MENGE-EINGABE
                               SUBTRACT BEW-MENGE-EINGABE FROM ART-AKTUELLER-BESTAND
                           ELSE
                               DISPLAY "NICHT GENUG BESTAND"
                               EXIT PARAGRAPH
                           END-IF
                       END-IF
                       MOVE AKT-DATUM TO ART-LETZTE-AENDERUNG
                       REWRITE ARTIKEL-SATZ
                       IF DATEI-OK
                           DISPLAY "BUCHUNG ERFOLGREICH"
                           DISPLAY "NEUER BESTAND: " ART-AKTUELLER-BESTAND
                           PERFORM BEWEGUNG-PROTOKOLLIEREN
                               USING BEW-TYP-EINGABE ARTIKEL-EINGABE
                               BEW-MENGE-EINGABE BEW-PREIS-EINGABE
                               BEW-BEM-EINGABE
                       ELSE
                           DISPLAY "FEHLER BEIM SPEICHERN: " ARTIKEL-STATUS
                       END-IF
                   ELSE
                       DISPLAY "MENGE MUSS POSITIV SEIN"
                   END-IF
               END-IF
           ELSE
               DISPLAY "UNGÜLTIGER BEWEGUNGSTYP"
           END-IF.

       INVENTUR-DURCHFUEHREN.
           DISPLAY " "
           DISPLAY "INVENTUR DURCHFÜHREN"
           DISPLAY "==================="
           PERFORM ARTIKEL-SUCHEN
           IF DATEN-GEFUNDEN = 'J'
               DISPLAY "ARTIKEL: " ART-BEZEICHNUNG
               DISPLAY "BUCHBESTAND: " ART-AKTUELLER-BESTAND
               DISPLAY "IST-BESTAND: " WITH NO ADVANCING
               ACCEPT INV-IST-BESTAND
               COMPUTE INV-DIFFERENZ = INV-IST-BESTAND - ART-AKTUELLER-BESTAND
               DISPLAY "DIFFERENZ: " INV-DIFFERENZ
               DISPLAY "KORREKTUR DURCHFÜHREN? (J/N): " WITH NO ADVANCING
               ACCEPT DATEN-GEFUNDEN
               IF DATEN-GEFUNDEN = 'J' OR 'j'
                   MOVE INV-IST-BESTAND TO ART-AKTUELLER-BESTAND
                   MOVE AKT-DATUM TO ART-LETZTE-AENDERUNG
                   REWRITE ARTIKEL-SATZ
                   IF DATEI-OK
                       DISPLAY "KORREKTUR DURCHGEFÜHRT"
                       PERFORM BEWEGUNG-PROTOKOLLIEREN
                           USING 'I' ARTIKEL-EINGABE INV-DIFFERENZ
                           ART-EK-PREIS "INVENTURKORREKTUR"
                   ELSE
                       DISPLAY "FEHLER BEIM SPEICHERN: " ARTIKEL-STATUS
                   END-IF
               ELSE
                   DISPLAY "KORREKTUR ABGEBROCHEN"
               END-IF
           END-IF.

       BERICHTSMENU.
           DISPLAY " "
           DISPLAY "BERICHTSMENÜ"
           DISPLAY "============"
           DISPLAY "B - BESTANDSLISTE"
           DISPLAY "M - MINDESTBESTANDSLISTE"
           DISPLAY "W - BEWEGUNGSLISTE"
           DISPLAY "A - ARTIKELSTAMMLISTE"
           DISPLAY "ZURÜCK (X)"
           DISPLAY " "
           DISPLAY "IHRE WAHL: " WITH NO ADVANCING
           ACCEPT BERICHT-AUSWAHL
           EVALUATE TRUE
               WHEN BESTANDSLISTE    PERFORM BESTANDSLISTE-DRUCKEN
               WHEN MINDBESTANDSLISTE PERFORM MINDBESTANDSLISTE-DRUCKEN
               WHEN BEWEGUNGSLISTE   PERFORM BEWEGUNGSLISTE-DRUCKEN
               WHEN ARTIKELSTAMMLISTE PERFORM ARTIKELSTAMMLISTE-DRUCKEN
               WHEN OTHER           CONTINUE
           END-EVALUATE.

       ARTIKEL-SUCHEN.
           MOVE 'N' TO DATEN-GEFUNDEN
           DISPLAY "ARTIKELNUMMER: " WITH NO ADVANCING
           ACCEPT ARTIKEL-EINGABE
           MOVE ARTIKEL-EINGABE TO ART-NUMMER
           READ ARTIKEL-DATEI
               KEY IS ART-NUMMER
           IF DATEI-OK
               IF AKTIVER-ARTIKEL
                   MOVE 'J' TO DATEN-GEFUNDEN
               ELSE
                   DISPLAY "ARTIKEL IST NICHT AKTIV"
               END-IF
           ELSE
               DISPLAY "ARTIKEL NICHT GEFUNDEN"
           END-IF.

       ARTIKELDATEN-ANZEIGEN.
           MOVE ART-NUMMER TO ANZ-ARTIKEL
           MOVE ART-BEZEICHNUNG TO ANZ-BEZ
           MOVE ART-AKTUELLER-BESTAND TO ANZ-BESTAND
           MOVE ART-MINDESTBESTAND TO ANZ-MINDBESTAND
           MOVE ART-LAGERORT TO ANZ-LAGERORT
           EVALUATE TRUE
               WHEN AKTIVER-ARTIKEL    MOVE "AKTIV" TO ANZ-STATUS
               WHEN INAKTIVER-ARTIKEL  MOVE "INAKTIV" TO ANZ-STATUS
               WHEN GELOESCHTER-ARTIKEL MOVE "GELÖSCHT" TO ANZ-STATUS
           END-EVALUATE
           DISPLAY " "
           DISPLAY "ARTIKELDATEN"
           DISPLAY "============"
           DISPLAY "ARTIKELNUMMER: " ANZ-ARTIKEL
           DISPLAY "BEZEICHNUNG: " ANZ-BEZ
           DISPLAY "AKTUELLER BESTAND: " ANZ-BESTAND
           DISPLAY "MINDESTBESTAND: " ANZ-MINDBESTAND
           DISPLAY "LAGERORT: " ANZ-LAGERORT
           DISPLAY "STATUS: " ANZ-STATUS.

       BEWEGUNG-PROTOKOLLIEREN.
           MOVE SPACES TO BEWEGUNG-SATZ
           MOVE NAECHSTE-BEW-NR TO BEW-NUMMER
           ADD 1 TO NAECHSTE-BEW-NR
           MOVE BEW-TYP-EINGABE TO BEW-TYP
           MOVE ARTIKEL-EINGABE TO BEW-ARTIKEL
           MOVE AKT-DATUM TO BEW-DATUM
           MOVE BEW-MENGE-EINGABE TO BEW-MENGE
           MOVE BEW-PREIS-EINGABE TO BEW-PREIS
           MOVE AKT-BENUTZER TO BEW-BENUTZER
           MOVE BEW-REF-EINGABE TO BEW-REFERENZ
           MOVE BEW-BEM-EINGABE TO BEW-BEMERKUNG
           WRITE BEWEGUNG-SATZ
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM PROTOKOLLIEREN DER BEWEGUNG"
           END-IF.

       BESTANDSLISTE-DRUCKEN.
           PERFORM BERICHTSKOPF
           MOVE "BESTANDSLISTE" TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BERICHTS-KOPFZEILE
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE "ARTIKELNUMMER  BEZEICHNUNG                  BESTAND  EINH.   PREIS     WERT" TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE 4 TO ZEILENZAHL
           MOVE 0 TO GESAMT-WERT
           MOVE LOW-VALUES TO ART-NUMMER
           START ARTIKEL-DATEI KEY IS GREATER THAN ART-NUMMER
           IF DATEI-OK
               PERFORM UNTIL DATEI-EOF
                   READ ARTIKEL-DATEI NEXT RECORD
                       AT END CONTINUE
                   END-READ
                   IF DATEI-OK
                       IF AKTIVER-ARTIKEL
                           MOVE ART-NUMMER TO BER-ARTIKEL
                           MOVE ART-BEZEICHNUNG TO BER-BEZ
                           MOVE ART-AKTUELLER-BESTAND TO BER-BESTAND
                           MOVE ART-EINHEIT TO BER-EINHEIT
                           MOVE ART-PREIS TO BER-PREIS
                           COMPUTE BER-WERT = ART-AKTUELLER-BESTAND * ART-PREIS
                           ADD BER-WERT TO GESAMT-WERT
                           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
                           ADD 1 TO ZEILENZAHL
                           IF ZEILENZAHL > 55
                               PERFORM BERICHTSFUSS
                               PERFORM BERICHTSKOPF
                               MOVE 4 TO ZEILENZAHL
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           PERFORM BERICHTSFUSS
           DISPLAY "BERICHT WURDE ERSTELLT".

       MINDBESTANDSLISTE-DRUCKEN.
           PERFORM BERICHTSKOPF
           MOVE "MINDESTBESTANDSLISTE" TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BERICHTS-KOPFZEILE
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE "ARTIKELNUMMER  BEZEICHNUNG                  BESTAND  MINDBEST.  DIFF." TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE 4 TO ZEILENZAHL
           MOVE LOW-VALUES TO ART-NUMMER
           START ARTIKEL-DATEI KEY IS GREATER THAN ART-NUMMER
           IF DATEI-OK
               PERFORM UNTIL DATEI-EOF
                   READ ARTIKEL-DATEI NEXT RECORD
                       AT END CONTINUE
                   END-READ
                   IF DATEI-OK
                       IF AKTIVER-ARTIKEL AND
                          ART-AKTUELLER-BESTAND < ART-MINDESTBESTAND
                           MOVE ART-NUMMER TO BER-ARTIKEL
                           MOVE ART-BEZEICHNUNG TO BER-BEZ
                           MOVE ART-AKTUELLER-BESTAND TO BER-BESTAND
                           MOVE ART-MINDESTBESTAND TO BER-EINHEIT
                           COMPUTE BER-WERT = ART-AKTUELLER-BESTAND - ART-MINDESTBESTAND
                           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
                           ADD 1 TO ZEILENZAHL
                           IF ZEILENZAHL > 55
                               PERFORM BERICHTSFUSS
                               PERFORM BERICHTSKOPF
                               MOVE 4 TO ZEILENZAHL
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           PERFORM BERICHTSFUSS
           DISPLAY "BERICHT WURDE ERSTELLT".

       BERICHTSKOPF.
           ADD 1 TO SEITENZAHL
           MOVE BERICHTS-KOPFZEILE TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           MOVE 2 TO ZEILENZAHL.

       BERICHTSFUSS.
           MOVE SPACES TO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           STRING "SEITE: " SEITENZAHL INTO BER-ZEILE-1
           WRITE BERICHT-ZEILE FROM BER-ZEILE-1
           IF GESAMT-WERT > 0
               MOVE SPACES TO BER-ZEILE-1
               STRING "GESAMTWERT: " GESAMT-WERT INTO BER-ZEILE-1
               WRITE BERICHT-ZEILE FROM BER-ZEILE-1
               MOVE 0 TO GESAMT-WERT
           END-IF.