       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKENAPPLIKATION.
       AUTHOR. DEEPSEEK-CHAT.
       DATE-WRITTEN. 2023-11-15.
       DATE-COMPILED. 2023-11-15.
       SECURITY. BANKINTERN.

      * Dieses Programm dient zur Verwaltung von Bankkonten
      * mit verschiedenen Funktionen wie Einzahlung, Auszahlung,
      * Kontostandabfrage und Überweisungen.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KUNDEN-DATEI ASSIGN TO KUNDENDAT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS KUNDEN-KONTONUMMER
               FILE STATUS IS KUNDEN-DATEI-STATUS.
           SELECT TRANSAKTIONS-DATEI ASSIGN TO TRANSAKT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TRANSAKT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD KUNDEN-DATEI
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           DATA RECORD IS KUNDEN-SATZ.
       01 KUNDEN-SATZ.
           05 KUNDEN-KONTONUMMER      PIC X(20).
           05 KUNDEN-NAME             PIC X(50).
           05 KUNDEN-VORNAME          PIC X(50).
           05 KUNDEN-STRASSE          PIC X(50).
           05 KUNDEN-PLZ              PIC X(10).
           05 KUNDEN-ORT              PIC X(50).
           05 KUNDEN-GEBURTSDATUM    PIC X(10).
           05 KUNDEN-KONTOSTAND      PIC S9(10)V99 COMP-3.
           05 KUNDEN-KONTOSTAND-EUR   PIC S9(10)V99 COMP-3.
           05 KUNDEN-KONTOTYP         PIC X(1).
              88 GIROKONTO            VALUE 'G'.
              88 SPARKONTO            VALUE 'S'.
              88 FESTGELDKONTO        VALUE 'F'.
           05 KUNDEN-DISPO-LIMIT      PIC S9(8)V99 COMP-3.
           05 KUNDEN-ZINSEN           PIC S9(3)V99 COMP-3.
           05 KUNDEN-LETZTE-AENDERUNG PIC X(8).
           05 KUNDEN-STATUS           PIC X(1).
              88 AKTIV               VALUE 'A'.
              88 GESPERRT             VALUE 'G'.
              88 GELOESCHT            VALUE 'L'.

       FD TRANSAKTIONS-DATEI
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS
           DATA RECORD IS TRANSAKTIONS-SATZ.
       01 TRANSAKTIONS-SATZ.
           05 TRANSAKT-KONTONUMMER    PIC X(20).
           05 TRANSAKT-DATUM          PIC X(8).
           05 TRANSAKT-ZEIT           PIC X(6).
           05 TRANSAKT-TYP            PIC X(1).
              88 EINZAHLUNG           VALUE 'E'.
              88 AUSZAHLUNG           VALUE 'A'.
              88 UEBERWEISUNG         VALUE 'U'.
              88 ZINSGUTSCHRIFT       VALUE 'Z'.
           05 TRANSAKT-BETRAG        PIC S9(10)V99 COMP-3.
           05 TRANSAKT-REFERENZ       PIC X(20).
           05 TRANSAKT-BEMERKUNG      PIC X(50).

       WORKING-STORAGE SECTION.
       01 DATEI-STATUS-VARIABLEN.
           05 KUNDEN-DATEI-STATUS     PIC X(2).
              88 DATEI-OK             VALUE '00'.
              88 DATEI-EOF             VALUE '10'.
              88 DATEI-NICHT-GEFUNDEN  VALUE '23'.
           05 TRANSAKT-STATUS        PIC X(2).

       01 SYSTEM-VARIABLEN.
           05 AKTUELLES-DATUM        PIC X(8).
           05 AKTUELLE-ZEIT          PIC X(6).
           05 SYSTEM-BENUTZER         PIC X(20).

       01 EINGABE-VARIABLEN.
           05 FUNKTIONSWAHL           PIC X(1).
              88 KONTO-ANLEGEN        VALUE 'A'.
              88 KONTO-AENDERN        VALUE 'E'.
              88 EINZAHLEN            VALUE 'I'.
              88 AUSZAHLEN            VALUE 'O'.
              88 KONTOSTAND            VALUE 'S'.
              88 UEBERWEISUNG-FUNKTION VALUE 'U'.
              88 PROGRAMM-ENDE         VALUE 'X'.
           05 KONTO-EINGABE           PIC X(20).
           05 BETRAG-EINGABE           PIC S9(10)V99.
           05 ZIELKONTO-EINGABE        PIC X(20).
           05 KUNDEN-DATEN-EINGABE.
              10 EING-NAME            PIC X(50).
              10 EING-VORNAME         PIC X(50).
              10 EING-STRASSE          PIC X(50).
              10 EING-PLZ             PIC X(10).
              10 EING-ORT             PIC X(50).
              10 EING-GEBURTSDATUM    PIC X(10).
              10 EING-KONTOTYP         PIC X(1).
              10 EING-DISPO            PIC S9(8)V99.

       01 AUSGABE-VARIABLEN.
           05 MELDUNG                 PIC X(100).
           05 KONTO-AUSGABE.
              10 AUSG-KONTONUMMER     PIC X(20).
              10 AUSG-NAME            PIC X(50).
              10 AUSG-VORNAME         PIC X(50).
              10 AUSG-KONTOSTAND      PIC Z(9).99.
              10 AUSG-KONTOTYP         PIC X(20).
              10 AUSG-DISPO           PIC Z(7).99.
              10 AUSG-STATUS           PIC X(10).

       01 HILFSVARIABLEN.
           05 TEMP-KONTONUMMER        PIC X(20).
           05 TEMP-BETRAG             PIC S9(10)V99.
           05 TEMP-ZIELKONTO          PIC X(20).
           05 DATENSATZ-GEFUNDEN      PIC X(1) VALUE 'N'.
           05 FEHLER-STATUS           PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       HAUPTSTEUERUNG.
           PERFORM DATEIEN-ÖFFNEN
           PERFORM SYSTEMDATEN-HOLEN
           PERFORM HAUPTSCHLEIFE
               UNTIL PROGRAMM-ENDE
           PERFORM DATEIEN-SCHLIESSEN
           STOP RUN.

       DATEIEN-ÖFFNEN.
           OPEN I-O KUNDEN-DATEI
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM ÖFFNEN DER KUNDENDATEI: "
                       KUNDEN-DATEI-STATUS
               MOVE 'J' TO FEHLER-STATUS
           END-IF
           OPEN EXTEND TRANSAKTIONS-DATEI
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM ÖFFNEN DER TRANSAKTIONSDATEI: "
                       TRANSAKT-STATUS
               MOVE 'J' TO FEHLER-STATUS
           END-IF.

       SYSTEMDATEN-HOLEN.
           ACCEPT AKTUELLES-DATUM FROM DATE
           ACCEPT AKTUELLE-ZEIT FROM TIME
           ACCEPT SYSTEM-BENUTZER FROM USER-ID.

       DATEIEN-SCHLIESSEN.
           CLOSE KUNDEN-DATEI
           CLOSE TRANSAKTIONS-DATEI.

       HAUPTSCHLEIFE.
           DISPLAY " "
           DISPLAY "BANKENAPPLIKATION - HAUPTMENÜ"
           DISPLAY "============================"
           DISPLAY "A - KONTO ANLEGEN"
           DISPLAY "E - KONTO ÄNDERN"
           DISPLAY "I - EINZAHLEN"
           DISPLAY "O - AUSZAHLEN"
           DISPLAY "S - KONTOSTAND ANZEIGEN"
           DISPLAY "U - ÜBERWEISUNG"
           DISPLAY "X - PROGRAMM BEENDEN"
           DISPLAY " "
           DISPLAY "IHRE WAHL: " WITH NO ADVANCING
           ACCEPT FUNKTIONSWAHL
           EVALUATE TRUE
               WHEN KONTO-ANLEGEN   PERFORM KONTO-ANLEGEN
               WHEN KONTO-AENDERN   PERFORM KONTO-AENDERN
               WHEN EINZAHLEN       PERFORM EINZAHLEN-FUNKTION
               WHEN AUSZAHLEN       PERFORM AUSZAHLEN-FUNKTION
               WHEN KONTOSTAND      PERFORM KONTOSTAND-ANZEIGEN
               WHEN UEBERWEISUNG-FUNKTION PERFORM UEBERWEISUNG-DURCHFUEHREN
               WHEN PROGRAMM-ENDE   CONTINUE
               WHEN OTHER          DISPLAY "UNGÜLTIGE EINGABE"
           END-EVALUATE.

       KONTO-ANLEGEN.
           DISPLAY " "
           DISPLAY "NEUES KONTO ANLEGEN"
           DISPLAY "==================="
           DISPLAY "KONTONUMMER: " WITH NO ADVANCING
           ACCEPT KONTO-EINGABE
           MOVE KONTO-EINGABE TO KUNDEN-KONTONUMMER
           READ KUNDEN-DATEI
               KEY IS KUNDEN-KONTONUMMER
           IF DATEI-OK
               DISPLAY "KONTO EXISTIERT BEREITS"
           ELSE
               PERFORM KUNDENDATEN-ERFASSEN
               MOVE 'A' TO KUNDEN-STATUS
               MOVE 0 TO KUNDEN-KONTOSTAND
               MOVE 0 TO KUNDEN-KONTOSTAND-EUR
               MOVE 0 TO KUNDEN-ZINSEN
               MOVE AKTUELLES-DATUM TO KUNDEN-LETZTE-AENDERUNG
               WRITE KUNDEN-SATZ
               IF DATEI-OK
                   DISPLAY "KONTO ERFOLGREICH ANGELEGT"
                   PERFORM TRANSAKTION-PROTOKOLLIEREN
                       USING 'N' KONTO-EINGABE 0 "KONTOERÖFFNUNG"
               ELSE
                   DISPLAY "FEHLER BEIM ANLEGEN DES KONTOS: "
                           KUNDEN-DATEI-STATUS
               END-IF
           END-IF.

       KUNDENDATEN-ERFASSEN.
           DISPLAY "NAME: " WITH NO ADVANCING
           ACCEPT EING-NAME
           MOVE EING-NAME TO KUNDEN-NAME
           DISPLAY "VORNAME: " WITH NO ADVANCING
           ACCEPT EING-VORNAME
           MOVE EING-VORNAME TO KUNDEN-VORNAME
           DISPLAY "STRASSE: " WITH NO ADVANCING
           ACCEPT EING-STRASSE
           MOVE EING-STRASSE TO KUNDEN-STRASSE
           DISPLAY "PLZ: " WITH NO ADVANCING
           ACCEPT EING-PLZ
           MOVE EING-PLZ TO KUNDEN-PLZ
           DISPLAY "ORT: " WITH NO ADVANCING
           ACCEPT EING-ORT
           MOVE EING-ORT TO KUNDEN-ORT
           DISPLAY "GEBURTSDATUM (TT.MM.JJJJ): " WITH NO ADVANCING
           ACCEPT EING-GEBURTSDATUM
           MOVE EING-GEBURTSDATUM TO KUNDEN-GEBURTSDATUM
           DISPLAY "KONTOTYP (G=GIRO, S=SPAREN, F=FESTGELD): "
                   WITH NO ADVANCING
           ACCEPT EING-KONTOTYP
           MOVE EING-KONTOTYP TO KUNDEN-KONTOTYP
           IF GIROKONTO
               DISPLAY "DISPORAHMEN: " WITH NO ADVANCING
               ACCEPT EING-DISPO
               MOVE EING-DISPO TO KUNDEN-DISPO-LIMIT
           ELSE
               MOVE 0 TO KUNDEN-DISPO-LIMIT
           END-IF.

       KONTO-AENDERN.
           DISPLAY " "
           DISPLAY "KONTO DATEN ÄNDERN"
           DISPLAY "=================="
           PERFORM KONTO-EINGABE-PRUEFEN
           IF DATENSATZ-GEFUNDEN = 'J'
               PERFORM KUNDENDATEN-ANZEIGEN
               PERFORM KUNDENDATEN-ERFASSEN
               MOVE AKTUELLES-DATUM TO KUNDEN-LETZTE-AENDERUNG
               REWRITE KUNDEN-SATZ
               IF DATEI-OK
                   DISPLAY "KUNDENDATEN ERFOLGREICH GEÄNDERT"
                   PERFORM TRANSAKTION-PROTOKOLLIEREN
                       USING 'A' KONTO-EINGABE 0 "DATENÄNDERUNG"
               ELSE
                   DISPLAY "FEHLER BEIM ÄNDERN DER DATEN: "
                           KUNDEN-DATEI-STATUS
               END-IF
           END-IF.

       EINZAHLEN-FUNKTION.
           DISPLAY " "
           DISPLAY "EINZAHLUNG DURCHFÜHREN"
           DISPLAY "====================="
           PERFORM KONTO-EINGABE-PRUEFEN
           IF DATENSATZ-GEFUNDEN = 'J'
               DISPLAY "AKTUELLER STAND: " KUNDEN-KONTOSTAND
               DISPLAY "BETRAG: " WITH NO ADVANCING
               ACCEPT BETRAG-EINGABE
               IF BETRAG-EINGABE > 0
                   ADD BETRAG-EINGABE TO KUNDEN-KONTOSTAND
                   MOVE AKTUELLES-DATUM TO KUNDEN-LETZTE-AENDERUNG
                   REWRITE KUNDEN-SATZ
                   IF DATEI-OK
                       DISPLAY "EINZAHLUNG ERFOLGREICH"
                       DISPLAY "NEUER STAND: " KUNDEN-KONTOSTAND
                       PERFORM TRANSAKTION-PROTOKOLLIEREN
                           USING 'E' KONTO-EINGABE BETRAG-EINGABE
                                   "EINZAHLUNG"
                   ELSE
                       DISPLAY "FEHLER BEIM EINZAHLEN: "
                               KUNDEN-DATEI-STATUS
                   END-IF
               ELSE
                   DISPLAY "BETRAG MUSS POSITIV SEIN"
               END-IF
           END-IF.

       AUSZAHLEN-FUNKTION.
           DISPLAY " "
           DISPLAY "AUSZAHLUNG DURCHFÜHREN"
           DISPLAY "======================"
           PERFORM KONTO-EINGABE-PRUEFEN
           IF DATENSATZ-GEFUNDEN = 'J'
               DISPLAY "AKTUELLER STAND: " KUNDEN-KONTOSTAND
               DISPLAY "VERFÜGBARER BETRAG: "
                       KUNDEN-KONTOSTAND + KUNDEN-DISPO-LIMIT
               DISPLAY "BETRAG: " WITH NO ADVANCING
               ACCEPT BETRAG-EINGABE
               IF BETRAG-EINGABE > 0
                   IF (KUNDEN-KONTOSTAND + KUNDEN-DISPO-LIMIT)
                       >= BETRAG-EINGABE
                       SUBTRACT BETRAG-EINGABE FROM KUNDEN-KONTOSTAND
                       MOVE AKTUELLES-DATUM TO KUNDEN-LETZTE-AENDERUNG
                       REWRITE KUNDEN-SATZ
                       IF DATEI-OK
                           DISPLAY "AUSZAHLUNG ERFOLGREICH"
                           DISPLAY "NEUER STAND: " KUNDEN-KONTOSTAND
                           PERFORM TRANSAKTION-PROTOKOLLIEREN
                               USING 'A' KONTO-EINGABE BETRAG-EINGABE
                                       "AUSZAHLUNG"
                       ELSE
                           DISPLAY "FEHLER BEIM AUSZAHLEN: "
                                   KUNDEN-DATEI-STATUS
                       END-IF
                   ELSE
                       DISPLAY "NICHT GENÜGEND GUTHABEN"
                   END-IF
               ELSE
                   DISPLAY "BETRAG MUSS POSITIV SEIN"
               END-IF
           END-IF.

       KONTOSTAND-ANZEIGEN.
           DISPLAY " "
           DISPLAY "KONTOSTAND ANZEIGEN"
           DISPLAY "==================="
           PERFORM KONTO-EINGABE-PRUEFEN
           IF DATENSATZ-GEFUNDEN = 'J'
               PERFORM KUNDENDATEN-ANZEIGEN
           END-IF.

       UEBERWEISUNG-DURCHFUEHREN.
           DISPLAY " "
           DISPLAY "ÜBERWEISUNG DURCHFÜHREN"
           DISPLAY "======================"
           DISPLAY "QUELLKONTO: " WITH NO ADVANCING
           ACCEPT KONTO-EINGABE
           MOVE KONTO-EINGABE TO KUNDEN-KONTONUMMER
           READ KUNDEN-DATEI
               KEY IS KUNDEN-KONTONUMMER
           IF DATEI-OK
               MOVE 'J' TO DATENSATZ-GEFUNDEN
               MOVE KUNDEN-KONTONUMMER TO TEMP-KONTONUMMER
               MOVE KUNDEN-KONTOSTAND TO TEMP-BETRAG
               DISPLAY "AKTUELLER STAND: " KUNDEN-KONTOSTAND
               DISPLAY "VERFÜGBARER BETRAG: "
                       KUNDEN-KONTOSTAND + KUNDEN-DISPO-LIMIT
               DISPLAY "ZIELKONTO: " WITH NO ADVANCING
               ACCEPT ZIELKONTO-EINGABE
               MOVE ZIELKONTO-EINGABE TO TEMP-ZIELKONTO
               DISPLAY "BETRAG: " WITH NO ADVANCING
               ACCEPT BETRAG-EINGABE
               IF BETRAG-EINGABE > 0
                   IF (KUNDEN-KONTOSTAND + KUNDEN-DISPO-LIMIT)
                       >= BETRAG-EINGABE
                       MOVE ZIELKONTO-EINGABE TO KUNDEN-KONTONUMMER
                       READ KUNDEN-DATEI
                           KEY IS KUNDEN-KONTONUMMER
                       IF DATEI-OK
                           ADD BETRAG-EINGABE TO KUNDEN-KONTOSTAND
                           MOVE AKTUELLES-DATUM TO KUNDEN-LETZTE-AENDERUNG
                           REWRITE KUNDEN-SATZ
                           IF DATEI-OK
                               MOVE TEMP-KONTONUMMER TO KUNDEN-KONTONUMMER
                               READ KUNDEN-DATEI
                                   KEY IS KUNDEN-KONTONUMMER
                               IF DATEI-OK
                                   SUBTRACT BETRAG-EINGABE
                                       FROM KUNDEN-KONTOSTAND
                                   MOVE AKTUELLES-DATUM
                                       TO KUNDEN-LETZTE-AENDERUNG
                                   REWRITE KUNDEN-SATZ
                                   IF DATEI-OK
                                       DISPLAY "ÜBERWEISUNG ERFOLGREICH"
                                       PERFORM TRANSAKTION-PROTOKOLLIEREN
                                           USING 'U' TEMP-KONTONUMMER
                                           BETRAG-EINGABE
                                           "ÜBERWEISUNG AN " TEMP-ZIELKONTO
                                       PERFORM TRANSAKTION-PROTOKOLLIEREN
                                           USING 'U' TEMP-ZIELKONTO
                                           BETRAG-EINGABE
                                           "ÜBERWEISUNG VON " TEMP-KONTONUMMER
                                   ELSE
                                       DISPLAY "FEHLER BEIM BELASTEN DES QUELLKONTOS"
                                   END-IF
                               ELSE
                                   DISPLAY "FEHLER BEIM LESEN DES QUELLKONTOS"
                               END-IF
                           ELSE
                               DISPLAY "FEHLER BEIM GUTSCHRIFT AUF ZIELKONTO"
                           END-IF
                       ELSE
                           DISPLAY "ZIELKONTO NICHT GEFUNDEN"
                       END-IF
                   ELSE
                       DISPLAY "NICHT GENÜGEND GUTHABEN"
                   END-IF
               ELSE
                   DISPLAY "BETRAG MUSS POSITIV SEIN"
               END-IF
           ELSE
               DISPLAY "QUELLKONTO NICHT GEFUNDEN"
           END-IF.

       KONTO-EINGABE-PRUEFEN.
           MOVE 'N' TO DATENSATZ-GEFUNDEN
           DISPLAY "KONTONUMMER: " WITH NO ADVANCING
           ACCEPT KONTO-EINGABE
           MOVE KONTO-EINGABE TO KUNDEN-KONTONUMMER
           READ KUNDEN-DATEI
               KEY IS KUNDEN-KONTONUMMER
           IF DATEI-OK
               MOVE 'J' TO DATENSATZ-GEFUNDEN
           ELSE
               DISPLAY "KONTO NICHT GEFUNDEN"
           END-IF.

       KUNDENDATEN-ANZEIGEN.
           MOVE KUNDEN-KONTONUMMER TO AUSG-KONTONUMMER
           MOVE KUNDEN-NAME TO AUSG-NAME
           MOVE KUNDEN-VORNAME TO AUSG-VORNAME
           MOVE KUNDEN-KONTOSTAND TO AUSG-KONTOSTAND
           EVALUATE TRUE
               WHEN GIROKONTO      MOVE "GIROKONTO" TO AUSG-KONTOTYP
               WHEN SPARKONTO      MOVE "SPARKONTO" TO AUSG-KONTOTYP
               WHEN FESTGELDKONTO  MOVE "FESTGELDKONTO" TO AUSG-KONTOTYP
           END-EVALUATE
           MOVE KUNDEN-DISPO-LIMIT TO AUSG-DISPO
           EVALUATE TRUE
               WHEN AKTIV     MOVE "AKTIV" TO AUSG-STATUS
               WHEN GESPERRT  MOVE "GESPERRT" TO AUSG-STATUS
               WHEN GELOESCHT MOVE "GELÖSCHT" TO AUSG-STATUS
           END-EVALUATE
           DISPLAY " "
           DISPLAY "KONTOINFORMATIONEN"
           DISPLAY "================="
           DISPLAY "KONTONUMMER: " AUSG-KONTONUMMER
           DISPLAY "NAME: " AUSG-NAME
           DISPLAY "VORNAME: " AUSG-VORNAME
           DISPLAY "KONTOTYP: " AUSG-KONTOTYP
           DISPLAY "KONTOSTAND: " AUSG-KONTOSTAND
           IF GIROKONTO
               DISPLAY "DISPORAHMEN: " AUSG-DISPO
           END-IF
           DISPLAY "STATUS: " AUSG-STATUS.

       TRANSAKTION-PROTOKOLLIEREN.
           MOVE SPACES TO TRANSAKTIONS-SATZ
           MOVE FUNCTION UPPER-CASE(FUNKTIONSWAHL) TO TRANSAKT-TYP
           MOVE KONTO-EINGABE TO TRANSAKT-KONTONUMMER
           MOVE AKTUELLES-DATUM TO TRANSAKT-DATUM
           MOVE AKTUELLE-ZEIT TO TRANSAKT-ZEIT
           MOVE BETRAG-EINGABE TO TRANSAKT-BETRAG
           MOVE "SYSTEM" TO TRANSAKT-REFERENZ
           MOVE MELDUNG TO TRANSAKT-BEMERKUNG
           WRITE TRANSAKTIONS-SATZ
           IF NOT DATEI-OK
               DISPLAY "FEHLER BEIM PROTOKOLLIEREN DER TRANSAKTION"
           END-IF.