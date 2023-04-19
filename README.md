Masterprojekt Masala
====================

Zu realisierende Funktionalitäten:
----------------------------------

- Benutzerverwaltung (Sign up, Login, Role, Admin)
- Packetverwaltung: Eigentümer/Maintainer kann Pakete hochladen
- Eigentümer (und auch Admin) kann Pakete übergeben
- Upload-Benutzer anzeigen: wie in statischen Seiten?
  Generell: Nutzung Masala-Daten in `cpm-manage` (Generierung statischer Seiten)
- Initialisierung: leere Datenbank mit admin-Nutzer, Datenbank aus
  aktuellem CPM füllen (mit admin als Paketeigentümer)
- Anzeige von Paketen (nutze vorhandene cpm-manage Funktionen)
- Suche von Paketen: interaktiv mittels Masala-DB

Vorgehen:
---------

1. Konzept/Use Cases präzisieren:
   Wie meldet man sich an? Wie läd man Pakete hoch? Wie werden diese sichtbar?
   Hierzu z.B. in existierende Paketverwaltungen schauen, z.B.
   [Hackage](https://hackage.haskell.org/upload)

2. Daraus ein ER-Modell ableiten und in Curry formulieren, vgl.
   [ER-Paket](https://cpm.curry-lang.org/pkgs/ertools.html)
   Hierzu kann man sich vielleicht am
   [alten ER-Modell von Masala](https://git.ps.informatik.uni-kiel.de/student-projects/bamapro-2018-ss/-/blob/master/erd.curry)
   orientieren.

3. Aus ER-Modell automatisch mittels [Spicey](https://cpm.curry-lang.org/pkgs/spicey.html)
   eine Web-Anwendung generieren.
   Hierzu vorher das
   [Papier zu Spicey](https://www.informatik.uni-kiel.de/~mh/papers/TPLP14.html)
   lesen.

4. Diese um die spezifische Funktionalität zur Benutzerverwaltung
   (sign in, login, password forgotten,...) erweitern.
   Hierzu kann man vieles aus der
   [Bachelorarbeit zur Benutzerverwaltung](https://git.ps.informatik.uni-kiel.de/theses/2018/2018-mbittermann-ba)
   übernehmen.

5. Diese um die Paket-Funktionalitäten erweitern, z.B. Upload, Eigentümer
   ändern,...
   Hierzu kann man vielleicht etwas aus dem
   [alten Masala](https://git.ps.informatik.uni-kiel.de/student-projects/bamapro-2018-ss)
   übernehmen.

6. ...

