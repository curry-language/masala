Sign Up
Vorbedingung: Benutzer nicht eingeloggt
Wo: Beliebige Seite
1. Benutzer klickt auf "Sign Up"
2. Anwendung leitet Benutzer weiter auf Registrierungsseite
3. Benutzer gibt E-Mail Adresse, Benutzername und 2 mal Passwort ein
4. Benutzer klickt auf "Sign Up"
5. Anwendung überprüft, dass das zweite Passwort gleich dem ersten ist
6. Anwendung sendet Daten an Server
7. Server überprüft, dass Nutzername und E-Mail frei sind
8. Wenn frei:
    1. Server trägt neuen Benutzer mit diesen Daten ein
    2. Server stellt Nutzer als "Not Trusted" ein
    3. Server stellt Nutzer als "Invalid" ein
    4. Server sendet Bestätigungsemail an Benutzer mit Validierungslink
    5. Benutzer wird über erfolgreiche Registrierung und Bestätigungsemail informiert
9. Wenn nicht frei:
    1. Anwendung informiert Benutzer, ob Benutzername oder E-Mail bereits vergeben ist
    2. Benutzer bleibt auf Registrierungseite
    3. Anwendung behält eingetragene Daten bei außer Passwort
    4. Anwendung markiert fehlerhafte Stelle

**Übergang von Invalid?**

Login
Vorbedingung: Benutzer nicht eingeloggt
Wo: Beliebige Seite
1. Benutzer klickt auf "Login"
2. Anwendung leitet Benutzer weiter auf Loginseite
3. Benutzer gibt wahlweise Benutzername oder E-Mail und Passwort an
4. Anwendung sendet Daten an Server
5. Server überprüft, dass Nutzer vorhanden ist und Passwort korrekt ist
6. Wenn vorhanden:
    1. Wenn korrekt:
        1. Server erstellt neuen Logintoken für Benutzer
        2. Server sendet Bestätigung und Token an Benutzer
        3. Anwendung leitet Benutzer unter Anwendung des Tokens auf Hauptseite weiter
    2. Wenn nicht korrekt:
        1. Anwendung informiert Benutzer darüber, dass Passwort falsch ist
        2. Benutzer bleibt auf Loginseite
        3. Anwendung löscht auf Seite eingetragenes Passwort, Benutzername bleibt
7. Wenn nicht vorhanden:
    1. Anwendung informiert Benutzer darüber, dass ein solcher Nutzer nicht existiert
    2. Benutzer bleibt auf Loginseite
    3. Anwendung löscht auf Seite eingetragene Daten

Logout
Vorbedingung: Benutzer eingeloggt
Wo: Beliebige Seite
1. Benutzer klickt auf "Logout"
2. Server löscht Logintoken
3. Anwendung leitet Benutzer auf Hauptseite ohne Token weiter

Upload Package
Vorbedingung: Benutzer eingeloggt
Wo: Hauptseite
1. Benutzer klickt auf "Upload Package"
2. Anwendung leitet Benutzer weiter auf Uploadseite
3. Benutzer gibt JSON-Datei und tar-Datei an, die das Package ausmachen
4. Benutzer klickt auf "Upload"
5. Anwendung informiert Benutzer, dass Upload permanent sein wird
6. Wenn Benutzer auf "Bestätigen" klickt:
    1. Anwendung überprüft, dass Namen der Dateien übereinstimmen
    2. Wenn übereinstimmen:
        1. Anwendung sendet Namen an Server
        2. Server überprüft, ob Package mit dem Namen schon existiert
        3. Wenn existiert:
            1. Anwendung informiert Benutzer, dass Package bereits existiert
            2. Anwendung informiert Benutzer, dass anderer Name benutzt werden muss oder ein Update hochgeladen werden soll
        4. Wenn nicht existiert:
            1. Server überprüft, ob Benutzer "Trusted" ist
            2. Wenn "Trusted":
                1. Anwendung sendet Datein an Server
                2. Server erstellt neuen Eintrag für neues Package
                3. Neues Package wird auf "Ungeprüft" eingestellt
                4. Benutzer wird als Ersteller eingestellt
                5. Anwendung informiert Benutzer über erfolgreiches Hochladen
            3. Wenn "Untrusted":
                1. Paket wird wird nicht publiziert
                2. Nur Admin kann dies ähnlich wie "Trusted" freischalten.
                3. Anwendung informiert Benutzer, dass dieser auf den
                Admin warten muss, solange noch "Untrusted" ist
                4. Anwendung sendet Email an Admin
    3. Wenn nicht übereinstimmen:
        1. Anwendung informiert Benutzer, dass die Dateien den gleichen Namen haben sollen
7. Wenn Benutzer auf "Abbrechen" klickt:
    1. Anwendung leiter Benutzer auf Uploadseite zurück
    2. Anwendung behält Daten bei

**Übergang von Untrusted -> Trusted?**

**Admin-Funktionalität für User**

**Admin-Funktionalität für Pakete: prüfen, publizieren**

**Benutzerrollen?**

