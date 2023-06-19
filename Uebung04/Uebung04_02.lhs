
#######################
geometric figures
#######################

primes !! 1
1. primes ruft die Funktion sieve auf, welche wiederrum die Funktion nats nutzt um eine Liste der natürlichen Zahlen 
   ab 2 zu generieren
2. zuerst wird also eine Liste der natürlichen Zahlen ab 2 generiert (2:nats 3)
3. auf diese Liste wird nun sieve angewendet (sieve(2:nats 3))
4. dadurch wird die erste Zahl (2) ausgegeben und gleichzeit aus der Liste nats 3 gefiltert
5. ausformuliert sieht die Liste jetzt folgendermaßen aus: 2: sieve (filter (\x -> x mod 2 /= 0) (nats 3))
6. jetzt wird der nächste Wert aus der Liste nats 3 generierte (2: sieve (filter (\x -> x mod 2 /= 0) (3: nats 4)))
7. nach Anwednung der Filterfunktion auf die Liste steht folgender Ausdruck: 2: sieve (3: filter (\x -> x mod 2 /= 0) (nats 4))
8. nach Anwendung von sieve steht: 2:3: sieve (filter (\x -> x mod 2 /= 0 and x mod 3 /= 0) (nats 4))

Speicher: die Liste liegt jetzt als Ausdruck für primes in der Form 2:3: ... vor, wobei '...' ein lazy Ausdruck für 
   die restliche Liste ist
   In jeder Runde der Berechnung wurde ein Wert für primes (erst 2, dann 3) zurückgegeben, der Rest der List bleibt als
   unendliche Liste ebenfalls im Speicher erhalten

9. Auf die generierte Liste 2:3:... wird nur der Ausdruck !! 1 angewendet ((2:3:...) !! 1)
10. Entsprechend der Definition von !! wird bei einem Indexzugriff größer 1 die Liste und der Indexzugriff 
    um 1 reduziert auf (3:...) !! 0
11. Der Indexzugriff ist nun 0 und somit wird das erste Element der Liste genommen (3)
Das Ergebnis der Auswertung ist also primes !! 1 = 3


Speicher: 
Für die Evaluation des Ausdruckes wird während des Prozesses eine Graph-Struktur erstellt, die die noch auszuwertenden
Ausdrücke repräsentiert. Als Knoten sind dabei die Ausdrücke zu finden und als Edges die Abhängigkeiten zwischend den Ausdrücken.
Ist ein Ausdruck ausgewertet, rückt der entsprechende Ergebniswert an die Stelle des Knotens.
In diesem Fall wird der Graph über die Funktionen nats, filter und sieves aufgebaut und verhält sich wie im Text
als Formel repräsentiert.
