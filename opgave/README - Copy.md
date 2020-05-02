Studenten worden aangemoedigd om zelf nog extra gewenste
functionaliteit te voorzien.
  - [DONE] pldoc server
  - comment alles
  - [NEARLY DONE] server
  - react native
  - [DONE] color name check
  - levels

![img](hex-1.svg)

# ## Bord voorstelling
```yaml
size: 11 * 11
turn: red
tiles: 32
    (B1) -> red
    (A6) -> blue
    (D4) -> red
    (B2) -> red
    (B3) -> red
    (B8) -> blue
    (B6) -> blue
    (E6) -> blue
    (C4) -> red
    (C8) -> blue
    (B7) -> blue
    (D7) -> blue
    (C5) -> red
    (D5) -> red
    (C3) -> red
    (D6) -> red
    (E7) -> red
    (G9) -> red
    (F5) -> blue
    (I7) -> blue
    (H4) -> blue
    (E11) -> red
    (K6) -> blue
    (F7) -> red
    (I6) -> blue
    (G4) -> blue
    (G7) -> red
    (F10) -> red
    (G8) -> red
    (J7) -> blue
    (I4) -> blue
    (I5) -> blue
state: won by blue
orientation: red * blue
```
### Je moet een foutboodschap uitschrijven!
Een bord kan ongeldig zijn als:

- [Done] Het aantal tegels dat aangekondigd is, niet overeenstemt met het effectieve aantal tegels
- [Done] De kleuren van de tegels niet overeenkomen met die in `orientation`
- [Done] De kleuren van de tegels niet matchen met `[a-zA-Z0-9]+`
- [Done] de `size` niet als getallen gescheiden door `*` is ingegeven

## Basis werking

Als het programma wordt uitgevoerd zonder argumenten:
- zoek je naar de beste volgende zet.
- Vervolgens schrijf je die weg naar standaard uitvoer.
- AI, als die in één zet kan winnen,zeker wint.

```bash
cat invoerFile | swipl -f none -t halt -g main -q main.pl
```


## Testen

Als je programma met een extra argument `TEST` wordt uitgevoerd, is de uitvoer
een lijst van alle mogelijke volgende borden gescheiden door regel die alleen
maar "`~`" bevat.

#### Output

![](hex-test-out-1.svg) ![](hex-test-out-2.svg) ![](hex-test-out-3.svg)
![](hex-test-out-4.svg) ![](hex-test-out-5.svg)

```bash
cat invoerFile | swipl -f none -t halt -g main -q main.pl TEST
```

```yaml
state: won by red
turn: blue
tiles: 5
    (C2) -> blue
    (A1) -> red
    (A2) -> red
    (A3) -> red
    (B2) -> blue
size: 3 * 3
orientation: red * blue
~
tiles: 5
    (A2) -> red
    (A1) -> red
    (B2) -> blue
    (B1) -> red
    (C2) -> blue
state: undecided
size: 3 * 3
orientation: red * blue
turn: blue
~
size: 3 * 3
tiles: 5
    (B2) -> blue
    (A1) -> red
    (A2) -> red
    (C1) -> red
    (C2) -> blue
orientation: red * blue
turn: blue
state: undecided
~
tiles: 5
    (A1) -> red
    (A2) -> red
    (B2) -> blue
    (C2) -> blue
    (C3) -> red
size: 3 * 3
turn: blue
state: undecided
orientation: red * blue
~
tiles: 5
    (A1) -> red
    (A2) -> red
    (B2) -> blue
    (B3) -> red
    (C2) -> blue
state: undecided
orientation: red * blue
size: 3 * 3
turn: blue
```

## SVG-voorstelling

Als je programma met `SVG` wordt uitgevoerd print je een geldige SVG van het bord. Als er meerdere borden
zijn, plaats je ze onder elkaar.

Tips:

- Bekijk de SVG's die in deze opgave gebruikt zijn (je mag er van afwijken)
- Met SVG groepen en translatie (`<g transform="translate(...)">`) kun je
  groepen van SVG-elementen verplaatsen.

Volgende commando's dienen een gepaste SVG terug te geven.

```bash
cat invoerFile | swipl -f none -t halt -g main -q main.pl SVG
cat invoerFile | swipl -f none -t halt -g main -q main.pl TEST SVG
cat inputFile | swipl -f none -t halt -g main -q main.pl SVG TEST
```

# Niet functionele eisen

Naast de basisfunctionaliteit zijn er enkele niet functionele eisen waar je
project aan moet voldoen. Deze niet functionele eisen zijn even belangrijk als
de functionele eisen van het project.

- De code moet **goed gedocumenteerd** zijn, er moet commentaar geschreven zijn
  bij elke (logische) regel.
- Je code moet **getest** zijn, dit wil zeggen dat je voor elk van de bewegingen
  zelf een test schrijft, zodat je zeker bent dat de basis functionaliteit
  werkt.
- Je code moet opgesplitst worden in logische modules.
- Schrijf je code zo dat algemene functionaliteit makkelijk kan hergebruikt
  worden.
- Je Con-tac-tix computer moet gebaseerd zijn op een variant van **min-max
  bomen** (alpha-bèta snoeien). We laten uitzonderingen toe op deze regel in
  onderling overleg met de assistent van het vak.
- Het spel moet testbaar zijn met de voorziene code op Ufora (volgt later).
  Projecten die niet testbaar zijn met deze automatische test zullen **automatisch**
  **als onontvankelijk verklaard** worden.

# Verslag

- Inleiding
- Interne bord voorstelling
- Algoritme (met kort voorbeeld)
- Conclusie (wat heb je gerealiseerd en wat beter kan)

# Checklist

Je project is veel meer dan alleen maar de code van de AI, hieronder een
checklist om na te gaan of je alle onderdelen hebt afgewerkt.

- [ ] Con-tac-tix implementatie
- [ ] Code documentatie pldoc
- [ ] Testcode (bijvoorbeeld PlUnit)
- [ ] Test functionaliteit (het TEST argument)
- [ ] Verslag
- [ ] Alles pushen naar `master` op
      `git@subgit.ugent.be:2019-2020/LP/{studentnr}`

# Indienen

## Bestandenstructuur

Je project moet volgende structuur hebben:

- `src/` bevat alle broncode (inclusief `main.pl`).
- `tests/` alle testcode.
- `extra/verslag.pdf` bevat de elektronische versie van je verslag. In deze map
  kun je ook eventueel extra bijlagen plaatsen.

Je directory structuur ziet er dus ongeveer zo uit:

```
|
|-- extra/
|   `-- verslag.pdf
|-- src/
|   |-- main.pl
|   `-- je broncode
`-- tests/
    `-- je testcode
```

## Compileren

De code zal bij het indienen getest worden met de opdracht
`swipl -s src/main.pl` door SubGIT met SWI prolog versie 8.0.3. Op UFora zal de
Dockerfile en bijhorende bronbestanden staan die SubGIT gebruikt om je code te
compileren en minimale testen op uit te voeren. Je kunt deze Docker ook
onmiddellijk van Dockerhub halen met volgende commando's:

```bash
docker pull beardhatcode/lp-project-2019-2020:latest
docker run -it --rm --mount type=bind,source={PAD},destination=/submission,readonly beardhatcode/lp-project-2019-2020:latest
```

Waarbij `{PAD}` vervangen moet worden met het absolute pad naar je project.