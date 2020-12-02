# Stage de L3 et Projet de M2

Ce git regroupe le travail réalisé durant mon stage de 3ème année de licence informatique à l'université d'Orléans nommé **Programmation réactive synchrone et Implantation d’une machine virtuelle**. ainsi que le projet de recherche effectué durant ma seconde année de master qui consistait à résumer l'article **Handling algebraic effects** de *G. Plotkin* et *M. Pretnar*.

## Stage de L3

---
Le but de ce stage est d'implanter une machine abstraite fonctionnelle réactive à partir de la machine abstraite **SECD**. Dans un premier temps une recherche bibliographique et une mise à niveau sur les lambda-calculs fût nécessaire. Après cela un peu de pratique histoire de bien cerner les enjeux et la logique d'une machine abstraite. Et enfin, après s'être approprié les concepts de la machine **SECD** on a fini par la modifier afin d'y implémenter les ajouts demandés.

### Recherche bibliographique

---
Voici la bibliographie principale qui m'a permis de mener à bien mon stage.

- Réactivité des systèmes coopératifs : le cas Réactive ML de Louis Mandrel et Cédric Pasteur
- The ZINC experiment : an economical implementation of the ML language de Xavier Leroy
- Programming Languages And Lambda Calculis de Mathias Felleisen et Matthew Flatt

### Mise en pratique

---
Il était nécessaire de s'entraîner à implémenter les langages ainsi que les machines abstraites afin de cerner toutes la difficulté d'un tel exercice. Pour cela, les langages ainsi que les machines abstraites étudiés ont été implémenté en *OCaml* dans leurs fichiers éponymes.

1. `lambda.ml`

2. `iswim.ml`

3. `iswimE.ml`

4. `machine_cc.ml`

5. `machine_scc.ml`

6. `machine_ck.ml`

7. `machine_cek.ml`

8. `machine_secd.ml`

### Création de notre propre langage et machine abstraite à partir de la machine **SECD**

---
Le résultat final ne sait pas fait en une fois, il y a eu plusieurs versions avec leurs lots de bon points et de défauts. Chaque version du langage et de la machine abstraite ont été gardé afin de retracer les avancés et les difficultés ressentis durant le stage. Le contenu de chaque fichier est expliqué brièvement. Tous les fichiers commençant par `lang_` est un fichier qui contient le langage de la machine qui lui est lié (toutes exceptions sera précisé).

1. `machine_secdCv1.ml` : Machine **SECD** avec les bases de la concurrence (threads,signaux).

2. `machine_secdCv2.ml` : Extension de la machine SECDCv1 avec l'ajout de la gestion d'erreurs.

3. `machine_secdCv3.ml` : Extension de la machine SECDCv2 avec l'ajout du partage par valeurs.

4. `machine_secdCv4.ml` : Modification de la machine SECDCv3 au niveau des commandes. On enlève les structures complexes de notre chaîne de contrôle.

5. `machine_ttsv1.ml` : Assez de modifications ont été apporté pour donner un nom propre à notre machine : **TTS**. Son fonctionnement est expliqué dans le rapport.

6. `machine_ttsv2.ml` : Modification de la machine TTSv1 au niveau de la commande *spawn* qui ne prend plus tous les composants du thread courant mais seulement l'environnement et la chaîne de contrôle.

7. `machine_ttsv3.ml` : Extension de la machine TTSv2 avec l'ajout de la récursion ainsi qu'une revisite de notre implémentation.

8. `machine_ttsiv1.ml` : Rajout des identifiants pour les threads ce qui implique un renommage de notre machine : **TTSI**. Son fonctionnement est expliqué dans le rapport.

9. `machine_ttsiv2.ml` : Extension de la machine TTSIv1 avec le rajout de la commande *emit*.

10. `machine_ttsiv3.ml` : Extension de la machine TTSIv2 avec l'ajout de la récursion.

11. `machine_ttsiv4.ml` : Extension de la machine TTSIv3 avec l'ajout des types __*avec patterns imbriqués*__.

12. `machine_ttsiv5.ml` : Extension de la machine TTSIv3 avec l'ajout des types __*sans patterns imbriqués*__.

13. `machine_ttsihv1.ml` : Version de la machine TTSI avec gestion d'erreur, sans récursion ni types (nommé **TTSIH**). Elle a toutefois été abandonné à partir de la version 2 (utilise le langage `lang_ttsih.ml`).

14. `machine_ttsihv2.ml` : Extension de la machine TTSIHv1 avec l'ajout de la récursion. Elle a toutefois été abandonné à partir de la version (utilise le langage `lang_ttsih.ml`).

15. `machine_ttsihv3.ml` : Version alternative de la machine TTSIH (cette fois-ci gardée) __*avec patterns imbriqués*__. Son fonctionnement est expliqué dans le rapport post-soutenance (utilise le langage `lang_ttsi.ml`).

16. machine_ttsihv4.ml : Version alternative de la machine TTSIH (cette fois-ci gardée) __*sans patterns imbriqués*__. Son fonctionnement est expliqué dans le rapport post-soutenance. (utilise le langage `lang_ttsi.ml`).

Afin de tous comprendre de la démarche deux rapports existent : un pre-soutenance et un post-soutenance. Un diaporama utilisé durant la soutenance est aussi présent.

## Projet de M2

---
Le projet recherche consiste à lire, comprendre et résumer un article scientifique proposé par un ou plusieurs enseignants. J'ai choisie de m'essayer à la lecture de l'article **Handling algebraic effects** de *G. Plotkin* et *M. Pretnar*. Mon aventure dans la découverte de ce document et sa compréhension est résumé dans le pdf `resumer.pdf`. Un travail supplémentaire est requis. Pour ma part ce fût de mettre en relation le travail effectué avec *Mme. Bousdira* et *M. Dabrowski* durant 6 mois sur un noyau de langage fonctionnel implémentant les effets algébriques et la vision des effets algébriques dans le papier. Tout est expliqué dans le pdf.
