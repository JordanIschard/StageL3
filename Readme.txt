Le but de ce git est d'implanter une machine abstraite fonctionnelle réactive à partir de la machine SECD. Tout d'abord il a fallu implanter différentes machines et langages. 
Voici une liste de tous ce qu'il se trouve dans notre git ainsi que l'explication des versions alternatives.


Partie sur les langages


Langages étudiées en amont de la création de notre machine concurrente fonctionnelle


1 . lambda.ml : c'est une implantation légère des lambda-calculs.

2 . isim.ml : c'est une implantation du langage iswim totalement fonctionnelle.

3 . isimE.ml : c'est une implantation du langage iswim avec gestion des erreurs.


Langages crée pour les différentes machines

1 . lang_secdCv1.ml : langage utilisé pour la machine SECDCv1

2 . lang_secdCv2.ml : langage utilisé pour la machine SECDCv2

3 . lang_secdCv3.ml : langage utilisé pour la machine SECDCv3

4 . lang_secdCv4.ml : langage utilisé pour la machine SECDCv4

5 . lang_tts.ml : langage utilisé pour toutes les versions de la machine TTS

6 . lang_ttsi.ml : langage utilisé pour toutes les versions de la machine TTSI ainsi que les versions 3 et 4 de la machine TTSIH

7 . lang_ttsih.ml : langage utilisé pour les version 1 et 2 de la machine TTSIH



Partie sur les machines abstraites


Machines étudiées en amont de la création de notre machine concurrente fonctionnelle


1 . machine_cc.ml : c'est une implantation de la machine CC.   

2 . machine_scc.ml : c'est une implantation de la machine SCC. 

3 . machine_ck.ml : c'est une implantation de la machine CK.

4 . machine_cek.ml : c'est une implantation de la machine CEK. 

5 . machine_secd.ml : c'est une implantation de la machine SECD.



Version de la machine SECD avec concurrence de test


1 . machine_secdCv1.ml : c'est une implantation de la machine SECD avec les bases de la concurrence (threads,signaux).

2 . machine_secdCv2.ml : c'est une extension de la machine SECDCv1 avec l'ajout de la gestion d'erreurs.

3 . machine_secdCv3.ml : c'est une extension de la machine SECDCv2 avec l'ajout du partage par valeurs.

4 . machine_secdCv4.ml : c'est une modification de la machine SECDCv3 au niveau des commandes. On enlève les structures complexes de notre chaîne de contrôle.



Version de la machine TTS


1 . machine_ttsv1.ml : c'est une implantation de la machine TTS expliquée dans le rapport post soutenance joint au git.

2 . machine_ttsv2.ml : c'est une modification de la machine TTSv1 au niveau de la règle spawn qui ne prend plus tous les composants du thread courant mais seulement l'environnement et 
                       la chaîne de contrôle.

3 . machine_ttsv3.ml : c'est une extension de la machine TTSv2 avec l'ajout de la récursion ainsi qu'une façon d'implanter différente



Version de la machine TTSI


1 . machine_ttsiv1.ml : c'est une implantation de la machine TTSI expliquée dans le rapport post soutenance joint au git.

2 . machine_ttsiv2.ml : c'est une extension de la machine TTSIv1 avec le rajout de la commande emit

3 . machine_ttsiv3.ml : c'est une extension de la machine TTSIv2 avec l'ajout de la récursion

4 . machine_ttsiv4.ml : c'est une extension de la machine TTSIv3 avec l'ajout des types avec patterns imbriqués

5 . machine_ttsiv5.ml : c'est une extension de la machine TTSIv3 avec l'ajout des types sans patterns imbriqués


Version de la machine TTSIH

1 . machine_ttsihv1.ml : c'est une implantation de la version expliquée en Annexe de la machine TTSI avec gestion d'erreur, sans récursion ni types.

2 . machine_ttsihv2.ml : c'est une extension de la machine TTSIHv1 avec l'ajout de la récursion

3 . machine_ttsihv3.ml : c'est une implantation de la machine TTSIH expliquée dans le rapport post soutenance joint au git avec patterns imbriqués

4 . machine_ttsihv4.ml : c'est une implantation de la machine TTSIH expliquée dans le rapport post soutenance joint au git sans patterns imbriqués



Partie sur les rapports

Il y a deux rapports ainsi qu'un diaporama. Le premier rapport s'arrête une semaine avant la soutenance et le second jusqu'à la fin du stage. Le diaporama quand à lui a servi de support durant la soutenance.
