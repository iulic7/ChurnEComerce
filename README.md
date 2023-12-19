# Prognozarea și analiza pierderii clienților din cadrul unei companii bazate pe comerț electronic

Clientul este elementul cel mai important pentru orice întreprindere. De client depinde dacă aceasta se va dezvolta ori va da faliment. Din acest motiv este important de a înțelege de ce unii clienți nu mai sunt interesații în serviciile oferite și ce decizii trebuie să ia compania pentru a menține acești clienți.
	O analiza a pierderii clienților poate arăta zonele slabe a companiei, sau îi va permite să efectuieze decizii ce va opri pierderea clienților.

# Scopul
Analiza și Construirea unui model predictiv care poate identifica clienții care riscă să părăsească compania. 

# Obiectivele
1. Prepararea datelor. Verificarea calității datelor, valorile lipsă.
2. Analiza distribuției datelor pentru a verifica orice valori aberante sau anomalii;
3. Vizualizarea datelor pentru obținerea infomrației despre comportamentul clienților;
4. Selectarea celor mai importante variabile pentru crearea modelului predictiv;
5. Găsirea și aplicarea modeluilui eficient pentru identificarea clienților ce doresc să plece;
6. Evaluarea modelului.


# Descrierea conținutului repozitoriului
Repozitoriul este formt din mapele:
- CoduriPython
- CoduriR
- Data
- Raport
- plots

# CoduriPython
**ModeleDupaCumTrebuie.ipynb** - conține codul pentru preprocesarea setului de date cients.xlsx. A fost efectuată transformarea datelor categoriale prin OneHotEncoding, și scalarea MINMAX. PEntru acestă problema au fost implementate, și evaluate următoarele model:
1. Random Forest
2. Regresia Logisitca
3. Retele neuronale
4. Support vector machine
   
Modele au fost evalute cu ajutorul matricilor de confuzie, curbei ROC, preciziei, recall, f1-score.

# CoduriR
**LucruIndivid.R**- conține toată analizia observațională inițială a setului de date, precum și găsirea unor date dublicate precum Mobile și Mobile Phone etc. În acest fișier este descrisă analiza exploratorie a datelor (AED), în care au fost create diagrame pentru a observa asocierea dintre variabile, precum și influența acestora pentru prezicere.

**ImplementareaMode.R** - conține crearea setului de date pentru antrenare și testare. Au fost folosite metode de determinare a variabilrlor ce sunt cele mia relevante pentru progrnozare. Antrenarea modelelor de Regresie logistică și Random Forest, precum și evaluarea acestora prin curba ROC, acuratețe.

# Date
**E Commerce Datset.xlsx** - conține setul de date inițială.
**clints.xlx** - conține setul de date folosit pentru transformare, și antrenarea modelelor.

# Plots
Conține toate diageamele obținute pe parcursul efectuării cercetării de la EDA pînă la evaluarea modelelor.

# Raport
Conține articolul și prezentarea.









 
