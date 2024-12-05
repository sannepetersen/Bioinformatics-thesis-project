# Kinetic data analysis
For the kinetic experiments the files are named exp1... up to and until exp14 .... This contains the raw data as csv file. 
To read the raw data and look at the plots and calculate GR etc. the R file called 96-well plate per row.R is used. 
There are separate files to analyse and plot the GR and yield distribution for the washed vs not washed experiment,
the HEPES experiment, the water experiment, the different initial dilutions experiment, the reproduction experiment and the
the salt experiments. 
There is a separate R file with also the data of one measurement bench experiment (exp 13).

# Broad growth screen analysis
Start with opening the file Loading data.R where the control data can be loaded and plotted from (no strain growth). 
Also the raw data which contains columns Value (Replicate 1) and y (Replicate 2) can be plotted from Loading data.R. 
Loading data .R also loads all.data.info.csv where the information of the calculated growth rates and yields is loaded and can be plotted. 

Following this, start by following the R files from 1 up to and until 12 for all data analyses. 
CSV files A up to and until T are the raw data from the plates. These are the conditions tested:

A1 Ammonium + C
A2 Nitrite + C
B1 Nitrate + C
B2 Alanine + C 
C1 Glycine + C 
C2 Serine + C 
D1 Glutamate + C 
D2 Proline + C 
E1 Aspartate + C 
E2 Glucosamine + C 
F1 no N control + C
F2 Urea + C 
G1 Threonine + C 
G2 Glutamine + C
H1 Arginine + C 
H2 Asparagine + C
I1 GlcNAc + C
I2 Alanine - C
J1 Glycine - C
J2 Serine - C
K1 Glutamate - C
K2 Proline - C
L1 Aspartate - C
L2 Glucosamine - C
M1 Threonine - C
M2 MB
N1 Glutamine - C
N2 Arginine - C
O1 Asparagine - C
O2 GlcNAc - C
P1 Alanine + N
P2 Glycine + N
Q1 Serine + N
Q2 Glutamate + N
R1 Proline + N
R2 Aspartate + N
S1 Asparagine + N
S2 Threonine + N
T1 Glutamine + N
T2 Arginine + N
