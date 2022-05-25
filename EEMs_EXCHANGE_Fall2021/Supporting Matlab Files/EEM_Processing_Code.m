%% Section 1: Important things to change prior to running the code

%Put any important Notes about the runs that are relevant here
special_notes='Dilution corrections and DOC normalization to 1 mg/L for all data and EEM contours. Exported from MatLab on May 23, 2022.'

%Set Parameters for manual plotting
nrows = 1;
ncols = 1;
clim_min = 0; %minimum fluorescence intensity graphed
clim_max = 0.45 %maximum fluorescence intensity graphed;  use max(eem.X(:),[],'omitnan') to normalize to the maximum peak intesity within the whole dataset
manualscale = false; %Set as true if you want to use the minimum/maximum scales set above. This will apply to all eems. Set to false if you want to autoscale for each individual eem 
autosave = true; %True will autosave all eems; false will no save and will display images individually
figuretitle = true %Will put a title at the top of the EEM based on the Sample Description tab in the sample log

%Is DOC concentration available in the sample log? Put true if yes, put
%false if no
DOCconcentration = true

%Demo - put a copy of the demo dataset in a writeable folder, e.g.
demopath='/Users/myer056/OneDrive - PNNL/Documents/GitHub/EXCHANGE/EEMs_EXCHANGE_Fall2021'; %edit for correct file destination


%% Section 2: Reading in EEMs and Absorbance Files
cd([demopath '/EEMs'])
filetype=3;ext = 'dat';RangeIn='A4..GJ253';headers=[0 1];display_opt=0;outdat=2;
[X,Emmat,Exmat,filelist_eem,outdata]=readineems(filetype,ext,RangeIn,headers,display_opt,outdat);
Ex=Exmat(1,:); %Since all files have the same excitation wavelengths
Em=Emmat(:,1); %Since all files have the same emission wavelengths


cd([demopath '/Absorbance'])
filetype='Abs';ext = 'dat_1_10';RangeIn='A4..J194';display_opt=0;outdat=2;
[S_abs,W_abs,wave_abs,filelist_abs]=readinscans(filetype,ext,RangeIn,display_opt,outdat);

%% Section 3: Reading in Sample Log

% Sample Log
cd(demopath)
[LogNUM,LogTXT]=xlsread('SampleLog.xlsx','log');

%Get filenames and text data from LogTXT
Log_Date=LogTXT(:,2);
Log_SampleDescription=LogTXT(:,3);
Log_SampleID=LogTXT(:,4);
log_IntegrationTime=LogTXT(:,6);
Log_EEMfile=LogTXT(:,7);
Log_ABSfile=LogTXT(:,8);

%Get numeric data from LogNUM
RepNo=LogNUM(:,5);
pathlength=LogNUM(:,9);
df=LogNUM(:,10);
if DOCconcentration
DOCconc=LogNUM(:,11);
end

%% Section 4: Match EEM and Absorbance Files with information from Sample Log

%Pair the EEMs with the other datasets
Pair_EEM_log=[Log_EEMfile Log_EEMfile]; %used for all numeric information in the log
Pair_EEM_abs=[Log_EEMfile Log_ABSfile];

% Obtain matching datasets - from loaded datasets
Sabs=matchsamples(filelist_eem,filelist_abs,Pair_EEM_abs,X,S_abs);% ABS scans that match filelist_eem

% numbers only                                          
replicates=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,RepNo);      % RepNo matching filelist_eem
dilfac=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,df);             % df matching filelist_eem
Abspath=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,pathlength);
if DOCconcentration
DOC=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,DOCconc);
end

% text only - need to remove headers, e.g. Log_Site(2:end,:);  
sample_description=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,Log_SampleDescription(2:end,:));
sampleID=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,Log_SampleID(2:end,:));     % sites matching filelist_eem
dates=matchsamples(filelist_eem,Log_EEMfile(2:end,:),Pair_EEM_log,X,Log_Date(2:end,:));         % dates matching filelist_eem


%% Section 5: Correct Absorbance and EEM files for pathlength, dilutions, and DOC normalizations

Sabst=Sabs.'
Abspatht=Abspath.'
Sabstc=bsxfun(@rdivide,Sabst,Abspatht);
Sabsc=Sabstc.'


% Attach Wavelength headers to scans
A=[wave_abs;Sabsc];         %add wavelengths to Absorbance scans

% Eliminate data below or above the wavelength range of Ex and Em correction files
Em_in=Em(Em<=600); 
X_in=X(:,Em<=600,:); 

% Optional - correct for dilution if necessary
% If samples were diluted before measuring EEMs and Abs scans, divide the
% corrected EEMs by the sorted dilution factors (dilfac NOT df).
XcRU1_df=undilute(X_in,dilfac); %Correct EEMs for Dilutions
Sabsct=Sabsc.'
dilfact=dilfac.'
Sabsct_df=bsxfun(@rdivide,Sabsct,dilfact) %Correct Absorbance for Dilutions
if DOCconcentration
XcRU1_norm=DOCnorm(XcRU1_df,DOC); %Normalize EEMs to 1 mg/L DOC
DOCt=DOC.'
Sabsct_norm=bsxfun(@rdivide,Sabsct_df,DOCt) %Normalize Absorbance to 1 mg/L DOC
Sabsc_norm=Sabsct_norm.'
else
Sabsc_df=Sabsct_df.'
end
%% Section 6: Building Final Sample Set
if DOCconcentration
mydata=assembledataset(XcRU1_norm,Ex,Em_in,'RSU','filelist',filelist_eem,'sampleID',sampleID,'sample_description',sample_description,'rep',replicates,[4]) %Build new 3D Matrix
else
mydata=assembledataset(XcRU1_df,Ex,Em_in,'RSU','filelist',filelist_eem,'sampleID',sampleID,'sample_description',sample_description,'rep',replicates,[4]) %Build new 3D Matrix
end

SubData=subdataset(mydata,[],mydata.Em>550,mydata.Ex<260);
SubData=subdataset(SubData,[],SubData.Em<260,SubData.Ex>450)
Xs=smootheem(SubData,[15 10],[3 3],[50 20],[50 10],[0 1 0 0],[20],3500,''); %Removes 1st and 2nd order Raman Scatter. Attempts to smooth 1st order. Note Rayleigh Scatter removed during instrument processing. 

%% Section 7: Saving Final Figures and Exporting Spectral Indices File
cd([demopath '/Figures'])
manualplots

cd(demopath)
SpectralIndicesExport(1)