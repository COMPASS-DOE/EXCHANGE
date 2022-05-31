function [] = SpectralIndicesExport(PathLength);
% Calculate and export absorbance and fluorescence indices out of the
% Aqualog absorbance (*ABS.dat) and FDOM processed (*PEM.dat) files.
%%%%%%%%%%%%%%%%%%%%%
%Input variables
%%%%%%%%%%%%%%%%%%%%%
%
% 1.    Pathlength: The absorbance and fluoresence pathlength used during the
%       analysis on the Aqualog, in cm.
%
%%%%%%%%%%%%%%%%%%%%%%%       
% Created by: 
%
% François Guillemette
% Earth, Ocean and Atmospheric Science
% Florida State University
% Tallahassee, Florida, USA
% guillemette@magnet.fsu.edu
%
%Based on: 
%The FDOMcorr toolbox for MATLAB (Murphy et al. 2010)
%   'The measurement of dissolved organic matter fluorescence 
% 	in aquatic environments: An interlaboratory comparison'
% 	Environmental Science & Technology, doi: 10.1021/es102362t 
%
% Copyright (C) 2011 KR Murphy,
% Water Research Centre
% The University of New South Wales
% Department of Civil and Environmental Engineering
% Sydney 2052, Australia
% krm@unsw.edu.au
%%%%%%%%%%%%%%%%%%%%%%%
%% 1. INITIALISE
pathlen_a=PathLength;
DOCconcentration=evalin('base','DOCconcentration')
nSample = evalin('base','W_abs');
nSample=size(nSample,1);
W_abs = evalin('base','W_abs');
if DOCconcentration
Sabsc = evalin('base','Sabsc_norm');
else
Sabsc=evalin('base','Sabsc_df');
end
X=evalin('base','mydata.X');
Ex=evalin('base','mydata.Ex');
Em=evalin('base','mydata.Em');
sample_description = evalin('base','sample_description');
SampleID=evalin('base','sampleID');
special_notes=evalin('base','special_notes')

%% 2. Absorbance at given WL

WL250=find(W_abs==251);
Abs250=Sabsc(WL250);

WL254=find(W_abs==254);
Abs254=Sabsc(WL254);

WL281=find(W_abs==254);
Abs281=Sabsc(WL281);

WL350=find(W_abs==350);
Abs350=Sabsc(WL350);

WL365=find(W_abs==365);
Abs365=Sabsc(WL365);

WL412=find(W_abs==413);
Abs412=Sabsc(WL412);

WL440=find(W_abs==440);
Abs440=Sabsc(WL440);

%% 3. Abs ratio and Spectral Slopes 

AbsRatio=Abs250./Abs365; %Abs ratio 250/365

if DOCconcentration
SUVA254=Abs254.*100
end

%Spectral Slopes
pathlen_a=repmat(PathLength,1,size(W_abs,1))';  %pathlength assumed to be 1 cm
WL_abs = evalin('base','wave_abs');
S_abs_in_meter=Sabsc./repmat(pathlen_a,1,size(WL_abs,2))*100;
WL275=find(WL_abs==275);
WL296=find(WL_abs==296);
WL350=find(WL_abs==350);
WL401=find(WL_abs==401);
WL275_296=WL_abs(WL275:WL296);
WL350_401=WL_abs(WL350:WL401);


for i=1:nSample,
    Abs275_296=log(S_abs_in_meter(i,WL275:WL296));
    Abs350_401=log(S_abs_in_meter(i,WL350:WL401));
    [p_275_296,s_275_296] = polyfit(WL275_296,Abs275_296,1);
    polydata_275_296 = polyval(p_275_296,WL275_296);
    sstot_275_296 = sum((Abs275_296 - mean(Abs275_296)).^2);
    ssres_275_296 = sum((Abs275_296 - polydata_275_296).^2);
    S275_296_r2(i,:) = 1 - (ssres_275_296 / sstot_275_296);
    S275_296(i,:)= p_275_296(1);
    
    [p_350_401,s_350_401] = polyfit(WL350_401,Abs350_401,1);
    polydata_350_401 = polyval(p_350_401,WL350_401);
    sstot_350_401 = sum((Abs350_401 - mean(Abs350_401)).^2);
    ssres_350_401 = sum((Abs350_401 - polydata_350_401).^2);
    S350_401_r2(i,:) = 1 - (ssres_350_401 / sstot_350_401);
    S350_401(i,:)= p_350_401(1);
end

S275_296=S275_296*-1;
S350_401=S350_401*-1;
Sr=S275_296./S350_401;

%% 4. Fluorescence Peaks and Indices 

outwaves=[350 450.573; ...  %C (humic-like)
    251 450.573; ...        %A (humic-like)
    290 349.513; ...        %T (tryptophan-like)
    269 304.1; ...         %B (tyrposine-like)
    320 411.349];           %M (marine/microbial-like)
            

%Create OutData matrix
outdata=NaN*ones(size(X,1),size(outwaves,1)); %first two rows are headers
for i=1:size(outwaves,1)
    p=X(:,Em==outwaves(i,2),Ex==outwaves(i,1));
    if ~isempty(p)
        outdata(1:end,i)=p;
    end
end



%Fluorescence Peaks
%C Peak: Ex 320-360, Em 420-460
ex1=find(Ex==320)
ex2=find(Ex==359)
em1=find(Em==419.761)
em2=find(Em==459.021)
M=X(:,em1:em2,ex1:ex2)
Cpeak = max(M,[],2:3)

%A Peak: Ex 260, Em 400-460
ex1=find(Ex==260)
em1=find(Em==399.064)
em2=find(Em==459.021)
M=X(:,em1:em2,ex1)
Apeak=max(M,[],2:3)

%T Peak: Ex 275, Em 330-350
ex1=find(Ex==275)
em1=find(Em==330.544)
em2=find(Em==351.022)
M=X(:,em1:em2,ex1)
Tpeak=max(M,[],2:3)

%B peak: Ex 275, Em 295-315
ex1=find(Ex==275)
em1=find(Em==294.313)
em2=find(Em==314.665)
M=X(:,em1:em2,ex1)
Bpeak=max(M,[],2:3)

%M peak, Ex 290-310, Em 370-410
ex1=find(Ex==290)
ex2=find(Ex==311)
em1=find(Em==369.282)
em2=find(Em==410.555)
M=X(:,em1:em2,ex1:ex2)
Mpeak=max(M,[],2:3)


%Fluorescence Indices

%Fluorescence Index (FI) Ex 370 nm, Em 470/520
ex1=find(Ex==371);
em1=find(Em==470.608);
em2=find(Em==519.457);
Fim=X(:,em1,ex1);
Fix=X(:,em2,ex1);
FI=Fim./Fix;

%Freshness Index (Fresh) Ex 310, Em 380/[Max (420-435)]
ex1=find(Ex==311);
em1=find(Em==380.721);
em2=find(Em==419.761);
em3=find(Em==435.901);
Frm1=X(:,em1,ex1);
Frm2=X(:,em2:em3,ex1);
Frm2max=max(Frm2,[],2:3)
FRESH=Frm1./Frm2max;


%Humification Index (HIX)Ex 254, Em (435-480)/[(300-345)+(435-480)]
ex1=find(Ex==254);
em1=find(Em==435.901);
em2=find(Em==479.889);
em3=find(Em==301.089);
em4=find(Em==344.189);
Hix1=X(:,em1:em2,ex1);
Hix2=sum(Hix1')';
Hix3=X(:,em3:em4,ex1);
Hix4=sum(Hix3')';
HIX=Hix2./(Hix4+Hix2);

%Biological Index (BIX) Ex 310, Em 380/430
ex1=find(Ex==311);
em1=find(Em==380.721);
em2=find(Em==431.286);
Bi1=X(:,em1,ex1);
Bi2=X(:,em2,ex1);
BIX=Bi1./Bi2;

%FDOM Ex 365, Em 480
ex1=find(Ex==365);
em1=find(Em==479.889);
FDOM=X(:,em1,ex1);
        
%% 5. Export Fluoresence and Absorbance Indices to specified folder

FNout='SpectralIndices1.xlsx';
fprintf('writing summary data SpectralIndices.xlsx to your current directory... Have a beer meanwhile!\n')

%% Write Out ReadMe Section
xlswrite(FNout,cellstr('C'),'ReadMe','B3')
xlswrite(FNout,cellstr('A'),'ReadMe','B4')
xlswrite(FNout,cellstr('T'),'ReadMe','B5')
xlswrite(FNout,cellstr('B'),'ReadMe','B6')
xlswrite(FNout,cellstr('M'),'ReadMe','B7')
xlswrite(FNout,cellstr('FI'),'ReadMe','B8')
xlswrite(FNout,cellstr('HIX'),'ReadMe','B9')
xlswrite(FNout,cellstr('Fresh'),'ReadMe','B10')
xlswrite(FNout,cellstr('BIX'),'ReadMe','B11')
xlswrite(FNout,cellstr('FDOM'),'ReadMe','B12')


xlswrite(FNout,cellstr('SUVA254'),'ReadMe','B16')
xlswrite(FNout,cellstr('S275-295'),'ReadMe','B17')
xlswrite(FNout,cellstr('S350-400'),'ReadMe','B18')
xlswrite(FNout,cellstr('Sr'),'ReadMe','B19')

xlswrite(FNout,cellstr('Visible Humic-Like'),'ReadMe','C3')
xlswrite(FNout,cellstr('UV Humic-Like'),'ReadMe','C4')
xlswrite(FNout,cellstr('Protein-Like (Tryptophan)'),'ReadMe','C5')
xlswrite(FNout,cellstr('Protein-Like (Tyrosine)'),'ReadMe','C6')
xlswrite(FNout,cellstr('Microbial Humic-Like'),'ReadMe','C7')
xlswrite(FNout,cellstr('Related to contribution of microbial (high) vs terrestrial sources (low)'),'ReadMe','C8')
xlswrite(FNout,cellstr('Related to the degree of humification'),'ReadMe','C9')
xlswrite(FNout,cellstr('Related to freshly produced DOM'),'ReadMe','C10')
xlswrite(FNout,cellstr('Related to autotrophic activity (>1 value indicates recently generated DOM)'),'ReadMe','C11')
xlswrite(FNout,cellstr('Common in situ sensor Ex/Em pair, most closely resembles C peak'),'ReadMe','C12')

xlswrite(FNout,cellstr('Specific UV Absorbance at 254 nm'),'ReadMe','C16')
xlswrite(FNout,cellstr('Spectral slope from 275-295 nm'),'ReadMe','C17')
xlswrite(FNout,cellstr('Spectral slope from 350-400 nm'),'ReadMe','C18')
xlswrite(FNout,cellstr('Spectral slope ratio'),'ReadMe','C19')


xlswrite(FNout,cellstr('Excitation (nm)'),'ReadMe','D2')
xlswrite(FNout,cellstr('Max: 320-360'),'ReadMe','D3')
xlswrite(FNout,cellstr('260'),'ReadMe','D4')
xlswrite(FNout,cellstr('275'),'ReadMe','D5')
xlswrite(FNout,cellstr('275'),'ReadMe','D6')
xlswrite(FNout,cellstr('Max: 290-310'),'ReadMe','D7')
xlswrite(FNout,cellstr('370'),'ReadMe','D8')
xlswrite(FNout,cellstr('254'),'ReadMe','D9')
xlswrite(FNout,cellstr('310'),'ReadMe','D10')
xlswrite(FNout,cellstr('310'),'ReadMe','D11')
xlswrite(FNout,cellstr('365'),'ReadMe','D12')


xlswrite(FNout,cellstr('(A254/DOC)*100 (units: L per mgC-m)'),'ReadMe','D16')
xlswrite(FNout,cellstr('ln(slope275-295)'),'ReadMe','D17')
xlswrite(FNout,cellstr('ln(slope350-400)'),'ReadMe','D18')
xlswrite(FNout,cellstr('S275-295/S350-400'),'ReadMe','D19')

xlswrite(FNout,cellstr('Emission (nm)'),'ReadMe','E2')
xlswrite(FNout,cellstr('Max: 420-460'),'ReadMe','E3')
xlswrite(FNout,cellstr('Max: 400-460'),'ReadMe','E4')
xlswrite(FNout,cellstr('Max: 330-350'),'ReadMe','E5')
xlswrite(FNout,cellstr('Max: 295-315'),'ReadMe','E6')
xlswrite(FNout,cellstr('Max: 370-410'),'ReadMe','E7')
xlswrite(FNout,cellstr('470/520'),'ReadMe','E8')
xlswrite(FNout,cellstr('(435_480)/[(300_345)+(435_480)]'),'ReadMe','E9')
xlswrite(FNout,cellstr('380/Max: 420_435'),'ReadMe','E10')
xlswrite(FNout,cellstr('380/430'),'ReadMe','E11')
xlswrite(FNout,cellstr('480'),'ReadMe','E12')


xlswrite(FNout,cellstr('Related with DOM aromaticity'),'ReadMe','E16')
xlswrite(FNout,cellstr('Higher generally related with lower molecular weight DOM'),'ReadMe','E17')
xlswrite(FNout,cellstr('Higher values generally idicated more aromatic, higher molecular weight DOM'),'ReadMe','E18')
xlswrite(FNout,cellstr('Inverseley related with DOM molecular size'),'ReadMe','E19')


xlswrite(FNout,cellstr('Reference'),'ReadMe','F2')
xlswrite(FNout,cellstr('Wunsch et al., 2019'),'ReadMe','F3')
xlswrite(FNout,cellstr('Wunsch et al., 2019'),'ReadMe','F4')
xlswrite(FNout,cellstr('Wunsch et al., 2019'),'ReadMe','F5')
xlswrite(FNout,cellstr('Wunsch et al., 2019'),'ReadMe','F6')
xlswrite(FNout,cellstr('Wunsch et al., 2019'),'ReadMe','F7')
xlswrite(FNout,cellstr('McKnight et al., 2001'),'ReadMe','F8')
xlswrite(FNout,cellstr('Ohno 2002'),'ReadMe','F9')
xlswrite(FNout,cellstr('Parlanti et. al., 2000'),'ReadMe','F10')
xlswrite(FNout,cellstr('Huguet et al., 2009'),'ReadMe','F11')
xlswrite(FNout,cellstr('Downing et al., 2012'),'ReadMe','F12')

xlswrite(FNout,cellstr('Weishaar et al., 2003'),'ReadMe','F16')
xlswrite(FNout,cellstr('Helms et al., 2008'),'ReadMe','F17')
xlswrite(FNout,cellstr('Helms et al., 2008'),'ReadMe','F18')
xlswrite(FNout,cellstr('Helms et al., 2008'),'ReadMe','F19')

xlswrite(FNout,cellstr('DOI'),'ReadMe','G2')
xlswrite(FNout,cellstr('10.1039/C8AY02422G'),'ReadMe','G3')
xlswrite(FNout,cellstr('10.1039/C8AY02422G'),'ReadMe','G4')
xlswrite(FNout,cellstr('10.1039/C8AY02422G'),'ReadMe','G5')
xlswrite(FNout,cellstr('10.1039/C8AY02422G'),'ReadMe','G6')
xlswrite(FNout,cellstr('10.1039/C8AY02422G'),'ReadMe','G7')
xlswrite(FNout,cellstr('10.4319/lo.2001.46.1.0038'),'ReadMe','G8')
xlswrite(FNout,cellstr('10.1021/es0155276'),'ReadMe','G9')
xlswrite(FNout,cellstr('10.1016/S0146-6380(00)00124-8'),'ReadMe','G10')
xlswrite(FNout,cellstr('10.1016/j.orggeochem.2009.03.002'),'ReadMe','G11')
xlswrite(FNout,cellstr('10.4319/lom.2012.10.767'),'ReadMe','G12')

xlswrite(FNout,cellstr('10.1021/es030360x'),'ReadMe','G16')
xlswrite(FNout,cellstr('lo.2008.53.3.0955'),'ReadMe','G17')
xlswrite(FNout,cellstr('lo.2008.53.3.0955'),'ReadMe','G18')
xlswrite(FNout,cellstr('lo.2008.53.3.0955'),'ReadMe','G19')

xlswrite(FNout,cellstr('Analysis Notes'),'ReadMe','J3')
xlswrite(FNout,cellstr(special_notes),'ReadMe','J4')

%% Write Out Optical Properties

xlswrite(FNout,cellstr('Sample ID'),'Data_Final','A1')
xlswrite(FNout,cellstr('Sample Description'),'Data_Final','B1')
xlswrite(FNout,cellstr(SampleID),'Data_Final','A2')
xlswrite(FNout,cellstr(sample_description),'Data_Final','B2')

% Absorbance indices
xlswrite(FNout,cellstr('Abs 250nm'),'Data_Final','C1')
xlswrite(FNout,cellstr('Abs 254nm'),'Data_Final','D1')
xlswrite(FNout,cellstr('Abs 281nm'),'Data_Final','E1')
xlswrite(FNout,cellstr('Abs 350nm'),'Data_Final','F1')
xlswrite(FNout,cellstr('Abs 365nm'),'Data_Final','G1')
xlswrite(FNout,cellstr('Abs 412nm'),'Data_Final','H1')
xlswrite(FNout,cellstr('Abs 440nm'),'Data_Final','I1')
xlswrite(FNout,cellstr('A250/A365'),'Data_Final','J1')
xlswrite(FNout,cellstr('SUVA254'),'Data_Final','K1')
xlswrite(FNout,cellstr('S275-295'),'Data_Final','L1')
xlswrite(FNout,cellstr('r2 of the fit'),'Data_Final','M1')
xlswrite(FNout,cellstr('S350-400'),'Data_Final','N1')
xlswrite(FNout,cellstr('r2 of the fit'),'Data_Final','O1')
xlswrite(FNout,cellstr('Sr'),'Data_Final','P1')
%Fluorescence peaks
xlswrite(FNout,cellstr('C'),'Data_Final','Q1')
xlswrite(FNout,cellstr('A'),'Data_Final','R1')
xlswrite(FNout,cellstr('T'),'Data_Final','S1')
xlswrite(FNout,cellstr('B'),'Data_Final','T1')
xlswrite(FNout,cellstr('M'),'Data_Final','U1')
xlswrite(FNout,cellstr('FDOM'),'Data_Final','V1')
%Fluorescence indices
xlswrite(FNout,cellstr('FI'),'Data_Final','W1')
xlswrite(FNout,cellstr('HIX'),'Data_Final','X1')
xlswrite(FNout,cellstr('FRESH'),'Data_Final','Y1')
xlswrite(FNout,cellstr('BIX'),'Data_Final','Z1')

%Write Out Absorbance and Fluorescence Data
xlswrite(FNout,Abs250,'Data_Final','C2')
xlswrite(FNout,Abs254,'Data_Final','D2')
xlswrite(FNout,Abs281,'Data_Final','E2')
xlswrite(FNout,Abs350,'Data_Final','F2')
xlswrite(FNout,Abs365,'Data_Final','G2')
xlswrite(FNout,Abs412,'Data_Final','H2')
xlswrite(FNout,Abs440,'Data_Final','I2')
xlswrite(FNout,AbsRatio,'Data_Final','J2')
if DOCconcentration
xlswrite(FNout,SUVA254,'Data_Final','K2')
end
xlswrite(FNout,S275_296,'Data_Final','L2')
xlswrite(FNout,S275_296_r2,'Data_Final','M2')
xlswrite(FNout,S350_401,'Data_Final','N2')
xlswrite(FNout,S350_401_r2,'Data_Final','O2')
xlswrite(FNout,Sr(:,1),'Data_Final','P2')
xlswrite(FNout,Cpeak,'Data_Final','Q2')
xlswrite(FNout,Apeak,'Data_Final','R2')
xlswrite(FNout,Tpeak,'Data_Final','S2')
xlswrite(FNout,Bpeak,'Data_Final','T2')
xlswrite(FNout,Mpeak,'Data_Final','U2')
%Note Covers 5 fluorescence peaks - columns P-T
xlswrite(FNout,FDOM,'Data_Final','V2')
xlswrite(FNout,FI,'Data_Final','W2')
xlswrite(FNout,HIX,'Data_Final','X2')
xlswrite(FNout,FRESH,'Data_Final','Y2')
xlswrite(FNout,BIX,'Data_Final','Z2')

xlswrite(FNout,cellstr('Sample ID'),'Data_Final','A1')
xlswrite(FNout,cellstr(''),'ReadMe','A1')

end

        
        
        