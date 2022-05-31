function Xu = undilute(Xd,dilfac) 
%
% <strong>Syntax</strong>
%
%   Xu=<strong>undilute</strong>(Xd,dilfac) 
%
% <a href="matlab: doc undilute">help for undilute</a> <- click on the link

%Obtain fluorescence intensities in an undiluted EEM dataset
%Inputs
%       Xd: 3d EEM dataset (n x em x ex) in which samples have been diluted
%   dilfac: dilution factors applying to each of the n samples
%           The order of data in dilfac must match the order of EEMs in Xd
%           note: for a 4x dilution then dilfac =1/4 = 0.25
%                 for a 10x dilution then dilfac =1/10 =0.1
%Outputs
%       Xu: 3d EEM dataset corrected for dilution
%
% Notice:
% This mfile is part of the drEEM toolbox. Please cite the toolbox
% as follows:
%
% Murphy K.R., Stedmon C.A., Graeber D. and R. Bro, Fluorescence
%     spectroscopy and multi-way techniques. PARAFAC, Anal. Methods, 2013, 
%     DOI:10.1039/c3ay41160e. 
%
% undilute: Copyright (C) 2013 Kathleen R. Murphy
% The University of New South Wales
% Dept Civil and Environmental Engineering
% Water Research Center
% UNSW 2052
% Sydney
% krm@unsw.edu.au
%
% $ Version 0.1.0 $ September 2013 $ First Release


Xu=Xd./repmat(dilfac,[1,size(Xd,2),size(Xd,3)]);