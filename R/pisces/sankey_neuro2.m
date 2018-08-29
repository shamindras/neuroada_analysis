clear;
clc;

% pre-stim - P35, Faces, Session1, Non-fisher transformed, non-absolute
% cormats, non-smoothed
matfiles = dir('../data/erp_cormats_us_cat_nonaft/pre_stim/*.mat') ;
T = length(matfiles); % number of millisecond recordings
disp(T);
filenames = cell(T,1) ;
N = 85; % number of channels
A = zeros(N,N,T); % setup the multidimensional array

for i = 1:T
    filenames{i} = matfiles(i).name;
    dat = load(strcat(matfiles(i).folder, '/', matfiles(i).name));
    dat2 = arrayfun(@(x) x > 0.25, dat.mat); % TODO: possibly remove this, 
                                           % just forcing an adjacency matrix 
                                           % by thresholding values that
                                           % are non-zero to equal 1, else
                                           % 0
    A(:,:,i) = dat2; %TODO: replace with dat.mat
    %A(:,:,i) = dat.mat; %TODO: replace with dat.mat
end

disp(size(A));

%run PisCES with alpha = 0.05
alpha = 0.05;
disp(alpha*ones(T,2));

[Z_pre_stim_01] = PisCES(A,'T', alpha*ones(T,2),floor(N/10), 50); 