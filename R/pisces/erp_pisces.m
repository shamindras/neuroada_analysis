clear;
clc;

% pre-stim - P35, Faces, Session1, Non-fisher transformed, non-absolute
% cormats, non-smoothed
matfiles = dir('data/erp_cormats_us_cat_nonaft/pre_stim/*.mat') ;
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
        
% Display the frequency histogram of the cluster assignments
hist(Z_pre_stim_01(:));
disp(size(Z_pre_stim_01));


clc;
% post-stim - P35, Faces, Session1, Non-fisher transformed, non-absolute
% cormats, non-smoothed
% MS 501 - 1000

matfiles = dir('data/erp_cormats_us_cat_nonaft/post_stim_501_to_1000/*.mat') ;
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
end

%disp(size(A));
%dat = A(:,:,1);
%disp(dat);
%disp(size(dat));

%run PisCES with alpha = 0.05
alpha = 0.05;
disp(alpha*ones(T,2));

[Z_post_stim_01] = PisCES(A,'T', alpha*ones(T,2),floor(N/10), 50); 

% Display the frequency histogram of the cluster assignments
hist(Z_post_stim_01(:));
disp(size(Z_post_stim_01));

%run PisCES_cv
%alphalist = [0.001 0.005 0.01 0.05 0.1 0.15];
%[modu like] = pisces_cv(A, alphalist, 'T', floor(N/10), 100, 5);

clc;
% post-stim - P35, Faces, Session1, Non-fisher transformed, non-absolute
% cormats, non-smoothed
% MS 1001 - 1500

matfiles = dir('data/erp_cormats_us_cat_nonaft/post_stim_1001_to_1500/*.mat') ;
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
end

%disp(size(A));
%dat = A(:,:,1);
%disp(dat);
%disp(size(dat));

%run PisCES with alpha = 0.05
alpha = 0.05;
disp(alpha*ones(T,2));

[Z_post_stim_02] = PisCES(A,'T', alpha*ones(T,2),floor(N/10), 50); 

% Display the frequency histogram of the cluster assignments
hist(Z_post_stim_02(:));
disp(size(Z_post_stim_02));

%run PisCES_cv
%alphalist = [0.001 0.005 0.01 0.05 0.1 0.15];
%[modu like] = pisces_cv(A, alphalist, 'T', floor(N/10), 100, 5);

hist(Z_pre_stim_01(:));
hist(Z_post_stim_01(:));
hist(Z_post_stim_02(:));