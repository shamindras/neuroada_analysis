%one easy example for PisCES and PisCES_cv
clear;
matfiles = dir('data/erp_cormats_us_cat_nonaft/post_stim/*.mat') ;
T = length(matfiles); % number of millisecond recordings
disp(T);
filenames = cell(T,1) ;
N = 85; % number of channels
A = zeros(N,N,T); % setup the multidimensional array
%dat = load(strcat(matfiles(1).folder, '/', matfiles(1).name));
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

disp(size(A));

%run PisCES with alpha = 0.05
alpha = 0.05;
disp(alpha*ones(T,2));

[Z] = PisCES(A,'T', alpha*ones(T,2),floor(N/10), 50); 

% Display the frequency histogram of the cluster assignments
hist(Z(:));

dat = A(:,:,1);
disp(dat);
disp(size(dat));

%run PisCES_cv
alphalist = [0.001 0.005 0.01 0.05 0.1 0.15];
[modu like] = pisces_cv(A, alphalist, 'T', floor(N/10), 100, 5);
