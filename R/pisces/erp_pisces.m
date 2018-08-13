%one easy example for PisCES and PisCES_cv
clear;
matfiles = dir('data/erp_cormats_us_cat_nonaft/post_stim_200/*.mat') ;
T = length(matfiles); % number of millisecond recordings
disp(T);
filenames = cell(T,1) ;
N = 85; % number of channels
A = zeros(N,N,T); % setup the multidimensional array
%dat = load(strcat(matfiles(1).folder, '/', matfiles(1).name));
for i = 1:T
    filenames{i} = matfiles(i).name;
    dat = load(strcat(matfiles(i).folder, '/', matfiles(i).name));
    A(:,:,i) = dat.mat;
end

%run PisCES with alpha = 0.05
alpha = 0.005;
[Z] = PisCES(A,'T', alpha*ones(T,2)); 

dat = A(:,:,1);
disp(dat);
disp(size(dat));

%run PisCES_cv
%alphalist = [0.001 0.005 0.01 0.05 0.1 0.15];
%[modu like] = pisces_cv(A, alphalist);
