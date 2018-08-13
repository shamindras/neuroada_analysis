%one easy example for PisCES and PisCES_cv

matfiles = dir('data/*.mat') ;
T = length(matfiles); % number of millisecond recordings
filenames = cell(N,1) ;
N = 85; % number of channels
A = zeros(N,N,T); % setup the multidimensional array
for t = 1:T;
    filenames{i} = matfiles(i).name;
    data = load(matfiles(i).name);
    A(:,:,t) = data;
end

%run PisCES with alpha = 0.1
[Z] = PisCES(A,'T', 0.1*ones(T,2)); 

%run PisCES_cv
alphalist = [0.05 0.1 0.15];
[modu like] = pisces_cv(A, alphalist);
