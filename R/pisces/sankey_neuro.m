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
    dat2 = arrayfun(@(x) abs(x) > 0.25, dat.mat); % TODO: possibly remove this, 
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

% Let's subset the A matrix to just be for 5 milliseconds
%A_rec = A(:, :, 1:5);
%A_rec2 = 	(A_rec);

for i=1:5
    A_rec{i} = A(:, :, i); % store in cell array
end

Z = Z_pre_stim_01(1:5, :)'; %TODO: Note we have transposed here to be consistent with the usage script!

K = max(max(Z));
n = size(Z, 1);

% We have the data and pisces community networks for each ms
% Let's run the Sankey for the first 5 milliseconds

% 1) initialize some parameters
% (the layout algorithm needs to know these in order to find a good layout)
% ################
% to reduce clutter, only draw flows that have at least this many nodes
param.min_flow_size = n/K^2;
% or comprise at least this fraction of its start cluster and destiation cluster
param.frac_min_flow_size = .15;

% 2) get ready for plots
% ################
% find a good layout (relabeling the clusters) and assemble the tables
% needed to draw the plots
[newZ] = layout_timeline(A_rec, Z, param);
[flow_rec, cluster_rec] = create_sankey_tables(newZ, A_rec);

% 3) make some plots
% ################

% draw a sankey diagram (not useful if only two networks)
param.draw_whole_timeline = 1;
param.draw_paired_plots = 0;
param.show_density_by_greyscale = 1; 
[sankey_param] = make_timeline_and_paired_plots(newZ, A_rec, flow_rec, cluster_rec, param);

% draw the flows for two time steps and their adjacency matrices
param.add_class_labels = 1;
param.draw_whole_timeline = 0;
param.draw_paired_plots = 1;
param.which_paired_plots = 2; % just times 1 & 2, make [1 2] to add a plot for times 2 & 3, etc
[paired_param] = make_timeline_and_paired_plots(newZ, A_rec, flow_rec, cluster_rec, param);

% like before, but only include the nodes that are in the large flows
% note: output arguments are helpful if you want to draw more stuff on top
% of this plot later.
% sm_flow, sm_cluster, sm_param: analogous to previous tables and param
% sm_Z and sm_K are only used if set_common_ordering = 1
% sm_Z identifies which flow each node in the plot belongs to
% sm_K identifies which cluster each node in the plot belongs to
set_common_ordering = 0;
paired_param.show_density_by_greyscale = 0; 
[sm_flow, sm_cluster, sm_Z, sm_K, sm_param] = make_compressed_paired_plots(newZ(:,1:2), ...
    flow_rec(1), A_rec(1:2), paired_param, set_common_ordering);

% like previous, but make the adjacency matrices have a common ordering
% (may be easier to compare them visually)
set_common_ordering = 1;
paired_param.max_clust1 = 5; % if you want to only show first 5 clusters
[sm_flow, sm_cluster, sm_Z, sm_K, sm_param] = make_compressed_paired_plots(newZ(:,1:2), ...
    flow_rec(1), A_rec(1:2), paired_param, set_common_ordering);