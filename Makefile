.PHONY : conda_all conda_dev0 conda_dev1 conda_prod0 clean

# Create the various conda environments
conda_all:
	conda env create -f=./conda_envs/sklearndev0/environment.yml
	conda env create -f=./conda_envs/sklearndev1/environment.yml
	conda env create -f=./conda_envs/sklearndev2/environment.yml
	conda env create -f=./conda_envs/sklearnprod0/environment.yml
	conda env create -f=./conda_envs/sklearnprod1/environment.yml

conda_dev0:
	conda env create -f=./conda_envs/sklearndev0/environment.yml

conda_dev1:
	conda env create -f=./conda_envs/sklearndev1/environment.yml

conda_dev2:
	conda env create -f=./conda_envs/sklearndev2/environment.yml

conda_prod0:
	conda env create -f=./conda_envs/sklearnprod0/environment.yml

conda_prod1:
	conda env create -f=./conda_envs/sklearnprod1/environment.yml

conda_rem_reinst_prod0:
	conda remove --name sklearnprod0 --all
	conda_prod0

conda_rem_reinst_prod1:
	conda remove --name sklearnprod1 --all
	conda_prod1

clean:
	find . -name .DS_Store -print0 | xargs -0 rm -rf
	find . -name .Rhistory -print0 | xargs -0 rm -rf
	find . -type d -name *_files -print0 | xargs -0 rm -rf
	find . -type d -name *_cache -print0 | xargs -0 rm -rf
