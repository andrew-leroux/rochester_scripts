#!/bin/sh
#$ -cwd
#$ -t 1-1000
#$ -R y
#$ -l mem_free=100G,h_vmem=100G
module load conda_R
Rscript process_raw_data_cluster.R $SGE_TASK_ID 1000
