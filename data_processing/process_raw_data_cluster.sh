#!/bin/sh
#$ -cwd
#$ -t 1-1000
#$ -R y
#$ -l mem_free=40G,h_vmem=40G
module load conda_R
Rscript process_raw_data_cluster.R $SGE_TASK_ID
