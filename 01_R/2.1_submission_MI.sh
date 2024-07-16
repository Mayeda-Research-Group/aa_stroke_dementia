#!/bin/bash
#$ -cwd #uses current working directory
# error = Merged with joblog
#$ -o /u/home/y/yixzhou/AA_stroke_dem/joblogs/MI_joblog.$JOB_ID.$TASK_ID #creates a file called joblog.jobidnumber to write to. 
#$ -j y 
#$ -l h_rt=8:00:00,h_data=8G #requests 8 hours, 8GB of data (per core) 
#$ -pe shared 1 #requests 1 core
# Email address to notify
#$ -M $USER@mail #don't change this line, finds your email in the system 
# Notify when
#$ -m bea #sends you an email (b) when the job begins (e) when job ends (a) when job is aborted (error)
# submit array job:
# run an array of n jobs; this corresponds to $SGE_TASK_ID
#$ -t 1-5:1
## 

# load the job environment:
. /u/local/Modules/default/init/modules.sh
module load R/4.1.0-BIO #loads R/4.1.0 for use 
export OMP_NUM_THREADS=1 #uses max 1 thread (needs to match -pe shared)


echo "======"
echo SGE_TASK_ID=$SGE_TASK_ID      
R CMD BATCH --no-save --no-restore "--args scenario_num=$SGE_TASK_ID"  /u/home/y/yixzhou/AA_stroke_dem/XX.2.2_multiple_imputation.R /u/home/y/yixzhou/AA_stroke_dem/console_output/MI_output.$JOB_ID.$SGE_TASK_ID
echo R CMD BATCH --no-save --no-restore \'--args scenario_num=$SGE_TASK_ID \'  /u/home/y/yixzhou/AA_stroke_dem/XX.2.2_multiple_imputation.R /u/home/y/yixzhou/AA_stroke_dem/console_output/MI_output.$JOB_ID.$SGE_TASK_ID

