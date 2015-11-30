from string import Template
from os import getcwd

numruns=10

project=getcwd().split('/')[-1]

runname=getcwd().split('/')[-2]

qsubfile = Template("""
#!/bin/sh
#PBS -j oe
#PBS -l mem=300mb
#PBS -V

RUN_NAME=${rname}
SCRATCH_DIR=/data/$$USER/$$RUN_NAME/r${run}
LOCAL_DIR=/home/shainen/randclust/build

mkdir -p $$SCRATCH_DIR

# Copy them from this directory to data director
cd $$LOCAL_DIR/$$RUN_NAME
cp -r ${prj} $$SCRATCH_DIR/

# Move to the data directory
cd $$SCRATCH_DIR/

# Run the script
time math -script ${prj}/runTWA.wl

# Remove the now-useless files
rm -r ${prj} 
""")

for rr in map(str,xrange(numruns)):
    with open(runname+"r_"+rr+".qsub", "w") as f:
        f.write(qsubfile.substitute(run=rr,rname=runname,prj=project))

