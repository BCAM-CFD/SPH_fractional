import sys
import os
import shutil
import os.path as op
import subprocess
import datetime
import shlex
from subprocess import PIPE, run


def modification_date(filename):
    t = os.path.getmtime(filename)
    return datetime.datetime.fromtimestamp(t)#, tz=datetime.timezone.utc)

#os.system("module load MPICH/3.3.2-GCC-9.3.0-default")
#subprocess.check_output(['zsh', '-c', 'source .zshrc && myMakeIntel'])
#(shlex.split(("module load MPICH/3.3.2-GCC-9.3.0-default")))
subprocess.call(shlex.split(("make AUTOCONF=: AUTOHEADER=: AUTOMAKE=: ACLOCAL=: -j 4")))
#os.system("myMakeIntel()")
#subprocess.call("myMakeIntel")
#date="_"+datetime.today().strftime('%Y_%m_%d-%H%M%S')
# try:
date=modification_date("./src/mcf")
# except:
# #date=datetime.datetime.today()
date="_"+date.strftime('%y%m%d-%H%M%S')
# date="_NEW"
print("\n#########\nAbout to copy (if newer)\nfile with suffix "+date+"\n#########")
bkp_file="./code_bkp/mcf"+date
working_file="/scratch/lsantelli/1apps/mcf"+date
shutil.copy2("./src/mcf",bkp_file)

processToCall= "rsync -azPS {} {}".format(bkp_file, working_file)
# subprocess.call(shlex.split(processToCall))
result = run(shlex.split(processToCall),stdout=PIPE, stderr=PIPE, universal_newlines=True )
#print("ciao",result.returncode, "due",result.stdout, "tre",result.stderr)
if result.stdout=="sending incremental file list\n":
    print("NOT COPIED:\nNO NEWER FILE CREATED")
    #print(result.stdout)
else:
    print("COPIED")
    print(result.stdout)
