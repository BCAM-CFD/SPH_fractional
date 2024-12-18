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


def get_latest_commit_hash():
    # Get the latest commit hash from the current Git repository
    result = subprocess.run(["git", "rev-parse", "HEAD"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        raise Exception("Failed to get the latest commit hash. Make sure you are in a Git repository.")
    return result.stdout.decode('utf-8').strip()

def append_commit_hash_to_program_name(program_name):
    commit_hash = get_latest_commit_hash()
    new_program_name = f"{program_name}_{commit_hash}"
    return new_program_name

def check_commit_already_done(commit_hash):
    backup_dir = "./code_bkp/"
    for filename in os.listdir(backup_dir):
        if commit_hash in filename:
            return True
    return False

def ask_user_confirmation():
    while True:
        user_input = input("The same commit has been already done at a different date. Do you want to proceed? (yes/no): ").strip().lower()
        if user_input in ["yes", "no"]:
            return user_input == "yes"
        print("Please enter 'yes' or 'no'.")

commit_hash = get_latest_commit_hash()
if check_commit_already_done(commit_hash):
    if not ask_user_confirmation():
        print("Operation aborted by the user.")
        sys.exit(0)

program_name = ""
github_program_name = append_commit_hash_to_program_name(program_name)
print(f"Program name with commit hash: {github_program_name}")




print("\n#########\nAbout to copy (if newer)\nfile with suffix "+date+"\n#########")
bkp_file="./code_bkp/mcf"+date+github_program_name
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






