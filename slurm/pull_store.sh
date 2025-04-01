source .env;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/_targets .;
