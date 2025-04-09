source .env;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/_targets .;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/data/processed/* ./data/processed/
