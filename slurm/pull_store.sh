source .env;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/stores ./;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/data/processed/* ./data/processed
