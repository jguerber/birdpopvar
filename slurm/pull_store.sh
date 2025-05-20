source .env;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/stores/ ./stores/;
rsync -azP $HPC_REMOTE:$HPC_REMOTE_PATH/data/processed/* ./data/processed/
