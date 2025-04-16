# Create docker_vol directory if it doesn't exist
if [ ! -f "./docker_vol" ]; then
    echo "Creating docker_vol directory for mounting the container"
    mkdir "docker_vol"
fi

docker-compose --env-file httr_settings.env build