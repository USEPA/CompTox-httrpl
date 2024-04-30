------------------------------------------

Installing Full httrpl in Docker Container
------------------------------------------

This package currently includes an experimental script for building a docker container with all tools and dependencies needed to pipeline HTTr data.
The script uses docker 1.28 and if the server doesn't have that version available, the script will install this version in the user's virtual environment.
*This is needed to make use of a configuration file.* 

To build the docker image, make sure execute permission to the file are applied
and make sure that in your home directory the following files are present *(they will be copied into the docker container)*:

+ `.mongopw`, format is a json object: `[{"host":<>, "db":"sbox_<>", "user":<>, "passwd":<>,"authMechanism":"SCRAM-SHA-256", "authSource":"admin"}]`, this file can also be created with the `bin/addKey.py` script.
+ `.passwd-s3fs`, format is `<public key>:<private key>`, this file is needed to mount S3 bucket containing raw data files.

Edit `httr_settings.env` in the main `httrpl` directory to specify the MongoDB host to use for unit tests (`TEST_HOST=`...)

Once these are confirmed, run:
```bash
cd (localpath)/httrpl/
./build.sh
```
This will create an image and a service called httr. (that can be changed by editing both yml files, described below)

The initial build can take > 30 minutes on an 8-core system, but each step is cached so following builds will be faster

To update the volume name, edit `local-docker-compose.yml` with the folder name. It currently is 'httr-Tests' but may change.
Make that directory match your own directory

To run container:
```bash
docker-compose --file local-docker-compose.yml --file docker-compose.yml --env-file httr_settings.env up
```
or simply:
```bash
cd (localpath)/httrpl/
./up.sh
```

Please note this will read any variable defined in httr_settings. This works only for docker versions 1.28 and higher, so if your server has docker version < 1.28 (curently the 836 server), you need to source first to the venv_httr_app, because those docker versions < 1.28 don't support the `--env-file` flag, run:
```bash
cd (localpath)/httrpl/
. venv_httr_app
```
This will start a container based on the image just built and run the command located in the CMD section of the Dockerfile

The container stops automatically when the script is finished. Currently the python script is set up to run. Modify the start.sh file to invoke the right script.

#### Additional information:

+ While one script is running, you can connect to the running container from a different terminal window by typing:
    ```bash
    docker ps
    ```
+ This will give you the list of running containers. Identify the one you are running, then use: 
    ```bash
    docker exec -it <container id> bash
    ```
+ You would then be inside the container file system.
+ From there you can start python3 or R and run scripts from their respective environments.
+ Whenever the container stops running, you will get kicked out of your back door access
+ You may use `docker run <image name> sleep infinity` if you wish to get it running (instead of `docker-compose --file ...`) and then it will never stop (for troubleshooting purposes so you can leave the back door open)
+ To stop it in that case just use `docker kill <container id>`

