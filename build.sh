if [ ! -d "root" ]
then
  mkdir root
fi

if [ ! -d "etc" ] 
then
  mkdir etc
fi

cd root
if [ ! -d ".mngdb" ] 
then
  mkdir .mngdb
fi

cd ..

cp ~/.mngdb/passwd root/.mngdb/
cp ~/.mongopw root/.mongopw
cp ~/.passwd-s3fs etc/.passwd-s3fs


if ! (docker-compose -v | grep 1.28.5) ; then
    USE_LOCAL_DC=1
    echo Using Python venv for docker-compose
else
    USE_LOCAL_DC=
    echo Using system docker-compose
fi

if [ "$USE_LOCAL_DC" -a ! -e venv_httr_app ]; then
    # create venv if needed
    python3 -m venv venv_httr_app
    . venv_httr_app/bin/activate
    pip install --upgrade pip
    pip install docker-compose==1.28.5
    deactivate
fi

if [ "$USE_LOCAL_DC" ]; then
    . venv_httr_app/bin/activate
fi

docker-compose --env-file httr_settings.env build

if [ "$USE_LOCAL_DC" ]; then
    deactivate
fi
