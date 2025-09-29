. ~/.bashrc

if [ ! -e venv_httr_app ]
then
  python3.6 -m venv venv_httr_app
  . venv_httr_app/bin/activate
  pip install --upgrade pip
  pip install docker-compose==1.28.5
  deactivate
fi

. venv_httr_app/bin/activate
pip install importlib-metadata==4.8.3
pip install pymongo==3.13.0
pip install mongoengine
pip install deepdiff
pip install pandas
pip install psutil
pip install pytest-order
pip install jsonschema
pip install redis
pip install celery

cp ~/.mongopw .


export HTTR_CELERY_MP=Y

wget -nv -O redis-stable.tar.gz http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
cp ../redis.conf .
cd redis-stable
src/redis-server redis.conf &
cd ../httr/lib
celery -A tasks worker --loglevel=INFO  --concurrency=8 &

