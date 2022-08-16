LOCAL_PORT=8888
LOCAL_DIR=$PWD
docker run -it -p $LOCAL_PORT:8888 -v $LOCAL_DIR:/home/jovyan/urban-shift:rw esridocker/arcgis-api-python-notebook
