# Data Hub Management

This directory contains tooling for managing datasets on the ESRI ArcGIS Data Hub site and ArcGIS Online (AGO).

## Installation and starting the Jupyter notebook

There are a few ways to install the [ArcGIS API for Python](https://developers.arcgis.com/python/guide/install-and-set-up/).


### Conda
If you use `conda` then you can install it into your environment of choice:

```sh
$ conda install -c esri arcgis
```

and work within a notebook environment 

```sh
$ jupyter notebook

# or jupyter lab if that is your preferred interface
$ jupyter-lab
```

This will typically open the notebook in your web browser, but if not you may need to copy the URL and token and put that in your browser's address bar.



### Docker
If you use `docker` then you can pull an image:

```sh
$ docker pull esridocker/arcgis-api-python-notebook
```

and then run it in a container with a shared volume so your filesystem is available within the docker context.

The file `run_docker_jupyter.sh` does this for you:

```sh
$ ./start_server.sh &
```

or you can run the docker command yourself:

```sh
$ docker run -it \
  -p 8888:8888 \
  -v $PWD:/home/jovyan/urban-shift:rw \
  esridocker/arcgis-api-python-notebook
```

Running the notebook within a Docker container will not instruct your browser to open the notebook page, so you will need to copy and paste the full URL into your address bar.
Be sure you are using the correct port on your local machine (by default `8888`), though this can be overridden.


## Using the notebook

The file `datahub_management.ipynb` is the main interface for creating and updating datasets on the ArcGIS Data Hub.
In that notebook are a series of examples and demonstrations of uploading and publishing different types of datasets (namely vector and raster).

The file `datahub_management.py` is a library of functions that support data management operations.
This library is imported and used through the notebook.





