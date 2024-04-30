FROM r-base:3.6.0 AS RBASE

ADD httr httr
ADD etc etc
ADD httrlib httrlib
ADD root root

RUN cp /lib/x86_64-linux-gnu/libcrypt.so.1 /lib/. \
&& apt-get update && apt-get install -y gcc-9-base libgcc-9-dev libc6-dev libssl-dev libsasl2-dev libz-dev pkg-config libxml2 libcurl4-openssl-dev libxml2-dev \
&& chmod 755 httr/scripts/package_loader.sh \
&& httr/scripts/./package_loader.sh \
&& Rscript httr/scripts/requirements.r \
&& chmod 755 httr/scripts/package_fixer.sh \
&& httr/scripts/./package_fixer.sh \
&& chmod 755 httr/scripts/httrlib_builder_no_pdf.sh \
&& httr/scripts/./httrlib_builder_no_pdf.sh \
&& chmod 600 /etc/.passwd-s3fs 

FROM continuumio/miniconda3:4.10.3p0-alpine

# needed for conda integration
SHELL ["/bin/bash", "-c"]

RUN conda install -c conda-forge python=3.6 \
 && conda install -c bioconda samtools=1.09 \
 && conda install -c anaconda ncurses \
 && conda install -c biobuilds hisat2 \
 && conda install -c conda-forge pandas \
 && conda install -c conda-forge pymongo \
 && conda install -c conda-forge mongoengine \
 && conda install -c conda-forge s3fs-fuse \
 && conda install -c conda-forge pytest \
 && conda install -c conda-forge deepdiff \
 && conda install -c anaconda psutil\
 && pip install pytest-order \
 && pip install jsonschema \
 && pip install celery

## change workdir to /
WORKDIR /

COPY --from=0 . .
RUN chmod 600 /etc/.passwd-s3fs

## Set the directory where you want to mount your s3 bucket
ARG S3_MOUNT_DIRECTORY=/httr/bucket/mountpoint
ENV S3_MOUNT_DIRECTORY=/httr/bucket/mountpoint

## Replace with your s3 bucket name
ARG S3_BUCKET_NAME=edap-ncct-external
ENV S3_BUCKET_NAME=edap-ncct-external

## change workdir to /
WORKDIR /
CMD ["bash", "/httr/scripts/start.sh"]