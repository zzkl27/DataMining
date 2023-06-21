# zzkl27/data_analysis:cuda11.6
FROM pytorch/pytorch:2.0.1-cuda11.7-cudnn8-devel
ENV TZ=Asia/Seoul
ENV DEBIAN_FRONTEND noninteractive
WORKDIR /root

RUN apt-get update -y && apt-get upgrade -y && apt install -y fonts-dejavu unixodbc-dev libcurl4-openssl-dev libzmq3-dev gdebi-core \
git vim gcc build-essential pkg-config openssl curl wget expect cmake \
libgtk-3-dev libavcodec-dev libavformat-dev libswscale-dev libv4l-dev libxvidcore-dev libx264-dev \
libjpeg-dev libpng-dev libtiff-dev \
gfortran openexr libatlas-base-dev libtbb2 libtbb-dev libdc1394-22-dev && \
apt install -y --no-install-recommends software-properties-common dirmngr && \
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
apt install -y --no-install-recommends r-base-dev && \
apt-get clean && rm -rf /var/lib/apt/lists/*

RUN pip install jupyterlab opencv-python tensorboard matplotlib scikit-learn ipywidgets
RUN jupyter lab --generate-config

# 주피터 서버 설정
RUN echo "c.ServerApp.password = u\"argon2:\$argon2id\$v=19\$m=10240,t=10,p=8\$E5kTv3MH5/G1ewc47C2SbQ\$UGVYYkXpDMbxIv02ULHMFU5Y2Yulh2haCOlNFm9ZMAI\"" >> /root/.jupyter/jupyter_lab_config.py && \
echo "c.ServerApp.ip = '0.0.0.0'" >> /root/.jupyter/jupyter_lab_config.py && \
echo "c.ServerApp.notebook_dir = '/workspace'" >> /root/.jupyter/jupyter_lab_config.py && \
echo "c.ServerApp.allow_root = True" >> /root/.jupyter/jupyter_lab_config.py && \
echo "c.ServerApp.open_browser = False" >> /root/.jupyter/jupyter_lab_config.py && \
echo "c.ServerApp.port = 8888" >> /root/.jupyter/jupyter_lab_config.py && \
echo "install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'IRkernel', 'pbdZMQ', 'uuid', 'digest', 'tidyverse', 'mlr3', 'caret')); IRkernel::installspec()" > test.R && \
Rscript test.R

RUN rm /root/*
WORKDIR /workspace
ENV WORKSPACE=/workspace 
ENV DATA=$WORKSPACE/leak WORK=$WORKSPACE/work STORAGE=$WORKSPACE/SSD
CMD jupyter lab > /root/jupyter.log
