#!/bin/bash

EXE=taketa_dl_jma_html
OPTIONS='--config /home/eric/1_Projects/JMA/par/0_com/config.nml --mode routine'

cat <<EOF>/etc/systemd/system/${EXE}.service
[Unit]
Description=${EXE}

[Service]
ExecStart=/usr/bin/${EXE} ${OPTIONS}
Restart=always

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl stop ${EXE}.service
make
make run
make install
systemctl enable ${EXE}.service
systemctl start ${EXE}.service
systemctl status ${EXE}.service

ps aux | rg ${EXE} 
